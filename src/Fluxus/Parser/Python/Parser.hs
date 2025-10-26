{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Python parser that converts tokens to AST
module Fluxus.Parser.Python.Parser
  ( -- * Parser types
    PythonParser
  , PythonParseError(..)
    -- * Main parsing functions
  , parsePython
  , runPythonParser
    -- * Statement parsers
  , parseStatement
  , parseExprStmt
  , parseAssignment
  , parseIfStmt
  , parseWhileStmt
  , parseForStmt
  , parseFuncDef
  , parseClassDef
    -- * Expression parsers
  , parseExpression
  , parseAtom
    -- * Utility parsers
  , parseBlock
  , parseParameters
  , parseArguments
  , parsePattern
  ) where

import Control.Monad (void)
import Control.Applicative ((<|>), optional, many, some)
import Data.Functor (($>))
import qualified Control.Applicative as A
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (many, some)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Fluxus.AST.Common as Common
import Fluxus.AST.Python
import qualified Fluxus.Parser.Python.Lexer as Lexer
import Fluxus.Parser.Python.Lexer (PythonToken(..), Keyword(..), Delimiter(..))

-- | Simple chainl1 implementation for left-associative operators
chainl1 :: PythonParser a -> PythonParser (a -> a -> a) -> PythonParser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = (do
      f <- op
      y <- p
      rest (f x y)) <|> return x

-- | Parser error type
data PythonParseError = PythonParseError
  { peMessage :: !Text
  , peLocation :: !SourceSpan
  } deriving (Eq, Show)

-- | Python parser type
type PythonParser = Parsec Void [Located PythonToken]

-- | Run the Python parser
runPythonParser :: Text -> [Located PythonToken] -> Either (ParseErrorBundle [Located PythonToken] Void) PythonAST
runPythonParser filename tokensList = parse parsePython (T.unpack filename) tokensList

-- | Main parser entry point
parsePython :: PythonParser PythonAST
parsePython = do
  module_ <- parseModule
  eof
  return $ PythonAST module_

-- | Parse a Python module
parseModule :: PythonParser PythonModule
parseModule = do
  skipNewlinesAndComments
  imports <- many (try parseImportStmt <* skipNewlinesAndComments)
  body <- parseModuleBody
  
  -- Extract docstring from module body
  let (docstring, bodyStmts) = extractDocstring body
  
  return $ PythonModule
    { pyModuleName = Nothing  -- Will be filled in later
    , pyModuleDoc = docstring
    , pyModuleImports = imports
    , pyModuleBody = bodyStmts
    }

-- | Parse module body with better handling of top-level statements
parseModuleBody :: PythonParser [Located PythonStmt]
parseModuleBody = many $ do
  skipNewlinesAndComments
  stmt <- parseStatement
  skipNewlinesAndComments
  return stmt

-- | Parse statements
parseStatement :: PythonParser (Located PythonStmt)
parseStatement = located $ choice
  [ try parseFuncDef
  , try parseClassDef
  , try parseIfStmt
  , try parseWhileStmt
  , try parseForStmt
  , try parseReturnStmt
  , try parseBreakStmt
  , try parseContinueStmt
  , try parsePassStmt
  , try parseImportStmt'
  , try parseAugAssignment
  , try parseAssignment
  , parseExprStmt
  ]

-- | Parse expression statements
parseExprStmt :: PythonParser PythonStmt
parseExprStmt = do
  expr <- parseExpression
  return $ PyExprStmt expr

-- | Parse assignment statements
parseAssignment :: PythonParser PythonStmt  
parseAssignment = do
  -- Use lookAhead to check if this looks like an assignment before consuming tokens
  lookAhead $ do
    _ <- parsePattern
    choice
      [ void $ delimiterP DelimComma  -- x, y = ...
      , void $ satisfy isAssignOp      -- x = ...  
      ]
  -- Now actually parse the assignment
  targets <- parsePattern `sepBy1` delimiterP DelimComma
  void $ satisfy isAssignOp
  value <- parseExpression
  return $ PyAssign targets value
  where
    isAssignOp (Located _ (TokenOperator Lexer.OpAssign)) = True
    isAssignOp _ = False

-- | Parse augmented assignment
parseAugAssignment :: PythonParser PythonStmt
parseAugAssignment = do
  -- Use lookAhead to check if this looks like an augmented assignment
  lookAhead $ do
    _ <- parsePattern
    void $ satisfy isAugOp
  -- Now actually parse the augmented assignment
  target <- parsePattern
  op <- parseAugOp
  value <- parseExpression
  return $ PyAugAssign target op value
  where
    isAugOp (Located _ (TokenOperator Lexer.OpPlusAssign)) = True
    isAugOp (Located _ (TokenOperator Lexer.OpMinusAssign)) = True
    isAugOp (Located _ (TokenOperator Lexer.OpMultAssign)) = True
    isAugOp (Located _ (TokenOperator Lexer.OpDivAssign)) = True
    isAugOp (Located _ (TokenOperator Lexer.OpModAssign)) = True
    isAugOp (Located _ (TokenOperator Lexer.OpPowerAssign)) = True
    isAugOp (Located _ (TokenOperator Lexer.OpFloorDivAssign)) = True
    isAugOp _ = False
    
    parseAugOp = do
      Located _ token <- anySingle
      case token of
        TokenOperator Lexer.OpPlusAssign -> return OpAdd
        TokenOperator Lexer.OpMinusAssign -> return OpSub
        TokenOperator Lexer.OpMultAssign -> return OpMul
        TokenOperator Lexer.OpDivAssign -> return Common.OpDiv
        TokenOperator Lexer.OpModAssign -> return Common.OpMod
        TokenOperator Lexer.OpPowerAssign -> return OpPow
        TokenOperator Lexer.OpFloorDivAssign -> return Common.OpFloorDiv
        _ -> fail "Expected augmented assignment operator"

-- | Parse if statements
parseIfStmt :: PythonParser PythonStmt
parseIfStmt = do
  void $ keywordP KwIf
  condition <- parseExpression
  void $ delimiterP DelimColon
  thenBody <- parseBlock
  elseBody <- option [] $ do
    void $ keywordP KwElse
    void $ delimiterP DelimColon
    parseBlock
  return $ PyIf condition thenBody elseBody

-- | Parse while statements
parseWhileStmt :: PythonParser PythonStmt
parseWhileStmt = do
  void $ keywordP KwWhile
  condition <- parseExpression
  void $ delimiterP DelimColon
  body <- parseBlock
  elseBody <- option [] $ do
    void $ keywordP KwElse
    void $ delimiterP DelimColon
    parseBlock
  return $ PyWhile condition body elseBody

-- | Parse for statements
parseForStmt :: PythonParser PythonStmt
parseForStmt = do
  void $ keywordP KwFor
  target <- parsePattern
  void $ keywordP KwIn
  iter <- parseExpression
  void $ delimiterP DelimColon
  body <- parseBlock
  elseBody <- option [] $ do
    void $ keywordP KwElse
    void $ delimiterP DelimColon
    parseBlock
  return $ PyFor target iter body elseBody

-- | Parse function definitions
parseFuncDef :: PythonParser PythonStmt
parseFuncDef = do
  isAsync <- option False (keywordP KwAsync $> True)
  void $ keywordP KwDef
  name <- parseIdentifier
  void $ delimiterP DelimLeftParen
  params <- parseParameters
  void $ delimiterP DelimRightParen
  returnType <- optional $ do
    void $ operator' Lexer.OpArrow
    parseTypeExpr
  void $ delimiterP DelimColon
  body <- parseBlock
  
  -- Extract docstring from function body
  let (docstring, bodyStmts) = extractDocstring body
  
  let funcDef = PythonFuncDef
        { pyFuncName = name
        , pyFuncDecorators = []  -- TODO: Parse decorators
        , pyFuncParams = params
        , pyFuncReturns = returnType
        , pyFuncBody = bodyStmts
        , pyFuncDoc = docstring
        , pyFuncIsAsync = isAsync
        }
  
  return $ if isAsync then PyAsyncFuncDef funcDef else PyFuncDef funcDef

-- | Parse class definitions
parseClassDef :: PythonParser PythonStmt
parseClassDef = do
  void $ keywordP KwClass
  name <- parseIdentifier
  bases <- option [] $ do
    void $ delimiterP DelimLeftParen
    parseExpression `sepBy` delimiterP DelimComma
    <* void (delimiterP DelimRightParen)
  void $ delimiterP DelimColon
  body <- parseBlock
  
  -- Extract docstring from class body
  let (docstring, bodyStmts) = extractDocstring body
  
  return $ PyClassDef $ PythonClassDef
    { pyClassName = name
    , pyClassDecorators = []  -- TODO: Parse decorators
    , pyClassBases = bases
    , pyClassKeywords = []    -- TODO: Parse keywordP arguments
    , pyClassBody = bodyStmts
    , pyClassDoc = docstring
    }

-- | Parse return statements
parseReturnStmt :: PythonParser PythonStmt
parseReturnStmt = do
  void $ keywordP KwReturn
  value <- optional parseExpression
  return $ PyReturn value

-- | Parse break statements
parseBreakStmt :: PythonParser PythonStmt
parseBreakStmt = keywordP KwBreak $> PyBreak

-- | Parse continue statements
parseContinueStmt :: PythonParser PythonStmt
parseContinueStmt = keywordP KwContinue $> PyContinue

-- | Parse pass statements
parsePassStmt :: PythonParser PythonStmt
parsePassStmt = keywordP KwPass $> PyPass

-- | Parse import statements
parseImportStmt :: PythonParser (Located PythonImport)
parseImportStmt = located $ choice
  [ try parseFromImport
  , parseRegularImport
  ]

parseImportStmt' :: PythonParser PythonStmt
parseImportStmt' = PyImport . (:[]) <$> parseImportStmt

parseRegularImport :: PythonParser PythonImport
parseRegularImport = do
  void $ keywordP KwImport
  modName <- parseModuleName
  alias <- optional $ do
    void $ keywordP KwAs
    parseIdentifier
  return $ ImportModule modName alias

parseFromImport :: PythonParser PythonImport
parseFromImport = do
  void $ keywordP KwFrom
  modName <- parseModuleName
  void $ keywordP KwImport
  choice
    [ do
        void $ operator' Lexer.OpMult
        return $ ImportFromStar modName
    , do
        names <- parseIdentifier `sepBy1` delimiterP DelimComma
        return $ ImportFrom modName names []  -- TODO: Parse aliases
    ]

-- | Parse expressions
parseExpression :: PythonParser (Located PythonExpr)
parseExpression = parseOrExpr

parseOrExpr :: PythonParser (Located PythonExpr)
parseOrExpr = do
  first <- parseAndExpr
  rest <- many $ do
    void $ keywordP KwOr
    parseAndExpr
  return $ foldl (\acc expr -> located' $ PyBoolOp OpOr [acc, expr]) first rest

parseAndExpr :: PythonParser (Located PythonExpr)
parseAndExpr = do
  first <- parseNotExpr
  rest <- many $ do
    void $ keywordP KwAnd
    parseNotExpr
  return $ foldl (\acc expr -> located' $ PyBoolOp OpAnd [acc, expr]) first rest

parseNotExpr :: PythonParser (Located PythonExpr)
parseNotExpr = choice
  [ do
      void $ keywordP KwNot
      expr <- parseNotExpr
      return $ located' $ PyUnaryOp OpNot expr
  , parseComparison
  ]

parseComparison :: PythonParser (Located PythonExpr)
parseComparison = do
  first <- parseArithExpr
  rest <- many $ do
    op <- parseCompOp
    expr <- parseArithExpr
    return (op, expr)
  case rest of
    [] -> return first
    _ -> return $ located' $ PyComparison (map fst rest) (first : map snd rest)

parseCompOp :: PythonParser ComparisonOp
parseCompOp = choice
  [ operator' Lexer.OpEq $> OpEq
  , operator' Lexer.OpNe $> OpNe
  , operator' Lexer.OpLe $> OpLe
  , operator' Lexer.OpGe $> OpGe
  , operator' Lexer.OpLt $> OpLt
  , operator' Lexer.OpGt $> OpGt
  , keywordP KwIs $> OpIs
  , keywordP KwIn $> OpEq  -- Using OpEq as placeholder for now
  ]

parseArithExpr :: PythonParser (Located PythonExpr)
parseArithExpr = chainl1 parseTermExpr parseAddOp
  where
    parseAddOp = choice
      [ operator' Lexer.OpPlus $> (\l r -> located' $ PyBinaryOp OpAdd l r)
      , operator' Lexer.OpMinus $> (\l r -> located' $ PyBinaryOp OpSub l r)
      ]

parseTermExpr :: PythonParser (Located PythonExpr)
parseTermExpr = chainl1 parseFactorExpr parseMulOp
  where
    parseMulOp = choice
      [ operator' Lexer.OpMult $> (\l r -> located' $ PyBinaryOp OpMul l r)
      , operator' Lexer.OpDiv $> (\l r -> located' $ PyBinaryOp Common.OpDiv l r)
      , operator' Lexer.OpMod $> (\l r -> located' $ PyBinaryOp Common.OpMod l r)
      , operator' Lexer.OpFloorDiv $> (\l r -> located' $ PyBinaryOp Common.OpFloorDiv l r)
      ]

parseFactorExpr :: PythonParser (Located PythonExpr)
parseFactorExpr = choice
  [ do
      op <- choice
        [ operator' Lexer.OpPlus $> OpPositive
        , operator' Lexer.OpMinus $> OpNegate
        , operator' Lexer.OpBitNot $> Common.OpBitNot
        ]
      expr <- parseFactorExpr
      return $ located' $ PyUnaryOp op expr
  , parsePowerExpr
  ]

parsePowerExpr :: PythonParser (Located PythonExpr)
parsePowerExpr = do
  base <- parseAtomExpr
  power <- optional $ do
    void $ operator' Lexer.OpPower
    parseFactorExpr
  case power of
    Nothing -> return base
    Just p -> return $ located' $ PyBinaryOp OpPow base p

parseAtomExpr :: PythonParser (Located PythonExpr)
parseAtomExpr = do
  atom <- parseAtom
  trailers <- many parseTrailer
  return $ foldl applyTrailer atom trailers
  where
    applyTrailer expr trailer = trailer expr

parseAtom :: PythonParser (Located PythonExpr)
parseAtom = choice
  [ located $ try parseLiteral
  , located parseParenExpr
  , located parseIdentifierExpr
  , located parseListLiteral
  , located parseTupleLiteral
  , located parseDictLiteral
  ]

parseLiteral :: PythonParser PythonExpr
parseLiteral = do
  Located _ token <- anySingle
  case token of
    TokenString text -> return $ PyLiteral $ PyString text
    TokenFString text exprs -> return $ PyLiteral $ PyFString text []  -- TODO: Parse embedded expressions
    TokenNumber text isFloat ->
      if isFloat
        then return $ PyLiteral $ PyFloat (read $ T.unpack text)
        else return $ PyLiteral $ PyInt (read $ T.unpack text)
    TokenKeyword KwTrue -> return $ PyLiteral $ PyBool True
    TokenKeyword KwFalse -> return $ PyLiteral $ PyBool False
    TokenKeyword KwNone -> return $ PyLiteral PyNone
    _ -> fail "Expected literal"

parseIdentifierExpr :: PythonParser PythonExpr
parseIdentifierExpr = PyVar <$> parseIdentifier

parseListLiteral :: PythonParser PythonExpr
parseListLiteral = do
  void $ delimiterP DelimLeftBracket
  void $ delimiterP DelimRightBracket
  return $ PyList []

parseTupleLiteral :: PythonParser PythonExpr
parseTupleLiteral = do
  void $ delimiterP DelimLeftParen
  elements <- parseExpression `sepBy` delimiterP DelimComma
  void $ delimiterP DelimRightParen
  return $ PyTuple elements

parseDictLiteral :: PythonParser PythonExpr
parseDictLiteral = do
  void $ delimiterP DelimLeftBrace
  void $ delimiterP DelimRightBrace
  return $ PyDict []

parseParenExpr :: PythonParser PythonExpr
parseParenExpr = do
  void $ delimiterP DelimLeftParen
  expr <- parseExpression
  void $ delimiterP DelimRightParen
  return $ locatedValue expr

-- | Parse expression trailers (calls, subscripts, attributes)
parseTrailer :: PythonParser (Located PythonExpr -> Located PythonExpr)
parseTrailer = choice
  [ parseCallTrailer
  , parseSubscriptTrailer
  , parseAttributeTrailer
  ]

parseCallTrailer :: PythonParser (Located PythonExpr -> Located PythonExpr)
parseCallTrailer = do
  void $ delimiterP DelimLeftParen
  args <- parseArguments
  void $ delimiterP DelimRightParen
  return $ \expr -> located' $ PyCall expr args

parseSubscriptTrailer :: PythonParser (Located PythonExpr -> Located PythonExpr)
parseSubscriptTrailer = do
  void $ delimiterP DelimLeftBracket
  slice <- parseSliceOrIndex
  void $ delimiterP DelimRightBracket
  return $ \expr -> located' $ PySubscript expr slice
  where
    parseSliceOrIndex = located $ SliceIndex <$> parseExpression  -- Simplified

parseAttributeTrailer :: PythonParser (Located PythonExpr -> Located PythonExpr)
parseAttributeTrailer = do
  void $ delimiterP DelimDot
  attr <- parseIdentifier
  return $ \expr -> located' $ PyAttribute expr attr

-- | Parse patterns
parsePattern :: PythonParser (Located PythonPattern)
parsePattern = located $ choice
  [ try (PatVar <$> parseIdentifier)
  , try (PatWildcard <$ parseUnderscore)
  ]

-- | Parse type expressions
parseTypeExpr :: PythonParser (Located PythonTypeExpr)
parseTypeExpr = located $ TypeName <$> parseQualifiedName

-- | Parse function parameters
parseParameters :: PythonParser [Located PythonParameter]
parseParameters = parseParameter `sepBy` delimiterP DelimComma
  where
    parseParameter = located $ do
      name <- parseIdentifier
      typeAnnotation <- optional $ do
        void $ delimiterP DelimColon
        parseTypeExpr
      defaultValue <- optional $ do
        void $ operator' Lexer.OpAssign
        parseExpression
      return $ ParamNormal name typeAnnotation defaultValue

-- | Parse function arguments  
parseArguments :: PythonParser [Located PythonArgument]
parseArguments = do
  -- Look ahead at the next token without consuming it
  input <- getInput
  case input of
    (Located _ (TokenDelimiter DelimRightParen) : _) -> 
      return []  -- Empty argument list
    _ -> 
      parseArgument `sepBy1` delimiterP DelimComma  -- Parse arguments
  where
    parseArgument = located $ choice
      [ try parseKeywordArgument
      , ArgPositional <$> parseExpression
      ]
    
    parseKeywordArgument = do
      name <- parseIdentifier
      void $ operator' Lexer.OpAssign
      value <- parseExpression
      return $ ArgKeyword name value

-- | Parse comprehension clauses
parseComprehension :: PythonParser PythonComprehension
parseComprehension = do
  isAsync <- option False (keywordP KwAsync $> True)
  void $ keywordP KwFor
  target <- parsePattern
  void $ keywordP KwIn
  iter <- parseExpression
  filters <- many $ do
    void $ keywordP KwIf
    parseExpression
  return $ PythonComprehension target iter filters isAsync

-- | Parse a block of statements
parseBlock :: PythonParser [Located PythonStmt]
parseBlock = do
  void $ satisfy $ \case
    Located _ TokenNewline -> True
    _ -> False
  void $ parseIndent
  stmts <- some (parseBlockStatement)
  void $ parseDedent
  return stmts
  where
    parseBlockStatement = do
      skipNewlinesAndComments
      stmt <- parseStatement
      skipNewlinesAndComments
      return stmt

-- | Utility parsers
parseIdentifier :: PythonParser Identifier
parseIdentifier = do
  Located _ token <- satisfy $ \case
    Located _ (TokenIdent _) -> True
    _ -> False
  case token of
    TokenIdent text -> return $ Identifier text
    _ -> fail "Expected identifier"

parseModuleName :: PythonParser ModuleName
parseModuleName = ModuleName . (\(Identifier t) -> t) <$> parseIdentifier

parseQualifiedName :: PythonParser QualifiedName
parseQualifiedName = do
  name <- parseIdentifier
  return $ QualifiedName [] name

parseUnderscore :: PythonParser ()
parseUnderscore = do
  Located _ token <- anySingle
  case token of
    TokenIdent "_" -> return ()
    _ -> fail "Expected underscore"

-- | Token matching utilities
keywordP :: Keyword -> PythonParser ()
keywordP kw = void $ satisfy $ \case
  Located _ (TokenKeyword kw') -> kw == kw'
  _ -> False

operator' :: Lexer.Operator -> PythonParser ()
operator' op = void $ satisfy $ \case
  Located _ (TokenOperator op') -> op == op'
  _ -> False

delimiterP :: Delimiter -> PythonParser ()
delimiterP delim = void $ satisfy $ \case
  Located _ (TokenDelimiter delim') -> delim == delim'
  _ -> False

parseIndent :: PythonParser ()
parseIndent = void $ satisfy $ \case
  Located _ (TokenIndent _) -> True
  _ -> False

parseDedent :: PythonParser ()
parseDedent = void $ satisfy $ \case
  Located _ (TokenDedent _) -> True
  _ -> False

skipNewlines :: PythonParser ()
skipNewlines = void $ many $ satisfy $ \case
  Located _ TokenNewline -> True
  _ -> False

skipComments :: PythonParser ()
skipComments = void $ many $ satisfy $ \case
  Located _ (TokenComment _) -> True
  _ -> False

skipNewlinesAndComments :: PythonParser ()
skipNewlinesAndComments = void $ many $ satisfy $ \case
  Located _ TokenNewline -> True
  Located _ (TokenComment _) -> True
  _ -> False

-- | Helper for creating located expressions
located :: PythonParser a -> PythonParser (Located a)
located parser = do
  value <- parser
  -- Create a dummy span since we can't easily get source positions
  let spanLoc = SourceSpan "<input>" (Common.SourcePos 0 0) (Common.SourcePos 0 0)
  return $ Located spanLoc value

located' :: a -> Located a
located' = noLoc

-- | Extract docstring from a list of statements
extractDocstring :: [Located PythonStmt] -> (Maybe Text, [Located PythonStmt])
extractDocstring [] = (Nothing, [])
extractDocstring (stmt:rest) = case locatedValue stmt of
  PyExprStmt (Located _ (PyLiteral (PyString text))) -> (Just text, rest)
  _ -> (Nothing, stmt:rest)

convertPos :: MP.SourcePos -> Common.SourcePos
convertPos pos = Common.SourcePos
  { posLine = unPos (sourceLine pos)
  , posColumn = unPos (sourceColumn pos)
  }