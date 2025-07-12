{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

-- | Python parser that converts tokens to AST
module Fluxus.Parser.Python.Parser
  ( -- * Parser types
    PythonParser
  , ParseError(..)
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
  , parseBinaryOp
  , parseCall
  , parseSubscript
  , parseAttribute
    -- * Utility parsers
  , parseBlock
  , parseParameters
  , parseArguments
  , parsePattern
  ) where

import Control.Monad (void, when)
import Control.Applicative ((<|>), many, some, optional)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Fluxus.AST.Common
import Fluxus.AST.Python
import Fluxus.Parser.Python.Lexer

-- | Parser error type
data ParseError = ParseError
  { peMessage :: !Text
  , peLocation :: !SourceSpan
  } deriving (Eq, Show)

-- | Python parser type
type PythonParser = Parsec Void [Located PythonToken]

-- | Run the Python parser
runPythonParser :: Text -> [Located PythonToken] -> Either (ParseErrorBundle [Located PythonToken] Void) PythonAST
runPythonParser filename tokens = parse parsePython (T.unpack filename) tokens

-- | Main parser entry point
parsePython :: PythonParser PythonAST
parsePython = do
  module_ <- parseModule
  eof
  return $ PythonAST module_

-- | Parse a Python module
parseModule :: PythonParser PythonModule
parseModule = do
  skipNewlines
  imports <- many (try parseImportStmt <* skipNewlines)
  body <- many (parseStatement <* skipNewlines)
  return $ PythonModule
    { pyModuleName = Nothing  -- Will be filled in later
    , pyModuleDoc = Nothing   -- TODO: Parse docstrings
    , pyModuleImports = imports
    , pyModuleBody = body
    }

-- | Parse statements
parseStatement :: PythonParser (Located PythonStmt)
parseStatement = located $ choice
  [ try parseAssignment
  , try parseAugAssignment
  , try parseIfStmt
  , try parseWhileStmt
  , try parseForStmt
  , try parseFuncDef
  , try parseClassDef
  , try parseReturnStmt
  , try parseBreakStmt
  , try parseContinueStmt
  , try parsePassStmt
  , try parseImportStmt'
  , parseExprStmt
  ]

-- | Parse expression statements
parseExprStmt :: PythonParser PythonStmt
parseExprStmt = PyExprStmt <$> parseExpression

-- | Parse assignment statements
parseAssignment :: PythonParser PythonStmt
parseAssignment = do
  targets <- parsePattern `sepBy1` satisfy isAssignOp
  void $ satisfy isAssignOp
  value <- parseExpression
  return $ PyAssign targets value
  where
    isAssignOp (Located _ (TokenOperator OpAssign)) = True
    isAssignOp _ = False

-- | Parse augmented assignment
parseAugAssignment :: PythonParser PythonStmt
parseAugAssignment = do
  target <- parsePattern
  op <- parseAugOp
  value <- parseExpression
  return $ PyAugAssign target op value
  where
    parseAugOp = do
      Located _ token <- anySingle
      case token of
        TokenOperator OpPlusAssign -> return OpAdd
        TokenOperator OpMinusAssign -> return OpSub
        TokenOperator OpMultAssign -> return OpMul
        TokenOperator OpDivAssign -> return OpDiv
        TokenOperator OpModAssign -> return OpMod
        TokenOperator OpPowerAssign -> return OpPow
        TokenOperator OpFloorDivAssign -> return OpFloorDiv
        _ -> fail "Expected augmented assignment operator"

-- | Parse if statements
parseIfStmt :: PythonParser PythonStmt
parseIfStmt = do
  void $ keyword KwIf
  condition <- parseExpression
  void $ delimiter DelimColon
  thenBody <- parseBlock
  elseBody <- option [] $ do
    void $ keyword KwElse
    void $ delimiter DelimColon
    parseBlock
  return $ PyIf condition thenBody elseBody

-- | Parse while statements
parseWhileStmt :: PythonParser PythonStmt
parseWhileStmt = do
  void $ keyword KwWhile
  condition <- parseExpression
  void $ delimiter DelimColon
  body <- parseBlock
  elseBody <- option [] $ do
    void $ keyword KwElse
    void $ delimiter DelimColon
    parseBlock
  return $ PyWhile condition body elseBody

-- | Parse for statements
parseForStmt :: PythonParser PythonStmt
parseForStmt = do
  void $ keyword KwFor
  target <- parsePattern
  void $ keyword KwIn
  iter <- parseExpression
  void $ delimiter DelimColon
  body <- parseBlock
  elseBody <- option [] $ do
    void $ keyword KwElse
    void $ delimiter DelimColon
    parseBlock
  return $ PyFor target iter body elseBody

-- | Parse function definitions
parseFuncDef :: PythonParser PythonStmt
parseFuncDef = do
  isAsync <- option False (keyword KwAsync $> True)
  void $ keyword KwDef
  name <- parseIdentifier
  void $ delimiter DelimLeftParen
  params <- parseParameters
  void $ delimiter DelimRightParen
  returnType <- optional $ do
    void $ operator OpArrow
    parseTypeExpr
  void $ delimiter DelimColon
  body <- parseBlock
  
  let funcDef = PythonFuncDef
        { pyFuncName = name
        , pyFuncDecorators = []  -- TODO: Parse decorators
        , pyFuncParams = params
        , pyFuncReturns = returnType
        , pyFuncBody = body
        , pyFuncDoc = Nothing    -- TODO: Parse docstrings
        , pyFuncIsAsync = isAsync
        }
  
  return $ if isAsync then PyAsyncFuncDef funcDef else PyFuncDef funcDef

-- | Parse class definitions
parseClassDef :: PythonParser PythonStmt
parseClassDef = do
  void $ keyword KwClass
  name <- parseIdentifier
  bases <- option [] $ do
    void $ delimiter DelimLeftParen
    parseExpression `sepBy` delimiter DelimComma
    <* void (delimiter DelimRightParen)
  void $ delimiter DelimColon
  body <- parseBlock
  
  return $ PyClassDef $ PythonClassDef
    { pyClassName = name
    , pyClassDecorators = []  -- TODO: Parse decorators
    , pyClassBases = bases
    , pyClassKeywords = []    -- TODO: Parse keyword arguments
    , pyClassBody = body
    , pyClassDoc = Nothing    -- TODO: Parse docstrings
    }

-- | Parse return statements
parseReturnStmt :: PythonParser PythonStmt
parseReturnStmt = do
  void $ keyword KwReturn
  value <- optional parseExpression
  return $ PyReturn value

-- | Parse break statements
parseBreakStmt :: PythonParser PythonStmt
parseBreakStmt = keyword KwBreak $> PyBreak

-- | Parse continue statements
parseContinueStmt :: PythonParser PythonStmt
parseContinueStmt = keyword KwContinue $> PyContinue

-- | Parse pass statements
parsePassStmt :: PythonParser PythonStmt
parsePassStmt = keyword KwPass $> PyPass

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
  void $ keyword KwImport
  modName <- parseModuleName
  alias <- optional $ do
    void $ keyword KwAs
    parseIdentifier
  return $ ImportModule modName alias

parseFromImport :: PythonParser PythonImport
parseFromImport = do
  void $ keyword KwFrom
  modName <- parseModuleName
  void $ keyword KwImport
  choice
    [ do
        void $ operator OpMult
        return $ ImportFromStar modName
    , do
        names <- parseIdentifier `sepBy1` delimiter DelimComma
        return $ ImportFrom modName names []  -- TODO: Parse aliases
    ]

-- | Parse expressions
parseExpression :: PythonParser (Located PythonExpr)
parseExpression = parseOrExpr

parseOrExpr :: PythonParser (Located PythonExpr)
parseOrExpr = do
  first <- parseAndExpr
  rest <- many $ do
    void $ keyword KwOr
    parseAndExpr
  return $ foldl (\acc expr -> located' $ PyBoolOp OpOr [acc, expr]) first rest

parseAndExpr :: PythonParser (Located PythonExpr)
parseAndExpr = do
  first <- parseNotExpr
  rest <- many $ do
    void $ keyword KwAnd
    parseNotExpr
  return $ foldl (\acc expr -> located' $ PyBoolOp OpAnd [acc, expr]) first rest

parseNotExpr :: PythonParser (Located PythonExpr)
parseNotExpr = choice
  [ do
      void $ keyword KwNot
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
  [ operator' OpEq $> OpEq
  , operator' OpNe $> OpNe
  , operator' OpLe $> OpLe
  , operator' OpGe $> OpGe
  , operator' OpLt $> OpLt
  , operator' OpGt $> OpGt
  , keyword KwIs $> OpIs
  , keyword KwIn $> OpIn
  ]

parseArithExpr :: PythonParser (Located PythonExpr)
parseArithExpr = chainl1 parseTermExpr parseAddOp
  where
    parseAddOp = choice
      [ operator' OpPlus $> (\l r -> located' $ PyBinaryOp OpAdd l r)
      , operator' OpMinus $> (\l r -> located' $ PyBinaryOp OpSub l r)
      ]

parseTermExpr :: PythonParser (Located PythonExpr)
parseTermExpr = chainl1 parseFactorExpr parseMulOp
  where
    parseMulOp = choice
      [ operator' OpMult $> (\l r -> located' $ PyBinaryOp OpMul l r)
      , operator' OpDiv $> (\l r -> located' $ PyBinaryOp OpDiv l r)
      , operator' OpMod $> (\l r -> located' $ PyBinaryOp OpMod l r)
      , operator' OpFloorDiv $> (\l r -> located' $ PyBinaryOp OpFloorDiv l r)
      ]

parseFactorExpr :: PythonParser (Located PythonExpr)
parseFactorExpr = choice
  [ do
      op <- choice
        [ operator' OpPlus $> OpPositive
        , operator' OpMinus $> OpNegate
        , operator' OpBitNot $> OpBitNot
        ]
      expr <- parseFactorExpr
      return $ located' $ PyUnaryOp op expr
  , parsePowerExpr
  ]

parsePowerExpr :: PythonParser (Located PythonExpr)
parsePowerExpr = do
  base <- parseAtomExpr
  power <- optional $ do
    void $ operator' OpPower
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
parseAtom = located $ choice
  [ parseLiteral
  , parseIdentifierExpr
  , parseListLiteral
  , parseTupleLiteral
  , parseDictLiteral
  , parseParenExpr
  ]

parseLiteral :: PythonParser PythonExpr
parseLiteral = do
  Located _ token <- anySingle
  case token of
    TokenString text -> return $ PyLiteral $ PyString text
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
  void $ delimiter DelimLeftBracket
  elements <- parseExpression `sepBy` delimiter DelimComma
  void $ delimiter DelimRightBracket
  return $ PyList elements

parseTupleLiteral :: PythonParser PythonExpr
parseTupleLiteral = do
  void $ delimiter DelimLeftParen
  elements <- parseExpression `sepBy` delimiter DelimComma
  void $ delimiter DelimRightParen
  return $ PyTuple elements

parseDictLiteral :: PythonParser PythonExpr
parseDictLiteral = do
  void $ delimiter DelimLeftBrace
  pairs <- parseDictPair `sepBy` delimiter DelimComma
  void $ delimiter DelimRightBrace
  return $ PyDict pairs
  where
    parseDictPair = do
      key <- parseExpression
      void $ delimiter DelimColon
      value <- parseExpression
      return (key, value)

parseParenExpr :: PythonParser PythonExpr
parseParenExpr = do
  void $ delimiter DelimLeftParen
  expr <- parseExpression
  void $ delimiter DelimRightParen
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
  void $ delimiter DelimLeftParen
  args <- parseArguments
  void $ delimiter DelimRightParen
  return $ \expr -> located' $ PyCall expr args

parseSubscriptTrailer :: PythonParser (Located PythonExpr -> Located PythonExpr)
parseSubscriptTrailer = do
  void $ delimiter DelimLeftBracket
  slice <- parseSliceOrIndex
  void $ delimiter DelimRightBracket
  return $ \expr -> located' $ PySubscript expr slice
  where
    parseSliceOrIndex = located $ SliceIndex <$> parseExpression  -- Simplified

parseAttributeTrailer :: PythonParser (Located PythonExpr -> Located PythonExpr)
parseAttributeTrailer = do
  void $ delimiter DelimDot
  attr <- parseIdentifier
  return $ \expr -> located' $ PyAttribute expr attr

-- | Parse patterns
parsePattern :: PythonParser (Located PythonPattern)
parsePattern = located $ choice
  [ PatVar <$> parseIdentifier
  , PatWildcard <$ char '_'
  ]

-- | Parse type expressions
parseTypeExpr :: PythonParser (Located PythonTypeExpr)
parseTypeExpr = located $ TypeName <$> parseQualifiedName

-- | Parse function parameters
parseParameters :: PythonParser [Located PythonParameter]
parseParameters = parseParameter `sepBy` delimiter DelimComma
  where
    parseParameter = located $ do
      name <- parseIdentifier
      typeAnnotation <- optional $ do
        void $ delimiter DelimColon
        parseTypeExpr
      defaultValue <- optional $ do
        void $ operator' OpAssign
        parseExpression
      return $ ParamNormal name typeAnnotation defaultValue

-- | Parse function arguments
parseArguments :: PythonParser [Located PythonArgument]
parseArguments = parseArgument `sepBy` delimiter DelimComma
  where
    parseArgument = located $ ArgPositional <$> parseExpression

-- | Parse a block of statements
parseBlock :: PythonParser [Located PythonStmt]
parseBlock = do
  skipNewlines
  void $ parseIndent
  stmts <- some (parseStatement <* skipNewlines)
  void $ parseDedent
  return stmts

-- | Utility parsers
parseIdentifier :: PythonParser Identifier
parseIdentifier = do
  Located _ token <- anySingle
  case token of
    TokenIdent text -> return $ Identifier text
    _ -> fail "Expected identifier"

parseModuleName :: PythonParser ModuleName
parseModuleName = ModuleName . (\(Identifier t) -> t) <$> parseIdentifier

parseQualifiedName :: PythonParser QualifiedName
parseQualifiedName = do
  name <- parseIdentifier
  return $ QualifiedName [] name

-- | Token matching utilities
keyword :: Keyword -> PythonParser ()
keyword kw = void $ satisfy $ \case
  Located _ (TokenKeyword kw') -> kw == kw'
  _ -> False

operator' :: Operator -> PythonParser ()
operator' op = void $ satisfy $ \case
  Located _ (TokenOperator op') -> op == op'
  _ -> False

delimiter :: Delimiter -> PythonParser ()
delimiter delim = void $ satisfy $ \case
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

-- | Helper for creating located expressions
located :: PythonParser a -> PythonParser (Located a)
located parser = do
  start <- getSourcePos
  value <- parser
  end <- getSourcePos
  let span = SourceSpan "<input>" (convertPos start) (convertPos end)
  return $ Located span value

located' :: a -> Located a
located' = noLoc

convertPos :: SourcePos -> SourcePos
convertPos pos = SourcePos
  { posLine = unPos (sourceLine pos)
  , posColumn = unPos (sourceColumn pos)
  }