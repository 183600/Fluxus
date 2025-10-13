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
import Control.Applicative (many)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (many)


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
runPythonParser filename tokenStream = parse parsePython (T.unpack filename) tokenStream

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
  , try parseTryStmt
  , try parseReturnStmt
  , try parseBreakStmt
  , try parseContinueStmt
  , try parsePassStmt
  , try parseImportStmt'
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
parseAssignment = try $ do
  -- Parse assignment more carefully
  -- Handle multiple targets (a, b, c = ...)
  targets <- parsePattern `sepBy1` (delimiterP DelimComma <* skipNewlinesAndComments)
  void $ operator' Lexer.OpAssign
  value <- parseExpression
  return $ PyAssign targets value

-- | Parse if statements
parseIfStmt :: PythonParser PythonStmt
parseIfStmt = do
  void $ keywordP KwIf
  condition <- parseExpression
  void $ delimiterP DelimColon
  thenBody <- parseBlock
  elifClauses <- many $ do
    void $ keywordP KwElif
    elifCondition <- parseExpression
    void $ delimiterP DelimColon
    elifBody <- parseBlock
    return (elifCondition, elifBody)
  elseBody <- option [] $ do
    void $ keywordP KwElse
    void $ delimiterP DelimColon
    parseBlock
  
  -- Combine elif clauses into nested if statements
  let finalBody = foldr (\(cond, body) acc -> [located' $ PyIf cond body acc]) elseBody elifClauses
  
  return $ PyIf condition thenBody finalBody

-- | Parse while statements
parseWhileStmt :: PythonParser PythonStmt
parseWhileStmt = do
  void $ keywordP KwWhile
  condition <- parseExpression
  void $ delimiterP DelimColon
  body <- parseBlock
  elseBody <- option [] $ do
    skipNewlinesAndComments
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
    skipNewlinesAndComments
    void $ keywordP KwElse
    void $ delimiterP DelimColon
    parseBlock
  return $ PyFor False target iter body elseBody

-- | Parse try statements
parseTryStmt :: PythonParser PythonStmt
parseTryStmt = do
  void $ keywordP KwTry
  void $ delimiterP DelimColon
  tryBody <- parseBlock
  exceptClauses <- some $ do
    skipNewlinesAndComments
    void $ keywordP KwExcept
    exceptType <- optional parseExpression
    exceptName <- optional $ do
      void $ keywordP KwAs
      parseIdentifier
    void $ delimiterP DelimColon
    exceptBody <- parseBlock
    return $ located' $ PythonExcept exceptType exceptName exceptBody False
  elseBody <- option [] $ do
    skipNewlinesAndComments
    void $ keywordP KwElse
    void $ delimiterP DelimColon
    parseBlock
  finallyBody <- option [] $ do
    skipNewlinesAndComments
    void $ keywordP KwFinally
    void $ delimiterP DelimColon
    parseBlock
  return $ PyTry tryBody exceptClauses elseBody finallyBody

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
        , pyFuncTypeParams = []  -- TODO: Parse type parameters
        , pyFuncParams = params
        , pyFuncReturns = returnType
        , pyFuncBody = bodyStmts
        , pyFuncDoc = docstring
        , pyFuncIsAsync = isAsync
        }
  
  return $ PyFuncDef funcDef

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
    , pyClassTypeParams = []  -- TODO: Parse type parameters
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
parseExpression = parseTupleExpr

-- | Parse tuple expressions (comma-separated expressions)
parseTupleExpr :: PythonParser (Located PythonExpr)
parseTupleExpr = do
  first <- parseOrExpr
  rest <- many $ do
    void $ delimiterP DelimComma
    skipNewlinesAndComments
    parseOrExpr
  case rest of
    [] -> return first
    _ -> return $ located' $ PyTuple (first : rest)

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
  [ located parseLambdaExpr
  , located $ try parseLiteral
  , located parseParenExpr
  , located parseIdentifierExpr
  , located $ try parseListLiteral
  , located $ try parseTupleLiteral
  , located $ try parseDictLiteral
  , located $ try parseSetLiteral
  ]

parseLiteral :: PythonParser PythonExpr
parseLiteral = do
  Located _ tokenValue <- anySingle
  case tokenValue of
    TokenString text -> return $ PyLiteral $ PyString text
    TokenFString _ exprs -> do
      -- Parse embedded expressions from the f-string
      embeddedExprs <- mapM parseEmbeddedExpr exprs
      let fStringParts = map (\e -> FStringExpr e Nothing Nothing) embeddedExprs
      return $ PyFString fStringParts
    TokenNumber text isFloat ->
      if isFloat
        then return $ PyLiteral $ PyFloat (read $ T.unpack text)
        else return $ PyLiteral $ PyInt (read $ T.unpack text)
    TokenKeyword KwTrue -> return $ PyLiteral $ PyBool True
    TokenKeyword KwFalse -> return $ PyLiteral $ PyBool False
    TokenKeyword KwNone -> return $ PyLiteral PyNone
    TokenIdent name -> return $ PyVar $ Identifier name  -- Handle identifiers as variables
    _ -> fail $ "Expected literal, got: " ++ show tokenValue

-- | Parse f-string embedded expressions
parseEmbeddedExpr :: Text -> PythonParser (Located PythonExpr)
parseEmbeddedExpr exprText = do
  -- Handle empty expressions
  if T.null exprText
    then return $ located' $ PyLiteral $ PyString ""
    else do
      -- For now, we'll create a variable reference for all expressions
      -- A full implementation would need to actually parse the expression text
      -- For now, we'll just treat the expression text as a variable name
      return $ located' $ PyVar $ Identifier exprText

-- The parseEmbeddedExpr function is already defined above, so we don't need to redefine it here

parseIdentifierExpr :: PythonParser PythonExpr
parseIdentifierExpr = PyVar <$> parseIdentifier

parseListLiteral :: PythonParser PythonExpr
parseListLiteral = do
  void $ delimiterP DelimLeftBracket
  
  -- Look ahead to determine if this is a list comprehension
  input <- getInput
  case input of
    [] -> do
      -- Empty list []
      void $ delimiterP DelimRightBracket
      return $ PyList []
    (Located _ (TokenDelimiter DelimRightBracket) : _) -> do
      -- Empty list []
      void $ delimiterP DelimRightBracket
      return $ PyList []
    _ -> do
      -- Try to parse as list comprehension first
      result <- tryParseListComprehension
      case result of
        Just compExpr -> return compExpr
        Nothing -> do
          -- Parse as regular list literal
          elements <- parseExpression `sepBy` delimiterP DelimComma
          void $ delimiterP DelimRightBracket
          return $ PyList elements

-- Try to parse list comprehension [expr for target in iter if filters]
tryParseListComprehension :: PythonParser (Maybe PythonExpr)
tryParseListComprehension = do
  input <- getInput
  case input of
    (Located _ (TokenKeyword KwFor) : _) -> return Nothing  -- Starts with 'for', not valid
    _ -> do
      -- Try to parse: expr for target in iter [if filter]...
      result <- optional $ try $ do
        expr <- parseExpression  -- The main expression
        
        -- Look for 'for' keyword to indicate comprehension
        inputAfterExpr <- getInput
        case inputAfterExpr of
          (Located _ (TokenKeyword KwFor) : _) -> do
            -- This is a list comprehension
            comprehensions <- parseComprehensions
            void $ delimiterP DelimRightBracket
            return $ PyListComp expr comprehensions
          _ -> fail "Not a list comprehension"
      
      return result

-- Parse comprehension clauses: for target in iter [if filter]...
parseComprehensions :: PythonParser [PythonComprehension]
parseComprehensions = some parseSingleComprehension

parseSingleComprehension :: PythonParser PythonComprehension
parseSingleComprehension = do
  void $ keywordP KwFor
  target <- parsePattern  -- Target variable/pattern
  void $ keywordP KwIn
  iter <- parseExpression  -- Iterator expression
  
  -- Parse optional filter conditions
  filters <- many $ try $ do
    void $ keywordP KwIf
    parseExpression
  
  return $ PythonComprehension
    { pyCompTarget = target
    , pyCompIter = iter
    , pyCompFilters = filters
    , pyCompAsync = False  -- TODO: Support async comprehensions
    }

parseTupleLiteral :: PythonParser PythonExpr
parseTupleLiteral = do
  void $ delimiterP DelimLeftParen
  elements <- parseTupleElements
  void $ delimiterP DelimRightParen
  return $ PyTuple elements
  where
    parseTupleElements = do
      input <- getInput
      case input of
        (Located _ (TokenDelimiter DelimRightParen) : _) -> return []
        _ -> do
          first <- parseExpression
          rest <- parseTupleRest
          return (first : rest)
    
    parseTupleRest = do
      input <- getInput
      case input of
        (Located _ (TokenDelimiter DelimComma) : _) -> do
          void $ delimiterP DelimComma
          input' <- getInput
          case input' of
            (Located _ (TokenDelimiter DelimRightParen) : _) -> return []
            _ -> do
              next <- parseExpression
              rest <- parseTupleRest
              return (next : rest)
        _ -> return []

parseDictLiteral :: PythonParser PythonExpr
parseDictLiteral = do
  void $ delimiterP DelimLeftBrace
  -- Look ahead to see if this is a dict or set
  input <- getInput
  case input of
    (Located _ (TokenDelimiter DelimRightBrace) : _) -> do
      -- Empty dict
      void $ delimiterP DelimRightBrace
      return $ PyDict []
    (Located _ TokenNewline : _) -> do
      -- Handle newline after opening brace
      void $ skipNewlinesAndComments
      parseDictContent
    _ -> do
      parseDictContent
  where
    parseDictContent = do
      -- Try to parse as dict by looking for colon
      firstExpr <- parseExpression
      input <- getInput
      case input of
        (Located _ (TokenDelimiter DelimColon) : _) -> do
          -- This is a dict or dict comprehension
          void $ delimiterP DelimColon
          value <- parseExpression
          -- Check if this is a dict comprehension
          input' <- getInput
          case input' of
            (Located _ (TokenKeyword Lexer.KwFor) : _) -> do
              -- This is a dict comprehension
              comprehensions <- parseComprehensions
              void $ delimiterP DelimRightBrace
              return $ PyDictComp firstExpr value comprehensions
            _ -> do
              -- Regular dict literal
              pairs <- parseDictPairsRest [(firstExpr, value)]
              void $ delimiterP DelimRightBrace
              return $ PyDict pairs
        _ -> do
          -- This might be a set, but since we're in dict parsing, fail
          fail "Expected dict literal"
    
    parseDictPairsRest acc = do
      input <- getInput
      case input of
        (Located _ (TokenDelimiter DelimComma) : _) -> do
          void $ delimiterP DelimComma
          skipNewlinesAndComments
          input' <- getInput
          case input' of
            (Located _ (TokenDelimiter DelimRightBrace) : _) -> return acc
            _ -> do
              key <- parseExpression
              void $ delimiterP DelimColon
              value <- parseExpression
              parseDictPairsRest ((key, value) : acc)
        _ -> return acc

parseSetLiteral :: PythonParser PythonExpr
parseSetLiteral = do
  void $ delimiterP DelimLeftBrace
  -- Look ahead to see if this is a dict or set
  input <- getInput
  case input of
    (Located _ (TokenDelimiter DelimRightBrace) : _) -> do
      -- Empty set
      void $ delimiterP DelimRightBrace
      return $ PySet []
    (Located _ TokenNewline : _) -> do
      -- Handle newline after opening brace
      void $ skipNewlinesAndComments
      parseSetContent
    _ -> do
      parseSetContent
  where
    parseSetContent = do
      -- Try to parse as set by ensuring no colon follows
      firstExpr <- parseExpression
      input <- getInput
      case input of
        (Located _ (TokenDelimiter DelimColon) : _) -> do
          -- This is a dict, but since we're in set parsing, fail
          fail "Expected set literal"
        (Located _ (TokenKeyword Lexer.KwFor) : _) -> do
          -- This is a set comprehension
          comprehensions <- parseComprehensions
          void $ delimiterP DelimRightBrace
          return $ PySetComp firstExpr comprehensions
        _ -> do
          -- This is a regular set
          elements <- parseSetElementsRest [firstExpr]
          void $ delimiterP DelimRightBrace
          return $ PySet elements
    
    parseSetElementsRest acc = do
      input <- getInput
      case input of
        (Located _ (TokenDelimiter DelimComma) : _) -> do
          void $ delimiterP DelimComma
          skipNewlinesAndComments
          input' <- getInput
          case input' of
            (Located _ (TokenDelimiter DelimRightBrace) : _) -> return acc
            _ -> do
              expr <- parseExpression
              parseSetElementsRest (expr : acc)
        _ -> return acc

parseLambdaExpr :: PythonParser PythonExpr
parseLambdaExpr = do
  void $ keywordP KwLambda
  params <- parseLambdaParameters
  void $ delimiterP DelimColon
  body <- parseExpression
  return $ PyLambda params body
  where
    parseLambdaParameters = parseLambdaParameter `sepBy` delimiterP DelimComma
    parseLambdaParameter = located $ do
      name <- parseIdentifier
      return $ ParamNormal name Nothing Nothing

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
parsePattern = located (choice
  [ try (PatVar <$> parseIdentifier)
  , try (PatWildcard <$ parseUnderscore)
  ] <?> "variable name or underscore pattern")

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
      , ArgPositional <$> parseArgumentExpr
      ]
    
    parseKeywordArgument = do
      name <- parseIdentifier
      void $ operator' Lexer.OpAssign
      value <- parseExpression
      return $ ArgKeyword name value
    
    -- Parse argument expression, which could be a generator expression or a normal expression
    parseArgumentExpr = do
      expr <- parseOrExpr
      -- Look ahead to see if this is a generator expression
      input <- getInput
      case input of
        (Located _ (TokenKeyword Lexer.KwFor) : _) -> do
          -- This is a generator expression
          -- Don't consume the 'for' keyword here, let parseComprehensions handle it
          comprehensions <- parseComprehensions
          return $ located' $ PyGenComp expr comprehensions
        _ -> return expr

-- | Parse a block of statements
parseBlock :: PythonParser [Located PythonStmt]
parseBlock = do
  skipNewlinesAndComments
  void $ parseIndent
  stmts <- many parseBlockStatement
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
  Located _ tokenValue <- satisfy $ \case
    Located _ (TokenIdent _) -> True
    _ -> False
  case tokenValue of
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
  Located _ tokenValue <- anySingle
  case tokenValue of
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
  let srcSpan = SourceSpan "<input>" (Common.SourcePos 0 0) (Common.SourcePos 0 0)
  return $ Located srcSpan value

located' :: a -> Located a
located' = noLoc

-- | Extract docstring from a list of statements
extractDocstring :: [Located PythonStmt] -> (Maybe Text, [Located PythonStmt])
extractDocstring [] = (Nothing, [])
extractDocstring (stmt:rest) = case locatedValue stmt of
  PyExprStmt (Located _ (PyLiteral (PyString text))) -> (Just text, rest)
  _ -> (Nothing, stmt:rest)
