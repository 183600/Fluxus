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
  , parseAnnAssign
  , parseIfStmt
  , parseWhileStmt
  , parseForStmt
  , parseYieldStmt
  , parseRaiseStmt
  , parseAssertStmt
  , parseDelStmt
  , parseGlobalStmt
  , parseNonlocalStmt
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

import Control.Monad (void, when)
import Control.Applicative ((<|>), optional, many, some)
import Data.Bifunctor (first)
import Data.Functor (($>))
import qualified Control.Applicative as A
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (many, some, SourcePos)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Fluxus.AST.Common (SourcePos(..), SourceSpan(..), Located(..))
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
  , try parseWithStmt
  , try parseTryStmt
  , try parseMatchStmt
  , try parseReturnStmt
  , try parseYieldStmt
  , try parseBreakStmt
  , try parseContinueStmt
  , try parsePassStmt
  , try parseRaiseStmt
  , try parseAssertStmt
  , try parseDelStmt
  , try parseGlobalStmt
  , try parseNonlocalStmt
  , try parseImportStmt'
  , try parseAugAssignment
  , try parseAnnAssign
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
  -- Parse the first target group (which may itself be a tuple pattern)
  firstTargetGroup <- coalescePatterns =<< parsePattern `sepBy1` delimiterP DelimComma
  void $ operator' Lexer.OpAssign
  parseAssignmentChain [firstTargetGroup]
  where
    isAssignOp (Located _ (TokenOperator Lexer.OpAssign)) = True
    isAssignOp _ = False

    coalescePatterns :: [Located PythonPattern] -> Located PythonPattern
    coalescePatterns [single] = single
    coalescePatterns pats =
      let combinedSpan = mergeSpans (locSpan (head pats)) (locSpan (last pats))
      in Located combinedSpan (PatTuple pats)

    parseAssignmentChain :: [Located PythonPattern] -> PythonParser PythonStmt
    parseAssignmentChain acc = do
      valueExpr <- parseExpression
      tokensAfter <- getInput
      case tokensAfter of
        (Located _ (TokenOperator Lexer.OpAssign) : _) ->
          case exprToPattern valueExpr of
            Just pat -> do
              void $ operator' Lexer.OpAssign
              parseAssignmentChain (acc ++ [pat])
            Nothing -> fail "Invalid assignment target in chained assignment"
        _ -> pure $ PyAssign acc valueExpr

-- | Parse annotated assignment statements
parseAnnAssign :: PythonParser PythonStmt
parseAnnAssign = do
  target <- parsePattern
  void $ delimiterP DelimColon
  annotation <- parseTypeExpr
  value <- optional $ do
    void $ operator' Lexer.OpAssign
    parseExpression
  pure $ PyAnnAssign target annotation value

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
  parseIfChain
  where
    parseIfChain = do
      condition <- parseExpression
      void $ delimiterP DelimColon
      thenBody <- parseBlock
      skipNewlinesAndComments
      elseBody <- parseIfOrelse
      pure $ PyIf condition thenBody elseBody

    parseIfOrelse = choice
      [ try parseElifClause
      , parseElseClause
      , pure []
      ]

    parseElifClause = do
      clause <- located $ do
        void $ keywordP KwElif
        condition <- parseExpression
        void $ delimiterP DelimColon
        body <- parseBlock
        skipNewlinesAndComments
        nestedElse <- parseIfOrelse
        pure $ PyIf condition body nestedElse
      pure [clause]

    parseElseClause = do
      void $ keywordP KwElse
      void $ delimiterP DelimColon
      parseBlock

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
  isAsync <- option False (keywordP KwAsync $> True)
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
  let constructor = if isAsync then PyAsyncFor else PyFor
  return $ constructor target iter body elseBody

-- | Parse with statements
parseWithStmt :: PythonParser PythonStmt
parseWithStmt = do
  isAsync <- option False (keywordP KwAsync $> True)
  void $ keywordP KwWith
  items <- parseWithItem `sepBy1` delimiterP DelimComma
  void $ delimiterP DelimColon
  body <- parseBlock
  let constructor = if isAsync then PyAsyncWith else PyWith
  pure $ constructor items body

parseWithItem :: PythonParser (Located PythonWithItem)
parseWithItem = located $ do
  contextExpr <- parseExpression
  alias <- optional $ do
    void $ keywordP KwAs
    parsePattern
  pure $ PythonWithItem
    { pyWithContext = contextExpr
    , pyWithVar = alias
    }

-- | Parse try statements
parseTryStmt :: PythonParser PythonStmt
parseTryStmt = do
  void $ keywordP KwTry
  void $ delimiterP DelimColon
  body <- parseBlock
  exceptClauses <- many parseExceptClause
  elseBody <- option [] $ do
    void $ keywordP KwElse
    void $ delimiterP DelimColon
    parseBlock
  finallyBody <- option [] $ do
    void $ keywordP KwFinally
    void $ delimiterP DelimColon
    parseBlock
  when (null exceptClauses && null finallyBody) $
    fail "try statement requires at least one except or finally clause"
  pure $ PyTry body exceptClauses elseBody finallyBody

parseExceptClause :: PythonParser (Located PythonExcept)
parseExceptClause = located $ do
  void $ keywordP KwExcept
  exceptType <- optional parseExpression
  exceptName <- case exceptType of
    Nothing -> pure Nothing
    Just _ -> optional $ do
      void $ keywordP KwAs
      parseIdentifier
  void $ delimiterP DelimColon
  clauseBody <- parseBlock
  pure $ PythonExcept
    { pyExceptType = exceptType
    , pyExceptName = exceptName
    , pyExceptBody = clauseBody
    }

parseMatchStmt :: PythonParser PythonStmt
parseMatchStmt = do
  void $ keywordP KwMatch
  subject <- parseExpression
  void $ delimiterP DelimColon
  cases <- parseMatchCaseBlock
  when (null cases) $
    fail "match statement requires at least one case clause"
  pure $ PyMatch subject cases

parseMatchCaseBlock :: PythonParser [Located PythonCase]
parseMatchCaseBlock = do
  void parseNewlineToken
  void parseIndent
  skipNewlinesAndComments
  cases <- some $ do
    clause <- parseCaseClause
    skipNewlinesAndComments
    pure clause
  void parseDedent
  pure cases

parseCaseClause :: PythonParser (Located PythonCase)
parseCaseClause = located $ do
  void $ keywordP KwCase
  pattern <- parseMatchPattern
  guardExpr <- optional $ do
    void $ keywordP KwIf
    parseExpression
  void $ delimiterP DelimColon
  body <- parseBlock
  pure $ PythonCase
    { pyCasePattern = pattern
    , pyCaseGuard = guardExpr
    , pyCaseBody = body
    }

parseNewlineToken :: PythonParser ()
parseNewlineToken = void $ satisfy $ \case
  Located _ TokenNewline -> True
  _ -> False

parseDecorators :: PythonParser [Located PythonDecorator]
parseDecorators = many (try parseDecorator)

parseDecorator :: PythonParser (Located PythonDecorator)
parseDecorator = located $ do
  void $ delimiterP DelimAt
  decoratorExpr <- parseExpression
  let (targetExpr, decoratorArgs) = case locValue decoratorExpr of
        PyCall func args -> (func, args)
        _ -> (decoratorExpr, [])
  skipComments
  parseDecoratorNewline
  pure $ PythonDecorator targetExpr decoratorArgs

parseDecoratorNewline :: PythonParser ()
parseDecoratorNewline = void $ satisfy $ \case
  Located _ TokenNewline -> True
  _ -> False

-- | Parse function definitions
parseFuncDef :: PythonParser PythonStmt
parseFuncDef = do
  decorators <- parseDecorators
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
        , pyFuncDecorators = decorators
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
  decorators <- parseDecorators
  void $ keywordP KwClass
  name <- parseIdentifier
  (bases, keywords) <- option ([], []) parseClassArguments
  void $ delimiterP DelimColon
  body <- parseBlock
  
  -- Extract docstring from class body
  let (docstring, bodyStmts) = extractDocstring body
  
  return $ PyClassDef $ PythonClassDef
    { pyClassName = name
    , pyClassDecorators = decorators
    , pyClassBases = bases
    , pyClassKeywords = keywords
    , pyClassBody = bodyStmts
    , pyClassDoc = docstring
    }

parseClassArguments :: PythonParser ([Located PythonExpr], [(Identifier, Located PythonExpr)])
parseClassArguments = do
  void $ delimiterP DelimLeftParen
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimRightParen) : _) -> do
      void $ delimiterP DelimRightParen
      pure ([], [])
    _ -> do
      first <- parseClassArgument
      (rest, _) <- parseCommaSeparatedRest DelimRightParen parseClassArgument
      void $ delimiterP DelimRightParen
      let allArgs = first : rest
          bases = [expr | Left expr <- allArgs]
          keywords = [pair | Right pair <- allArgs]
      pure (bases, keywords)

parseClassArgument :: PythonParser (Either (Located PythonExpr) (Identifier, Located PythonExpr))
parseClassArgument = do
  expr <- parseExpression
  tokensAfter <- getInput
  case tokensAfter of
    (Located _ (TokenOperator Lexer.OpAssign) : _) ->
      case locValue expr of
        PyVar ident -> do
          void $ operator' Lexer.OpAssign
          value <- parseExpression
          pure $ Right (ident, value)
        _ -> fail "Expected identifier in class keyword argument"
    _ -> pure $ Left expr

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

-- | Parse yield statements
parseYieldStmt :: PythonParser PythonStmt
parseYieldStmt = do
  void $ keywordP KwYield
  isFrom <- optional (keywordP KwFrom)
  case isFrom of
    Just _ -> PyYieldFrom <$> parseExpression
    Nothing -> PyYield <$> optional parseExpression

-- | Parse raise statements
parseRaiseStmt :: PythonParser PythonStmt
parseRaiseStmt = do
  void $ keywordP KwRaise
  exc <- optional parseExpression
  cause <- case exc of
    Nothing -> pure Nothing
    Just _ -> optional $ do
      void $ keywordP KwFrom
      parseExpression
  pure $ PyRaise exc cause

-- | Parse assert statements
parseAssertStmt :: PythonParser PythonStmt
parseAssertStmt = do
  void $ keywordP KwAssert
  testExpr <- parseExpression
  messageExpr <- optional $ do
    void $ delimiterP DelimComma
    parseExpression
  pure $ PyAssert testExpr messageExpr

-- | Parse delete statements
parseDelStmt :: PythonParser PythonStmt
parseDelStmt = do
  void $ keywordP KwDel
  targets <- parseExpression `sepBy1` delimiterP DelimComma
  pure $ PyDel targets

-- | Parse global statements
parseGlobalStmt :: PythonParser PythonStmt
parseGlobalStmt = do
  void $ keywordP KwGlobal
  names <- parseIdentifier `sepBy1` delimiterP DelimComma
  pure $ PyGlobal names

-- | Parse nonlocal statements
parseNonlocalStmt :: PythonParser PythonStmt
parseNonlocalStmt = do
  void $ keywordP KwNonlocal
  names <- parseIdentifier `sepBy1` delimiterP DelimComma
  pure $ PyNonlocal names

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
        items <- parseImportItems
        return $ ImportFrom modName items
    ]

parseImportItems :: PythonParser [(Identifier, Maybe Identifier)]
parseImportItems = parseImportItem `sepBy1` delimiterP DelimComma

parseImportItem :: PythonParser (Identifier, Maybe Identifier)
parseImportItem = do
  name <- parseIdentifier
  alias <- optional $ do
    void $ keywordP KwAs
    parseIdentifier
  pure (name, alias)

-- | Parse expressions
parseExpression :: PythonParser (Located PythonExpr)
parseExpression = parseLambdaExpr

parseLambdaExpr :: PythonParser (Located PythonExpr)
parseLambdaExpr = do
  tokens <- getInput
  case tokens of
    (Located _ (TokenKeyword KwLambda) : _) -> located parseLambdaBody
    _ -> parseNamedExpr

parseLambdaBody :: PythonParser PythonExpr
parseLambdaBody = do
  void $ keywordP KwLambda
  params <- parseLambdaParameters
  void $ delimiterP DelimColon
  body <- parseExpression
  pure $ PyLambda params body

parseLambdaParameters :: PythonParser [Located PythonParameter]
parseLambdaParameters = do
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimColon) : _) -> pure []
    _ -> parseParamList False False
  where
    parseParamList seenVarArgs seenKwArgs = do
      param <- parseLambdaParam seenVarArgs seenKwArgs
      tokensAfter <- getInput
      let seenVarNext = seenVarArgs || isVarParam param
          seenKwNext = seenKwArgs || isKwParam param
      case tokensAfter of
        (Located _ (TokenDelimiter DelimComma) : _) -> do
          void $ delimiterP DelimComma
          tokensNext <- getInput
          case tokensNext of
            (Located _ (TokenDelimiter DelimColon) : _) -> pure [param]
            _ -> do
              rest <- parseParamList seenVarNext seenKwNext
              pure (param : rest)
        _ -> pure [param]

    parseLambdaParam seenVarArgs seenKwArgs = choice
      [ parseLambdaKwVarArg seenKwArgs
      , parseLambdaVarArg seenVarArgs
      , parseLambdaStandard seenVarArgs
      ]

    parseLambdaStandard seenVarArgs = located $ do
      name <- parseIdentifier
      defaultValue <- optional parseDefault
      if seenVarArgs
        then pure $ ParamKwOnly name Nothing defaultValue
        else pure $ ParamNormal name Nothing defaultValue

    parseLambdaVarArg seenVarArgs = located $ do
      if seenVarArgs
        then fail "Multiple var-positional parameters in lambda"
        else do
          operator' Lexer.OpMult
          name <- parseIdentifier
          pure $ ParamVarArgs name Nothing

    parseLambdaKwVarArg seenKwArgs = located $ do
      if seenKwArgs
        then fail "Multiple var-keyword parameters in lambda"
        else do
          operator' Lexer.OpPower
          name <- parseIdentifier
          pure $ ParamKwArgs name Nothing

    parseDefault = do
      void $ operator' Lexer.OpAssign
      parseExpression

    isVarParam (Located _ ParamVarArgs{}) = True
    isVarParam _ = False

    isKwParam (Located _ ParamKwArgs{}) = True
    isKwParam _ = False

parseNamedExpr :: PythonParser (Located PythonExpr)
parseNamedExpr = do
  expr <- parseConditionalExpr
  parseWalrus expr
  where
    parseWalrus expr =
      (do
        operator' Lexer.OpWalrus
        target <- case exprToPattern expr of
          Just pat -> pure pat
          Nothing -> fail "Invalid assignment expression target"
        value <- parseNamedExpr
        pure $ located' $ PyNamedExpr target value
      ) <|> pure expr

parseConditionalExpr :: PythonParser (Located PythonExpr)
parseConditionalExpr = do
  thenExpr <- parseOrExpr
  option thenExpr $ do
    try $ do
      void $ keywordP KwIf
      condition <- parseOrExpr
      void $ keywordP KwElse
      elseExpr <- parseConditionalExpr
      pure $ located' $ PyIfExp condition thenExpr elseExpr

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
  , parseAwaitExpr
  ]

parseAwaitExpr :: PythonParser (Located PythonExpr)
parseAwaitExpr = do
  isAwait <- optional (keywordP KwAwait)
  expr <- parseComparison
  case isAwait of
    Just _ -> pure $ located' $ PyAwait expr
    Nothing -> pure expr

parseComparison :: PythonParser (Located PythonExpr)
parseComparison = do
  first <- parseBitOrExpr
  rest <- many $ do
    op <- parseCompOp
    expr <- parseBitOrExpr
    return (op, expr)
  case rest of
    [] -> return first
    _ -> return $ located' $ PyComparison (map fst rest) (first : map snd rest)

parseBitOrExpr :: PythonParser (Located PythonExpr)
parseBitOrExpr = chainl1 parseBitXorExpr (operator' Lexer.OpBitOr $> mkBinary OpBitOr)

parseBitXorExpr :: PythonParser (Located PythonExpr)
parseBitXorExpr = chainl1 parseBitAndExpr (operator' Lexer.OpBitXor $> mkBinary OpBitXor)

parseBitAndExpr :: PythonParser (Located PythonExpr)
parseBitAndExpr = chainl1 parseShiftExpr (operator' Lexer.OpBitAnd $> mkBinary OpBitAnd)

parseShiftExpr :: PythonParser (Located PythonExpr)
parseShiftExpr = chainl1 parseArithExpr parseShiftOp
  where
    parseShiftOp = choice
      [ operator' Lexer.OpLeftShift $> mkBinary OpShiftL
      , operator' Lexer.OpRightShift $> mkBinary OpShiftR
      ]

mkBinary :: BinaryOp -> (Located PythonExpr -> Located PythonExpr -> Located PythonExpr)
mkBinary op = \l r -> located' $ PyBinaryOp op l r

parseCompOp :: PythonParser ComparisonOp
parseCompOp = choice
  [ operator' Lexer.OpEq $> OpEq
  , operator' Lexer.OpNe $> OpNe
  , operator' Lexer.OpLe $> OpLe
  , operator' Lexer.OpGe $> OpGe
  , operator' Lexer.OpLt $> OpLt
  , operator' Lexer.OpGt $> OpGt
  , try $ keywordP KwIs *> keywordP KwNot $> OpIsNot
  , keywordP KwIs $> OpIs
  , try $ keywordP KwNot *> keywordP KwIn $> OpNotIn
  , keywordP KwIn $> OpIn
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
  , parseParenOrGenerator
  , located parseIdentifierExpr
  , located parseListLiteral
  , located parseDictOrSetLiteral
  ]

parseLiteralValue :: PythonParser PythonLiteral
parseLiteralValue = do
  Located _ token <- anySingle
  case token of
    TokenString text -> return $ PyString text
    TokenBytes text -> return $ PyBytes text
    TokenFString segments -> do
      pySegments <- mapM convertFStringSegment segments
      return $ PyFString pySegments
    TokenNumber text isFloat ->
      if isFloat
        then return $ PyFloat (read $ T.unpack text)
        else return $ PyInt (read $ T.unpack text)
    TokenKeyword KwTrue -> return $ PyBool True
    TokenKeyword KwFalse -> return $ PyBool False
    TokenKeyword KwNone -> return PyNone
    TokenOperator Lexer.OpEllipsis -> return PyEllipsis
    _ -> fail "Expected literal"

parseLiteral :: PythonParser PythonExpr
parseLiteral = PyLiteral <$> parseLiteralValue

convertFStringSegment :: Lexer.FStringSegment -> PythonParser PythonFStringSegment
convertFStringSegment segment = case segment of
  Lexer.FStringLiteralSegment text _ -> pure (PythonFStringLiteral text)
  Lexer.FStringExpressionSegment exprText spanInfo ->
    case parseFStringExpression exprText spanInfo of
      Left err -> fail (T.unpack err)
      Right locatedExpr -> pure (PythonFStringExpr locatedExpr)

parseFStringExpression :: Text -> SourceSpan -> Either Text (Located PythonExpr)
parseFStringExpression exprText spanInfo = do
  tokens <- first (T.pack . MP.errorBundlePretty) $
    Lexer.runPythonLexer (spanFilename spanInfo) exprText
  let shifted = map (shiftFStringToken spanInfo) tokens
      eofToken = Located spanInfo TokenEOF
      sourceName = T.unpack (spanFilename spanInfo)
  case MP.parse (parseExpression <* MP.eof) sourceName (shifted ++ [eofToken]) of
    Left err -> Left (T.pack (show err))
    Right parsed -> Right parsed

shiftFStringToken :: SourceSpan -> Located PythonToken -> Located PythonToken
shiftFStringToken base (Located spanInfo token) =
  let span' = shiftSourceSpan base spanInfo
  in Located span' token

shiftSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
shiftSourceSpan base relative = SourceSpan
  { spanFilename = spanFilename base
  , spanStart = shiftSourcePos (spanStart base) (spanStart relative)
  , spanEnd = shiftSourcePos (spanStart base) (spanEnd relative)
  }

shiftSourcePos :: SourcePos -> SourcePos -> SourcePos
shiftSourcePos base relative = SourcePos
  { posLine = posLine base + posLine relative - 1
  , posColumn =
      if posLine relative == 1
        then posColumn base + posColumn relative - 1
        else posColumn relative
  }

parseIdentifierExpr :: PythonParser PythonExpr
parseIdentifierExpr = PyVar <$> parseIdentifier

startsComprehension :: [Located PythonToken] -> Bool
startsComprehension tokens = case tokens of
  (Located _ (TokenKeyword KwFor) : _) -> True
  (Located _ (TokenKeyword KwAsync) : rest) ->
    case rest of
      (Located _ (TokenKeyword KwFor) : _) -> True
      _ -> False
  _ -> False

isColonNext :: [Located PythonToken] -> Bool
isColonNext (Located _ (TokenDelimiter DelimColon) : _) = True
isColonNext _ = False

parseCommaSeparatedRest :: Delimiter -> PythonParser a -> PythonParser ([a], Bool)
parseCommaSeparatedRest closing parser = go []
  where
    go acc = do
      tokens <- getInput
      case tokens of
        (Located _ (TokenDelimiter delim) : _) | delim == closing ->
          pure (reverse acc, False)
        _ -> do
          void $ delimiterP DelimComma
          tokensAfter <- getInput
          case tokensAfter of
            (Located _ (TokenDelimiter delim) : _) | delim == closing ->
              pure (reverse acc, True)
            _ -> do
              item <- parser
              go (item : acc)

parseListLiteral :: PythonParser PythonExpr
parseListLiteral = do
  void $ delimiterP DelimLeftBracket
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimRightBracket) : _) -> do
      void $ delimiterP DelimRightBracket
      pure $ PyList []
    _ -> do
      first <- parseExpression
      tokensAfter <- getInput
      if startsComprehension tokensAfter
        then do
          comps <- some parseComprehension
          void $ delimiterP DelimRightBracket
          pure $ PyListComp first comps
        else do
          (rest, _) <- parseCommaSeparatedRest DelimRightBracket parseExpression
          void $ delimiterP DelimRightBracket
          pure $ PyList (first : rest)

parseParenOrGenerator :: PythonParser (Located PythonExpr)
parseParenOrGenerator = located $ do
  void $ delimiterP DelimLeftParen
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimRightParen) : _) -> do
      void $ delimiterP DelimRightParen
      pure $ PyTuple []
    _ -> do
      first <- parseExpression
      tokensAfter <- getInput
      if startsComprehension tokensAfter
        then do
          comps <- some parseComprehension
          void $ delimiterP DelimRightParen
          pure $ PyGenComp first comps
        else do
          (rest, trailing) <- parseCommaSeparatedRest DelimRightParen parseExpression
          void $ delimiterP DelimRightParen
          case (rest, trailing) of
            ([], False) -> pure $ locatedValue first
            _ -> pure $ PyTuple (first : rest)

parseDictPair :: PythonParser (Located PythonExpr, Located PythonExpr)
parseDictPair = do
  key <- parseExpression
  void $ delimiterP DelimColon
  value <- parseExpression
  pure (key, value)

parseDictRest :: [(Located PythonExpr, Located PythonExpr)] -> PythonParser [(Located PythonExpr, Located PythonExpr)]
parseDictRest acc = do
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimRightBrace) : _) -> pure (reverse acc)
    _ -> do
      void $ delimiterP DelimComma
      tokensAfter <- getInput
      case tokensAfter of
        (Located _ (TokenDelimiter DelimRightBrace) : _) -> pure (reverse acc)
        _ -> do
          pair <- parseDictPair
          parseDictRest (pair : acc)

parseDictOrSetLiteral :: PythonParser PythonExpr
parseDictOrSetLiteral = do
  void $ delimiterP DelimLeftBrace
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimRightBrace) : _) -> do
      void $ delimiterP DelimRightBrace
      pure $ PyDict []
    _ -> do
      first <- parseExpression
      tokensAfterFirst <- getInput
      if isColonNext tokensAfterFirst
        then do
          void $ delimiterP DelimColon
          firstValue <- parseExpression
          tokensAfterValue <- getInput
          if startsComprehension tokensAfterValue
            then do
              comps <- some parseComprehension
              void $ delimiterP DelimRightBrace
              pure $ PyDictComp first firstValue comps
            else do
              pairs <- parseDictRest [(first, firstValue)]
              void $ delimiterP DelimRightBrace
              pure $ PyDict pairs
        else if startsComprehension tokensAfterFirst
          then do
            comps <- some parseComprehension
            void $ delimiterP DelimRightBrace
            pure $ PySetComp first comps
          else do
            (rest, _) <- parseCommaSeparatedRest DelimRightBrace parseExpression
            void $ delimiterP DelimRightBrace
            pure $ PySet (first : rest)

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
  sliceNode <- parseSliceOrIndex
  void $ delimiterP DelimRightBracket
  return $ \expr -> located' $ PySubscript expr sliceNode

parseSliceOrIndex :: PythonParser (Located PythonSlice)
parseSliceOrIndex = do
  first <- parseSliceItem
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimComma) : _) -> do
      void $ delimiterP DelimComma
      tokensAfter <- getInput
      case tokensAfter of
        (Located _ (TokenDelimiter DelimRightBracket) : _) ->
          pure $ wrapExtSlice [first]
        _ -> do
          rest <- parseAdditionalSlices [first]
          pure $ wrapExtSlice rest
    _ -> pure first

parseAdditionalSlices :: [Located PythonSlice] -> PythonParser [Located PythonSlice]
parseAdditionalSlices acc = do
  nextSlice <- parseSliceItem
  let acc' = acc ++ [nextSlice]
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimComma) : _) -> do
      void $ delimiterP DelimComma
      tokensAfter <- getInput
      case tokensAfter of
        (Located _ (TokenDelimiter DelimRightBracket) : _) ->
          pure acc'
        _ -> parseAdditionalSlices acc'
    _ -> pure acc'

parseSliceItem :: PythonParser (Located PythonSlice)
parseSliceItem = located $ do
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimColon) : _) ->
      parseSliceComponents Nothing
    _ -> do
      expr <- parseExpression
      tokensAfter <- getInput
      if isColonNext tokensAfter
        then parseSliceComponents (Just expr)
        else pure $ SliceIndex expr

parseSliceComponents :: Maybe (Located PythonExpr) -> PythonParser PythonSlice
parseSliceComponents startExpr = do
  void $ delimiterP DelimColon
  stopExpr <- optional parseExpression
  tokensAfter <- getInput
  stepExpr <- if isColonNext tokensAfter
    then do
      void $ delimiterP DelimColon
      optional parseExpression
    else pure Nothing
  pure $ SliceSlice startExpr stopExpr stepExpr

wrapExtSlice :: [Located PythonSlice] -> Located PythonSlice
wrapExtSlice slices =
  let spanStart = locSpan (head slices)
      spanEnd = locSpan (last slices)
  in Located (mergeSpans spanStart spanEnd) (SliceExtSlice slices)

parseAttributeTrailer :: PythonParser (Located PythonExpr -> Located PythonExpr)
parseAttributeTrailer = do
  void $ delimiterP DelimDot
  attr <- parseIdentifier
  return $ \expr -> located' $ PyAttribute expr attr

-- | Structural pattern parsing for match statements
parseMatchPattern :: PythonParser (Located PythonPattern)
parseMatchPattern = located parseMatchPatternBody

parseMatchPatternBody :: PythonParser PythonPattern
parseMatchPatternBody = do
  first <- parseMatchAsPattern
  rest <- many (operator' Lexer.OpBitOr *> parseMatchAsPattern)
  case rest of
    [] -> pure (locValue first)
    _ -> pure $ PatOr (NE.fromList (first : rest))

parseMatchAsPattern :: PythonParser (Located PythonPattern)
parseMatchAsPattern = do
  base <- parseMatchPrimaryPattern
  tokens <- getInput
  case tokens of
    (Located _ (TokenKeyword KwAs) : _) -> do
      void $ keywordP KwAs
      alias <- located parseIdentifier
      let combined = mergeSpans (locSpan base) (locSpan alias)
      pure $ Located combined (PatAs base (locValue alias))
    _ -> pure base

parseMatchPrimaryPattern :: PythonParser (Located PythonPattern)
parseMatchPrimaryPattern = do
  tokens <- getInput
  case tokens of
    (Located _ (TokenOperator Lexer.OpMult) : _) -> parseMatchStarPattern
    (Located _ (TokenDelimiter DelimLeftBracket) : _) -> parseMatchListPattern
    (Located _ (TokenDelimiter DelimLeftParen) : _) -> parseMatchParenPattern
    (Located _ (TokenDelimiter DelimLeftBrace) : _) -> parseMatchMappingPattern
    (Located _ (TokenIdent "_") : _) -> parseWildcardPattern
    (Located _ tok : _)
      | isLiteralStart tok -> parseMatchLiteralPattern
    (Located _ (TokenIdent _) : _) -> parseMatchNamePattern
    _ -> fail "Unexpected token in match pattern"

isLiteralStart :: PythonToken -> Bool
isLiteralStart = \case
  TokenOperator Lexer.OpPlus -> True
  TokenOperator Lexer.OpMinus -> True
  tok -> isLiteralToken tok

parseMatchLiteralPattern :: PythonParser (Located PythonPattern)
parseMatchLiteralPattern = located $ do
  literal <- parsePatternLiteralValue
  pure $ PatLiteral literal

parseMatchStarPattern :: PythonParser (Located PythonPattern)
parseMatchStarPattern = located $ do
  operator' Lexer.OpMult
  target <- parseCapturePattern
  pure $ PatStarred target

parseCapturePattern :: PythonParser (Located PythonPattern)
parseCapturePattern = choice
  [ parseWildcardPattern
  , located $ PatVar <$> parseIdentifier
  ]

parseMatchListPattern :: PythonParser (Located PythonPattern)
parseMatchListPattern = located $ do
  void $ delimiterP DelimLeftBracket
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimRightBracket) : _) -> do
      void $ delimiterP DelimRightBracket
      pure $ PatList []
    _ -> do
      first <- parseMatchSequenceElement
      (rest, _) <- parseCommaSeparatedRest DelimRightBracket parseMatchSequenceElement
      void $ delimiterP DelimRightBracket
      pure $ PatList (first : rest)

parseMatchSequenceElement :: PythonParser (Located PythonPattern)
parseMatchSequenceElement = do
  tokens <- getInput
  case tokens of
    (Located _ (TokenOperator Lexer.OpMult) : _) -> parseMatchStarPattern
    _ -> parseMatchPattern

parseMatchParenPattern :: PythonParser (Located PythonPattern)
parseMatchParenPattern = located $ do
  void $ delimiterP DelimLeftParen
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimRightParen) : _) -> do
      void $ delimiterP DelimRightParen
      pure $ PatTuple []
    _ -> do
      first <- parseMatchPattern
      (rest, trailing) <- parseCommaSeparatedRest DelimRightParen parseMatchPattern
      void $ delimiterP DelimRightParen
      case (rest, trailing) of
        ([], False) -> pure $ locValue first
        _ -> pure $ PatTuple (first : rest)

parseMatchMappingPattern :: PythonParser (Located PythonPattern)
parseMatchMappingPattern = located $ do
  void $ delimiterP DelimLeftBrace
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimRightBrace) : _) -> do
      void $ delimiterP DelimRightBrace
      pure $ PatMapping [] Nothing
    _ -> do
      (pairs, restName) <- parseMappingItems [] Nothing
      void $ delimiterP DelimRightBrace
      pure $ PatMapping pairs restName
  where
    parseMappingItems acc restName = do
      entry <- parseMatchMappingEntry restName
      let (acc', restName') = case entry of
            Left pair -> (pair : acc, restName)
            Right name -> (acc, Just name)
      tokensAfter <- getInput
      case tokensAfter of
        (Located _ (TokenDelimiter DelimComma) : _) -> do
          void $ delimiterP DelimComma
          tokensNext <- getInput
          case tokensNext of
            (Located _ (TokenDelimiter DelimRightBrace) : _) -> pure (reverse acc', restName')
            _ -> parseMappingItems acc' restName'
        _ -> pure (reverse acc', restName')

parseMatchMappingEntry :: Maybe Identifier -> PythonParser (Either (Located PythonExpr, Located PythonPattern) Identifier)
parseMatchMappingEntry restName = do
  tokens <- getInput
  case tokens of
    (Located _ (TokenOperator Lexer.OpPower) : _) -> do
      void $ operator' Lexer.OpPower
      name <- parseIdentifier
      case restName of
        Just _ -> fail "Multiple ** captures in mapping pattern"
        Nothing -> pure $ Right name
    _ -> do
      keyExpr <- parseMappingKeyExpr
      void $ delimiterP DelimColon
      valuePattern <- parseMatchPattern
      pure $ Left (keyExpr, valuePattern)

parseMappingKeyExpr :: PythonParser (Located PythonExpr)
parseMappingKeyExpr = located $ do
  literal <- parsePatternLiteralValue
  pure $ PyLiteral literal

parseMatchNamePattern :: PythonParser (Located PythonPattern)
parseMatchNamePattern = located parseMatchNamePatternBody

parseMatchNamePatternBody :: PythonParser PythonPattern
parseMatchNamePatternBody = do
  nameLoc <- located parseIdentifier
  let baseExpr = Located (locSpan nameLoc) (PyVar (locValue nameLoc))
  (qualifiedExpr, sawDot) <- parseAttributeChain baseExpr False
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimLeftParen) : _) -> do
      (posArgs, kwArgs) <- parseClassPatternArgs
      pure $ PatClass qualifiedExpr posArgs kwArgs
    _ | sawDot -> pure $ PatValue qualifiedExpr
      | otherwise -> pure $ PatVar (locValue nameLoc)

parseClassPatternArgs :: PythonParser ([Located PythonPattern], [(Identifier, Located PythonPattern)])
parseClassPatternArgs = do
  void $ delimiterP DelimLeftParen
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimRightParen) : _) -> do
      void $ delimiterP DelimRightParen
      pure ([], [])
    _ -> do
      first <- parseClassPatternArg
      (rest, _) <- parseCommaSeparatedRest DelimRightParen parseClassPatternArg
      void $ delimiterP DelimRightParen
      collectArgs (first : rest)
  where
    collectArgs args = go [] [] False args
    go pos kw _ [] = pure (reverse pos, reverse kw)
    go pos kw seenKw (arg:rest) = case arg of
      Left pat -> do
        when seenKw $ fail "Positional subpatterns must precede keyword subpatterns"
        go (pat:pos) kw seenKw rest
      Right (name, pat) -> go pos ((name, pat):kw) True rest

parseClassPatternArg :: PythonParser (Either (Located PythonPattern) (Identifier, Located PythonPattern))
parseClassPatternArg = choice
  [ try $ do
      name <- parseIdentifier
      void $ operator' Lexer.OpAssign
      pat <- parseMatchPattern
      pure $ Right (name, pat)
  , Left <$> parseMatchPattern
  ]

parsePatternLiteralValue :: PythonParser PythonLiteral
parsePatternLiteralValue = do
  tokens <- getInput
  case tokens of
    (Located _ (TokenOperator Lexer.OpPlus) : _) -> do
      void $ operator' Lexer.OpPlus
      parseLiteralValue
    (Located _ (TokenOperator Lexer.OpMinus) : _) -> do
      void $ operator' Lexer.OpMinus
      literal <- parseLiteralValue
      case literal of
        PyInt n -> pure $ PyInt (negate n)
        PyFloat f -> pure $ PyFloat (negate f)
        PyComplex real imag -> pure $ PyComplex (negate real) (negate imag)
        _ -> fail "Unary minus is only allowed on numeric literals in patterns"
    (Located _ tok : _) | isLiteralToken tok -> parseLiteralValue
    _ -> fail "Expected literal in pattern"

parseAttributeChain :: Located PythonExpr -> Bool -> PythonParser (Located PythonExpr, Bool)
parseAttributeChain expr seenDot = do
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimDot) : _) -> do
      void $ delimiterP DelimDot
      attr <- located parseIdentifier
      let span' = mergeSpans (locSpan expr) (locSpan attr)
          newExpr = Located span' (PyAttribute expr (locValue attr))
      parseAttributeChain newExpr True
    _ -> pure (expr, seenDot)

isLiteralToken :: PythonToken -> Bool
isLiteralToken = \case
  TokenString _ -> True
  TokenBytes _ -> True
  TokenFString _ -> True
  TokenNumber _ _ -> True
  TokenOperator Lexer.OpEllipsis -> True
  TokenKeyword KwTrue -> True
  TokenKeyword KwFalse -> True
  TokenKeyword KwNone -> True
  _ -> False

exprToPattern :: Located PythonExpr -> Maybe (Located PythonPattern)
exprToPattern (Located span expr) = case expr of
  PyVar ident -> Just (Located span (PatVar ident))
  PyTuple elems -> do
    pats <- traverse exprToPattern elems
    Just (Located span (PatTuple pats))
  PyList elems -> do
    pats <- traverse exprToPattern elems
    Just (Located span (PatList pats))
  _ -> Nothing

-- | Parse patterns
parsePattern :: PythonParser (Located PythonPattern)
parsePattern = located parsePatternBody

parsePatternBody :: PythonParser PythonPattern
parsePatternBody = do
  first <- parsePatternAtom
  (rest, trailing) <- parsePatternTupleTail
  case (rest, trailing) of
    ([], False) -> pure (locValue first)
    _ -> pure $ PatTuple (first : rest)

parsePatternAtom :: PythonParser (Located PythonPattern)
parsePatternAtom = choice
  [ parseStarPattern
  , parseParenPattern
  , parseListPattern
  , try parseLiteralPattern
  , try parseWildcardPattern
  , parseIdentifierPattern
  ]

parseIdentifierPattern :: PythonParser (Located PythonPattern)
parseIdentifierPattern = located $ PatVar <$> parseIdentifier

parseWildcardPattern :: PythonParser (Located PythonPattern)
parseWildcardPattern = located $ parseUnderscore $> PatWildcard

parseLiteralPattern :: PythonParser (Located PythonPattern)
parseLiteralPattern = located $ PatLiteral <$> parseLiteralValue

parseListPattern :: PythonParser (Located PythonPattern)
parseListPattern = located $ do
  void $ delimiterP DelimLeftBracket
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimRightBracket) : _) -> do
      void $ delimiterP DelimRightBracket
      pure $ PatList []
    _ -> do
      first <- parsePattern
      (rest, _) <- parseCommaSeparatedRest DelimRightBracket parsePattern
      void $ delimiterP DelimRightBracket
      pure $ PatList (first : rest)

parseParenPattern :: PythonParser (Located PythonPattern)
parseParenPattern = located $ do
  void $ delimiterP DelimLeftParen
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimRightParen) : _) -> do
      void $ delimiterP DelimRightParen
      pure $ PatTuple []
    _ -> do
      first <- parsePattern
      (rest, trailing) <- parseCommaSeparatedRest DelimRightParen parsePattern
      void $ delimiterP DelimRightParen
      case (rest, trailing) of
        ([], False) -> pure $ locValue first
        _ -> pure $ PatTuple (first : rest)

parseStarPattern :: PythonParser (Located PythonPattern)
parseStarPattern = located $ do
  operator' Lexer.OpMult
  inner <- parsePattern
  pure $ PatStarred inner

parsePatternTupleTail :: PythonParser ([Located PythonPattern], Bool)
parsePatternTupleTail = do
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimComma) : _) -> do
      void $ delimiterP DelimComma
      tokensAfter <- getInput
      if isPatternTerminator tokensAfter
        then pure ([], True)
        else do
          nextPat <- parsePattern
          (rest, trailing) <- parsePatternTupleTail
          pure (nextPat : rest, trailing)
    _ -> pure ([], False)

isPatternTerminator :: [Located PythonToken] -> Bool
isPatternTerminator [] = True
isPatternTerminator (Located _ token : _) = case token of
  TokenDelimiter DelimRightParen -> True
  TokenDelimiter DelimRightBracket -> True
  TokenDelimiter DelimRightBrace -> True
  TokenDelimiter DelimColon -> True
  TokenOperator Lexer.OpAssign -> True
  TokenOperator Lexer.OpWalrus -> True
  TokenKeyword KwIn -> True
  _ -> False

-- | Parse type expressions
parseTypeExpr :: PythonParser (Located PythonTypeExpr)
parseTypeExpr = parseTypeUnionExpr

parseTypeUnionExpr :: PythonParser (Located PythonTypeExpr)
parseTypeUnionExpr = do
  first <- parseTypePostfixExpr
  rest <- many $ do
    operator' Lexer.OpBitOr
    parseTypePostfixExpr
  case rest of
    [] -> pure first
    _  -> pure $ wrapUnion (first : rest)
  where
    wrapUnion types =
      let startSpan = locSpan (head types)
          endSpan = locSpan (last types)
      in Located (mergeSpans startSpan endSpan) (TypeUnion types)

parseTypePostfixExpr :: PythonParser (Located PythonTypeExpr)
parseTypePostfixExpr = do
  base <- parseTypePrimaryExpr
  parseTypeSuffix base
  where
    parseTypeSuffix current = do
      tokens <- getInput
      case tokens of
        (Located _ (TokenDelimiter DelimLeftBracket) : _) ->
          case locValue current of
            TypeName qn | isLiteralQualified qn -> do
              literalType <- parseLiteralTypeSubscript current
              parseTypeSuffix literalType
            _ -> do
              args <- parseTypeArgumentList
              let nextSpan = case reverse args of
                    (lastArg:_) -> mergeSpans (locSpan current) (locSpan lastArg)
                    [] -> locSpan current
                  nextNode = Located nextSpan (TypeSubscript current args)
              parseTypeSuffix nextNode
        _ -> pure current

parseTypePrimaryExpr :: PythonParser (Located PythonTypeExpr)
parseTypePrimaryExpr = choice
  [ parseParenTypeExpr
  , parseBracketTupleTypeExpr
  , parseEllipsisTypeExpr
  , located $ TypeName <$> parseQualifiedName
  ]

parseParenTypeExpr :: PythonParser (Located PythonTypeExpr)
parseParenTypeExpr = located $ do
  void $ delimiterP DelimLeftParen
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimRightParen) : _) -> do
      void $ delimiterP DelimRightParen
      pure $ TypeTuple []
    _ -> do
      first <- parseTypeExpr
      (rest, trailing) <- parseCommaSeparatedRest DelimRightParen parseTypeExpr
      void $ delimiterP DelimRightParen
      case (rest, trailing) of
        ([], False) -> pure (locValue first)
        _ -> pure $ TypeTuple (first : rest)

parseBracketTupleTypeExpr :: PythonParser (Located PythonTypeExpr)
parseBracketTupleTypeExpr = located $ do
  void $ delimiterP DelimLeftBracket
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimRightBracket) : _) -> do
      void $ delimiterP DelimRightBracket
      pure $ TypeTuple []
    _ -> do
      first <- parseTypeExpr
      (rest, _) <- parseCommaSeparatedRest DelimRightBracket parseTypeExpr
      void $ delimiterP DelimRightBracket
      pure $ TypeTuple (first : rest)

parseEllipsisTypeExpr :: PythonParser (Located PythonTypeExpr)
parseEllipsisTypeExpr = located $ do
  operator' Lexer.OpEllipsis
  pure $ TypeName (QualifiedName [] (Identifier "..."))

parseTypeArgumentList :: PythonParser [Located PythonTypeExpr]
parseTypeArgumentList = do
  void $ delimiterP DelimLeftBracket
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimRightBracket) : _) -> do
      void $ delimiterP DelimRightBracket
      pure []
    _ -> do
      first <- parseTypeExpr
      (rest, _) <- parseCommaSeparatedRest DelimRightBracket parseTypeExpr
      void $ delimiterP DelimRightBracket
      pure (first : rest)

parseLiteralTypeSubscript :: Located PythonTypeExpr -> PythonParser (Located PythonTypeExpr)
parseLiteralTypeSubscript base = do
  literalExprs <- parseLiteralArgumentList
  when (null literalExprs) $
    fail "typing.Literal requires at least one literal argument"
  let literalNodes = map wrapLiteral literalExprs
      endSpan = locSpan (last literalExprs)
      combinedSpan = mergeSpans (locSpan base) endSpan
  case literalNodes of
    [single] -> pure single { locSpan = combinedSpan }
    _ -> pure $ Located combinedSpan (TypeUnion literalNodes)
  where
    wrapLiteral exprLoc = Located (locSpan exprLoc) (TypeLiteral exprLoc)

parseLiteralArgumentList :: PythonParser [Located PythonExpr]
parseLiteralArgumentList = do
  void $ delimiterP DelimLeftBracket
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimRightBracket) : _) ->
      fail "typing.Literal cannot be empty"
    _ -> do
      first <- parseLiteralConstantExpr
      (rest, _) <- parseCommaSeparatedRest DelimRightBracket parseLiteralConstantExpr
      void $ delimiterP DelimRightBracket
      pure (first : rest)

parseLiteralConstantExpr :: PythonParser (Located PythonExpr)
parseLiteralConstantExpr = located $ do
  literal <- parsePatternLiteralValue
  pure $ PyLiteral literal

isLiteralQualified :: QualifiedName -> Bool
isLiteralQualified qn =
  let simple = T.toLower (identifierToText (qnName qn))
      canonical = canonicalQualifiedName qn
  in simple == "literal" || canonical == "typing.literal"

identifierToText :: Identifier -> Text
identifierToText (Identifier txt) = txt

moduleNameToText :: ModuleName -> Text
moduleNameToText (ModuleName txt) = txt

canonicalQualifiedName :: QualifiedName -> Text
canonicalQualifiedName qn =
  let modules = map (T.toLower . moduleNameToText) (qnModule qn)
      nameTxt = T.toLower (identifierToText (qnName qn))
  in T.intercalate "." (modules ++ [nameTxt])

-- | Parse function parameters
parseParameters :: PythonParser [Located PythonParameter]
parseParameters = do
  tokens <- getInput
  case tokens of
    (Located _ (TokenDelimiter DelimRightParen) : _) -> pure []
    _ -> parseParamList False False
  where
    parseParamList :: Bool -> Bool -> PythonParser [Located PythonParameter]
    parseParamList seenVarArgs seenKwArgs = do
      param <- parseParameter seenVarArgs seenKwArgs
      tokensAfter <- getInput
      let seenVar' = seenVarArgs || isVarArgs param
          seenKw' = seenKwArgs || isKwArgs param
      case tokensAfter of
        (Located _ (TokenDelimiter DelimComma) : _) -> do
          void $ delimiterP DelimComma
          tokensNext <- getInput
          case tokensNext of
            (tok:_) | isClosing tok -> pure [param]
            _ -> do
              rest <- parseParamList seenVar' seenKw'
              pure (param : rest)
        _ -> pure [param]

    parseParameter :: Bool -> Bool -> PythonParser (Located PythonParameter)
    parseParameter seenVarArgs seenKwArgs = choice
      [ parseKwVarArgs seenKwArgs
      , parseVarArgs seenVarArgs
      , parseStandardParam seenVarArgs
      ]

    parseStandardParam :: Bool -> PythonParser (Located PythonParameter)
    parseStandardParam seenVarArgs = located $ do
      name <- parseIdentifier
      typeAnnotation <- optional parseAnnotation
      defaultValue <- optional parseDefault
      if seenVarArgs
        then pure $ ParamKwOnly name typeAnnotation defaultValue
        else pure $ ParamNormal name typeAnnotation defaultValue

    parseVarArgs :: Bool -> PythonParser (Located PythonParameter)
    parseVarArgs seenVarArgs = located $ do
      if seenVarArgs
        then fail "Multiple var-positional parameters are not allowed"
        else do
          operator' Lexer.OpMult
          name <- parseIdentifier
          typeAnnotation <- optional parseAnnotation
          pure $ ParamVarArgs name typeAnnotation

    parseKwVarArgs :: Bool -> PythonParser (Located PythonParameter)
    parseKwVarArgs seenKwArgs = located $ do
      if seenKwArgs
        then fail "Multiple var-keyword parameters are not allowed"
        else do
          operator' Lexer.OpPower
          name <- parseIdentifier
          typeAnnotation <- optional parseAnnotation
          pure $ ParamKwArgs name typeAnnotation

    parseAnnotation :: PythonParser (Located PythonTypeExpr)
    parseAnnotation = do
      void $ delimiterP DelimColon
      parseTypeExpr

    parseDefault :: PythonParser (Located PythonExpr)
    parseDefault = do
      void $ operator' Lexer.OpAssign
      parseExpression

    isVarArgs :: Located PythonParameter -> Bool
    isVarArgs (Located _ param) = case param of
      ParamVarArgs{} -> True
      _ -> False

    isKwArgs :: Located PythonParameter -> Bool
    isKwArgs (Located _ param) = case param of
      ParamKwArgs{} -> True
      _ -> False

    isClosing :: Located PythonToken -> Bool
    isClosing (Located _ token) = case token of
      TokenDelimiter DelimRightParen -> True
      _ -> False

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
      [ try parseKwStarArgument
      , try parseStarArgument
      , try parseKeywordArgument
      , ArgPositional <$> parseExpression
      ]
    
    parseKeywordArgument = do
      name <- parseIdentifier
      void $ operator' Lexer.OpAssign
      value <- parseExpression
      return $ ArgKeyword name value

    parseStarArgument = do
      operator' Lexer.OpMult
      ArgStarred <$> parseExpression

    parseKwStarArgument = do
      operator' Lexer.OpPower
      ArgKwStarred <$> parseExpression

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
parseModuleName = do
  first <- parseIdentifier
  rest <- many $ do
    delimiterP DelimDot
    parseIdentifier
  let parts = first : rest
      moduleText = T.intercalate "." (map (\(Identifier t) -> t) parts)
  pure $ ModuleName moduleText

parseQualifiedName :: PythonParser QualifiedName
parseQualifiedName = do
  first <- parseTypeIdentifier
  rest <- many $ do
    delimiterP DelimDot
    parseTypeIdentifier
  let idents = first : rest
      modules = map (ModuleName . identifierToText) (init idents)
      finalName = last idents
  pure $ QualifiedName modules finalName

parseTypeIdentifier :: PythonParser Identifier
parseTypeIdentifier = parseIdentifier <|> parseKeywordIdentifier
  where
    parseKeywordIdentifier = do
      Located _ token <- satisfy isTypeKeywordToken
      case token of
        TokenKeyword KwNone -> pure (Identifier "None")
        TokenKeyword KwTrue -> pure (Identifier "True")
        TokenKeyword KwFalse -> pure (Identifier "False")
        _ -> fail "Unsupported keyword in type expression"
    isTypeKeywordToken (Located _ (TokenKeyword kw)) = kw `elem` [KwNone, KwTrue, KwFalse]
    isTypeKeywordToken _ = False

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
  before <- getInput
  value <- parser
  after <- getInput
  let consumedCount = length before - length after
      consumedTokens = take consumedCount before
      spanLoc = case consumedTokens of
        (startTok:_) ->
          let endTok = last consumedTokens
          in mergeSpans (locSpan startTok) (locSpan endTok)
        [] -> case before of
          (nextTok:_) -> zeroWidthSpan (locSpan nextTok)
          [] -> defaultSpan "<input>"
  return $ Located spanLoc value

mergeSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSpans (SourceSpan file start _) (SourceSpan _ _ end) = SourceSpan file start end

zeroWidthSpan :: SourceSpan -> SourceSpan
zeroWidthSpan (SourceSpan file start _) = SourceSpan file start start

defaultSpan :: Text -> SourceSpan
defaultSpan file = SourceSpan file (Common.SourcePos 0 0) (Common.SourcePos 0 0)

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