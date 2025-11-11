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
import Data.Bifunctor (first)
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
    Left err -> Left (T.pack (MP.errorBundlePretty err))
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
  , parseLiteralPattern
  , parseWildcardPattern
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
parseTypeExpr = located $ TypeName <$> parseQualifiedName

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