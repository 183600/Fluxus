{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Go parser that converts tokens to AST
module Fluxus.Parser.Go.Parser
  ( -- * Parser types
    GoParser
  , GoParseError(..)
    -- * Main parsing functions
  , parseGo
  , runGoParser
    -- * Top-level parsers
  , parsePackage
  , parseFile
    -- * Declaration parsers
  , parseDeclaration
  , parseFuncDecl
  , parseTypeDecl
  , parseVarDecl
  , parseConstDecl
    -- * Statement parsers
  , parseStatement
  , parseBlockStmt
  , parseIfStmt
  , parseForStmt
  , parseSwitchStmt
    -- * Expression parsers
  , parseExpression
  , parseUnaryExpr
  , parseAtomExpr
    -- * Type parsers
  , parseGoType
  , parseStructType
  , parseInterfaceType
    -- * Utility parsers
  , parseIdentifierList
  , parseExpressionList
  ) where

import Control.Monad (void, when)
import Control.Applicative ((<|>), some, optional, many, (<$))
import Data.Functor (($>))
import qualified Control.Applicative as A
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (many)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Fluxus.AST.Common as Common
import Fluxus.AST.Go
import Fluxus.Parser.Go.Lexer

-- | Simple chainl1 implementation for left-associative operators
chainl1 :: GoParser a -> GoParser (a -> a -> a) -> GoParser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = (do
      f <- op
      y <- p
      rest (f x y)) <|> return x

-- | Parser error type
data GoParseError = GoParseError
  { peMessage :: !Text
  , peLocation :: !SourceSpan
  } deriving (Eq, Show)

-- | Go parser type
type GoParser = Parsec Void [Located GoToken]

-- | Run the Go parser
runGoParser :: Text -> [Located GoToken] -> Either (ParseErrorBundle [Located GoToken] Void) GoAST
runGoParser filename tokens = parse parseGo (T.unpack filename) tokens

-- | Main parser entry point
parseGo :: GoParser GoAST
parseGo = do
  package <- parsePackage
  eof
  return $ GoAST package

-- | Parse a Go package (single file for now)
parsePackage :: GoParser GoPackage
parsePackage = do
  file <- parseFile
  return $ GoPackage
    { goPackageName = goFilePackage file
    , goPackageFiles = [file]
    }

-- | Parse a Go file
parseFile :: GoParser GoFile
parseFile = do
  skipCommentsAndNewlines
  void $ goKeywordP GoKwPackage
  packageName <- parseGoIdentifier
  skipCommentsAndNewlines
  
  imports <- MP.many (parseImportDecl <* skipCommentsAndNewlines)
  decls <- MP.many (parseDeclaration <* skipCommentsAndNewlines)
  
  return $ GoFile
    { goFileName = "<input>"
    , goFilePackage = packageName
    , goFileImports = concat imports
    , goFileDecls = decls
    }

-- | Parse import declarations
parseImportDecl :: GoParser [Located GoImport]
parseImportDecl = do
  void $ goKeywordP GoKwImport
  choice
    [ do
        void $ goDelimiterP GoDelimLeftParen
        imports <- many (located parseImportSpec <* skipCommentsAndNewlines)
        void $ goDelimiterP GoDelimRightParen
        return imports
    , do
        imp <- located parseImportSpec
        return [imp]
    ]
  where
    parseImportSpec = do
      alias <- optional $ choice
        [ goDelimiterP GoDelimDot $> Nothing   -- . import
        , parseGoIdentifier >>= \i -> return (Just i)
        ]
      path <- parseGoString
      case alias of
        Nothing -> return $ GoImportNormal Nothing path
        Just Nothing -> return $ GoImportDot path
        Just (Just ident) 
          | ident == Identifier "_" -> return $ GoImportBlank path
          | otherwise -> return $ GoImportNormal (Just ident) path

-- | Parse top-level declarations
parseDeclaration :: GoParser (Located GoDecl)
parseDeclaration = located $ choice
  [ try parseFuncDecl
  , try parseTypeDecl
  , try parseVarDecl
  , parseConstDecl
  ]

-- | Parse function declarations
parseFuncDecl :: GoParser GoDecl
parseFuncDecl = do
  void $ goKeywordP GoKwFunc
  
  -- Check for method receiver
  receiver <- optional $ do
    void $ goDelimiterP GoDelimLeftParen
    recv <- parseReceiver
    void $ goDelimiterP GoDelimRightParen
    return recv
  
  name <- parseGoIdentifier
  
  void $ goDelimiterP GoDelimLeftParen
  params <- parseParameterList
  void $ goDelimiterP GoDelimRightParen
  
  -- Skip return type parsing for now
  let results = []
  
  body <- optional parseBlockStmt
  
  let func = GoFunction
        { goFuncName = Just name
        , goFuncParams = params
        , goFuncResults = results
        , goFuncBody = body
        }
  
  case receiver of
    Nothing -> return $ GoFuncDecl func
    Just recv -> return $ GoMethodDecl recv func

-- | Parse type declarations
parseTypeDecl :: GoParser GoDecl
parseTypeDecl = do
  void $ goKeywordP GoKwType
  name <- parseGoIdentifier
  typeExpr <- parseGoType
  return $ GoTypeDecl name typeExpr

-- | Parse variable declarations
parseVarDecl :: GoParser GoDecl
parseVarDecl = do
  void $ goKeywordP GoKwVar
  choice
    [ do
        void $ goDelimiterP GoDelimLeftParen
        specs <- many (parseVarSpec <* skipNewlines)
        void $ goDelimiterP GoDelimRightParen
        return $ GoVarDecl (concat specs)
    , do
        specs <- parseVarSpec
        return $ GoVarDecl specs
    ]
  where
    parseVarSpec = do
      names <- parseIdentifierList
      typeExpr <- optional parseGoType
      values <- optional $ do
        void $ goOperatorP GoOpAssign
        parseExpressionList
      
      let specs = case values of
            Nothing -> map (\name -> (name, typeExpr, Nothing)) names
            Just vals -> zipWith (\name val -> (name, typeExpr, Just val)) names vals
      return specs

-- | Parse constant declarations
parseConstDecl :: GoParser GoDecl
parseConstDecl = do
  void $ goKeywordP GoKwConst
  choice
    [ do
        void $ goDelimiterP GoDelimLeftParen
        specs <- many (parseConstSpec <* skipNewlines)
        void $ goDelimiterP GoDelimRightParen
        return $ GoConstDecl (concat specs)
    , do
        specs <- parseConstSpec
        return $ GoConstDecl specs
    ]
  where
    parseConstSpec = do
      names <- parseIdentifierList
      typeExpr <- optional parseGoType
      void $ goOperatorP GoOpAssign
      values <- parseExpressionList
      
      let specs = zipWith (\name val -> (name, typeExpr, val)) names values
      return specs

-- | Parse statements
parseStatement :: GoParser (Located GoStmt)
parseStatement = located $ choice
  [ try parseSimpleStmt
  , try parseIfStmt
  , try parseForStmt
  , try parseSwitchStmt
  , try parseSelectStmt
  , try parseBlockStmt'
  , try parseReturnStmt
  , try parseBreakStmt
  , try parseContinueStmt
  , try parseGotoStmt
  , try parseFallthroughStmt
  , try parseDeferStmt
  , try parseGoStmt
  , parseEmptyStmt
  ]

-- | Parse simple statements
parseSimpleStmt :: GoParser GoStmt
parseSimpleStmt = choice
  [ try parseAssignment
  , try parseShortVarDecl
  , try parseIncDecStmt
  , try parseSendStmt
  , parseExprStmt
  ]

-- | Parse expression statements
parseExprStmt :: GoParser GoStmt
parseExprStmt = GoExprStmt <$> parseExpression

-- | Parse assignment
parseAssignment :: GoParser GoStmt
parseAssignment = do
  lhs <- parseExpressionList
  void $ goOperatorP GoOpAssign
  rhs <- parseExpressionList
  return $ GoAssign lhs rhs

-- | Parse short variable declaration
parseShortVarDecl :: GoParser GoStmt
parseShortVarDecl = do
  names <- parseIdentifierList
  void $ goOperatorP GoOpDefine
  values <- parseExpressionList
  return $ GoDefine names values

-- | Parse increment/decrement statements
parseIncDecStmt :: GoParser GoStmt
parseIncDecStmt = do
  expr <- parseExpression
  op <- choice
    [ goOperatorP GoOpIncrement $> True
    , goOperatorP GoOpDecrement $> False
    ]
  return $ GoIncDec expr op

-- | Parse send statements
parseSendStmt :: GoParser GoStmt
parseSendStmt = do
  channel <- parseExpression
  void $ goOperatorP GoOpArrow
  value <- parseExpression
  return $ GoSend channel value

-- | Parse if statements
parseIfStmt :: GoParser GoStmt
parseIfStmt = do
  void $ goKeywordP GoKwIf
  
  -- Optional simple statement
  simpleStmt <- optional $ try $ do
    stmt <- parseSimpleStmt
    void $ goDelimiterP GoDelimSemicolon
    return $ located' stmt
  
  condition <- parseExpression
  thenBody <- located parseBlockStmt'
  
  elseBody <- optional $ do
    void $ goKeywordP GoKwElse
    choice
      [ located parseIfStmt      -- else if
      , located parseBlockStmt'  -- else block
      ]
  
  return $ GoIf simpleStmt condition thenBody elseBody

-- | Parse for statements
parseForStmt :: GoParser GoStmt
parseForStmt = do
  void $ goKeywordP GoKwFor
  
  choice
    [ try parseRangeFor
    , try parseForClause
    , parseInfiniteFor
    ]
  where
    parseRangeFor = do
      key <- optional parseGoIdentifier
      value <- optional $ do
        void $ goDelimiterP GoDelimComma
        parseGoIdentifier
      isDefine <- choice
        [ goOperatorP GoOpDefine $> True
        , goOperatorP GoOpAssign $> False
        ]
      void $ goKeywordP GoKwRange
      expr <- parseExpression
      body <- located parseBlockStmt'
      
      let rangeClause = GoRangeClause
            { goRangeKey = key
            , goRangeValue = value
            , goRangeDefine = isDefine
            , goRangeExpr = expr
            }
      return $ GoRange rangeClause body
    
    parseForClause = do
      init <- optional $ do
        stmt <- parseSimpleStmt
        void $ goDelimiterP GoDelimSemicolon
        return $ located' stmt
      
      condition <- optional $ do
        expr <- parseExpression
        void $ goDelimiterP GoDelimSemicolon
        return expr
      
      post <- optional $ do
        stmt <- parseSimpleStmt
        return $ located' stmt
      
      body <- located parseBlockStmt'
      
      let forClause = Just $ GoForClause
            { goForInit = init
            , goForCond = condition
            , goForPost = post
            }
      return $ GoFor forClause body
    
    parseInfiniteFor = do
      body <- located parseBlockStmt'
      return $ GoFor Nothing body

-- | Parse switch statements
parseSwitchStmt :: GoParser GoStmt
parseSwitchStmt = do
  void $ goKeywordP GoKwSwitch
  
  simpleStmt <- optional $ try $ do
    stmt <- parseSimpleStmt
    void $ goDelimiterP GoDelimSemicolon
    return $ located' stmt
  
  expr <- optional parseExpression
  
  void $ goDelimiterP GoDelimLeftBrace
  cases <- many parseCaseClause
  void $ goDelimiterP GoDelimRightBrace
  
  return $ GoSwitch simpleStmt expr cases
  where
    parseCaseClause = located $ choice
      [ do
          void $ goKeywordP GoKwCase
          exprs <- parseExpressionList
          void $ goDelimiterP GoDelimColon
          stmts <- many parseStatement
          return $ GoCase exprs stmts
      , do
          void $ goKeywordP GoKwDefault
          void $ goDelimiterP GoDelimColon
          stmts <- many parseStatement
          return $ GoDefault stmts
      ]

-- | Parse select statements
parseSelectStmt :: GoParser GoStmt
parseSelectStmt = do
  void $ goKeywordP GoKwSelect
  void $ goDelimiterP GoDelimLeftBrace
  cases <- many parseCommClause
  void $ goDelimiterP GoDelimRightBrace
  return $ GoSelect cases
  where
    parseCommClause = located $ choice
      [ do
          void $ goKeywordP GoKwCase
          comm <- parseSimpleStmt
          void $ goDelimiterP GoDelimColon
          stmts <- many parseStatement
          return $ GoCommClause (Just $ located' comm) stmts
      , do
          void $ goKeywordP GoKwDefault
          void $ goDelimiterP GoDelimColon
          stmts <- many parseStatement
          return $ GoCommClause Nothing stmts
      ]

-- | Parse block statements
parseBlockStmt :: GoParser (Located GoStmt)
parseBlockStmt = located parseBlockStmt'

parseBlockStmt' :: GoParser GoStmt
parseBlockStmt' = do
  void $ goDelimiterP GoDelimLeftBrace
  skipCommentsAndNewlines
  stmts <- many (parseStatement <* skipCommentsAndNewlines)
  void $ goDelimiterP GoDelimRightBrace
  return $ GoBlock stmts

-- | Parse return statements
parseReturnStmt :: GoParser GoStmt
parseReturnStmt = do
  void $ goKeywordP GoKwReturn
  exprs <- option [] parseExpressionList
  return $ GoReturn exprs

-- | Parse break statements
parseBreakStmt :: GoParser GoStmt
parseBreakStmt = do
  void $ goKeywordP GoKwBreak
  label <- optional parseGoIdentifier
  return $ GoBreak label

-- | Parse continue statements
parseContinueStmt :: GoParser GoStmt
parseContinueStmt = do
  void $ goKeywordP GoKwContinue
  label <- optional parseGoIdentifier
  return $ GoContinue label

-- | Parse goto statements
parseGotoStmt :: GoParser GoStmt
parseGotoStmt = do
  void $ goKeywordP GoKwGoto
  label <- parseGoIdentifier
  return $ GoGoto label

-- | Parse fallthrough statements
parseFallthroughStmt :: GoParser GoStmt
parseFallthroughStmt = goKeywordP GoKwFallthrough $> GoFallthrough

-- | Parse defer statements
parseDeferStmt :: GoParser GoStmt
parseDeferStmt = do
  void $ goKeywordP GoKwDefer
  expr <- parseExpression
  return $ GoDefer expr

-- | Parse go statements (goroutines)
parseGoStmt :: GoParser GoStmt
parseGoStmt = do
  void $ goKeywordP GoKwGo
  expr <- parseExpression
  return $ GoGo expr

-- | Parse empty statements
parseEmptyStmt :: GoParser GoStmt
parseEmptyStmt = goDelimiterP GoDelimSemicolon $> GoEmpty

-- | Parse expressions
parseExpression :: GoParser (Located GoExpr)
parseExpression = parseOrExpr

parseOrExpr :: GoParser (Located GoExpr)
parseOrExpr = chainl1 parseAndExpr parseOrOp
  where
    parseOrOp = do
      void $ goOperatorP GoOpOr
      return $ \l r -> located' $ GoBinaryOp OpOr l r

parseAndExpr :: GoParser (Located GoExpr)
parseAndExpr = chainl1 parseEqualityExpr parseAndOp
  where
    parseAndOp = do
      void $ goOperatorP GoOpAnd
      return $ \l r -> located' $ GoBinaryOp OpAnd l r

parseEqualityExpr :: GoParser (Located GoExpr)
parseEqualityExpr = chainl1 parseRelationalExpr parseEqOp
  where
    parseEqOp = choice
      [ goOperatorP GoOpEq $> (\l r -> located' $ GoComparison OpEq l r)
      , goOperatorP GoOpNe $> (\l r -> located' $ GoComparison OpNe l r)
      ]

parseRelationalExpr :: GoParser (Located GoExpr)
parseRelationalExpr = chainl1 parseAdditiveExpr parseRelOp
  where
    parseRelOp = choice
      [ goOperatorP GoOpLt $> (\l r -> located' $ GoComparison OpLt l r)
      , goOperatorP GoOpLe $> (\l r -> located' $ GoComparison OpLe l r)
      , goOperatorP GoOpGt $> (\l r -> located' $ GoComparison OpGt l r)
      , goOperatorP GoOpGe $> (\l r -> located' $ GoComparison OpGe l r)
      ]

parseAdditiveExpr :: GoParser (Located GoExpr)
parseAdditiveExpr = chainl1 parseMultiplicativeExpr parseAddOp
  where
    parseAddOp = choice
      [ goOperatorP GoOpPlus $> (\l r -> located' $ GoBinaryOp OpAdd l r)
      , goOperatorP GoOpMinus $> (\l r -> located' $ GoBinaryOp OpSub l r)
      , goOperatorP GoOpBitOr $> (\l r -> located' $ GoBinaryOp OpBitOr l r)
      , goOperatorP GoOpBitXor $> (\l r -> located' $ GoBinaryOp OpBitXor l r)
      ]

parseMultiplicativeExpr :: GoParser (Located GoExpr)
parseMultiplicativeExpr = chainl1 parseUnaryExpr parseMulOp
  where
    parseMulOp = choice
      [ goOperatorP GoOpMult $> (\l r -> located' $ GoBinaryOp OpMul l r)
      , goOperatorP GoOpDiv $> (\l r -> located' $ GoBinaryOp OpDiv l r)
      , goOperatorP GoOpMod $> (\l r -> located' $ GoBinaryOp OpMod l r)
      , goOperatorP GoOpBitAnd $> (\l r -> located' $ GoBinaryOp OpBitAnd l r)
      , goOperatorP GoOpBitClear $> (\l r -> located' $ GoBinaryOp OpBitXor l r)  -- &^ implemented as XOR
      , goOperatorP GoOpLeftShift $> (\l r -> located' $ GoBinaryOp OpShiftL l r)
      , goOperatorP GoOpRightShift $> (\l r -> located' $ GoBinaryOp OpShiftR l r)
      ]

-- | Parse unary expressions
parseUnaryExpr :: GoParser (Located GoExpr)
parseUnaryExpr = choice
  [ do
      op <- choice
        [ goOperatorP GoOpPlus $> OpPositive
        , goOperatorP GoOpMinus $> OpNegate
        , goOperatorP GoOpNot $> OpNot
        , goOperatorP GoOpBitXor $> OpBitNot
        , goOperatorP GoOpAddress $> OpPositive  -- Placeholder
        , goOperatorP GoOpMult $> OpNegate       -- Placeholder (dereference)
        , goOperatorP GoOpArrow $> OpPositive    -- Placeholder (receive)
        ]
      expr <- parseUnaryExpr
      return $ located' $ GoUnaryOp op expr
  , parseAtomExpr
  ]

-- | Parse atomic expressions with postfix operators
parseAtomExpr :: GoParser (Located GoExpr)
parseAtomExpr = do
  atom <- parseAtom
  postfixes <- many parsePostfix
  return $ foldl applyPostfix atom postfixes
  where
    applyPostfix expr postfix = postfix expr

-- | Parse atomic expressions
parseAtom :: GoParser (Located GoExpr)
parseAtom = located $ choice
  [ parseGoLiteral
  , parseGoIdentifierExpr
  , parseParenExpr
  , parseCompositeLit
  ]

-- | Parse Go literals
parseGoLiteral :: GoParser GoExpr
parseGoLiteral = do
  Located _ token <- anySingle
  case token of
    GoTokenInt text -> return $ GoLiteral $ GoInt (read $ T.unpack text)
    GoTokenFloat text -> return $ GoLiteral $ GoFloat (read $ T.unpack text)
    GoTokenImag text -> return $ GoLiteral $ GoImag (read $ T.unpack $ T.init text)  -- Remove 'i'
    GoTokenString text -> return $ GoLiteral $ GoString text
    GoTokenRawString text -> return $ GoLiteral $ GoRawString text
    GoTokenRune char -> return $ GoLiteral $ GoRune char
    _ -> fail "Expected literal"

-- | Parse identifiers as expressions
parseGoIdentifierExpr :: GoParser GoExpr
parseGoIdentifierExpr = GoIdent <$> parseGoIdentifier

-- | Parse parenthesized expressions
parseParenExpr :: GoParser GoExpr
parseParenExpr = do
  void $ goDelimiterP GoDelimLeftParen
  expr <- parseExpression
  void $ goDelimiterP GoDelimRightParen
  return $ locatedValue expr

-- | Parse composite literals
parseCompositeLit :: GoParser GoExpr
parseCompositeLit = do
  typeExpr <- optional parseGoType
  void $ goDelimiterP GoDelimLeftBrace
  elements <- parseExpression `sepBy` goDelimiterP GoDelimComma
  void $ goDelimiterP GoDelimRightBrace
  return $ GoCompositeLit typeExpr elements

-- | Parse postfix operators
parsePostfix :: GoParser (Located GoExpr -> Located GoExpr)
parsePostfix = choice
  [ parseCall
  , parseIndex
  , parseSlice
  , parseSelector
  , parseTypeAssertion
  ]

-- | Parse function calls
parseCall :: GoParser (Located GoExpr -> Located GoExpr)
parseCall = do
  void $ goDelimiterP GoDelimLeftParen
  args <- parseExpressionList
  void $ goDelimiterP GoDelimRightParen
  return $ \expr -> located' $ GoCall expr args

-- | Parse array/slice indexing
parseIndex :: GoParser (Located GoExpr -> Located GoExpr)
parseIndex = do
  void $ goDelimiterP GoDelimLeftBracket
  index <- parseExpression
  void $ goDelimiterP GoDelimRightBracket
  return $ \expr -> located' $ GoIndex expr index

-- | Parse slice expressions
parseSlice :: GoParser (Located GoExpr -> Located GoExpr)
parseSlice = do
  void $ goDelimiterP GoDelimLeftBracket
  low <- optional parseExpression
  void $ goDelimiterP GoDelimColon
  high <- optional parseExpression
  max_ <- optional $ do
    void $ goDelimiterP GoDelimColon
    parseExpression
  void $ goDelimiterP GoDelimRightBracket
  
  let sliceExpr = GoSliceExpr
        { goSliceLow = low
        , goSliceHigh = high
        , goSliceMax = max_
        }
  return $ \expr -> located' $ GoSlice expr sliceExpr

-- | Parse selector expressions
parseSelector :: GoParser (Located GoExpr -> Located GoExpr)
parseSelector = do
  void $ goDelimiterP GoDelimDot
  field <- parseGoIdentifier
  return $ \expr -> located' $ GoSelector expr field

-- | Parse type assertions
parseTypeAssertion :: GoParser (Located GoExpr -> Located GoExpr)
parseTypeAssertion = do
  void $ goDelimiterP GoDelimDot
  void $ goDelimiterP GoDelimLeftParen
  typeExpr <- parseGoType
  void $ goDelimiterP GoDelimRightParen
  return $ \expr -> located' $ GoTypeAssert expr typeExpr

-- | Parse Go types
parseGoType :: GoParser (Located GoType)
parseGoType = located $ choice
  [ try parseArrayType
  , try parseSliceType
  , try parseMapType
  , try parseChanType
  , try parsePointerType
  , try parseFuncType
  , try parseInterfaceType
  , try parseStructType
  , try parseBasicType
  ]

-- | Parse basic types
parseBasicType :: GoParser GoType
parseBasicType = GoBasicType <$> parseGoIdentifier

-- | Parse array types
parseArrayType :: GoParser GoType
parseArrayType = do
  void $ goDelimiterP GoDelimLeftBracket
  size <- parseExpression
  void $ goDelimiterP GoDelimRightBracket
  elemType <- parseGoType
  return $ GoArrayType size elemType

-- | Parse slice types
parseSliceType :: GoParser GoType
parseSliceType = do
  void $ goDelimiterP GoDelimLeftBracket
  void $ goDelimiterP GoDelimRightBracket
  elemType <- parseGoType
  return $ GoSliceType elemType

-- | Parse map types
parseMapType :: GoParser GoType
parseMapType = do
  void $ goKeywordP GoKwMap
  void $ goDelimiterP GoDelimLeftBracket
  keyType <- parseGoType
  void $ goDelimiterP GoDelimRightBracket
  valueType <- parseGoType
  return $ GoMapType keyType valueType

-- | Parse channel types
parseChanType :: GoParser GoType
parseChanType = choice
  [ do
      void $ goOperatorP GoOpArrow
      void $ goKeywordP GoKwChan
      elemType <- parseGoType
      return $ GoChanType GoChanRecv elemType
  , do
      void $ goKeywordP GoKwChan
      choice
        [ do
            void $ goOperatorP GoOpArrow
            elemType <- parseGoType
            return $ GoChanType GoChanSend elemType
        , do
            elemType <- parseGoType
            return $ GoChanType GoChanBidi elemType
        ]
  ]

-- | Parse pointer types
parsePointerType :: GoParser GoType
parsePointerType = do
  void $ goOperatorP GoOpMult
  baseType <- parseGoType
  return $ GoPointerType baseType

-- | Parse function types
parseFuncType :: GoParser GoType
parseFuncType = do
  void $ goKeywordP GoKwFunc
  void $ goDelimiterP GoDelimLeftParen
  params <- parseParameterList
  void $ goDelimiterP GoDelimRightParen
  
  results <- optional $ choice
    [ do
        void $ goDelimiterP GoDelimLeftParen
        res <- parseParameterList
        void $ goDelimiterP GoDelimRightParen
        return res
    , do
        res <- parseGoType
        return [GoField [] res Nothing]
    ]
  
  return $ GoFuncType params (maybe [] id results)

-- | Parse interface types
parseInterfaceType :: GoParser GoType
parseInterfaceType = do
  void $ goKeywordP GoKwInterface
  void $ goDelimiterP GoDelimLeftBrace
  methods <- many parseMethodSpec
  void $ goDelimiterP GoDelimRightBrace
  return $ GoInterfaceType methods
  where
    parseMethodSpec = do
      name <- parseGoIdentifier
      typeExpr <- parseGoType
      return $ GoMethod name typeExpr

-- | Parse struct types
parseStructType :: GoParser GoType
parseStructType = do
  void $ goKeywordP GoKwStruct
  void $ goDelimiterP GoDelimLeftBrace
  fields <- many parseFieldDecl
  void $ goDelimiterP GoDelimRightBrace
  return $ GoStructType (concat fields)
  where
    parseFieldDecl = do
      names <- option [] parseIdentifierList
      typeExpr <- parseGoType
      tag <- optional parseGoString
      return [GoField names typeExpr tag]

-- | Parse method receivers
parseReceiver :: GoParser GoReceiver
parseReceiver = do
  name <- optional parseGoIdentifier
  typeExpr <- parseGoType
  return $ GoReceiver name typeExpr

-- | Utility parsers
parseGoIdentifier :: GoParser Identifier
parseGoIdentifier = do
  Located _ token <- anySingle
  case token of
    GoTokenIdent text -> return $ Identifier text
    _ -> fail "Expected identifier"

parseGoString :: GoParser Text
parseGoString = do
  Located _ token <- anySingle
  case token of
    GoTokenString text -> return text
    GoTokenRawString text -> return text
    _ -> fail "Expected string"

parseIdentifierList :: GoParser [Identifier]
parseIdentifierList = parseGoIdentifier `sepBy1` goDelimiterP GoDelimComma

parseExpressionList :: GoParser [Located GoExpr]
parseExpressionList = parseExpression `sepBy1` goDelimiterP GoDelimComma

parseParameterList :: GoParser [GoField]
parseParameterList = option [] (parseParameter `sepBy1` goDelimiterP GoDelimComma)
  where
    parseParameter = do
      names <- option [] parseIdentifierList
      typeExpr <- parseGoType
      return $ GoField names typeExpr Nothing

-- | Token matching utilities
goKeywordP :: GoKeyword -> GoParser ()
goKeywordP kw = void $ satisfy $ \case
  Located _ (GoTokenKeyword kw') -> kw == kw'
  _ -> False

goOperatorP :: GoOperator -> GoParser ()
goOperatorP op = void $ satisfy $ \case
  Located _ (GoTokenOperator op') -> op == op'
  _ -> False

goDelimiterP :: GoDelimiter -> GoParser ()
goDelimiterP delim = void $ satisfy $ \case
  Located _ (GoTokenDelimiter delim') -> delim == delim'
  _ -> False

skipNewlines :: GoParser ()
skipNewlines = void $ MP.many $ satisfy $ \case
  Located _ GoTokenNewline -> True
  _ -> False

skipComments :: GoParser ()
skipComments = void $ MP.many $ satisfy $ \case
  Located _ (GoTokenComment _) -> True
  _ -> False

skipCommentsAndNewlines :: GoParser ()
skipCommentsAndNewlines = void $ MP.many $ satisfy $ \case
  Located _ GoTokenNewline -> True
  Located _ (GoTokenComment _) -> True
  _ -> False

-- | Helper for creating located expressions
located :: GoParser a -> GoParser (Located a)
located parser = do
  value <- parser
  -- Create a dummy span since we can't easily get source positions
  let span = SourceSpan "<input>" (Common.SourcePos 0 0) (Common.SourcePos 0 0)
  return $ Located span value

located' :: a -> Located a
located' = noLoc

convertPos :: MP.SourcePos -> Common.SourcePos
convertPos pos = Common.SourcePos
  { posLine = unPos (sourceLine pos)
  , posColumn = unPos (sourceColumn pos)
  }