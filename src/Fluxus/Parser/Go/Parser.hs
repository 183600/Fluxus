{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

-- | Go parser that converts tokens to AST
module Fluxus.Parser.Go.Parser
  ( -- * Parser types
    GoParser
  , ParseError(..)
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
  , parseBlock
  , parseIfStmt
  , parseForStmt
  , parseSwitchStmt
    -- * Expression parsers
  , parseExpression
  , parseUnaryExpr
  , parseBinaryExpr
  , parseAtomExpr
  , parseCall
    -- * Type parsers
  , parseType
  , parseStructType
  , parseInterfaceType
    -- * Utility parsers
  , parseIdentifierList
  , parseExpressionList
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
import Fluxus.AST.Go
import Fluxus.Parser.Go.Lexer

-- | Parser error type
data ParseError = ParseError
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
  skipNewlines
  void $ goKeyword GoKwPackage
  packageName <- parseGoIdentifier
  skipNewlines
  
  imports <- many (parseImportDecl <* skipNewlines)
  decls <- many (parseDeclaration <* skipNewlines)
  
  return $ GoFile
    { goFileName = "<input>"
    , goFilePackage = packageName
    , goFileImports = concat imports
    , goFileDecls = decls
    }

-- | Parse import declarations
parseImportDecl :: GoParser [Located GoImport]
parseImportDecl = do
  void $ goKeyword GoKwImport
  choice
    [ do
        void $ goDelimiter GoDelimLeftParen
        imports <- many (located parseImportSpec <* skipNewlines)
        void $ goDelimiter GoDelimRightParen
        return imports
    , do
        imp <- located parseImportSpec
        return [imp]
    ]
  where
    parseImportSpec = do
      alias <- optional $ choice
        [ goDelimiter GoDelimDot $> Nothing   -- . import
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
  void $ goKeyword GoKwFunc
  
  -- Check for method receiver
  receiver <- optional $ do
    void $ goDelimiter GoDelimLeftParen
    recv <- parseReceiver
    void $ goDelimiter GoDelimRightParen
    return recv
  
  name <- parseGoIdentifier
  
  void $ goDelimiter GoDelimLeftParen
  params <- parseParameterList
  void $ goDelimiter GoDelimRightParen
  
  results <- optional $ choice
    [ do
        void $ goDelimiter GoDelimLeftParen
        res <- parseParameterList
        void $ goDelimiter GoDelimRightParen
        return res
    , do
        res <- parseGoType
        return [GoField [] res Nothing]
    ]
  
  body <- optional parseBlockStmt
  
  let func = GoFunction
        { goFuncName = Just name
        , goFuncParams = params
        , goFuncResults = maybe [] id results
        , goFuncBody = body
        }
  
  case receiver of
    Nothing -> return $ GoFuncDecl func
    Just recv -> return $ GoMethodDecl recv func

-- | Parse type declarations
parseTypeDecl :: GoParser GoDecl
parseTypeDecl = do
  void $ goKeyword GoKwType
  name <- parseGoIdentifier
  typeExpr <- parseGoType
  return $ GoTypeDecl name typeExpr

-- | Parse variable declarations
parseVarDecl :: GoParser GoDecl
parseVarDecl = do
  void $ goKeyword GoKwVar
  choice
    [ do
        void $ goDelimiter GoDelimLeftParen
        specs <- many (parseVarSpec <* skipNewlines)
        void $ goDelimiter GoDelimRightParen
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
        void $ goOperator GoOpAssign
        parseExpressionList
      
      let specs = case values of
            Nothing -> map (\name -> (name, typeExpr, Nothing)) names
            Just vals -> zipWith (\name val -> (name, typeExpr, Just val)) names vals
      return specs

-- | Parse constant declarations
parseConstDecl :: GoParser GoDecl
parseConstDecl = do
  void $ goKeyword GoKwConst
  choice
    [ do
        void $ goDelimiter GoDelimLeftParen
        specs <- many (parseConstSpec <* skipNewlines)
        void $ goDelimiter GoDelimRightParen
        return $ GoConstDecl (concat specs)
    , do
        specs <- parseConstSpec
        return $ GoConstDecl specs
    ]
  where
    parseConstSpec = do
      names <- parseIdentifierList
      typeExpr <- optional parseGoType
      void $ goOperator GoOpAssign
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
  void $ goOperator GoOpAssign
  rhs <- parseExpressionList
  return $ GoAssign lhs rhs

-- | Parse short variable declaration
parseShortVarDecl :: GoParser GoStmt
parseShortVarDecl = do
  names <- parseIdentifierList
  void $ goOperator GoOpDefine
  values <- parseExpressionList
  return $ GoDefine names values

-- | Parse increment/decrement statements
parseIncDecStmt :: GoParser GoStmt
parseIncDecStmt = do
  expr <- parseExpression
  op <- choice
    [ goOperator GoOpIncrement $> True
    , goOperator GoOpDecrement $> False
    ]
  return $ GoIncDec expr op

-- | Parse send statements
parseSendStmt :: GoParser GoStmt
parseSendStmt = do
  channel <- parseExpression
  void $ goOperator GoOpArrow
  value <- parseExpression
  return $ GoSend channel value

-- | Parse if statements
parseIfStmt :: GoParser GoStmt
parseIfStmt = do
  void $ goKeyword GoKwIf
  
  -- Optional simple statement
  simpleStmt <- optional $ try $ do
    stmt <- parseSimpleStmt
    void $ goDelimiter GoDelimSemicolon
    return $ located' stmt
  
  condition <- parseExpression
  thenBody <- located parseBlockStmt'
  
  elseBody <- optional $ do
    void $ goKeyword GoKwElse
    choice
      [ located parseIfStmt      -- else if
      , located parseBlockStmt'  -- else block
      ]
  
  return $ GoIf simpleStmt condition thenBody elseBody

-- | Parse for statements
parseForStmt :: GoParser GoStmt
parseForStmt = do
  void $ goKeyword GoKwFor
  
  choice
    [ try parseRangeFor
    , try parseForClause
    , parseInfiniteFor
    ]
  where
    parseRangeFor = do
      key <- optional parseGoIdentifier
      value <- optional $ do
        void $ goDelimiter GoDelimComma
        parseGoIdentifier
      isDefine <- choice
        [ goOperator GoOpDefine $> True
        , goOperator GoOpAssign $> False
        ]
      void $ goKeyword GoKwRange
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
        void $ goDelimiter GoDelimSemicolon
        return $ located' stmt
      
      condition <- optional $ do
        expr <- parseExpression
        void $ goDelimiter GoDelimSemicolon
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
  void $ goKeyword GoKwSwitch
  
  simpleStmt <- optional $ try $ do
    stmt <- parseSimpleStmt
    void $ goDelimiter GoDelimSemicolon
    return $ located' stmt
  
  expr <- optional parseExpression
  
  void $ goDelimiter GoDelimLeftBrace
  cases <- many parseCaseClause
  void $ goDelimiter GoDelimRightBrace
  
  return $ GoSwitch simpleStmt expr cases
  where
    parseCaseClause = located $ choice
      [ do
          void $ goKeyword GoKwCase
          exprs <- parseExpressionList
          void $ goDelimiter GoDelimColon
          stmts <- many parseStatement
          return $ GoCase exprs stmts
      , do
          void $ goKeyword GoKwDefault
          void $ goDelimiter GoDelimColon
          stmts <- many parseStatement
          return $ GoDefault stmts
      ]

-- | Parse select statements
parseSelectStmt :: GoParser GoStmt
parseSelectStmt = do
  void $ goKeyword GoKwSelect
  void $ goDelimiter GoDelimLeftBrace
  cases <- many parseCommClause
  void $ goDelimiter GoDelimRightBrace
  return $ GoSelect cases
  where
    parseCommClause = located $ choice
      [ do
          void $ goKeyword GoKwCase
          comm <- parseSimpleStmt
          void $ goDelimiter GoDelimColon
          stmts <- many parseStatement
          return $ GoCommCase (Just $ located' comm) stmts
      , do
          void $ goKeyword GoKwDefault
          void $ goDelimiter GoDelimColon
          stmts <- many parseStatement
          return $ GoCommDefault stmts
      ]

-- | Parse block statements
parseBlockStmt :: GoParser (Located GoStmt)
parseBlockStmt = located parseBlockStmt'

parseBlockStmt' :: GoParser GoStmt
parseBlockStmt' = do
  void $ goDelimiter GoDelimLeftBrace
  stmts <- many parseStatement
  void $ goDelimiter GoDelimRightBrace
  return $ GoBlock stmts

-- | Parse return statements
parseReturnStmt :: GoParser GoStmt
parseReturnStmt = do
  void $ goKeyword GoKwReturn
  exprs <- option [] parseExpressionList
  return $ GoReturn exprs

-- | Parse break statements
parseBreakStmt :: GoParser GoStmt
parseBreakStmt = do
  void $ goKeyword GoKwBreak
  label <- optional parseGoIdentifier
  return $ GoBreak label

-- | Parse continue statements
parseContinueStmt :: GoParser GoStmt
parseContinueStmt = do
  void $ goKeyword GoKwContinue
  label <- optional parseGoIdentifier
  return $ GoContinue label

-- | Parse goto statements
parseGotoStmt :: GoParser GoStmt
parseGotoStmt = do
  void $ goKeyword GoKwGoto
  label <- parseGoIdentifier
  return $ GoGoto label

-- | Parse fallthrough statements
parseFallthroughStmt :: GoParser GoStmt
parseFallthroughStmt = goKeyword GoKwFallthrough $> GoFallthrough

-- | Parse defer statements
parseDeferStmt :: GoParser GoStmt
parseDeferStmt = do
  void $ goKeyword GoKwDefer
  expr <- parseExpression
  return $ GoDefer expr

-- | Parse go statements (goroutines)
parseGoStmt :: GoParser GoStmt
parseGoStmt = do
  void $ goKeyword GoKwGo
  expr <- parseExpression
  return $ GoGo expr

-- | Parse empty statements
parseEmptyStmt :: GoParser GoStmt
parseEmptyStmt = goDelimiter GoDelimSemicolon $> GoEmpty

-- | Parse expressions
parseExpression :: GoParser (Located GoExpr)
parseExpression = parseOrExpr

parseOrExpr :: GoParser (Located GoExpr)
parseOrExpr = chainl1 parseAndExpr parseOrOp
  where
    parseOrOp = do
      void $ goOperator GoOpOr
      return $ \l r -> located' $ GoBinaryOp OpOr l r

parseAndExpr :: GoParser (Located GoExpr)
parseAndExpr = chainl1 parseEqualityExpr parseAndOp
  where
    parseAndOp = do
      void $ goOperator GoOpAnd
      return $ \l r -> located' $ GoBinaryOp OpAnd l r

parseEqualityExpr :: GoParser (Located GoExpr)
parseEqualityExpr = chainl1 parseRelationalExpr parseEqOp
  where
    parseEqOp = choice
      [ goOperator GoOpEq $> (\l r -> located' $ GoComparison OpEq l r)
      , goOperator GoOpNe $> (\l r -> located' $ GoComparison OpNe l r)
      ]

parseRelationalExpr :: GoParser (Located GoExpr)
parseRelationalExpr = chainl1 parseAdditiveExpr parseRelOp
  where
    parseRelOp = choice
      [ goOperator GoOpLt $> (\l r -> located' $ GoComparison OpLt l r)
      , goOperator GoOpLe $> (\l r -> located' $ GoComparison OpLe l r)
      , goOperator GoOpGt $> (\l r -> located' $ GoComparison OpGt l r)
      , goOperator GoOpGe $> (\l r -> located' $ GoComparison OpGe l r)
      ]

parseAdditiveExpr :: GoParser (Located GoExpr)
parseAdditiveExpr = chainl1 parseMultiplicativeExpr parseAddOp
  where
    parseAddOp = choice
      [ goOperator GoOpPlus $> (\l r -> located' $ GoBinaryOp OpAdd l r)
      , goOperator GoOpMinus $> (\l r -> located' $ GoBinaryOp OpSub l r)
      , goOperator GoOpBitOr $> (\l r -> located' $ GoBinaryOp OpBitOr l r)
      , goOperator GoOpBitXor $> (\l r -> located' $ GoBinaryOp OpBitXor l r)
      ]

parseMultiplicativeExpr :: GoParser (Located GoExpr)
parseMultiplicativeExpr = chainl1 parseUnaryExpr parseMulOp
  where
    parseMulOp = choice
      [ goOperator GoOpMult $> (\l r -> located' $ GoBinaryOp OpMul l r)
      , goOperator GoOpDiv $> (\l r -> located' $ GoBinaryOp OpDiv l r)
      , goOperator GoOpMod $> (\l r -> located' $ GoBinaryOp OpMod l r)
      , goOperator GoOpBitAnd $> (\l r -> located' $ GoBinaryOp OpBitAnd l r)
      , goOperator GoOpBitClear $> (\l r -> located' $ GoBinaryOp OpBitXor l r)  -- &^ implemented as XOR
      , goOperator GoOpLeftShift $> (\l r -> located' $ GoBinaryOp OpShiftL l r)
      , goOperator GoOpRightShift $> (\l r -> located' $ GoBinaryOp OpShiftR l r)
      ]

-- | Parse unary expressions
parseUnaryExpr :: GoParser (Located GoExpr)
parseUnaryExpr = choice
  [ do
      op <- choice
        [ goOperator GoOpPlus $> OpPositive
        , goOperator GoOpMinus $> OpNegate
        , goOperator GoOpNot $> OpNot
        , goOperator GoOpBitXor $> OpBitNot
        , goOperator GoOpAddress $> OpPositive  -- Placeholder
        , goOperator GoOpMult $> OpNegate       -- Placeholder (dereference)
        , goOperator GoOpArrow $> OpPositive    -- Placeholder (receive)
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
  void $ goDelimiter GoDelimLeftParen
  expr <- parseExpression
  void $ goDelimiter GoDelimRightParen
  return $ locatedValue expr

-- | Parse composite literals
parseCompositeLit :: GoParser GoExpr
parseCompositeLit = do
  typeExpr <- optional parseGoType
  void $ goDelimiter GoDelimLeftBrace
  elements <- parseExpression `sepBy` goDelimiter GoDelimComma
  void $ goDelimiter GoDelimRightBrace
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
  void $ goDelimiter GoDelimLeftParen
  args <- parseExpressionList
  void $ goDelimiter GoDelimRightParen
  return $ \expr -> located' $ GoCall expr args

-- | Parse array/slice indexing
parseIndex :: GoParser (Located GoExpr -> Located GoExpr)
parseIndex = do
  void $ goDelimiter GoDelimLeftBracket
  index <- parseExpression
  void $ goDelimiter GoDelimRightBracket
  return $ \expr -> located' $ GoIndex expr index

-- | Parse slice expressions
parseSlice :: GoParser (Located GoExpr -> Located GoExpr)
parseSlice = do
  void $ goDelimiter GoDelimLeftBracket
  low <- optional parseExpression
  void $ goDelimiter GoDelimColon
  high <- optional parseExpression
  max_ <- optional $ do
    void $ goDelimiter GoDelimColon
    parseExpression
  void $ goDelimiter GoDelimRightBracket
  
  let sliceExpr = GoSliceExpr
        { goSliceLow = low
        , goSliceHigh = high
        , goSliceMax = max_
        }
  return $ \expr -> located' $ GoSlice expr sliceExpr

-- | Parse selector expressions
parseSelector :: GoParser (Located GoExpr -> Located GoExpr)
parseSelector = do
  void $ goDelimiter GoDelimDot
  field <- parseGoIdentifier
  return $ \expr -> located' $ GoSelector expr field

-- | Parse type assertions
parseTypeAssertion :: GoParser (Located GoExpr -> Located GoExpr)
parseTypeAssertion = do
  void $ goDelimiter GoDelimDot
  void $ goDelimiter GoDelimLeftParen
  typeExpr <- parseGoType
  void $ goDelimiter GoDelimRightParen
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
  , parseBasicType
  ]

-- | Parse basic types
parseBasicType :: GoParser GoType
parseBasicType = GoBasicType <$> parseGoIdentifier

-- | Parse array types
parseArrayType :: GoParser GoType
parseArrayType = do
  void $ goDelimiter GoDelimLeftBracket
  size <- parseExpression
  void $ goDelimiter GoDelimRightBracket
  elemType <- parseGoType
  return $ GoArrayType size elemType

-- | Parse slice types
parseSliceType :: GoParser GoType
parseSliceType = do
  void $ goDelimiter GoDelimLeftBracket
  void $ goDelimiter GoDelimRightBracket
  elemType <- parseGoType
  return $ GoSliceType elemType

-- | Parse map types
parseMapType :: GoParser GoType
parseMapType = do
  void $ goKeyword GoKwMap
  void $ goDelimiter GoDelimLeftBracket
  keyType <- parseGoType
  void $ goDelimiter GoDelimRightBracket
  valueType <- parseGoType
  return $ GoMapType keyType valueType

-- | Parse channel types
parseChanType :: GoParser GoType
parseChanType = choice
  [ do
      void $ goOperator GoOpArrow
      void $ goKeyword GoKwChan
      elemType <- parseGoType
      return $ GoChanType GoChanRecv elemType
  , do
      void $ goKeyword GoKwChan
      choice
        [ do
            void $ goOperator GoOpArrow
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
  void $ goOperator GoOpMult
  baseType <- parseGoType
  return $ GoPointerType baseType

-- | Parse function types
parseFuncType :: GoParser GoType
parseFuncType = do
  void $ goKeyword GoKwFunc
  void $ goDelimiter GoDelimLeftParen
  params <- parseParameterList
  void $ goDelimiter GoDelimRightParen
  
  results <- optional $ choice
    [ do
        void $ goDelimiter GoDelimLeftParen
        res <- parseParameterList
        void $ goDelimiter GoDelimRightParen
        return res
    , do
        res <- parseGoType
        return [GoField [] res Nothing]
    ]
  
  return $ GoFuncType params (maybe [] id results)

-- | Parse interface types
parseInterfaceType :: GoParser GoType
parseInterfaceType = do
  void $ goKeyword GoKwInterface
  void $ goDelimiter GoDelimLeftBrace
  methods <- many parseMethodSpec
  void $ goDelimiter GoDelimRightBrace
  return $ GoInterfaceType methods
  where
    parseMethodSpec = do
      name <- parseGoIdentifier
      typeExpr <- parseGoType
      return $ GoMethod name typeExpr

-- | Parse struct types
parseStructType :: GoParser GoType
parseStructType = do
  void $ goKeyword GoKwStruct
  void $ goDelimiter GoDelimLeftBrace
  fields <- many parseFieldDecl
  void $ goDelimiter GoDelimRightBrace
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
parseIdentifierList = parseGoIdentifier `sepBy1` goDelimiter GoDelimComma

parseExpressionList :: GoParser [Located GoExpr]
parseExpressionList = parseExpression `sepBy1` goDelimiter GoDelimComma

parseParameterList :: GoParser [GoField]
parseParameterList = parseParameter `sepBy` goDelimiter GoDelimComma
  where
    parseParameter = do
      names <- option [] parseIdentifierList
      typeExpr <- parseGoType
      return $ GoField names typeExpr Nothing

-- | Token matching utilities
goKeyword :: GoKeyword -> GoParser ()
goKeyword kw = void $ satisfy $ \case
  Located _ (GoTokenKeyword kw') -> kw == kw'
  _ -> False

goOperator :: GoOperator -> GoParser ()
goOperator op = void $ satisfy $ \case
  Located _ (GoTokenOperator op') -> op == op'
  _ -> False

goDelimiter :: GoDelimiter -> GoParser ()
goDelimiter delim = void $ satisfy $ \case
  Located _ (GoTokenDelimiter delim') -> delim == delim'
  _ -> False

skipNewlines :: GoParser ()
skipNewlines = void $ many $ satisfy $ \case
  Located _ GoTokenNewline -> True
  _ -> False

-- | Helper for creating located expressions
located :: GoParser a -> GoParser (Located a)
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