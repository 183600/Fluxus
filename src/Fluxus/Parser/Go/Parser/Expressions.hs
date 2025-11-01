{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Fluxus.Parser.Go.Parser.Expressions
  ( parseExpression
  , parseUnaryExpr
  , parseAtomExpr
  , parseGoLiteral
  , parseGoIdentifierExpr
  , parseParenExpr
  , parseCompositeLit
  , parsePostfix
  , parseCall
  , parseIndex
  , parseSlice
  , parseSelector
  , parseTypeAssertion
  , parseGoType
  , parseStructType
  , parseInterfaceType
  , parseExpressionList
  , parseParameterList
  ) where

import Control.Applicative (optional, many)
import Control.Monad (void)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (lookAhead)
import qualified Text.Megaparsec as MP

import Fluxus.AST.Common
  ( Identifier(..)
  , Located(..)
  , locatedValue
  , BinaryOp(..)
  , UnaryOp(..)
  , ComparisonOp(..)
  )
import Fluxus.AST.Go
  ( GoExpr(..)
  , GoLiteral(..)
  , GoField(..)
  , GoSliceExpr(..)
  , GoType(..)
  , GoChanDirection(..)
  )
import Fluxus.Parser.Go.Lexer
  ( GoDelimiter(..)
  , GoKeyword(..)
  , GoOperator(..)
  , GoToken(..)
  )
import Fluxus.Parser.Go.Parser.Common
  ( GoParser
  , chainl1
  , located
  , located'
  , parseGoIdentifier
  , parseGoString
  , goKeywordP
  , goOperatorP
  , goDelimiterP
  , skipCommentsAndNewlines
  , parseIdentifierList
  )

-- | Parse expressions with operator precedence.
parseExpression :: Monad m => GoParser m (Located GoExpr)
parseExpression = parseOrExpr

parseOrExpr :: Monad m => GoParser m (Located GoExpr)
parseOrExpr = chainl1 parseAndExpr parseOrOp
  where
    parseOrOp = do
      void $ goOperatorP GoOpOr
      pure $ \l r -> located' $ GoBinaryOp OpOr l r

parseAndExpr :: Monad m => GoParser m (Located GoExpr)
parseAndExpr = chainl1 parseEqualityExpr parseAndOp
  where
    parseAndOp = do
      void $ goOperatorP GoOpAnd
      pure $ \l r -> located' $ GoBinaryOp OpAnd l r

parseEqualityExpr :: Monad m => GoParser m (Located GoExpr)
parseEqualityExpr = chainl1 parseRelationalExpr parseEqOp
  where
    parseEqOp = MP.choice
      [ goOperatorP GoOpEq $> (\l r -> located' $ GoComparison OpEq l r)
      , goOperatorP GoOpNe $> (\l r -> located' $ GoComparison OpNe l r)
      ]

parseRelationalExpr :: Monad m => GoParser m (Located GoExpr)
parseRelationalExpr = chainl1 parseAdditiveExpr parseRelOp
  where
    parseRelOp = MP.choice
      [ goOperatorP GoOpLt $> (\l r -> located' $ GoComparison OpLt l r)
      , goOperatorP GoOpLe $> (\l r -> located' $ GoComparison OpLe l r)
      , goOperatorP GoOpGt $> (\l r -> located' $ GoComparison OpGt l r)
      , goOperatorP GoOpGe $> (\l r -> located' $ GoComparison OpGe l r)
      ]

parseAdditiveExpr :: Monad m => GoParser m (Located GoExpr)
parseAdditiveExpr = chainl1 parseMultiplicativeExpr parseAddOp
  where
    parseAddOp = MP.choice
      [ goOperatorP GoOpPlus $> (\l r -> located' $ GoBinaryOp OpAdd l r)
      , goOperatorP GoOpMinus $> (\l r -> located' $ GoBinaryOp OpSub l r)
      , goOperatorP GoOpBitOr $> (\l r -> located' $ GoBinaryOp OpBitOr l r)
      , goOperatorP GoOpBitXor $> (\l r -> located' $ GoBinaryOp OpBitXor l r)
      ]

parseMultiplicativeExpr :: Monad m => GoParser m (Located GoExpr)
parseMultiplicativeExpr = chainl1 parseUnaryExpr parseMulOp
  where
    parseMulOp = MP.choice
      [ goOperatorP GoOpMult $> (\l r -> located' $ GoBinaryOp OpMul l r)
      , goOperatorP GoOpDiv $> (\l r -> located' $ GoBinaryOp OpDiv l r)
      , goOperatorP GoOpMod $> (\l r -> located' $ GoBinaryOp OpMod l r)
      , goOperatorP GoOpBitAnd $> (\l r -> located' $ GoBinaryOp OpBitAnd l r)
      , goOperatorP GoOpBitClear $> (\l r -> located' $ GoBinaryOp OpBitXor l r)
      , goOperatorP GoOpLeftShift $> (\l r -> located' $ GoBinaryOp OpShiftL l r)
      , goOperatorP GoOpRightShift $> (\l r -> located' $ GoBinaryOp OpShiftR l r)
      ]

-- | Parse unary expressions.
parseUnaryExpr :: Monad m => GoParser m (Located GoExpr)
parseUnaryExpr = MP.choice
  [ do
      op <- MP.choice
        [ goOperatorP GoOpPlus $> OpPositive
        , goOperatorP GoOpMinus $> OpNegate
        , goOperatorP GoOpNot $> OpNot
        , goOperatorP GoOpBitXor $> OpBitNot
        , goOperatorP GoOpAddress $> OpPositive
        , goOperatorP GoOpMult $> OpNegate
        , goOperatorP GoOpArrow $> OpPositive
        ]
      expr <- parseUnaryExpr
      pure $ located' $ GoUnaryOp op expr
  , parseAtomExpr
  ]

-- | Parse atomic expressions with postfix operators.
parseAtomExpr :: Monad m => GoParser m (Located GoExpr)
parseAtomExpr = do
  atom <- parseAtom
  postfixes <- many parsePostfix
  pure $ foldl (\expr f -> f expr) atom postfixes
  where
    parseAtom = located $ MP.choice
      [ parseGoLiteral
      , parseGoIdentifierExpr
      , parseParenExpr
      , parseCompositeLit
      ]

-- | Parse Go literals.
parseGoLiteral :: Monad m => GoParser m GoExpr
parseGoLiteral = do
  Located _ token <- MP.satisfy isLiteralToken
  case token of
    GoTokenInt text -> pure $ GoLiteral $ GoInt (read $ T.unpack text)
    GoTokenFloat text -> pure $ GoLiteral $ GoFloat (read $ T.unpack text)
    GoTokenImag text -> pure $ GoLiteral $ GoImag (read $ T.unpack $ T.init text)
    GoTokenString text -> pure $ GoLiteral $ GoString text
    GoTokenRawString text -> pure $ GoLiteral $ GoRawString text
    GoTokenRune char -> pure $ GoLiteral $ GoRune char
    _ -> fail "Expected literal"
  where
    isLiteralToken (Located _ tok) = case tok of
      GoTokenInt _ -> True
      GoTokenFloat _ -> True
      GoTokenImag _ -> True
      GoTokenString _ -> True
      GoTokenRawString _ -> True
      GoTokenRune _ -> True
      _ -> False

-- | Parse identifiers as expressions.
parseGoIdentifierExpr :: Monad m => GoParser m GoExpr
parseGoIdentifierExpr = GoIdent <$> parseGoIdentifier

-- | Parse parenthesized expressions.
parseParenExpr :: Monad m => GoParser m GoExpr
parseParenExpr = do
  void $ goDelimiterP GoDelimLeftParen
  expr <- parseExpression
  void $ goDelimiterP GoDelimRightParen
  pure $ locatedValue expr

-- | Parse composite literals.
parseCompositeLit :: Monad m => GoParser m GoExpr
parseCompositeLit = do
  typeExpr <- optional parseGoType
  void $ goDelimiterP GoDelimLeftBrace
  elements <- parseExpression `MP.sepBy` goDelimiterP GoDelimComma
  void $ goDelimiterP GoDelimRightBrace
  pure $ GoCompositeLit typeExpr elements

-- | Parse postfix operators.
parsePostfix :: Monad m => GoParser m (Located GoExpr -> Located GoExpr)
parsePostfix = MP.choice
  [ parseCall
  , parseIndex
  , parseSlice
  , parseSelector
  , parseTypeAssertion
  ]

parseCall :: Monad m => GoParser m (Located GoExpr -> Located GoExpr)
parseCall = do
  void $ goDelimiterP GoDelimLeftParen
  args <- parseExpressionList
  void $ goDelimiterP GoDelimRightParen
  pure $ \expr -> located' $ GoCall expr args

parseIndex :: Monad m => GoParser m (Located GoExpr -> Located GoExpr)
parseIndex = do
  void $ goDelimiterP GoDelimLeftBracket
  index <- parseExpression
  void $ goDelimiterP GoDelimRightBracket
  pure $ \expr -> located' $ GoIndex expr index

parseSlice :: Monad m => GoParser m (Located GoExpr -> Located GoExpr)
parseSlice = do
  void $ goDelimiterP GoDelimLeftBracket
  low <- optional parseExpression
  void $ goDelimiterP GoDelimColon
  high <- optional parseExpression
  maxExpr <- optional $ do
    void $ goDelimiterP GoDelimColon
    parseExpression
  void $ goDelimiterP GoDelimRightBracket
  let sliceExpr = GoSliceExpr
        { goSliceLow = low
        , goSliceHigh = high
        , goSliceMax = maxExpr
        }
  pure $ \expr -> located' $ GoSlice expr sliceExpr

parseSelector :: Monad m => GoParser m (Located GoExpr -> Located GoExpr)
parseSelector = do
  void $ goDelimiterP GoDelimDot
  field <- parseGoIdentifier
  pure $ \expr -> located' $ GoSelector expr field

parseTypeAssertion :: Monad m => GoParser m (Located GoExpr -> Located GoExpr)
parseTypeAssertion = do
  void $ goDelimiterP GoDelimDot
  void $ goDelimiterP GoDelimLeftParen
  typeExpr <- parseGoType
  void $ goDelimiterP GoDelimRightParen
  pure $ \expr -> located' $ GoTypeAssert expr typeExpr

-- | Parse Go types.
parseGoType :: Monad m => GoParser m (Located GoType)
parseGoType = located $ MP.choice
  [ MP.try parseArrayType
  , MP.try parseSliceType
  , MP.try parseMapType
  , MP.try parseChanType
  , MP.try parsePointerType
  , MP.try parseFuncType
  , MP.try parseInterfaceType
  , MP.try parseStructType
  , MP.try parseEllipsisType
  , parseBasicType
  ]

parseBasicType :: Monad m => GoParser m GoType
parseBasicType = GoBasicType <$> parseGoIdentifier

parseArrayType :: Monad m => GoParser m GoType
parseArrayType = do
  void $ goDelimiterP GoDelimLeftBracket
  size <- parseExpression
  void $ goDelimiterP GoDelimRightBracket
  elemType <- parseGoType
  pure $ GoArrayType size elemType

parseSliceType :: Monad m => GoParser m GoType
parseSliceType = do
  void $ goDelimiterP GoDelimLeftBracket
  void $ goDelimiterP GoDelimRightBracket
  elemType <- parseGoType
  pure $ GoSliceType elemType

parseMapType :: Monad m => GoParser m GoType
parseMapType = do
  void $ goKeywordP GoKwMap
  void $ goDelimiterP GoDelimLeftBracket
  keyType <- parseGoType
  void $ goDelimiterP GoDelimRightBracket
  valueType <- parseGoType
  pure $ GoMapType keyType valueType

parseChanType :: Monad m => GoParser m GoType
parseChanType = MP.choice
  [ do
      void $ goOperatorP GoOpArrow
      void $ goKeywordP GoKwChan
      elemType <- parseGoType
      pure $ GoChanType GoChanRecv elemType
  , do
      void $ goKeywordP GoKwChan
      MP.choice
        [ do
            void $ goOperatorP GoOpArrow
            elemType <- parseGoType
            pure $ GoChanType GoChanSend elemType
        , do
            elemType <- parseGoType
            pure $ GoChanType GoChanBidi elemType
        ]
  ]

parsePointerType :: Monad m => GoParser m GoType
parsePointerType = do
  void $ goOperatorP GoOpMult
  baseType <- parseGoType
  pure $ GoPointerType baseType

parseEllipsisType :: Monad m => GoParser m GoType
parseEllipsisType = do
  void $ goOperatorP GoOpEllipsis
  elemType <- parseGoType
  pure $ GoEllipsisType elemType

parseFuncType :: Monad m => GoParser m GoType
parseFuncType = do
  void $ goKeywordP GoKwFunc
  void $ goDelimiterP GoDelimLeftParen
  params <- parseParameterList
  void $ goDelimiterP GoDelimRightParen
  results <- optional $ MP.choice
    [ do
        void $ goDelimiterP GoDelimLeftParen
        res <- parseParameterList
        void $ goDelimiterP GoDelimRightParen
        pure res
    , do
        res <- parseGoType
        pure [GoField [] res Nothing]
    ]
  pure $ GoFuncType params (fromMaybe [] results)

parseInterfaceType :: Monad m => GoParser m GoType
parseInterfaceType = do
  void $ goKeywordP GoKwInterface
  void $ goDelimiterP GoDelimLeftBrace
  methods <- many parseMethodSpec
  void $ goDelimiterP GoDelimRightBrace
  pure $ GoInterfaceType methods
  where
    parseMethodSpec = do
      name <- parseGoIdentifier
      typeExpr <- parseGoType
      pure $ GoMethod name typeExpr

parseStructType :: Monad m => GoParser m GoType
parseStructType = do
  void $ goKeywordP GoKwStruct
  void $ goDelimiterP GoDelimLeftBrace
  skipCommentsAndNewlines
  fields <- many (parseFieldDecl <* skipCommentsAndNewlines)
  void $ goDelimiterP GoDelimRightBrace
  pure $ GoStructType (concat fields)
  where
    parseFieldDecl = MP.choice
      [ MP.try $ do
          names <- parseIdentifierList
          skipCommentsAndNewlines
          typeExpr <- parseGoType
          skipCommentsAndNewlines
          tag <- optional parseGoString
          pure [GoField names typeExpr tag]
      , do
          typeExpr <- parseGoType
          skipCommentsAndNewlines
          tag <- optional parseGoString
          pure [GoField [] typeExpr tag]
      ]

-- | Parse method receivers.
parseExpressionList :: Monad m => GoParser m [Located GoExpr]
parseExpressionList = parseExpression `MP.sepBy1` goDelimiterP GoDelimComma

parseParameterList :: Monad m => GoParser m [GoField]
parseParameterList = do
  mClose <- optional $ lookAhead (goDelimiterP GoDelimRightParen)
  case mClose of
    Just _ -> pure []
    Nothing -> do
      fields <- parseFieldDecl `MP.sepBy` goDelimiterP GoDelimComma
      pure $ concat fields
  where
    parseFieldDecl = do
      skipCommentsAndNewlines
      MP.choice
        [ MP.try $ do
            names <- parseIdentifierList
            skipCommentsAndNewlines
            typeExpr <- parseGoType
            pure [GoField names typeExpr Nothing]
        , do
            typeExpr <- parseGoType
            pure [GoField [] typeExpr Nothing]
        ]
