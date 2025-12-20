{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}



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

import Control.Applicative (optional, many, (<|>))
import Control.Monad (void)
import Control.Monad.Logger (MonadLogger)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Text.Megaparsec (lookAhead)
import qualified Text.Megaparsec as MP
import Text.Read (readMaybe)

import Fluxus.AST.Common (BinaryOp(..), ComparisonOp(..), Identifier(..), Located(..), UnaryOp(..), locatedValue)
import Fluxus.AST.Go 
  ( GoExpr(..)
  , GoLiteral(..)
  , GoType(..)
  , GoField(..)
  , GoMethod(..)
  , GoSliceExpr(..)
  , GoChannel(..)
  , GoFunction(..)
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
-- This import is necessary due to circular dependency between Expressions and Statements modules
-- We use parseBlockStmt' at line 302 in parseFunctionLit
import {-# SOURCE #-} Fluxus.Parser.Go.Parser.Statements (parseBlockStmt')

-- | Parse expressions with operator precedence.
parseExpression :: MonadLogger m => GoParser m (Located GoExpr)
parseExpression = parseOrExpr

parseOrExpr :: MonadLogger m => GoParser m (Located GoExpr)
parseOrExpr = chainl1 parseAndExpr parseOrOp
  where
    parseOrOp :: GoParser m (Located GoExpr -> Located GoExpr -> Located GoExpr)
    parseOrOp = do
      void $ goOperatorP GoOpOr
      pure $ \l r -> located' $ GoBinaryOp OpOr l r

parseAndExpr :: MonadLogger m => GoParser m (Located GoExpr)
parseAndExpr = chainl1 parseEqualityExpr parseAndOp
  where
    parseAndOp :: GoParser m (Located GoExpr -> Located GoExpr -> Located GoExpr)
    parseAndOp = do
      void $ goOperatorP GoOpAnd
      pure $ \l r -> located' $ GoBinaryOp OpAnd l r

parseEqualityExpr :: MonadLogger m => GoParser m (Located GoExpr)
parseEqualityExpr = chainl1 parseRelationalExpr parseEqOp
  where
    parseEqOp :: GoParser m (Located GoExpr -> Located GoExpr -> Located GoExpr)
    parseEqOp = MP.choice
      [ goOperatorP GoOpEq $> (\l r -> located' $ GoComparison OpEq l r)
      , goOperatorP GoOpNe $> (\l r -> located' $ GoComparison OpNe l r)
      ]

parseRelationalExpr :: MonadLogger m => GoParser m (Located GoExpr)
parseRelationalExpr = chainl1 parseAdditiveExpr parseRelOp
  where
    parseRelOp :: GoParser m (Located GoExpr -> Located GoExpr -> Located GoExpr)
    parseRelOp = MP.choice
      [ goOperatorP GoOpLt $> (\l r -> located' $ GoComparison OpLt l r)
      , goOperatorP GoOpLe $> (\l r -> located' $ GoComparison OpLe l r)
      , goOperatorP GoOpGt $> (\l r -> located' $ GoComparison OpGt l r)
      , goOperatorP GoOpGe $> (\l r -> located' $ GoComparison OpGe l r)
      ]

parseAdditiveExpr :: MonadLogger m => GoParser m (Located GoExpr)
parseAdditiveExpr = chainl1 parseMultiplicativeExpr parseAddOp
  where
    parseAddOp :: GoParser m (Located GoExpr -> Located GoExpr -> Located GoExpr)
    parseAddOp = MP.choice
      [ goOperatorP GoOpPlus $> (\l r -> located' $ GoBinaryOp OpAdd l r)
      , goOperatorP GoOpMinus $> (\l r -> located' $ GoBinaryOp OpSub l r)
      , goOperatorP GoOpBitOr $> (\l r -> located' $ GoBinaryOp OpBitOr l r)
      , goOperatorP GoOpBitXor $> (\l r -> located' $ GoBinaryOp OpBitXor l r)
      ]

parseMultiplicativeExpr :: MonadLogger m => GoParser m (Located GoExpr)
parseMultiplicativeExpr = chainl1 parseUnaryExpr parseMulOp
  where
    parseMulOp :: GoParser m (Located GoExpr -> Located GoExpr -> Located GoExpr)
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
parseUnaryExpr :: MonadLogger m => GoParser m (Located GoExpr)
parseUnaryExpr = MP.choice
  [ do
      builder <- MP.choice
        [ goOperatorP GoOpPlus $> (\x -> GoUnaryOp OpPositive x)
        , goOperatorP GoOpMinus $> (\x -> GoUnaryOp OpNegate x)
        , goOperatorP GoOpNot $> (\x -> GoUnaryOp OpNot x)
        , goOperatorP GoOpBitXor $> (\x -> GoUnaryOp OpBitNot x)
        , goOperatorP GoOpAddress $> GoAddress
        , goOperatorP GoOpMult $> GoDeref
        , goOperatorP GoOpArrow $> GoReceive
        ]
      expr <- parseUnaryExpr
      pure $ located' $ builder expr
  , parseAtomExpr
  ]

-- | Parse atomic expressions with postfix operators.
parseAtomExpr :: MonadLogger m => GoParser m (Located GoExpr)
parseAtomExpr = do
  atom <- parseAtom
  postfixes <- many parsePostfix
  pure $ foldl (\expr f -> f expr) atom postfixes
  where
    parseAtom = located $ MP.choice
      [ parseGoLiteral
      , MP.try parseCompositeLit
      , parseGoIdentifierExpr
      , parseParenExpr
      , parseFuncLiteral
      ]

-- | Parse Go literals.
parseGoLiteral :: GoParser m GoExpr
parseGoLiteral = do
  Located _ token <- MP.satisfy isLiteralToken
  case token of
    GoTokenInt text -> 
      case readMaybe (T.unpack text) of
        Just value -> pure $ GoLiteral $ GoInt value
        Nothing -> fail $ "Invalid integer literal: " ++ T.unpack text
    GoTokenFloat text -> 
      case readMaybe (T.unpack text) of
        Just value -> pure $ GoLiteral $ GoFloat value
        Nothing -> fail $ "Invalid float literal: " ++ T.unpack text
    GoTokenImag text -> 
      case readMaybe (T.unpack $ T.init text) of
        Just value -> pure $ GoLiteral $ GoImag value
        Nothing -> fail $ "Invalid imaginary literal: " ++ T.unpack text
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
parseGoIdentifierExpr :: GoParser m GoExpr
parseGoIdentifierExpr = GoIdent <$> parseGoIdentifier

-- | Parse parenthesized expressions.
parseParenExpr :: MonadLogger m => GoParser m GoExpr
parseParenExpr = do
  void $ goDelimiterP GoDelimLeftParen
  expr <- parseExpression
  void $ goDelimiterP GoDelimRightParen
  pure $ locatedValue expr

-- | Parse composite literals.
parseCompositeLit :: MonadLogger m => GoParser m GoExpr
parseCompositeLit = do
  typeExpr <- optional parseGoType
  -- Look ahead to ensure we have a brace
  _ <- MP.lookAhead $ goDelimiterP GoDelimLeftBrace
  case typeExpr of
    Just ty@(Located _ (GoMapType _ _)) -> GoMapLit ty <$> withBraces parseMapEntries
    _ -> do
      elements <- withBraces parseLiteralElements
      pure $ buildComposite typeExpr elements
  where
    buildComposite mType elements =
      case (mType, literalFieldsOnly elements) of
        (Just ty, Just fields) -> GoStructLit ty fields
        _ -> GoCompositeLit mType (map literalElementExpr elements)

    parseMapEntries = parseMapEntry `MP.sepEndBy` commaSeparator

    parseMapEntry = do
      skipCommentsAndNewlines
      key <- parseExpression
      skipCommentsAndNewlines
      void $ goDelimiterP GoDelimColon
      skipCommentsAndNewlines
      value <- parseExpression
      pure (key, value)

    parseLiteralElements = parseLiteralElement `MP.sepEndBy` commaSeparator

    parseLiteralElement = do
      skipCommentsAndNewlines
      MP.try parseFieldElement <|> (LiteralValue <$> parseExpression)

    parseFieldElement = do
      fieldName <- parseGoIdentifier
      skipCommentsAndNewlines
      void $ goDelimiterP GoDelimColon
      skipCommentsAndNewlines
      expr <- parseExpression
      pure $ LiteralField fieldName expr


data LiteralElement
  = LiteralField Identifier (Located GoExpr)
  | LiteralValue (Located GoExpr)

literalElementExpr :: LiteralElement -> Located GoExpr
literalElementExpr (LiteralField _ expr) = expr
literalElementExpr (LiteralValue expr) = expr

literalFieldsOnly :: [LiteralElement] -> Maybe [(Identifier, Located GoExpr)]
literalFieldsOnly elements =
  let fields = [ (name, expr) | LiteralField name expr <- elements ]
  in if length fields == length elements
       then Just fields
       else Nothing

withBraces :: GoParser m a -> GoParser m a
withBraces parser = do
  void $ goDelimiterP GoDelimLeftBrace
  skipCommentsAndNewlines
  result <- parser
  skipCommentsAndNewlines
  void $ goDelimiterP GoDelimRightBrace
  pure result

commaSeparator :: GoParser m ()
commaSeparator = do
  skipCommentsAndNewlines
  goDelimiterP GoDelimComma
  skipCommentsAndNewlines

-- | Parse function literals.
parseFuncLiteral :: MonadLogger m => GoParser m GoExpr
parseFuncLiteral = do
  void $ goKeywordP GoKwFunc
  void $ goDelimiterP GoDelimLeftParen
  params <- parseParameterList
  void $ goDelimiterP GoDelimRightParen
  results <- optional $ MP.choice
    [ MP.try $ do
        void $ goDelimiterP GoDelimLeftParen
        res <- parseParameterList
        void $ goDelimiterP GoDelimRightParen
        pure res
    , do
        ty <- parseGoType
        pure [GoField [] ty Nothing]
    ]
  body <- located parseBlockStmt'
  pure $ GoFuncLit GoFunction
    { goFuncName = Nothing
    , goFuncParams = params
    , goFuncResults = fromMaybe [] results
    , goFuncBody = Just body
    }


-- | Parse postfix operators.
parsePostfix :: MonadLogger m => GoParser m (Located GoExpr -> Located GoExpr)
parsePostfix = MP.choice
  [ parseCall
  , parseIndex
  , parseSlice
  , parseSelector
  , parseTypeAssertion
  ]

parseCall :: MonadLogger m => GoParser m (Located GoExpr -> Located GoExpr)
parseCall = do
  void $ goDelimiterP GoDelimLeftParen
  args <- optional parseExpressionList
  void $ goDelimiterP GoDelimRightParen
  let finalArgs = fromMaybe [] args
  pure $ \expr -> located' $ GoCall expr finalArgs

parseIndex :: MonadLogger m => GoParser m (Located GoExpr -> Located GoExpr)
parseIndex = do
  void $ goDelimiterP GoDelimLeftBracket
  index <- parseExpression
  void $ goDelimiterP GoDelimRightBracket
  pure $ \expr -> located' $ GoIndex expr index

parseSlice :: MonadLogger m => GoParser m (Located GoExpr -> Located GoExpr)
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

parseSelector :: GoParser m (Located GoExpr -> Located GoExpr)
parseSelector = do
  void $ goDelimiterP GoDelimDot
  field <- parseGoIdentifier
  pure $ \expr -> located' $ GoSelector expr field

parseTypeAssertion :: MonadLogger m => GoParser m (Located GoExpr -> Located GoExpr)
parseTypeAssertion = do
  void $ goDelimiterP GoDelimDot
  void $ goDelimiterP GoDelimLeftParen
  typeExpr <- parseGoType
  void $ goDelimiterP GoDelimRightParen
  pure $ \expr -> located' $ GoTypeAssert expr typeExpr

-- | Parse Go types.
parseGoType :: MonadLogger m => GoParser m (Located GoType)
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

parseBasicType :: GoParser m GoType
parseBasicType = GoBasicType <$> parseGoIdentifier

parseArrayType :: MonadLogger m => GoParser m GoType
parseArrayType = do
  void $ goDelimiterP GoDelimLeftBracket
  size <- parseExpression
  void $ goDelimiterP GoDelimRightBracket
  elemType <- parseGoType
  pure $ GoArrayType size elemType

parseSliceType :: MonadLogger m => GoParser m GoType
parseSliceType = do
  void $ goDelimiterP GoDelimLeftBracket
  void $ goDelimiterP GoDelimRightBracket
  elemType <- parseGoType
  pure $ GoSliceType elemType

parseMapType :: MonadLogger m => GoParser m GoType
parseMapType = do
  void $ goKeywordP GoKwMap
  void $ goDelimiterP GoDelimLeftBracket
  keyType <- parseGoType
  void $ goDelimiterP GoDelimRightBracket
  valueType <- parseGoType
  pure $ GoMapType keyType valueType

parseChanType :: MonadLogger m => GoParser m GoType
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

parsePointerType :: MonadLogger m => GoParser m GoType
parsePointerType = do
  void $ goOperatorP GoOpMult
  baseType <- parseGoType
  pure $ GoPointerType baseType

parseEllipsisType :: MonadLogger m => GoParser m GoType
parseEllipsisType = do
  void $ goOperatorP GoOpEllipsis
  elemType <- parseGoType
  pure $ GoEllipsisType elemType

parseFuncType :: MonadLogger m => GoParser m GoType
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

parseInterfaceType :: MonadLogger m => GoParser m GoType
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

parseStructType :: MonadLogger m => GoParser m GoType
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
parseExpressionList :: MonadLogger m => GoParser m [Located GoExpr]
parseExpressionList = parseExpression `MP.sepBy1` goDelimiterP GoDelimComma

parseParameterList :: MonadLogger m => GoParser m [GoField]
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




