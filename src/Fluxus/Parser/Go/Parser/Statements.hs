{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Fluxus.Parser.Go.Parser.Statements
  ( parseStatement
  , parseSimpleStmt
  , parseExprStmt
  , parseAssignment
  , parseShortVarDecl
  , parseIncDecStmt
  , parseSendStmt
  , parseIfStmt
  , parseForStmt
  , parseSwitchStmt
  , parseSelectStmt
  , parseBlockStmt
  , parseBlockStmt'
  , parseReturnStmt
  , parseBreakStmt
  , parseContinueStmt
  , parseGotoStmt
  , parseFallthroughStmt
  , parseDeferStmt
  , parseGoStmt
  , parseEmptyStmt
  ) where

import Control.Applicative (optional, many)
import Control.Monad (void)
import Data.Functor (($>))
import qualified Data.Text as T
import Text.Megaparsec (anySingle, lookAhead)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Error (errorBundlePretty)

import Fluxus.AST.Common (Located(..), BinaryOp(..))
import Fluxus.AST.Go
  ( GoStmt(..)
  , GoRangeClause(..)
  , GoForClause(..)
  , GoCaseClause(..)
  , GoCommClause(..)
  , GoBinaryOp(..)
  , GoExpr(..)
  )
import Fluxus.Parser.Go.Lexer
  ( GoDelimiter(..)
  , GoKeyword(..)
  , GoOperator(..)
  , GoToken(..)
  )
import Fluxus.Parser.Go.Parser.Common
  ( GoParser
  , logDebug
  , located
  , located'
  , goKeywordP
  , goOperatorP
  , goDelimiterP
  , skipCommentsAndNewlines
  , parseGoIdentifier
  , parseIdentifierList
  , textShow
  )
import Fluxus.Parser.Go.Parser.Expressions
  ( parseExpression
  , parseExpressionList
  )

-- | Parse Go statements.
parseStatement :: Monad m => GoParser m (Located GoStmt)
parseStatement = located $ MP.choice
  [ MP.try parseReturnStmt
  , MP.try parseBreakStmt
  , MP.try parseContinueStmt
  , MP.try parseGotoStmt
  , MP.try parseFallthroughStmt
  , MP.try parseDeferStmt
  , MP.try parseGoStmt
  , MP.try parseIfStmt
  , MP.try parseForStmt
  , MP.try parseSwitchStmt
  , MP.try parseSelectStmt
  , MP.try parseBlockStmt'
  , MP.try parseSimpleStmt
  , parseEmptyStmt
  ]

-- | Parse simple statements.
parseSimpleStmt :: Monad m => GoParser m GoStmt
parseSimpleStmt = MP.choice
  [ MP.try parseAssignment
  , MP.try parseShortVarDecl
  , MP.try parseIncDecStmt
  , MP.try parseSendStmt
  , parseExprStmt
  ]

parseExprStmt :: Monad m => GoParser m GoStmt
parseExprStmt = GoExprStmt <$> parseExpression

parseAssignment :: Monad m => GoParser m GoStmt
parseAssignment = do
  lhs <- parseExpressionList
  op <- parseAssignOperator
  rhs <- parseExpressionList
  buildAssignment op lhs rhs
  where
    parseAssignOperator = do
      Located _ token <- MP.satisfy isAssignToken
      case token of
        GoTokenOperator opToken -> pure opToken
        _ -> fail "Expected assignment operator"
    isAssignToken (Located _ (GoTokenOperator opTok)) = opTok `elem` assignOps
    isAssignToken _ = False
    assignOps =
      [ GoOpAssign
      , GoOpPlusAssign
      , GoOpMinusAssign
      , GoOpMultAssign
      , GoOpDivAssign
      , GoOpModAssign
      , GoOpBitAndAssign
      , GoOpBitOrAssign
      , GoOpBitXorAssign
      , GoOpBitClearAssign
      , GoOpLeftShiftAssign
      , GoOpRightShiftAssign
      ]
    buildAssignment GoOpAssign lhs' rhs' = pure $ GoAssign lhs' rhs'
    buildAssignment opToken [lhsExpr] [rhsExpr] =
      case compoundBinary opToken of
        Just binOp ->
          let combined = located' $ GoBinaryOp binOp lhsExpr rhsExpr
          in pure $ GoAssign [lhsExpr] [combined]
        Nothing -> fail "Unsupported compound assignment operator"
    buildAssignment _ _ _ = fail "Compound assignment requires single operands"

    compoundBinary GoOpPlusAssign = Just OpAdd
    compoundBinary GoOpMinusAssign = Just OpSub
    compoundBinary GoOpMultAssign = Just OpMul
    compoundBinary GoOpDivAssign = Just OpDiv
    compoundBinary GoOpModAssign = Just OpMod
    compoundBinary GoOpBitAndAssign = Just OpBitAnd
    compoundBinary GoOpBitOrAssign = Just OpBitOr
    compoundBinary GoOpBitXorAssign = Just OpBitXor
    compoundBinary GoOpLeftShiftAssign = Just OpShiftL
    compoundBinary GoOpRightShiftAssign = Just OpShiftR
    compoundBinary GoOpBitClearAssign = Just OpBitXor
    compoundBinary _ = Nothing

parseShortVarDecl :: Monad m => GoParser m GoStmt
parseShortVarDecl = do
  names <- parseIdentifierList
  void $ goOperatorP GoOpDefine
  values <- parseExpressionList
  pure $ GoDefine names values

parseIncDecStmt :: Monad m => GoParser m GoStmt
parseIncDecStmt = do
  expr <- parseExpression
  op <- MP.choice
    [ goOperatorP GoOpIncrement $> True
    , goOperatorP GoOpDecrement $> False
    ]
  pure $ GoIncDec expr op

parseSendStmt :: Monad m => GoParser m GoStmt
parseSendStmt = do
  channel <- parseExpression
  void $ goOperatorP GoOpArrow
  value <- parseExpression
  pure $ GoSend channel value

parseIfStmt :: Monad m => GoParser m GoStmt
parseIfStmt = do
  void $ goKeywordP GoKwIf
  simpleStmt <- optional $ MP.try $ do
    stmt <- parseSimpleStmt
    void $ goDelimiterP GoDelimSemicolon
    pure $ located' stmt
  condition <- parseExpression
  thenBody <- located parseBlockStmt'
  elseBody <- optional $ do
    void $ goKeywordP GoKwElse
    MP.choice
      [ located parseIfStmt
      , located parseBlockStmt'
      ]
  pure $ GoIf simpleStmt condition thenBody elseBody

parseForStmt :: Monad m => GoParser m GoStmt
parseForStmt = do
  void $ goKeywordP GoKwFor
  MP.choice
    [ MP.try parseRangeFor
    , MP.try parseForClause
    , parseInfiniteFor
    ]
  where
    parseRangeFor = do
      key <- optional parseGoIdentifier
      value <- optional $ do
        void $ goDelimiterP GoDelimComma
        parseGoIdentifier
      isDefine <- MP.choice
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
      pure $ GoRange rangeClause body

    parseForClause = do
      initClause <- optional $ do
        stmt <- parseSimpleStmt
        void $ goDelimiterP GoDelimSemicolon
        pure $ located' stmt
      condition <- optional $ do
        expr <- parseExpression
        void $ goDelimiterP GoDelimSemicolon
        pure expr
      post <- optional $ located' <$> parseSimpleStmt
      body <- located parseBlockStmt'
      let clause = Just $ GoForClause
            { goForInit = initClause
            , goForCond = condition
            , goForPost = post
            }
      pure $ GoFor clause body

    parseInfiniteFor = do
      body <- located parseBlockStmt'
      pure $ GoFor Nothing body

parseSwitchStmt :: Monad m => GoParser m GoStmt
parseSwitchStmt = do
  void $ goKeywordP GoKwSwitch
  simpleStmt <- optional $ MP.try $ do
    stmt <- parseSimpleStmt
    void $ goDelimiterP GoDelimSemicolon
    pure $ located' stmt
  expr <- optional parseExpression
  void $ goDelimiterP GoDelimLeftBrace
  cases <- many parseCaseClause
  void $ goDelimiterP GoDelimRightBrace
  pure $ GoSwitch simpleStmt expr cases
  where
    parseCaseClause = located $ MP.choice
      [ do
          void $ goKeywordP GoKwCase
          exprs <- parseExpressionList
          void $ goDelimiterP GoDelimColon
          stmts <- many parseStatement
          pure $ GoCase exprs stmts
      , do
          void $ goKeywordP GoKwDefault
          void $ goDelimiterP GoDelimColon
          stmts <- many parseStatement
          pure $ GoDefault stmts
      ]

parseSelectStmt :: Monad m => GoParser m GoStmt
parseSelectStmt = do
  void $ goKeywordP GoKwSelect
  void $ goDelimiterP GoDelimLeftBrace
  cases <- many parseCommClause
  void $ goDelimiterP GoDelimRightBrace
  pure $ GoSelect cases
  where
    parseCommClause = located $ MP.choice
      [ do
          void $ goKeywordP GoKwCase
          comm <- parseSimpleStmt
          void $ goDelimiterP GoDelimColon
          stmts <- many parseStatement
          pure $ GoCommClause (Just $ located' comm) stmts
      , do
          void $ goKeywordP GoKwDefault
          void $ goDelimiterP GoDelimColon
          stmts <- many parseStatement
          pure $ GoCommClause Nothing stmts
      ]

parseBlockStmt :: Monad m => GoParser m (Located GoStmt)
parseBlockStmt = located parseBlockStmt'

parseBlockStmt' :: Monad m => GoParser m GoStmt
parseBlockStmt' = do
  logDebug "parseBlockStmt': entering"
  void $ goDelimiterP GoDelimLeftBrace
  skipCommentsAndNewlines
  stmts <- many (parseStatement <* skipCommentsAndNewlines)
  logDebug $ "parseBlockStmt': statements parsed = " <> textShow (length stmts)
  nextToken <- MP.optional $ lookAhead anySingle
  logDebug $ "parseBlockStmt': next token before closing = " <> maybe "<none>" textShow nextToken
  void $ goDelimiterP GoDelimRightBrace
  logDebug "parseBlockStmt': exiting"
  pure $ GoBlock stmts

parseReturnStmt :: Monad m => GoParser m GoStmt
parseReturnStmt = do
  void $ goKeywordP GoKwReturn
  result <- MP.observing (MP.try parseExpressionList)
  exprs <- case result of
    Left err -> do
      logDebug $ "parseReturnStmt: failed to parse expressions: " <> T.pack (errorBundlePretty err)
      pure []
    Right es -> do
      logDebug $ "parseReturnStmt: parsed expressions count = " <> textShow (length es)
      pure es
  pure $ GoReturn exprs

parseBreakStmt :: Monad m => GoParser m GoStmt
parseBreakStmt = do
  void $ goKeywordP GoKwBreak
  label <- optional parseGoIdentifier
  pure $ GoBreak label

parseContinueStmt :: Monad m => GoParser m GoStmt
parseContinueStmt = do
  void $ goKeywordP GoKwContinue
  label <- optional parseGoIdentifier
  pure $ GoContinue label

parseGotoStmt :: Monad m => GoParser m GoStmt
parseGotoStmt = do
  void $ goKeywordP GoKwGoto
  label <- parseGoIdentifier
  pure $ GoGoto label

parseFallthroughStmt :: Monad m => GoParser m GoStmt
parseFallthroughStmt = goKeywordP GoKwFallthrough $> GoFallthrough

parseDeferStmt :: Monad m => GoParser m GoStmt
parseDeferStmt = do
  void $ goKeywordP GoKwDefer
  expr <- parseExpression
  pure $ GoDefer expr

parseGoStmt :: Monad m => GoParser m GoStmt
parseGoStmt = do
  void $ goKeywordP GoKwGo
  expr <- parseExpression
  pure $ GoGo expr

parseEmptyStmt :: Monad m => GoParser m GoStmt
parseEmptyStmt = goDelimiterP GoDelimSemicolon $> GoEmpty
