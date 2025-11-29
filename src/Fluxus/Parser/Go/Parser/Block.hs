{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Fluxus.Parser.Go.Parser.Block
  ( parseBlockStmt
  , parseBlockStmt'
  ) where

import Control.Monad (void)
import Control.Monad.Logger (MonadLogger)
import qualified Data.Text as T
import Text.Megaparsec (anySingle, lookAhead)
import qualified Text.Megaparsec as MP

import Fluxus.AST.Common (Located(..))
import Fluxus.AST.Go (GoStmt(..))
import Fluxus.Parser.Go.Lexer
  ( GoDelimiter(..)
  )
import Fluxus.Parser.Go.Parser.Common
  ( GoParser
  , logDebug
  , located
  , goDelimiterP
  , skipCommentsAndNewlines
  , textShow
  )

parseBlockStmt :: MonadLogger m => GoParser m (Located GoStmt)
parseBlockStmt = located parseBlockStmt'

parseBlockStmt' :: MonadLogger m => GoParser m GoStmt
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

-- Forward declaration for parseStatement to break circular dependency
-- This will be provided by the Expressions module through dependency injection
parseStatement :: MonadLogger m => GoParser m (Located GoStmt)
parseStatement = error "parseStatement: not implemented, should be provided by Statements module"