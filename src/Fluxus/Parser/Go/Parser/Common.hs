{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Fluxus.Parser.Go.Parser.Common
  ( GoParser
  , logDebug
  , chainl1
  , located
  , located'
  , mergeSpans
  , spanFromTokens
  , spanAtOffset
  , defaultSpan
  , goKeywordP
  , goOperatorP
  , goDelimiterP
  , skipNewlines
  , skipComments
  , skipCommentsAndNewlines
  , parseGoIdentifier
  , parseGoString
  , parseIdentifierList
  , textShow
  ) where

import Control.Monad (void)
import Control.Monad.Logger (LoggingT, logDebugNS)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (ParsecT, getInput, satisfy)
import qualified Text.Megaparsec as MP

import Fluxus.AST.Common
  ( Identifier(..)
  , Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , noLoc
  )
import Fluxus.Parser.Go.Lexer
  ( GoDelimiter(..)
  , GoKeyword(..)
  , GoOperator(..)
  , GoToken(..)
  )

-- | Parser monad for Go parsing with logging support.
type GoParser m = ParsecT Void [Located GoToken] (LoggingT m)

parserLogSource :: Text
parserLogSource = "fluxus.go.parser"

-- | Emit a debug log message within the Go parser.
logDebug :: Monad m => Text -> GoParser m ()
logDebug msg = lift (logDebugNS parserLogSource msg)

-- | Left-associative chain combinator.
chainl1 :: Monad m => GoParser m a -> GoParser m (a -> a -> a) -> GoParser m a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = do
      mx <- MP.optional $ do
        f <- op
        y <- p
        pure (f, y)
      case mx of
        Nothing -> pure x
        Just (f, y) -> rest (f x y)

-- | Merge two source spans into one encompassing span.
mergeSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSpans (SourceSpan file start _) (SourceSpan _ _ end) = SourceSpan file start end

-- | Default zero-length span for a given file.
defaultSpan :: Text -> SourceSpan
defaultSpan file = SourceSpan file (SourcePos 0 0) (SourcePos 0 0)

zeroWidthSpan :: SourceSpan -> SourceSpan
zeroWidthSpan (SourceSpan file start _) = SourceSpan file start start

-- | Build a span covering all consumed tokens.
spanFromTokens :: [Located a] -> SourceSpan
spanFromTokens [] = defaultSpan "<unknown>"
spanFromTokens tokens =
  mergeSpans (locSpan (head tokens)) (locSpan (last tokens))

-- | Compute a span for a parse error offset.
spanAtOffset :: Text -> [Located a] -> Int -> SourceSpan
spanAtOffset fallback tokens offset =
  case drop offset tokens of
    (Located span _ : _) -> span
    [] ->
      case reverse tokens of
        (Located span _ : _) ->
          let endPos = spanEnd span
          in SourceSpan (spanFilename span) endPos endPos
        [] -> defaultSpan fallback

-- | Decorate a parser result with source span information.
located :: Monad m => GoParser m a -> GoParser m (Located a)
located parser = do
  before <- getInput
  result <- parser
  after <- getInput
  let consumedCount = length before - length after
      (consumed, _) = splitAt consumedCount before
      spanLoc = case consumed of
        [] -> case before of
          (Located span _ : _) -> zeroWidthSpan span
          [] -> defaultSpan "<unknown>"
        _  -> spanFromTokens consumed
  pure $ Located spanLoc result

-- | Create a value without precise location information.
located' :: a -> Located a
located' = noLoc

-- | Match a keyword token.
goKeywordP :: Monad m => GoKeyword -> GoParser m ()
goKeywordP kw = void $ satisfy $ \case
  Located _ (GoTokenKeyword kw') -> kw == kw'
  _ -> False

-- | Match an operator token.
goOperatorP :: Monad m => GoOperator -> GoParser m ()
goOperatorP op = void $ satisfy $ \case
  Located _ (GoTokenOperator op') -> op == op'
  _ -> False

-- | Match a delimiter token.
goDelimiterP :: Monad m => GoDelimiter -> GoParser m ()
goDelimiterP delim = void $ satisfy $ \case
  Located _ (GoTokenDelimiter delim') -> delim == delim'
  _ -> False

-- | Skip newline tokens.
skipNewlines :: Monad m => GoParser m ()
skipNewlines = void $ MP.many $ satisfy $ \case
  Located _ GoTokenNewline -> True
  _ -> False

-- | Skip comment tokens.
skipComments :: Monad m => GoParser m ()
skipComments = void $ MP.many $ satisfy $ \case
  Located _ (GoTokenComment _) -> True
  _ -> False

-- | Skip comments and newlines.
skipCommentsAndNewlines :: Monad m => GoParser m ()
skipCommentsAndNewlines = void $ MP.many $ satisfy $ \case
  Located _ GoTokenNewline -> True
  Located _ (GoTokenComment _) -> True
  _ -> False

-- | Parse a Go identifier token.
parseGoIdentifier :: Monad m => GoParser m Identifier
parseGoIdentifier = do
  Located _ token <- satisfy $ \case
    Located _ (GoTokenIdent _) -> True
    _ -> False
  case token of
    GoTokenIdent text -> pure (Identifier text)
    _ -> fail "Expected identifier"

-- | Parse a Go string or raw string literal.
parseGoString :: Monad m => GoParser m Text
parseGoString = do
  Located _ token <- satisfy $ \case
    Located _ (GoTokenString _) -> True
    Located _ (GoTokenRawString _) -> True
    _ -> False
  case token of
    GoTokenString text -> pure text
    GoTokenRawString text -> pure text
    _ -> fail "Expected string"

-- | Parse a comma-separated list of identifiers.
parseIdentifierList :: Monad m => GoParser m [Identifier]
parseIdentifierList = parseGoIdentifier `MP.sepBy1` goDelimiterP GoDelimComma

-- | Show a value as text.
textShow :: Show a => a -> Text
textShow = T.pack . show
