{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fluxus.Parser.Go.Parser.Common
  ( GoTokenStream(..)
  , GoParser
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
import Control.Monad.Logger (MonadLogger, logDebugNS)
import Control.Monad.Trans.Class (lift)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Data.List.NonEmpty as NE
import Text.Megaparsec
  ( ParsecT
  , PosState(..)
  , Stream(..)
  , TraversableStream(..)
  , VisualStream(..)
  , getInput
  , satisfy
  )
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Pos as MPP

import Fluxus.AST.Common
  ( Identifier(..)
  , Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , locSpan
  , locValue
  , noLoc
  )
import Fluxus.Parser.Go.Lexer
  ( GoDelimiter(..)
  , GoKeyword(..)
  , GoOperator(..)
  , GoToken(..)
  )

newtype GoTokenStream = GoTokenStream { unGoTokenStream :: [Located GoToken] }
  deriving stock (Eq, Show)

type GoParser m = ParsecT Void GoTokenStream m

listProxy :: Proxy [Located GoToken]
listProxy = Proxy

instance Stream GoTokenStream where
  type Token GoTokenStream = Located GoToken
  type Tokens GoTokenStream = [Located GoToken]

  tokenToChunk _ = MP.tokenToChunk listProxy
  tokensToChunk _ = MP.tokensToChunk listProxy
  chunkToTokens _ = MP.chunkToTokens listProxy
  chunkLength _ = MP.chunkLength listProxy
  chunkEmpty _ = MP.chunkEmpty listProxy

  take1_ (GoTokenStream xs) =
    case MP.take1_ xs of
      Nothing -> Nothing
      Just (tok, rest) -> Just (tok, GoTokenStream rest)

  takeN_ n (GoTokenStream xs) =
    fmap (fmap GoTokenStream) (MP.takeN_ n xs)

  takeWhile_ f (GoTokenStream xs) =
    let (chunk, rest) = MP.takeWhile_ f xs
    in (chunk, GoTokenStream rest)

instance VisualStream GoTokenStream where
  showTokens _ ts =
    unwords . map (T.unpack . showGoToken . locValue) . NE.toList $ ts
  tokensLength _ = length

instance TraversableStream GoTokenStream where
  reachOffset offset posState =
    let GoTokenStream input = pstateInput posState
        (pre, post) = splitAt offset input
        newPos = case (post, pre) of
          (Located span _ : _, _) -> toSourcePosStart span
          ([], Located span _ : _) -> toSourcePosEnd span
          _ -> pstateSourcePos posState
        ctx = case post of
          (Located _ tok : _) -> Just (T.unpack (showGoToken tok))
          [] -> Just "<eof>"
        newState =
          posState
            { pstateInput = GoTokenStream post
            , pstateOffset = offset
            , pstateSourcePos = newPos
            }
    in (ctx, newState)

parserLogSource :: Text
parserLogSource = "fluxus.go.parser"

logDebug :: MonadLogger m => Text -> GoParser m ()
logDebug msg = lift (logDebugNS parserLogSource msg)

chainl1 :: GoParser m a -> GoParser m (a -> a -> a) -> GoParser m a
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

mergeSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSpans (SourceSpan file start _) (SourceSpan _ _ end) = SourceSpan file start end

defaultSpan :: Text -> SourceSpan
defaultSpan file = SourceSpan file (SourcePos 0 0) (SourcePos 0 0)

zeroWidthSpan :: SourceSpan -> SourceSpan
zeroWidthSpan (SourceSpan file start _) = SourceSpan file start start

spanFromTokens :: [Located a] -> SourceSpan
spanFromTokens [] = defaultSpan "<unknown>"
spanFromTokens tokens =
  mergeSpans (locSpan (head tokens)) (locSpan (last tokens))

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

located :: GoParser m a -> GoParser m (Located a)
located parser = do
  GoTokenStream before <- getInput
  result <- parser
  GoTokenStream after <- getInput
  let consumedCount = length before - length after
      (consumed, _) = splitAt consumedCount before
      spanLoc = case consumed of
        [] -> case before of
          (Located span _ : _) -> zeroWidthSpan span
          [] -> defaultSpan "<unknown>"
        _  -> spanFromTokens consumed
  pure $ Located spanLoc result

located' :: a -> Located a
located' = noLoc

goKeywordP :: GoKeyword -> GoParser m ()
goKeywordP kw = void $ satisfy $ \case
  Located _ (GoTokenKeyword kw') -> kw == kw'
  _ -> False

goOperatorP :: GoOperator -> GoParser m ()
goOperatorP op = void $ satisfy $ \case
  Located _ (GoTokenOperator op') -> op == op'
  _ -> False

goDelimiterP :: GoDelimiter -> GoParser m ()
goDelimiterP delim = void $ satisfy $ \case
  Located _ (GoTokenDelimiter delim') -> delim == delim'
  _ -> False

skipNewlines :: GoParser m ()
skipNewlines = void $ MP.many $ satisfy $ \case
  Located _ GoTokenNewline -> True
  _ -> False

skipComments :: GoParser m ()
skipComments = void $ MP.many $ satisfy $ \case
  Located _ (GoTokenComment _) -> True
  _ -> False

skipCommentsAndNewlines :: GoParser m ()
skipCommentsAndNewlines = void $ MP.many $ satisfy $ \case
  Located _ GoTokenNewline -> True
  Located _ (GoTokenComment _) -> True
  _ -> False

parseGoIdentifier :: GoParser m Identifier
parseGoIdentifier = do
  Located _ token <- satisfy $ \case
    Located _ (GoTokenIdent _) -> True
    _ -> False
  case token of
    GoTokenIdent text -> pure (Identifier text)
    _ -> fail "Expected identifier"

parseGoString :: GoParser m Text
parseGoString = do
  Located _ token <- satisfy $ \case
    Located _ (GoTokenString _) -> True
    Located _ (GoTokenRawString _) -> True
    _ -> False
  case token of
    GoTokenString text -> pure text
    GoTokenRawString text -> pure text
    _ -> fail "Expected string"

parseIdentifierList :: GoParser m [Identifier]
parseIdentifierList = parseGoIdentifier `MP.sepBy1` goDelimiterP GoDelimComma

textShow :: Show a => a -> Text
textShow = T.pack . show

toSourcePosStart :: SourceSpan -> MPP.SourcePos
toSourcePosStart (SourceSpan fn (SourcePos line col) _) =
  MPP.SourcePos (T.unpack fn) (MPP.mkPos (line + 1)) (MPP.mkPos (col + 1))

toSourcePosEnd :: SourceSpan -> MPP.SourcePos
toSourcePosEnd (SourceSpan fn _ (SourcePos line col)) =
  MPP.SourcePos (T.unpack fn) (MPP.mkPos (line + 1)) (MPP.mkPos (col + 1))

showGoToken :: GoToken -> Text
showGoToken tok = case tok of
  GoTokenKeyword kw -> T.pack (show kw)
  GoTokenIdent name -> name
  GoTokenInt n -> n
  GoTokenFloat d -> d
  GoTokenImag i -> i
  GoTokenString s -> "\"" <> s <> "\""
  GoTokenRawString s -> "`" <> s <> "`"
  GoTokenRune c -> "'" <> T.singleton c <> "'"
  GoTokenOperator op -> T.pack (show op)
  GoTokenDelimiter delim -> T.pack (show delim)
  GoTokenComment _ -> T.pack "/* comment */"
  GoTokenNewline -> T.pack "\\n"
  GoTokenEOF -> T.pack "<EOF>"
  GoTokenError e -> T.pack "<ERROR:" <> e <> T.pack ">"
