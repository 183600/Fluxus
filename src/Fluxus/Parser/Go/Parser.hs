{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Go parser that converts tokens to AST
module Fluxus.Parser.Go.Parser
  ( -- * Parser types
    GoParser
  , GoParseError(..)
    -- * Main parsing functions
  , parseGo
  , runGoParser
  , runGoParserWithLogger
    -- * Top-level parsers
  , parsePackage
  , parseFile
    -- * Declaration parsers
  , parseDeclaration
  , parseFuncDecl
  , parseTypeDecl
  , parseVarDecl
  , parseConstDecl
  , parseImportDecl
    -- * Statement parsers
  , parseStatement
  , parseBlockStmt
  , parseIfStmt
  , parseForStmt
  , parseSwitchStmt
  , parseSelectStmt
  , parseReturnStmt
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
  , parseParameterList
  ) where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Functor.Identity (Identity(..))
import Data.List (partition)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import Text.Megaparsec (ParseErrorBundle(..), runParserT, VisualStream(..), TraversableStream(..), PosState(..))
import Text.Megaparsec.Error (errorOffset, errorBundlePretty)
import qualified Text.Megaparsec.Pos as MPP
import Control.Monad.Logger (MonadLogger, LogLevel, LogSource, LogStr, Loc, runLoggingT, LoggingT, runNoLoggingT, NoLoggingT)

import Fluxus.AST.Common (SourceSpan(..), SourcePos(..), Located(..))
import Fluxus.AST.Go
  ( GoAST(..)
  , GoPackage(..)
  , GoFile(..)
  , GoDecl(..)
  )
import Fluxus.Parser.Go.Lexer (GoKeyword(..), GoToken(..), GoOperator(..), GoDelimiter(..))
import Fluxus.Parser.Go.Parser.Common
  ( GoParser
  , logDebug
  , goKeywordP
  , skipCommentsAndNewlines
  , parseGoIdentifier
  , parseIdentifierList
  , spanAtOffset
  , textShow
  )
import Fluxus.Parser.Go.Parser.Declarations
  ( parseDeclaration
  , parseFuncDecl
  , parseTypeDecl
  , parseVarDecl
  , parseConstDecl
  , parseImportDecl
  )
import Fluxus.Parser.Go.Parser.Statements
  ( parseStatement
  , parseBlockStmt
  , parseIfStmt
  , parseForStmt
  , parseSwitchStmt
  , parseSelectStmt
  , parseReturnStmt
  )
import Fluxus.Parser.Go.Parser.Expressions
  ( parseExpression
  , parseUnaryExpr
  , parseAtomExpr
  , parseGoType
  , parseStructType
  , parseInterfaceType
  , parseExpressionList
  , parseParameterList
  )

-- | Parser error type with message and source location.
data GoParseError = GoParseError
  { peMessage :: !Text
  , peLocation :: !SourceSpan
  } deriving stock (Eq, Show)

-- | VisualStream instance for token stream
instance VisualStream [Located GoToken] where
  showTokens _ ts = unwords . NE.toList $ fmap (T.unpack . showGoToken . locValue) ts

-- | TraversableStream instance for token stream
instance TraversableStream [Located GoToken] where
  reachOffset offset posState =
    let (pre, post) = splitAt offset (pstateInput posState)
        newPos = case (post, pre) of
          (Located span _ : _, _) -> toSourcePosStart span
          ([], Located span _ : _) -> toSourcePosEnd span
          _ -> pstateSourcePos posState
        ctx = case post of
          (Located _ tok : _) -> Just (T.unpack (showGoToken tok))
          [] -> Just "<eof>"
    in ( ctx
       , posState
           { pstateInput = post
           , pstateOffset = offset
           , pstateSourcePos = newPos
           }
       )

-- | Convert our SourcePos to Megaparsec SourcePos (start position)
toSourcePosStart :: SourceSpan -> MPP.SourcePos
toSourcePosStart (SourceSpan fn (SourcePos line col) _) =
  MPP.SourcePos (T.unpack fn) (MPP.mkPos (line + 1)) (MPP.mkPos (col + 1))

-- | Convert our SourcePos to Megaparsec SourcePos (end position)
toSourcePosEnd :: SourceSpan -> MPP.SourcePos
toSourcePosEnd (SourceSpan fn _ (SourcePos line col)) =
  MPP.SourcePos (T.unpack fn) (MPP.mkPos (line + 1)) (MPP.mkPos (col + 1))

-- | Show a token as text for error messages
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
  GoTokenComment _ -> "/* comment */"
  GoTokenNewline -> "\\n"
  GoTokenEOF -> "<EOF>"
  GoTokenError e -> "<ERROR:" <> e <> ">"

-- | Run the Go parser producing an AST.
runGoParser :: Text -> [Located GoToken] -> Either GoParseError GoAST
runGoParser filename tokens =
  runIdentity $
    runNoLoggingT parser
  where
    parser :: NoLoggingT Identity (Either GoParseError GoAST)
    parser =
      fmap (first (bundleToGoParseError filename tokens)) $
        runParserT parseGo (T.unpack filename) tokens

-- | Run the Go parser with a custom logging function.
runGoParserWithLogger
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> Text
  -> [Located GoToken]
  -> IO (Either GoParseError GoAST)
runGoParserWithLogger logger filename tokens =
  fmap (first (bundleToGoParseError filename tokens)) $
    runLoggingT
      (runParserT parseGo (T.unpack filename) tokens :: LoggingT IO (Either (ParseErrorBundle [Located GoToken] Void) GoAST))
      logger

-- | Main parser entry point.
parseGo :: MonadLogger m => GoParser m GoAST
parseGo = GoAST <$> parsePackage

-- | Parse a Go package (currently single file).
parsePackage :: MonadLogger m => GoParser m GoPackage
parsePackage = do
  file <- parseFile
  pure GoPackage
    { goPackageName = goFilePackage file
    , goPackageFiles = [file]
    }

-- | Parse a Go source file.
parseFile :: MonadLogger m => GoParser m GoFile
parseFile = do
  skipCommentsAndNewlines
  void $ goKeywordP GoKwPackage
  packageName <- parseGoIdentifier
  skipCommentsAndNewlines

  let lookAheadImport = MP.lookAhead $ do
        skipCommentsAndNewlines
        goKeywordP GoKwImport

  importGroups <- MP.many $ do
    lookAheadImport
    skipCommentsAndNewlines
    parseImportDecl <* skipCommentsAndNewlines

  decls <- MP.many $ do
    skipCommentsAndNewlines
    d <- parseDeclaration
    skipCommentsAndNewlines
    pure d

  logDebug $ "parseFile: decls count = " <> textShow (length decls)

  skipCommentsAndNewlines

  let (importDecls, otherDecls) = partition isImportDecl decls
      imports = concat importGroups ++ concatMap extractImports importDecls

  pure GoFile
    { goFileName = "<input>"
    , goFilePackage = packageName
    , goFileImports = imports
    , goFileDecls = otherDecls
    }
  where
    isImportDecl (Located _ (GoImportDecl _)) = True
    isImportDecl _ = False

    extractImports (Located _ (GoImportDecl imps)) = imps
    extractImports _ = []

-- | Convert a Megaparsec error bundle to our parser error.
bundleToGoParseError
  :: Text
  -> [Located GoToken]
  -> ParseErrorBundle [Located GoToken] Void
  -> GoParseError
bundleToGoParseError filename tokens bundle =
  let firstError = NE.head (bundleErrors bundle)
      offset = errorOffset firstError
      location = spanAtOffset filename tokens offset
      message = T.stripEnd (T.pack (errorBundlePretty bundle))
  in GoParseError message location
