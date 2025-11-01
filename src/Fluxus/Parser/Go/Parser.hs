{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

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
import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (first)
import Data.Functor.Identity (Identity(..))
import Data.List (partition)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle(..), runParserT)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Error (errorBundlePretty, errorOffset)
import Control.Monad.Logger (LogLevel, LogSource, LogStr, Loc, runLoggingT)

import Fluxus.AST.Common (SourceSpan, Located(..))
import Fluxus.AST.Go
  ( GoAST(..)
  , GoPackage(..)
  , GoFile(..)
  , GoDecl(..)
  )
import Fluxus.Parser.Go.Lexer (GoKeyword(..), GoToken)
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

-- | Run the Go parser producing an AST.
runGoParser :: Text -> [Located GoToken] -> Either GoParseError GoAST
runGoParser filename tokens =
  runIdentity $
    fmap (first (bundleToGoParseError filename tokens)) $
      runLoggingT
        (runParserT parseGo (T.unpack filename) tokens)
        (\_ _ _ _ -> pure ())

-- | Run the Go parser with a custom logging function.
runGoParserWithLogger
  :: Monad m
  => (Loc -> LogSource -> LogLevel -> LogStr -> m ())
  -> Text
  -> [Located GoToken]
  -> m (Either GoParseError GoAST)
runGoParserWithLogger logger filename tokens =
  fmap (first (bundleToGoParseError filename tokens)) $
    runLoggingT (runParserT parseGo (T.unpack filename) tokens) logger

-- | Main parser entry point.
parseGo :: MonadIO m => GoParser m GoAST
parseGo = GoAST <$> parsePackage

-- | Parse a Go package (currently single file).
parsePackage :: MonadIO m => GoParser m GoPackage
parsePackage = do
  file <- parseFile
  pure GoPackage
    { goPackageName = goFilePackage file
    , goPackageFiles = [file]
    }

-- | Parse a Go source file.
parseFile :: MonadIO m => GoParser m GoFile
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
