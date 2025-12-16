{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

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
import Text.Megaparsec (ParseErrorBundle(..), runParserT)
import Text.Megaparsec.Error (errorOffset, errorBundlePretty)
import Control.Monad.Logger (MonadLogger, LogLevel, LogSource, LogStr, Loc, runLoggingT, LoggingT, runNoLoggingT, NoLoggingT)

import Fluxus.AST.Common (SourceSpan(..), Located(..))
import Fluxus.AST.Go (GoAST(..), GoPackage(..), GoFile(..), GoDecl(..))
import Fluxus.Parser.Go.Lexer (GoKeyword(..), GoToken(..))
import Fluxus.Parser.Go.Parser.Common
  ( GoParser
  , GoTokenStream(..)
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
runGoParser filename tokensList =
  runIdentity $
    runNoLoggingT parser
  where
    parser :: NoLoggingT Identity (Either GoParseError GoAST)
    parser =
      fmap (first (bundleToGoParseError filename tokensList)) $
        runParserT parseGo (T.unpack filename) (GoTokenStream tokensList)

-- | Run the Go parser with a custom logging function.
runGoParserWithLogger
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> Text
  -> [Located GoToken]
  -> IO (Either GoParseError GoAST)
runGoParserWithLogger logger filename tokens =
  fmap (first (bundleToGoParseError filename tokens)) $
    runLoggingT
      (runParserT parseGo (T.unpack filename) (GoTokenStream tokens) :: LoggingT IO (Either (ParseErrorBundle GoTokenStream Void) GoAST))
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
  -> ParseErrorBundle GoTokenStream Void
  -> GoParseError
bundleToGoParseError filename tokens bundle =
  let firstError = NE.head (bundleErrors bundle)
      offset = errorOffset firstError
      location = spanAtOffset filename tokens offset
      message = T.stripEnd (T.pack (errorBundlePretty bundle))
  in GoParseError message location
