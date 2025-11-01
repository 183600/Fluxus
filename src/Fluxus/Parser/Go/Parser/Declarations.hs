{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Fluxus.Parser.Go.Parser.Declarations
  ( parseDeclaration
  , parseFuncDecl
  , parseTypeDecl
  , parseVarDecl
  , parseConstDecl
  , parseImportDecl
  , parseImportDeclStmt
  ) where

import Control.Applicative (optional, many)
import Control.Monad (void)
import Data.Functor (($>))
import Data.Maybe (fromMaybe, isJust)
import qualified Text.Megaparsec as MP

import Fluxus.AST.Common (Located(..))
import Fluxus.AST.Go
  ( GoDecl(..)
  , GoFunction(..)
  , GoReceiver(..)
  , GoImport(..)
  , GoField(..)
  )
import Fluxus.Parser.Go.Lexer
  ( GoDelimiter(..)
  , GoKeyword(..)
  , GoOperator(..)
  )
import Fluxus.Parser.Go.Parser.Common
  ( GoParser
  , logDebug
  , located
  , located'
  , goKeywordP
  , goOperatorP
  , goDelimiterP
  , parseGoIdentifier
  , parseGoString
  , parseIdentifierList
  , skipCommentsAndNewlines
  , textShow
  )
import Fluxus.Parser.Go.Parser.Expressions
  ( parseGoType
  , parseExpressionList
  , parseParameterList
  )
import Fluxus.Parser.Go.Parser.Statements (parseBlockStmt)

-- | Parse top-level declarations.
parseDeclaration :: Monad m => GoParser m (Located GoDecl)
parseDeclaration = located $ MP.choice
  [ MP.try parseFuncDecl
  , MP.try parseTypeDecl
  , MP.try parseVarDecl
  , MP.try parseConstDecl
  , parseImportDeclStmt
  ]

-- | Parse function declarations and methods.
parseFuncDecl :: Monad m => GoParser m GoDecl
parseFuncDecl = do
  logDebug "parseFuncDecl: entering"
  void $ goKeywordP GoKwFunc
  logDebug "parseFuncDecl: parsed func keyword"
  receiver <- optional $ MP.try $ do
    void $ goDelimiterP GoDelimLeftParen
    recv <- parseReceiver
    void $ goDelimiterP GoDelimRightParen
    pure recv
  logDebug $ "parseFuncDecl: receiver parsed = " <> textShow (isJust receiver)
  name <- parseGoIdentifier
  logDebug $ "parseFuncDecl: name = " <> textShow name
  void $ goDelimiterP GoDelimLeftParen
  params <- parseParameterList
  logDebug $ "parseFuncDecl: params count = " <> textShow (length params)
  void $ goDelimiterP GoDelimRightParen
  results <- optional $ MP.choice
    [ MP.try $ do
        void $ goDelimiterP GoDelimLeftParen
        res <- parseParameterList
        void $ goDelimiterP GoDelimRightParen
        pure res
    , MP.try $ do
        res <- parseGoType
        pure [GoField [] res Nothing]
    , pure []
    ]
  logDebug $ "parseFuncDecl: results count = " <> textShow (maybe 0 length results)
  body <- optional parseBlockStmt
  logDebug $ "parseFuncDecl: has body = " <> textShow (isJust body)
  let func = GoFunction
        { goFuncName = Just name
        , goFuncParams = params
        , goFuncResults = fromMaybe [] results
        , goFuncBody = body
        }
  case receiver of
    Nothing -> do
      logDebug "parseFuncDecl: returning function decl"
      pure $ GoFuncDecl func
    Just recv -> do
      logDebug "parseFuncDecl: returning method decl"
      pure $ GoMethodDecl recv func

-- | Parse type declarations.
parseTypeDecl :: Monad m => GoParser m GoDecl
parseTypeDecl = do
  void $ goKeywordP GoKwType
  name <- parseGoIdentifier
  typeExpr <- parseGoType
  pure $ GoTypeDecl name typeExpr

-- | Parse variable declarations.
parseVarDecl :: Monad m => GoParser m GoDecl
parseVarDecl = do
  void $ goKeywordP GoKwVar
  MP.choice
    [ do
        void $ goDelimiterP GoDelimLeftParen
        specs <- many (parseVarSpec <* skipCommentsAndNewlines)
        void $ goDelimiterP GoDelimRightParen
        pure $ GoVarDecl (concat specs)
    , GoVarDecl <$> parseVarSpec
    ]
  where
    parseVarSpec = do
      names <- parseIdentifierList
      typeExpr <- optional parseGoType
      values <- optional $ do
        void $ goOperatorP GoOpAssign
        parseExpressionList
      pure $ case values of
        Nothing -> map (\name -> (name, typeExpr, Nothing)) names
        Just vals -> zipWith (\name val -> (name, typeExpr, Just val)) names vals

-- | Parse constant declarations.
parseConstDecl :: Monad m => GoParser m GoDecl
parseConstDecl = do
  void $ goKeywordP GoKwConst
  MP.choice
    [ do
        void $ goDelimiterP GoDelimLeftParen
        specs <- many (parseConstSpec <* skipCommentsAndNewlines)
        void $ goDelimiterP GoDelimRightParen
        pure $ GoConstDecl (concat specs)
    , GoConstDecl <$> parseConstSpec
    ]
  where
    parseConstSpec = do
      names <- parseIdentifierList
      typeExpr <- optional parseGoType
      void $ goOperatorP GoOpAssign
      values <- parseExpressionList
      pure $ zipWith (\name val -> (name, typeExpr, val)) names values

-- | Parse import declarations returning declaration nodes.
parseImportDeclStmt :: Monad m => GoParser m GoDecl
parseImportDeclStmt = GoImportDecl <$> parseImportDecl

-- | Parse import declarations.
parseImportDecl :: Monad m => GoParser m [Located GoImport]
parseImportDecl = do
  void $ goKeywordP GoKwImport
  skipCommentsAndNewlines
  MP.choice
    [ do
        void $ goDelimiterP GoDelimLeftParen
        imports <- many $ do
          skipCommentsAndNewlines
          MP.notFollowedBy (goDelimiterP GoDelimRightParen)
          imp <- parseImportSpec
          skipCommentsAndNewlines
          pure $ located' imp
        skipCommentsAndNewlines
        void $ goDelimiterP GoDelimRightParen
        pure imports
    , do
        imp <- parseImportSpec
        pure [located' imp]
    ]
  where
    parseImportSpec = MP.choice
      [ MP.try $ do
          void $ goDelimiterP GoDelimDot
          skipCommentsAndNewlines
          path <- parseGoString
          pure $ GoImportDot path
      , MP.try $ do
          alias <- parseGoIdentifier
          skipCommentsAndNewlines
          path <- parseGoString
          if alias == Identifier "_"
            then pure $ GoImportBlank path
            else pure $ GoImportNormal (Just alias) path
      , GoImportNormal Nothing <$> parseGoString
      ]

parseReceiver :: Monad m => GoParser m GoReceiver
parseReceiver = do
  name <- optional parseGoIdentifier
  typeExpr <- parseGoType
  pure $ GoReceiver name typeExpr
