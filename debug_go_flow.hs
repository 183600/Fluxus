{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Common
import Fluxus.AST.Go
import Control.Monad (void, when)
import Text.Megaparsec
import Text.Megaparsec.Error
import Text.Megaparsec.Char
import Data.Void
import qualified Text.Megaparsec as MP
import qualified Fluxus.AST.Common as Common

main :: IO ()
main = do
    content <- TIO.readFile "simple_test.go"
    putStrLn "=== Debug Go Parser Flow ==="
    
    case runGoLexer "simple_test.go" content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn "=== Step by Step Parsing ==="
            
            -- Step 1: Parse package
            let (tokens1, result1) = runParserStep "parse package" parsePackageStep tokens
            putStrLn $ "After package parse: " ++ show (length tokens1) ++ " tokens remaining"
            
            -- Step 2: Parse imports
            let (tokens2, result2) = runParserStep "parse imports" parseImportsStep tokens1
            putStrLn $ "After imports parse: " ++ show (length tokens2) ++ " tokens remaining"
            
            -- Step 3: Look at what tokens remain
            putStrLn "=== Remaining tokens ==="
            mapM_ (\(i, token) -> putStrLn $ show i ++ ": " ++ show token) (zip [0..] tokens2)
            
            -- Step 4: Try to parse declaration
            let (tokens3, result3) = runParserStep "parse declaration" parseDeclarationStep tokens2
            putStrLn $ "After declaration parse: " ++ show (length tokens3) ++ " tokens remaining"
            
            putStrLn "=== Results ==="
            putStrLn $ "Package result: " ++ show result1
            putStrLn $ "Imports result: " ++ show result2
            putStrLn $ "Declaration result: " ++ show result3
  where
    runParserStep :: String -> Parsec Void [Located GoToken] a -> [Located GoToken] -> ([Located GoToken], Either String a)
    runParserStep name parser tokens = 
        case runParser parser name tokens of
            Left err -> (tokens, Left $ show err)
            Right result -> (tokens, Right result)
    
    parsePackageStep :: Parsec Void [Located GoToken] Identifier
    parsePackageStep = do
        skipCommentsAndNewlines
        void $ goKeywordP GoKwPackage
        packageName <- parseGoIdentifier
        skipCommentsAndNewlines
        return packageName
    
    parseImportsStep :: Parsec Void [Located GoToken] [Located GoImport]
    parseImportsStep = do
        imports <- MP.many (parseImportDecl <* skipCommentsAndNewlines)
        return $ concat imports
    
    parseDeclarationStep :: Parsec Void [Located GoToken] (Located GoDecl)
    parseDeclarationStep = do
        decl <- parseDeclaration
        skipCommentsAndNewlines
        return decl
    
    -- Import the required functions from the Go parser
    skipCommentsAndNewlines :: Parsec Void [Located GoToken] ()
    skipCommentsAndNewlines = void $ MP.many $ satisfy $ \case
        Located _ GoTokenNewline -> True
        Located _ (GoTokenComment _) -> True
        _ -> False
    
    goKeywordP :: GoKeyword -> Parsec Void [Located GoToken] ()
    goKeywordP kw = void $ satisfy $ \case
        Located _ (GoTokenKeyword kw') -> kw == kw'
        _ -> False
    
    parseGoIdentifier :: Parsec Void [Located GoToken] Identifier
    parseGoIdentifier = do
        Located _ token <- anySingle
        case token of
            GoTokenIdent text -> return $ Identifier text
            _ -> fail "Expected identifier"
    
    parseImportDecl :: Parsec Void [Located GoToken] [Located GoImport]
    parseImportDecl = do
        void $ goKeywordP GoKwImport
        skipCommentsAndNewlines
        choice
            [ do
                -- Single import
                imp <- parseImportSpec
                return [noLoc imp]
            ]
        where
            parseImportSpec = do
                path <- parseGoString
                return $ GoImportNormal Nothing path
            
            parseGoString :: Parsec Void [Located GoToken] T.Text
            parseGoString = do
                Located _ token <- anySingle
                case token of
                    GoTokenString text -> return text
                    GoTokenRawString text -> return text
                    _ -> fail "Expected string"
    
    parseDeclaration :: Parsec Void [Located GoToken] (Located GoDecl)
    parseDeclaration = do
        decl <- parseFuncDecl
        return $ noLoc decl
    
    parseFuncDecl :: Parsec Void [Located GoToken] GoDecl
    parseFuncDecl = do
        void $ goKeywordP GoKwFunc
        name <- parseGoIdentifier
        void $ goDelimiterP GoDelimLeftParen
        void $ goDelimiterP GoDelimRightParen
        body <- optional parseBlockStmt
        let func = GoFunction
                { goFuncName = Just name
                , goFuncParams = []
                , goFuncResults = []
                , goFuncBody = body
                }
        return $ GoFuncDecl func
    
    goDelimiterP :: GoDelimiter -> Parsec Void [Located GoToken] ()
    goDelimiterP delim = void $ satisfy $ \case
        Located _ (GoTokenDelimiter delim') -> delim == delim'
        _ -> False
    
    parseBlockStmt :: Parsec Void [Located GoToken] (Located GoStmt)
    parseBlockStmt = do
        void $ goDelimiterP GoDelimLeftBrace
        skipCommentsAndNewlines
        void $ goDelimiterP GoDelimRightBrace
        return $ noLoc $ GoBlock []
    
    noLoc :: a -> Located a
    noLoc x = Located (SourceSpan "<input>" (Common.SourcePos {posLine = 0, posColumn = 0}) (Common.SourcePos {posLine = 0, posColumn = 0})) x