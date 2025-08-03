{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Common
import Fluxus.AST.Go
import Control.Monad (void)
import Text.Megaparsec
import Data.Void
import qualified Text.Megaparsec as MP
import qualified Fluxus.AST.Common as Common

main :: IO ()
main = do
    content <- TIO.readFile "simple_test.go"
    putStrLn "=== Testing Go Parser Import Section ==="
    
    case runGoLexer "simple_test.go" content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn $ "Total tokens: " ++ show (length tokens)
            
            -- Test the import parsing specifically
            let afterPackage = drop 3 tokens  -- Skip package, main, newline
            putStrLn $ "Tokens after package: " ++ show (length afterPackage)
            
            putStrLn "\n=== Testing parseImportDecl on import tokens ==="
            case runParser parseImportDecl "import" afterPackage of
                Left err -> putStrLn $ "Import parse error: " ++ show err
                Right imports -> putStrLn $ "Import parsed successfully: " ++ show (length imports)
            
            putStrLn "\n=== Testing MP.many parseImportDecl ==="
            case runParser (MP.many parseImportDecl) "many-imports" afterPackage of
                Left err -> putStrLn $ "Many imports error: " ++ show err
                Right imports -> putStrLn $ "Many imports parsed: " ++ show (length $ concat imports)
            
            putStrLn "\n=== Testing what happens after imports ==="
            case runParser parseImportsAndShowRemaining "imports-test" afterPackage of
                Left err -> putStrLn $ "Imports test error: " ++ show err
                Right (imports, remaining) -> do
                    putStrLn $ "Imports parsed: " ++ show (length $ concat imports)
                    putStrLn $ "Remaining tokens: " ++ show (length remaining)
                    putStrLn "Remaining tokens:"
                    mapM_ printToken (take 5 remaining)
  where
    printToken token = putStrLn $ "  " ++ show token
    
    parseImportsAndShowRemaining = do
        imports <- MP.many parseImportDecl
        remaining <- getInput
        return (imports, remaining)
    
    -- Import the required functions from the Go parser
    parseImportDecl :: GoParser [Located GoImport]
    parseImportDecl = do
        skipCommentsAndNewlines
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
    
    goKeywordP :: GoKeyword -> Parsec Void [Located GoToken] ()
    goKeywordP kw = void $ satisfy $ \case
        Located _ (GoTokenKeyword kw') -> kw == kw'
        _ -> False
    
    goDelimiterP :: GoDelimiter -> Parsec Void [Located GoToken] ()
    goDelimiterP delim = void $ satisfy $ \case
        Located _ (GoTokenDelimiter delim') -> delim == delim'
        _ -> False
    
    parseGoIdentifier :: Parsec Void [Located GoToken] Identifier
    parseGoIdentifier = do
        Located _ token <- anySingle
        case token of
            GoTokenIdent text -> return $ Identifier text
            _ -> fail "Expected identifier"
    
    parseGoString :: Parsec Void [Located GoToken] T.Text
    parseGoString = do
        Located _ token <- anySingle
        case token of
            GoTokenString text -> return text
            GoTokenRawString text -> return text
            _ -> fail "Expected string"
    
    skipCommentsAndNewlines :: Parsec Void [Located GoToken] ()
    skipCommentsAndNewlines = void $ MP.many $ satisfy $ \case
        Located _ GoTokenNewline -> True
        Located _ (GoTokenComment _) -> True
        _ -> False
    
    noLoc :: a -> Located a
    noLoc x = Located (SourceSpan "<test>" (Common.SourcePos 0 0) (Common.SourcePos 0 0)) x