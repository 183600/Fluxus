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
    putStrLn "=== Detailed Go Parser Analysis ==="
    
    case runGoLexer "simple_test.go" content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn $ "Total tokens: " ++ show (length tokens)
            
            -- Find where function starts
            let funcIndex = findFuncStart tokens
            case funcIndex of
                Nothing -> putStrLn "No function found!"
                Just idx -> do
                    putStrLn $ "Function starts at index: " ++ show idx
                    let funcTokens = drop idx tokens
                    putStrLn $ "Function token count: " ++ show (length funcTokens)
                    
                    -- Test parsing the function step by step
                    putStrLn "\n=== Step-by-step function parsing ==="
                    testStepByStep funcTokens
                    
                    -- Test the full function declaration parser
                    putStrLn "\n=== Full function declaration parsing ==="
                    case runParser parseFuncDecl "function" funcTokens of
                        Left err -> putStrLn $ "Function parse error: " ++ show err
                        Right decl -> putStrLn $ "Function parsed successfully: " ++ show decl
  where
    findFuncStart [] = Nothing
    findFuncStart (Located _ (GoTokenKeyword GoKwFunc) : _) = Just 0
    findFuncStart (_:xs) = fmap (+1) (findFuncStart xs)
    
    testStepByStep tokens = do
        -- Test 1: Just the func keyword
        case runParser parseFuncKeyword "func" tokens of
            Left err -> putStrLn "  Failed to parse 'func' keyword"
            Right remaining -> putStrLn "  ✓ 'func' keyword parsed"
            
        -- Test 2: Func + name
        case runParser parseFuncName "func+name" tokens of
            Left err -> putStrLn "  Failed to parse function name"
            Right (name, remaining) -> putStrLn $ "  ✓ Function name parsed: " ++ show name
            
        -- Test 3: Func + name + params
        case runParser parseFuncParams "func+name+params" tokens of
            Left err -> putStrLn "  Failed to parse function parameters"
            Right (params, remaining) -> putStrLn $ "  ✓ Function params parsed: " ++ show (length params)
            
        -- Test 4: Func + name + params + body
        case runParser parseFuncBody "func+name+params+body" tokens of
            Left err -> putStrLn "  Failed to parse function body"
            Right (body, remaining) -> putStrLn $ "  ✓ Function body parsed: " ++ show (fmap (const ()) body)
    
    parseFuncKeyword = do
        void $ goKeywordP GoKwFunc
        return ()
    
    parseFuncName = do
        void $ goKeywordP GoKwFunc
        name <- parseGoIdentifier
        return (name, [])
    
    parseFuncParams = do
        void $ goKeywordP GoKwFunc
        name <- parseGoIdentifier
        void $ goDelimiterP GoDelimLeftParen
        void $ goDelimiterP GoDelimRightParen
        return ([], [])
    
    parseFuncBody = do
        void $ goKeywordP GoKwFunc
        name <- parseGoIdentifier
        void $ goDelimiterP GoDelimLeftParen
        void $ goDelimiterP GoDelimRightParen
        body <- optional parseBlockStmt
        return (body, [])
    
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
    
    -- Import required functions from Go parser
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
    
    parseBlockStmt :: Parsec Void [Located GoToken] (Located GoStmt)
    parseBlockStmt = do
        void $ goDelimiterP GoDelimLeftBrace
        -- Skip content for now
        void $ many $ satisfy $ \case
            Located _ (GoTokenDelimiter GoDelimRightBrace) -> False
            _ -> True
        void $ goDelimiterP GoDelimRightBrace
        return $ noLoc $ GoBlock []
    
    noLoc :: a -> Located a
    noLoc x = Located (SourceSpan "<test>" (Common.SourcePos 0 0) (Common.SourcePos 0 0)) x