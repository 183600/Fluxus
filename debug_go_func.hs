{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Common
import Fluxus.AST.Go
import Control.Monad (when)

main :: IO ()
main = do
    content <- TIO.readFile "test_go_simple.go"
    putStrLn "=== Debug Go Function Parsing ==="
    
    case runGoLexer "simple_test.go" content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn "=== All Tokens ==="
            mapM_ printToken tokens
            putStrLn ""
            
            -- Find the 'func' token position
            let funcPos = findFuncPosition tokens
            case funcPos of
                Nothing -> putStrLn "No 'func' token found"
                Just pos -> do
                    putStrLn $ "'func' found at position: " ++ show pos
                    let funcTokens = drop pos tokens
                    putStrLn $ "Tokens from func position: " ++ show (length funcTokens)
                    putStrLn "=== Attempting to parse function ==="
                    
                    -- Try to parse just the function part
                    case runGoParser "test" funcTokens of
                        Left err -> do
                            putStrLn $ "Function parse error: " ++ show err
                            putStrLn "=== Individual token test ==="
                            -- Test each token individually
                            mapM_ testToken funcTokens
                        Right ast -> putStrLn $ "Function parse success: " ++ show ast
  where
    printToken token = putStrLn $ "  " ++ show token
    
    findFuncPosition [] = Nothing
    findFuncPosition (Located _ (GoTokenKeyword GoKwFunc) : _) = Just 0
    findFuncPosition (_:xs) = fmap (+1) (findFuncPosition xs)
    
    testToken (Located tokenSpan tokenItem) = case tokenItem of
        GoTokenIdent text -> putStrLn $ "  Identifier: " ++ T.unpack text
        GoTokenKeyword kw -> putStrLn $ "  Keyword: " ++ show kw
        GoTokenDelimiter delim -> putStrLn $ "  Delimiter: " ++ show delim
        GoTokenOperator op -> putStrLn $ "  Operator: " ++ show op
        _ -> putStrLn $ "  Other token: " ++ show tokenItem