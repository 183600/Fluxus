#!/usr/bin/env runhaskell

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Go

-- | Test a simple Go code snippet
testSimpleGo :: T.Text -> IO ()
testSimpleGo input = do
    putStrLn "=== Testing Go Parser ==="
    putStrLn "Input:"
    TIO.putStrLn input
    putStrLn ""
    
    -- Test lexer
    putStrLn "=== Lexer ==="
    case runGoLexer "test.go" input of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn $ "Tokens (" ++ show (length tokens) ++ "):"
            mapM_ (putStrLn . ("  " ++) . show) (take 10 tokens)
            if length tokens > 10 then putStrLn "  ..." else return ()
            putStrLn ""
            
            -- Test parser
            putStrLn "=== Parser ==="
            case runGoParser "test.go" tokens of
                Left err -> putStrLn $ "Parser error: " ++ show err
                Right ast -> do
                    putStrLn "Success! AST:"
                    print ast

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            -- Test with simple cases
            testSimpleGo "package main"
            putStrLn "\n" ++ replicate 50 '='
            testSimpleGo "package main\n\nfunc main() {}"
        [filename] -> do
            content <- TIO.readFile filename
            testSimpleGo content
        _ -> putStrLn "Usage: runhaskell test_go_parser_simple.hs [filename]"