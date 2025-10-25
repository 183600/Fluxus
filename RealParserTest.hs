{-# LANGUAGE OverloadedStrings #-}
module Main where

import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

testFile :: FilePath -> IO ()
testFile filename = do
    content <- TIO.readFile filename
    putStrLn $ "=== Testing " ++ filename ++ " ==="
    putStrLn "Content:"
    TIO.putStrLn content
    putStrLn ""
    
    putStrLn "=== Lexer Test ==="
    case runPythonLexer (T.pack filename) content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn $ "Successfully tokenized into " ++ show (length tokens) ++ " tokens"
            mapM_ print (take 10 tokens)
            putStrLn "..."
            
            putStrLn "\n=== Parser Test ==="
            case runPythonParser (T.pack filename) tokens of
                Left err -> putStrLn $ "Parser error: " ++ show err
                Right ast -> do
                    putStrLn "Parser succeeded!"
                    print ast

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> testFile filename
        _ -> do
            putStrLn "Testing built-in examples..."
            testFile "test_real_python.py"
            testFile "simple_test.py"