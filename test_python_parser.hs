#!/usr/bin/env stack
-- stack --resolver lts-21.22 script --package megaparsec --package text

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import Fluxus.AST.Python

main :: IO ()
main = do
    content <- TIO.readFile "examples/python/fibonacci.py"
    putStrLn "=== Python Fibonacci Parser Test ==="
    putStrLn "File content:"
    TIO.putStrLn content
    
    putStrLn "\n=== Lexer Test ==="
    case runPythonLexer "fibonacci.py" content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn $ "Tokens: " ++ show (take 20 tokens)
            
            putStrLn "\n=== Parser Test ==="
            case runPythonParser "fibonacci.py" tokens of
                Left err -> putStrLn $ "Parser error: " ++ show err
                Right ast -> putStrLn $ "AST: " ++ show ast