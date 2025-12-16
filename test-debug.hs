#!/usr/bin/env runhaskell

-- | Debug test program for the Python parser in Fluxus
-- This program tests the parser with various Python expressions

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Fluxus.Parser.Python.Parser (runPythonParser)
import Fluxus.Parser.Python.Lexer (runPythonLexer)
import Text.Megaparsec (ParseErrorBundle)

-- | Test the parser with a given input
testParser :: Text -> Text -> IO ()
testParser testName input = do
    putStrLn $ "=== Testing: " ++ T.unpack testName ++ " ==="
    putStrLn $ "Input: " ++ T.unpack input
    putStrLn "AST:"
    
    case runPythonLexer "test.py" input of
        Left err -> do
            putStrLn $ "LEXER ERROR: " ++ show (err :: ParseErrorBundle Text Void)
        Right lexerTokens -> do
            putStrLn $ "SUCCESS: Lexed " ++ show (length lexerTokens) ++ " tokens"
            case runPythonParser "test.py" lexerTokens of
                Left parseErr -> do
                    putStrLn $ "PARSER ERROR: " ++ show parseErr
                Right ast -> do
                    putStrLn $ "SUCCESS: Parsed successfully"
                    print ast
    putStrLn ""

-- | Test cases for Python expressions
main :: IO ()
main = do
    putStrLn "Python Parser Debug Test"
    putStrLn "========================"
    putStrLn ""
    
    -- Test case 1: Simple print with number
    testParser "print(42)" "print(42)"
    
    -- Test case 2: Simple print with string
    testParser "print(\"Hello\")" "print(\"Hello\")"
    
    -- Test case 3: Variable assignment
    testParser "Variable assignment" "x = 42"
    
    -- Test case 4: Function definition
    testParser "Function definition" "def foo():\n    return 42"
    
    -- Test case 5: Expression with operator
    testParser "Expression with operator" "1 + 2 * 3"
    
    putStrLn "Test completed!"