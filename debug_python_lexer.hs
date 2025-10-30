#!/usr/bin/env runhaskell

-- | Simple test program to debug the Python lexer in Fluxus
-- This program tests the lexer with simple Python expressions to identify tokenization issues

module Main (main) where

import Fluxus.Parser.Python.Lexer
import Fluxus.AST.Common
import qualified Data.Text as T
import Data.Text (Text)

-- | Test a simple Python expression with the lexer
testLexer :: Text -> Text -> IO ()
testLexer testName input = do
    putStrLn $ "=== Testing: " ++ T.unpack testName ++ " ==="
    putStrLn $ "Input: " ++ T.unpack input
    putStrLn "Tokens:"
    
    case runPythonLexer (T.pack "test.py") input of
        Left err -> do
            putStrLn $ "ERROR: " ++ show err
        Right tokens -> do
            putStrLn $ "SUCCESS: Found " ++ show (length tokens) ++ " tokens"
            mapM_ printToken tokens
    putStrLn ""

-- | Print a token with its location information
printToken :: Located PythonToken -> IO ()
printToken (Located loc token) = do
    let posStr = show (posLine (spanStart loc)) ++ ":" ++ show (posColumn (spanStart loc))
    putStrLn $ "  " ++ posStr ++ " -> " ++ show token

-- | Test cases for simple Python expressions
main :: IO ()
main = do
    putStrLn "Fluxus Python Lexer Debug Test"
    putStrLn "=============================="
    putStrLn ""
    
    -- Test case 1: Simple print with number
    testLexer (T.pack "print(42)") (T.pack "print(42)")
    
    -- Test case 2: Simple print with string
    testLexer (T.pack "print(\"Hello\")") (T.pack "print(\"Hello\")")
    
    -- Test case 3: Multiple statements
    testLexer (T.pack "Multiple statements") (T.pack "print(42)\nprint(\"Hello\")")
    
    -- Test case 4: Function definition
    testLexer (T.pack "Function definition") (T.pack "def foo():\n    return 42")
    
    -- Test case 5: Variable assignment
    testLexer (T.pack "Variable assignment") (T.pack "x = 42")
    
    -- Test case 6: Expression with operator
    testLexer (T.pack "Expression with operator") (T.pack "1 + 2 * 3")
    
    -- Test case 7: Potential issues
    testLexer (T.pack "Potential issues") (T.pack "if x: pass")
    
    -- Test case 8: String with escaped quotes
    testLexer (T.pack "Escaped quotes") (T.pack "print(\"Hello \\\"world\\\"\")")
    
    -- Test case 9: Boolean literals
    testLexer (T.pack "Boolean literals") (T.pack "True and False")
    
    putStrLn "Test completed!"