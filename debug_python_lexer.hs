#!/usr/bin/env runhaskell

-- | Debug program for the Python lexer in Fluxus
-- This program tests the lexer with various Python expressions

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Fluxus.Parser.Python.Lexer (runPythonLexer, PythonToken)
import Fluxus.AST.Common (Located(..))
import Text.Megaparsec (ParseErrorBundle)

-- | Test the lexer with a given input
testLexer :: Text -> Text -> IO ()
testLexer testName input = do
    putStrLn $ "=== Testing: " ++ T.unpack testName ++ " ==="
    putStrLn $ "Input: " ++ T.unpack input
    putStrLn "Tokens:"
    
    case runPythonLexer "test.py" input of
        Left err -> do
            putStrLn $ "ERROR: " ++ show (err :: ParseErrorBundle Text Void)
        Right lexerTokens -> do
            putStrLn $ "SUCCESS: Found " ++ show (length lexerTokens) ++ " tokens"
            mapM_ printToken lexerTokens
    putStrLn ""

-- | Print a token
printToken :: Located PythonToken -> IO ()
printToken locatedToken = putStrLn $ "  " ++ show locatedToken

-- | Test cases for Python expressions
main :: IO ()
main = do
    putStrLn "Python Lexer Debug Test"
    putStrLn "======================="
    putStrLn ""
    
    -- Test case 1: Simple print with number
    testLexer "print(42)" "print(42)"
    
    -- Test case 2: Simple print with string
    testLexer "print(\"Hello\")" "print(\"Hello\")"
    
    -- Test case 3: Multiple statements
    testLexer "Multiple statements" "print(42)\nprint(\"Hello\")"
    
    -- Test case 4: Function definition
    testLexer "Function definition" "def foo():\n    return 42"
    
    -- Test case 5: Variable assignment
    testLexer "Variable assignment" "x = 42"
    
    -- Test case 6: Expression with operator
    testLexer "Expression with operator" "1 + 2 * 3"
    
    putStrLn "Test completed!"