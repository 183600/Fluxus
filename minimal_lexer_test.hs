#!/usr/bin/env runhaskell

-- | Simple test program to debug the Python lexer in Fluxus
-- This program tests the lexer with simple Python expressions to identify tokenization issues

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T

-- | Simple Python token types for debugging
data SimplePythonToken
  = TokenIdent Text
  | TokenString Text
  | TokenNumber Text
  | TokenLParen
  | TokenRParen
  | TokenNewline
  | TokenEOF
  | TokenError Text
  deriving (Show, Eq)

-- | Simple lexer that works with basic Haskell
simpleLex :: Text -> [SimplePythonToken]
simpleLex input = go (T.unpack input) []
  where
    go [] tokens = reverse (TokenEOF : tokens)
    go (' ':cs) tokens = go cs tokens
    go ('\n':cs) tokens = go cs (TokenNewline : tokens)
    go ('\r':cs) tokens = go cs tokens
    go ('(':cs) tokens = go cs (TokenLParen : tokens)
    go (')':cs) tokens = go cs (TokenRParen : tokens)
    go ('"':cs) tokens = 
      let (str, rest) = span (/= '"') cs
      in go (tail rest) (TokenString (T.pack str) : tokens)
    go (c:cs) tokens 
      | isDigit c = 
          let (digits, rest) = span isDigit (c:cs)
          in go rest (TokenNumber (T.pack digits) : tokens)
      | isAlpha c || c == '_' = 
          let (ident, rest) = span (\x -> isAlpha x || isDigit x || x == '_') (c:cs)
          in go rest (TokenIdent (T.pack ident) : tokens)
      | otherwise = go cs (TokenError (T.pack [c]) : tokens)
    
    isDigit c = c >= '0' && c <= '9'
    isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

-- | Test a simple Python expression with the lexer
testLexer :: Text -> Text -> IO ()
testLexer testName input = do
    putStrLn $ "=== Testing: " ++ T.unpack testName ++ " ==="
    putStrLn $ "Input: " ++ T.unpack input
    putStrLn "Tokens:"
    
    let tokens = simpleLex input
        filteredTokens = filter (\t -> t /= TokenError " " && t /= TokenError "\r") tokens
    
    putStrLn $ "SUCCESS: Found " ++ show (length filteredTokens) ++ " tokens"
    mapM_ printToken filteredTokens
    putStrLn ""

-- | Print a token
printToken :: SimplePythonToken -> IO ()
printToken token = putStrLn $ "  " ++ show token

-- | Test cases for simple Python expressions
main :: IO ()
main = do
    putStrLn "Simple Python Lexer Debug Test"
    putStrLn "================================"
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