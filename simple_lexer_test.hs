#!/usr/bin/env runhaskell

-- | Simple test program to debug the Python lexer in Fluxus
-- This program tests the lexer with simple Python expressions to identify tokenization issues

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import Data.Char (isAlphaNum, isAlpha, isDigit)

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

-- | Lexer state
data LexerState = LexerState
  { atLineStart :: Bool
  } deriving (Show, Eq)

-- | Lexer type
type SimpleLexer = StateT LexerState (Parsec Void Text)

-- | Run the simple lexer
runSimpleLexer :: Text -> Text -> Either (ParseErrorBundle Text Void) [SimplePythonToken]
runSimpleLexer filename input = parse (evalStateT lexPython initialLexerState) (T.unpack filename) input
  where
    initialLexerState = LexerState { atLineStart = True }

-- | Main lexer
lexPython :: SimpleLexer [SimplePythonToken]
lexPython = do
  tokens <- many simpleToken
  eof
  return tokens

-- | Parse a single token
simpleToken :: SimpleLexer SimplePythonToken
simpleToken = choice
  [ try identifier
  , try stringLiteral
  , try numberLiteral
  , lparen
  , rparen
  , newline
  , whitespace
  ]
  <|> (eof $> TokenEOF)

-- | Parse identifier
identifier :: SimpleLexer SimplePythonToken
identifier = do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  return $ TokenIdent (T.pack (first : rest))

-- | Parse string literal
stringLiteral :: SimpleLexer SimplePythonToken
stringLiteral = do
  _ <- char '"'
  content <- many (satisfy (/= '"'))
  _ <- char '"'
  return $ TokenString (T.pack content)

-- | Parse number literal
numberLiteral :: SimpleLexer SimplePythonToken
numberLiteral = do
  digits <- some digitChar
  return $ TokenNumber (T.pack digits)

-- | Parse left parenthesis
lparen :: SimpleLexer SimplePythonToken
lparen = char '(' $> TokenLParen

-- | Parse right parenthesis
rparen :: SimpleLexer SimplePythonToken
rparen = char ')' $> TokenRParen

-- | Parse newline
newline :: SimpleLexer SimplePythonToken
newline = do
  _ <- char '\n' <|> (char '\r' *> optional (char '\n'))
  modify $ \s -> s { atLineStart = True }
  return TokenNewline

-- | Parse whitespace
whitespace :: SimpleLexer SimplePythonToken
whitespace = do
  _ <- many (char ' ' <|> char '\t')
  -- Return a dummy token that will be filtered out
  return $ TokenError "whitespace"

-- | Test a simple Python expression with the lexer
testLexer :: Text -> Text -> IO ()
testLexer testName input = do
    putStrLn $ "=== Testing: " ++ T.unpack testName ++ " ==="
    putStrLn $ "Input: " ++ T.unpack input
    putStrLn "Tokens:"
    
    case runSimpleLexer "test.py" input of
        Left err -> do
            putStrLn $ "ERROR: " ++ errorBundlePretty err
        Right tokens -> do
            let filteredTokens = filter (\t -> t /= TokenError "whitespace") tokens
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