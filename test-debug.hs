{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

-- | Test program to debug Python parser issues
module Main where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (many, some, eof)
import qualified Text.Megaparsec as MP

import Fluxus.AST.Common as Common
import Fluxus.AST.Python
import Fluxus.Parser.Python.Lexer (runPythonLexer, PythonToken(..))
import Fluxus.Parser.Python.Parser
import qualified Fluxus.Parser.Python.Lexer as Lexer

-- | Test case data structure
data TestCase = TestCase
  { testName :: Text
  , testCode :: Text
  , expectedTokens :: [PythonToken]
  } deriving (Eq, Show)

-- | Test cases for debugging
testCases :: [TestCase]
testCases =
  [ TestCase
      { testName = "Simple number literal"
      , testCode = "42"
      , expectedTokens = [TokenNumber "42" False]
      }
  , TestCase
      { testName = "Simple string literal"
      , testCode = "\"Hello\""
      , expectedTokens = [TokenString "Hello"]
      }
  , TestCase
      { testName = "Print call with number"
      , testCode = "print(42)"
      , expectedTokens = [TokenIdent "print", TokenDelimiter Lexer.DelimLeftParen, TokenNumber "42" False, TokenDelimiter Lexer.DelimRightParen]
      }
  , TestCase
      { testName = "Print call with string"
      , testCode = "print(\"Hello\")"
      , expectedTokens = [TokenIdent "print", TokenDelimiter Lexer.DelimLeftParen, TokenString "Hello", TokenDelimiter Lexer.DelimRightParen]
      }
  , TestCase
      { testName = "Variable assignment"
      , testCode = "x = 42"
      , expectedTokens = [TokenIdent "x", TokenOperator Lexer.OpAssign, TokenNumber "42" False]
      }
  ]

-- | Helper to print token list nicely
printTokens :: [Located PythonToken] -> IO ()
printTokens tokens = do
  putStrLn "Tokens:"
  mapM_ printToken tokens
  where
    printToken (Located span token) = do
      putStrLn $ "  " ++ show token ++ " at " ++ show span

-- | Test lexer functionality
testLexer :: TestCase -> IO ()
testLexer testCase = do
  putStrLn $ "\n=== Testing Lexer: " ++ T.unpack (testName testCase) ++ " ==="
  putStrLn $ "Code: " ++ T.unpack (testCode testCase)
  
  case runPythonLexer "test" (testCode testCase) of
    Left err -> do
      putStrLn "LEXER ERROR:"
      putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn "LEXER SUCCESS:"
      printTokens tokens
      
      -- Compare with expected tokens
      let actualTokens = map locValue tokens
          expectedTokensList = expectedTokens testCase
      if actualTokens == expectedTokensList
        then putStrLn "✓ Tokens match expected output"
        else do
          putStrLn "✗ Tokens don't match expected output"
          putStrLn $ "Expected: " ++ show expectedTokensList
          putStrLn $ "Actual:   " ++ show actualTokens

-- | Test parser functionality
testParser :: TestCase -> IO ()
testParser testCase = do
  putStrLn $ "\n=== Testing Parser: " ++ T.unpack (testName testCase) ++ " ==="
  putStrLn $ "Code: " ++ T.unpack (testCode testCase)
  
  -- First, get tokens from lexer
  case runPythonLexer "test" (testCode testCase) of
    Left err -> do
      putStrLn "LEXER ERROR (before parser):"
      putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn "Tokens to parse:"
      printTokens tokens
      
      -- Now try to parse
      let result = runPythonParser "test" tokens
      case result of
        Left err -> do
          putStrLn "PARSER ERROR:"
          putStrLn $ "Parse error: " ++ show err
          
          -- Try to get more detailed error information
          putStrLn "\nDetailed error analysis:"
          analyzeParseError err tokens
        Right ast -> do
          putStrLn "PARSER SUCCESS:"
          putStrLn $ "AST: " ++ show ast

-- | Analyze parse error to provide more debugging information
analyzeParseError :: ParseErrorBundle [Located PythonToken] Void -> [Located PythonToken] -> IO ()
analyzeParseError err tokens = do
  putStrLn $ "Error details:"
  putStrLn $ "Parse error: " ++ show err
  
  putStrLn "\nTokens being parsed:"
  mapM_ (\(i, token) -> putStrLn $ "  [" ++ show i ++ "] " ++ show token) (zip [0..] tokens)
  
  -- Check for common issues
  putStrLn "\nCommon issue checks:"
  checkForCommonIssues tokens 0

-- | Check for common parsing issues
checkForCommonIssues :: [Located PythonToken] -> Int -> IO ()
checkForCommonIssues tokens errorOffset = do
  -- Check if error occurs at a literal position
  if errorOffset < length tokens
    then case locValue (tokens !! errorOffset) of
      TokenNumber _ _ -> putStrLn "  - Error at number literal - parseLiteral should handle this"
      TokenString _ -> putStrLn "  - Error at string literal - parseLiteral should handle this"
      TokenKeyword kw -> 
        case kw of
          Lexer.KwTrue -> putStrLn "  - Error at True keyword - parseLiteral should handle this"
          Lexer.KwFalse -> putStrLn "  - Error at False keyword - parseLiteral should handle this"
          Lexer.KwNone -> putStrLn "  - Error at None keyword - parseLiteral should handle this"
          _ -> putStrLn "  - Error at keyword that might not be expected as literal"
      TokenIdent _ -> putStrLn "  - Error at identifier - this suggests parseLiteral is being called when it shouldn't be"
      TokenDelimiter _ -> putStrLn "  - Error at delimiter - this suggests parseLiteral is being called when it shouldn't be"
      TokenOperator _ -> putStrLn "  - Error at operator - this suggests parseLiteral is being called when it shouldn't be"
      _ -> putStrLn "  - Error at unexpected token type"
    else putStrLn "  - Error position beyond token list"

-- | Test individual parser components
testParserComponents :: IO ()
testParserComponents = do
  putStrLn "\n=== Testing Individual Parser Components ==="
  putStrLn "Note: Direct parseLiteral testing removed due to API issues."
  putStrLn "Focus is on full pipeline testing."

-- | Test the entire parsing pipeline
testFullPipeline :: IO ()
testFullPipeline = do
  putStrLn "\n=== Full Parser Pipeline Test ==="
  
  -- Test with simple expressions that should work
  let simpleTests = ["42", "\"Hello\"", "True", "False", "None"]
  
  mapM_ testSimpleExpression simpleTests
  
  where
    testSimpleExpression code = do
      putStrLn $ "\nTesting simple expression: " ++ code
      case runPythonLexer "test" (T.pack code) of
        Left err -> putStrLn $ "  LEXER ERROR: " ++ MP.errorBundlePretty err
        Right tokens -> do
          putStrLn $ "  Tokens: " ++ show (map locValue tokens)
          let result = runPythonParser "test" tokens
          case result of
            Left err -> putStrLn $ "  PARSER ERROR: " ++ show err
            Right ast -> putStrLn $ "  SUCCESS: " ++ show ast

-- | Main test runner
main :: IO ()
main = do
  putStrLn "Debug Python Parser Test Suite"
  putStrLn "================================"
  
  -- Test individual components first
  testParserComponents
  
  -- Test full pipeline with simple expressions
  testFullPipeline
  
  -- Test with our test cases
  mapM_ testLexer testCases
  mapM_ testParser testCases
  
  putStrLn "\n=== Test Summary ==="
  putStrLn "If you see 'Expected literal' errors, they are likely occurring in the parseLiteral function"
  putStrLn "around line 456 in /home/qwe12345678/hyperstatic2/src/Fluxus/Parser/Python/Parser.hs"
  putStrLn "The error happens when parseLiteral receives a token it doesn't know how to handle."