{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser

main :: IO ()
main = do
  -- Test 1: Simple type declaration (should work)
  putStrLn "=== Test 1: Simple type ==="
  let input1 = "package main\ntype MyInt int"
  case runGoLexer "test.go" (T.pack input1) of
    Left err -> putStrLn $ "Lexer failed: " ++ show err
    Right tokens1 -> do
      putStrLn $ "Tokens: " ++ show tokens1
      case runGoParser "test.go" tokens1 of
        Left err -> putStrLn $ "Parser failed: " ++ show err
        Right ast1 -> putStrLn $ "AST: " ++ show ast1
  
  -- Test 2: Simple struct (minimal)
  putStrLn "\n=== Test 2: Simple struct ==="
  let input2 = "package main\ntype Person struct { Name string }"
  case runGoLexer "test.go" (T.pack input2) of
    Left err -> putStrLn $ "Lexer failed: " ++ show err
    Right tokens2 -> do
      putStrLn $ "Tokens: " ++ show tokens2
      case runGoParser "test.go" tokens2 of
        Left err -> putStrLn $ "Parser failed: " ++ show err
        Right ast2 -> putStrLn $ "AST: " ++ show ast2