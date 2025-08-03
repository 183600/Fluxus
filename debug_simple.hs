{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Text.Megaparsec
import Fluxus.Parser.Go.Lexer (runGoLexer)
import Fluxus.Parser.Go.Parser

main :: IO ()
main = do
  let source = "package main\n\nfunc main() {\n}\n"
  putStrLn "=== Debugging Go Parser ==="
  putStrLn $ "Source: " ++ show source
  
  putStrLn "\n=== Step 1: Lexical Analysis ==="
  case runGoLexer "<debug>" (T.pack source) of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show tokens
      putStrLn $ "Number of tokens: " ++ show (length tokens)
      
      putStrLn "\n=== Step 2: Parse Package ==="
      case runGoParser "<debug>" tokens of
        Left err -> putStrLn $ "Parser error: " ++ show err
        Right ast -> putStrLn $ "AST: " ++ show ast