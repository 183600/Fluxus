#!/usr/bin/env stack
-- stack script --resolver lts-21.25

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser

main :: IO ()
main = do
  let source = "package main\n\nfunc main() {\n}\n"
  putStrLn "=== Debugging Go Parser ==="
  putStrLn $ "Source: " ++ show source
  
  putStrLn "\n=== Step 1: Lexical Analysis ==="
  case lexGo "<debug>" (T.pack source) of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show tokens
      
      putStrLn "\n=== Step 2: Parse Package ==="
      case runGoParser "<debug>" tokens of
        Left err -> putStrLn $ "Parser error: " ++ show err
        Right ast -> putStrLn $ "AST: " ++ show ast
      
      putStrLn "\n=== Step 3: Manual parseDeclaration Test ==="
      -- Try to manually parse parseDeclaration after skipping package and imports
      case parseTest (parsePackageAndSkipToDecls >> parseDeclaration) tokens of
        Left err -> putStrLn $ "parseDeclaration error: " ++ show err  
        Right decl -> putStrLn $ "parseDeclaration success: " ++ show decl

parsePackageAndSkipToDecls = do
  skipCommentsAndNewlines
  void $ goKeywordP GoKwPackage
  _ <- parseGoIdentifier
  skipCommentsAndNewlines
  -- Skip any imports
  _ <- many (parseImportDecl >> skipCommentsAndNewlines)
  skipCommentsAndNewlines
  return ()