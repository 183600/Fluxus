#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import Fluxus.AST.Python
import Fluxus.AST.Common (Located(..), locatedValue, locValue)

main :: IO ()
main = do
  let source = T.unlines
        [ "from typing import Callable"
        , "callback: Callable[[int, str], bool] = lambda a, b: True"]
  
  putStrLn "Testing source code:"
  putStrLn $ T.unpack source
  putStrLn "\n=== Lexer output ==="
  
  case runPythonLexer "test.py" source of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show (length tokens)
      
      putStrLn "\n=== Parser output ==="
      case runPythonParser "test.py" tokens of
        Left perr -> putStrLn $ "Parser error: " ++ show perr
        Right (PythonAST module_) -> do
          putStrLn $ "Parsed module with " ++ show (length (pyModuleBody module_)) ++ " statements"
          mapM_ (\stmt -> putStrLn $ "Statement: " ++ show (locatedValue stmt)) (pyModuleBody module_)