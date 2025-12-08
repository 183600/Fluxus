#!/usr/bin/env stack
-- stack script --resolver lts-21.25

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import Fluxus.AST.Python
import Fluxus.AST.Common (Located(..), locatedValue)

main :: IO ()
main = do
  -- Test with a simple statement first
  let input1 = "x = 42"
  putStrLn $ "Testing simple statement: " ++ T.unpack input1
  
  case runPythonLexer "test.py" input1 of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show (map locatedValue tokens)
      case runPythonParser "test.py" tokens of
        Left perr -> putStrLn $ "Parser error: " ++ show perr
        Right (PythonAST module_) -> putStrLn $ "Success! Module has " ++ show (length (pyModuleBody module_)) ++ " statements"
  
  putStrLn $ "\n" ++ replicate 50 '-' ++ "\n"
  
  -- Test with a match statement
  let input2 = T.unlines
        [ "match data:"
        , "    case 1:"
        , "        pass"
        ]
  putStrLn $ "Testing simple match statement: " ++ T.unpack input2
  
  case runPythonLexer "test.py" input2 of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show (map locatedValue tokens)
      case runPythonParser "test.py" tokens of
        Left perr -> putStrLn $ "Parser error: " ++ show perr
        Right (PythonAST module_) -> putStrLn $ "Success! Module has " ++ show (length (pyModuleBody module_)) ++ " statements"