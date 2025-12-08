#!/usr/bin/env stack
-- stack script --resolver lts-21.25

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import Fluxus.AST.Python
import Fluxus.AST.Common (Located(..), locatedValue)

testParse :: T.Text -> IO ()
testParse input = do
  putStrLn $ "Testing: " ++ T.unpack input
  case runPythonLexer "test.py" input of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      case runPythonParser "test.py" tokens of
        Left perr -> putStrLn $ "Parser error: " ++ show perr
        Right (PythonAST module_) -> putStrLn $ "Success! Module has " ++ show (length (pyModuleBody module_)) ++ " statements"
  putStrLn ""

main :: IO ()
main = do
  -- Test progressively more complex match statements
  testParse "match x:\n    case 1:\n        pass"
  
  testParse "match x:\n    case [1]:\n        pass"
  
  testParse "match x:\n    case [1, 2]:\n        pass"
  
  testParse "match x:\n    case [head]:\n        pass"
  
  testParse "match x:\n    case [head, tail]:\n        pass"
  
  testParse "match x:\n    case [head, *tail]:\n        pass"
  
  testParse "match x:\n    case [head, *tail] if head > 0:\n        pass"