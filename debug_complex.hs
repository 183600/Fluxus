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
  -- Test with the original failing case
  let input = T.unlines
        [ "match data:"
        , "    case [head, *tail] if head > 0:"
        , "        pass"
        ]
  putStrLn $ "Testing complex match statement: " ++ T.unpack input
  
  case runPythonLexer "test.py" input of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Number of tokens: " ++ show (length tokens)
      putStrLn $ "First few tokens: " ++ show (take 10 (map locatedValue tokens))
      case runPythonParser "test.py" tokens of
        Left perr -> putStrLn $ "Parser error: " ++ show perr
        Right (PythonAST module_) -> putStrLn $ "Success! Module has " ++ show (length (pyModuleBody module_)) ++ " statements"