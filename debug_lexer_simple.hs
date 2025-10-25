#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Fluxus.Parser.Python.Lexer
import qualified Data.Text as T

main :: IO ()
main = do
  putStrLn "Testing Python lexer..."
  
  -- Test simple expression
  let input1 = "x + 42"
  putStrLn $ "Testing: " ++ show input1
  case runPythonLexer "test.py" input1 of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right tokens -> do
      putStrLn $ "SUCCESS: " ++ show (length tokens) ++ " tokens"
      mapM_ (putStrLn . ("  " ++) . show) tokens
  
  putStrLn ""
  
  -- Test keywords
  let input2 = "def"
  putStrLn $ "Testing: " ++ show input2
  case runPythonLexer "test.py" input2 of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right tokens -> do
      putStrLn $ "SUCCESS: " ++ show (length tokens) ++ " tokens"
      mapM_ (putStrLn . ("  " ++) . show) tokens