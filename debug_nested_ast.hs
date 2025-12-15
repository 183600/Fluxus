#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Fluxus.Parser.Python.Parser
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  source <- TIO.readFile "test_nested_while.py"
  case parsePython "<test>" source of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right ast -> print ast
