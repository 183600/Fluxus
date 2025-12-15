#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Fluxus.Parser.Python.Lexer
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  source <- TIO.readFile "test_fstring1.py"
  case lexPython "test_fstring1.py" source of
    Left err -> putStrLn $ "Lex error: " ++ show err
    Right tokens -> mapM_ print tokens
