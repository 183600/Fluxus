{-# LANGUAGE OverloadedStrings #-}

-- Test program to verify boolean literals are correctly recognized by the lexer
module TestBoolLexer where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Fluxus.Parser.Python.Lexer as Lexer

testBoolLiterals :: IO ()
testBoolLiterals = do
  let input = "true_var = True\nfalse_var = False\nresult = True and False"
  case Lexer.runPythonLexer "test.py" (T.pack input) of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn "Tokens recognized:"
      mapM_ print tokens

main :: IO ()
main = testBoolLiterals