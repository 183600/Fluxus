{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Fluxus.Parser.Python.Lexer

main :: IO ()
main = do
  let input = "True False true false"
  case runPythonLexer "test.py" input of
    Left err -> putStrLn $ "Lexer failed: " ++ show err
    Right tokens -> do
      putStrLn $ "Number of tokens: " ++ show (length tokens)
      mapM_ print tokens