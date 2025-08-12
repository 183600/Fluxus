module Main where

import Fluxus.Parser.Python.Parser
import Fluxus.Parser.Python.Lexer
import Fluxus.AST.Common
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main = do
  content <- TIO.readFile "simple_while_test.py"
  case runPythonLexer (T.pack "simple_while_test.py") content of
    Left err -> putStrLn ("Lexer error: " ++ show err)
    Right tokens -> do
      putStrLn "Lexer success!"
      mapM_ printToken tokens
      case runPythonParser (T.pack "simple_while_test.py") tokens of
        Left err -> putStrLn ("Parser error: " ++ show err)
        Right ast -> putStrLn "Parser success!"
  where
    printToken (Located _ token) = putStrLn $ "  " ++ show token