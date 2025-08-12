module Main where

import Fluxus.Parser.Python.Parser
import Fluxus.Parser.Python.Lexer
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main = do
  content <- TIO.readFile "comprehensive_control_test.py"
  case runPythonLexer (T.pack "comprehensive_control_test.py") content of
    Left err -> putStrLn ("Lexer error: " ++ show err)
    Right tokens -> do
      putStrLn "Lexer success!"
      case runPythonParser (T.pack "comprehensive_control_test.py") tokens of
        Left err -> putStrLn ("Parser error: " ++ show err)
        Right ast -> putStrLn "Parser success!"