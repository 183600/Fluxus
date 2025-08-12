module Main where

import Fluxus.Parser.Python.Parser
import Fluxus.Parser.Python.Lexer
import Fluxus.AST.Common
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main = do
  let code = "count = 0\nwhile count < 3:\n    print(count)\n    count = count + 1\nelse:\n    print(\"Finished\")\n"
  case runPythonLexer (T.pack "<input>") (T.pack code) of
    Left err -> putStrLn ("Lexer error: " ++ show err)
    Right tokens -> do
      putStrLn "Lexer success!"
      mapM_ printToken tokens
      case runPythonParser (T.pack "<input>") tokens of
        Left err -> putStrLn ("Parser error: " ++ show err)
        Right ast -> putStrLn "Parser success!"
  where
    printToken (Located _ token) = putStrLn $ "  " ++ show token