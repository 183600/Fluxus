{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import Text.Megaparsec (errorBundlePretty, parse)
import Control.Monad (void)

main :: IO ()
main = do
  let code = "def greet():\n    print(\"Hello\")\n"
  
  putStrLn "=== Code ==="
  putStrLn $ T.unpack code
  
  putStrLn "\n=== Lexing ==="
  case runPythonLexer "test.py" code of
    Left err -> putStrLn $ "Lexer error: " ++ errorBundlePretty err
    Right tokens -> do
      putStrLn $ "Token count: " ++ show (length tokens)
      putStrLn "Last 3 tokens:"
      mapM_ print (drop (length tokens - 3) tokens)
      
      putStrLn "\n=== Parsing ==="
      case runPythonParser "test.py" tokens of
        Left err -> putStrLn $ "Parser error: " ++ show err
        Right ast -> do
          putStrLn "Success!"
          print ast
