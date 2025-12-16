{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  let code = T.unlines
        [ "def greet(name):"
        , "    return f\"Hello {name}\""
        , ""
        , "print(greet(\"Fluxus\"))"
        ]
  
  putStrLn "=== Lexing ==="
  case runPythonLexer "test.py" code of
    Left err -> putStrLn $ "Lexer error: " ++ errorBundlePretty err
    Right tokens -> do
      putStrLn "Tokens:"
      mapM_ print tokens
      
      putStrLn "\n=== Parsing ==="
      case runPythonParser "test.py" tokens of
        Left err -> putStrLn $ "Parser error: " ++ show err
        Right ast -> do
          putStrLn "AST:"
          print ast
