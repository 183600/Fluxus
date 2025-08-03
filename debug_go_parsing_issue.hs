{-# LANGUAGE OverloadedStrings #-}

import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

main :: IO ()
main = do
  let content = "package main\n\nimport \"fmt\"\n\nfunc main() {\n    fmt.Println(\"Hello, World!\")\n}\n\nfunc add(a int, b int) int {\n    return a + b\n}"
  
  putStrLn "=== Input Go Code ==="
  TIO.putStrLn content
  putStrLn ""
  
  putStrLn "=== Tokenizing ==="
  case runGoLexer "<test>" content of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show (length tokens) ++ " tokens found"
      mapM_ print (take 20 tokens)
      putStrLn ""
      
      putStrLn "=== Parsing ==="
      case runGoParser "<test>" tokens of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right ast -> do
          putStrLn $ "Parse success! AST: " ++ show ast