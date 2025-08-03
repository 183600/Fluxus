{-# LANGUAGE OverloadedStrings #-}

import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

main :: IO ()
main = do
  content <- TIO.readFile "debug_for_simple.go"
  
  putStrLn "=== Go File Parsing Test ==="
  TIO.putStrLn content
  putStrLn ""
  
  putStrLn "=== Tokenizing ==="
  case runGoLexer "debug_for_simple.go" content of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show (length tokens) ++ " tokens found"
      putStrLn ""
      
      putStrLn "=== Parsing full Go file ==="
      case runGoParser "debug_for_simple.go" tokens of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right ast -> do
          putStrLn $ "Parse success! AST: " ++ show ast