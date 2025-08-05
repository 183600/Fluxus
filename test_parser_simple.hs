{-# LANGUAGE OverloadedStrings #-}

import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import qualified Data.Text as T
import Text.Megaparsec

main :: IO ()
main = do
  let content = T.pack "package main"
  
  putStrLn "=== Simple Parser Test ==="
  putStrLn $ "Input: " ++ T.unpack content
  
  case runGoLexer "test.go" content of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show tokens
      putStrLn ""
      
      putStrLn "=== Testing Parser ==="
      case runGoParser "test.go" tokens of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right ast -> putStrLn $ "Parse success! AST: " ++ show ast