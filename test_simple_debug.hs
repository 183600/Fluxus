{-# LANGUAGE OverloadedStrings #-}

import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Control.Monad.IO.Class

main :: IO ()
main = do
  let content = T.pack "package main\n\nfunc main() {\n}\n"
  
  putStrLn "=== Go File Parsing Test ==="
  TIO.putStrLn content
  putStrLn ""
  
  putStrLn "=== Tokenizing ==="
  case runGoLexer "test.go" content of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show (length tokens) ++ " tokens found"
      putStrLn ""
      
      putStrLn "=== Parsing full Go file ==="
      case runGoParser "test.go" tokens of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right ast -> do
          putStrLn $ "Parse success! AST: " ++ show ast