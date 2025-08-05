{-# LANGUAGE OverloadedStrings #-}

import Fluxus.Parser.Go.Lexer
import qualified Data.Text as T
import Text.Megaparsec
import Data.Void

main :: IO ()
main = do
  let content = T.pack "package main"
  
  putStrLn "=== Simple Lexer Test ==="
  putStrLn $ "Input: " ++ T.unpack content
  
  case runParser lexGo "test.go" content of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Success! Tokens: " ++ show tokens