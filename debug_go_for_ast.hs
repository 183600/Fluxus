{-# LANGUAGE OverloadedStrings #-}

import Fluxus.Parser.Go.Parser
import Fluxus.Lexer.Go.Lexer
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  let source = T.unlines
        [ "package main"
        , "func main() {"
        , "    for i := 1; i <= 3; i++ {"
        , "    }"
        , "}"
        ]
  
  putStrLn "=== GO FOR LOOP DEBUG ==="
  putStrLn "Source code:"
  TIO.putStrLn source
  putStrLn ""
  
  case runGoLexer source of
    Left lexErr -> do
      putStrLn "Lexer error:"
      print lexErr
    Right tokens -> do
      putStrLn "=== TOKENS ==="
      mapM_ print (take 30 tokens)
      putStrLn ""
      
      case runGoParser "test.go" tokens of
        Left parseErr -> do
          putStrLn "Parser error:"
          print parseErr
        Right ast -> do
          putStrLn "=== AST ==="
          print ast