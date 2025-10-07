{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser

main :: IO ()
main = do
  let input = "package main\ntype Person struct {\n    Name string;\n    Age int;\n}"
  case runGoLexer "test.go" (T.pack input) of
    Left err -> putStrLn $ "Lexer failed: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show tokens
      case runGoParser "test.go" tokens of
        Left err -> putStrLn $ "Parser failed: " ++ show err
        Right ast -> putStrLn $ "AST: " ++ show ast