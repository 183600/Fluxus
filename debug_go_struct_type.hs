{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser

main :: IO ()
main = do
  -- Test just the struct type parsing
  putStrLn "=== Test: Struct type only ==="
  let input = "struct { Name string; Age int; }"
  case runGoLexer "test.go" (T.pack input) of
    Left err -> putStrLn $ "Lexer failed: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show tokens
      -- Try to parse just the type part
      case MP.parse (evalStateT parseGoType initialParserState) "test.go" tokens of
        Left err -> putStrLn $ "Type parser failed: " ++ show err
        Right type_ -> putStrLn $ "Type: " ++ show type_