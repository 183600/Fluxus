{-# LANGUAGE OverloadedStrings #-}
import Fluxus.Parser.Go.Parser
import Fluxus.Parser.Go.Lexer
import Fluxus.AST.Go
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
  -- 测试实际的Go代码，包含分号
  let goCodeWithSemicolons = T.unlines
        [ "package main"
        , ""
        , "type Person struct {"
        , "    Name string;"
        , "    Age int;"
        , "}"
        ]
  
  putStrLn "=== Testing Go struct with semicolons ==="
  putStrLn "Code:"
  putStrLn (T.unpack goCodeWithSemicolons)
  
  case runGoLexer "test.go" goCodeWithSemicolons of
    Left err -> putStrLn $ "Lexer failed: " ++ show err
    Right tokens -> do
      putStrLn "Lexer tokens:"
      mapM_ (putStrLn . show . locValue) tokens
      
      case runGoParser "test.go" tokens of
        Left err -> putStrLn $ "Parser failed: " ++ show err
        Right ast -> do
          putStrLn "Parser succeeded!"
          print ast
  
  -- 测试没有分号的Go代码（更常见的写法）
  let goCodeWithoutSemicolons = T.unlines
        [ "package main"
        , ""
        , "type Person struct {"
        , "    Name string"
        , "    Age int"
        , "}"
        ]
  
  putStrLn "\n=== Testing Go struct without semicolons ==="
  putStrLn "Code:"
  putStrLn (T.unpack goCodeWithoutSemicolons)
  
  case runGoLexer "test.go" goCodeWithoutSemicolons of
    Left err -> putStrLn $ "Lexer failed: " ++ show err
    Right tokens -> do
      putStrLn "Lexer tokens:"
      mapM_ (putStrLn . show . locValue) tokens
      
      case runGoParser "test.go" tokens of
        Left err -> putStrLn $ "Parser failed: " ++ show err
        Right ast -> do
          putStrLn "Parser succeeded!"
          print ast