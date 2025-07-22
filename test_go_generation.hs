{-# LANGUAGE OverloadedStrings #-}
module Main where

import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import Fluxus.CodeGen.Go
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Test cases for verifying the fixes
testCases :: [(String, T.Text)]
testCases =
  [ ("Function with parameters", "def add(a, b):\n    return a + b")
  , ("For loop", "for i in range(10):\n    print(i)")
  , ("Variable declarations", "x = 42\ny = x * 2")
  , ("Complex expressions", "result = (x + y) * z / 2")
  , ("fmt.Printf support", "print(\"Hello, World!\")")
  , ("Fibonacci function", T.unlines
      [ "def fibonacci(n):"
      , "    if n <= 1:"
      , "        return n"
      , "    return fibonacci(n-1) + fibonacci(n-2)"
      , ""
      , "for i in range(10):"
      , "    print(f\"fib({i}) = {fibonacci(i)}\")"
      ])
  ]

testFile :: String -> T.Text -> IO ()
testFile name content = do
  putStrLn $ "\n=== Testing " ++ name ++ " ==="
  putStrLn "Python code:"
  TIO.putStrLn content
  
  putStrLn "=== Lexer Test ==="
  case runPythonLexer (T.pack name) content of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Successfully tokenized into " ++ show (length tokens) ++ " tokens"
      
      putStrLn "=== Parser Test ==="
      case runPythonParser (T.pack name) tokens of
        Left err -> putStrLn $ "Parser error: " ++ show err
        Right ast -> do
          putStrLn "Parser succeeded!"
          
          putStrLn "=== Go Code Generation Test ==="
          let config = defaultGoConfig "main"
              goUnit = generateGoFromPython ast config
              generated = generateGo goUnit
          
          putStrLn "Generated Go code:"
          TIO.putStrLn generated
          
          -- Write to file for verification
          let filename = "generated_" ++ map (\c -> if c == ' ' then '_' else c) (map toLower (take 10 name)) ++ ".go"
          TIO.writeFile filename generated
          putStrLn $ "Written to: " ++ filename

main :: IO ()
main = do
  putStrLn "=== Testing Python to Go Code Generation Fixes ==="
  mapM_ (\(name, content) -> testFile name content) testCases
  
  putStrLn "\n=== All tests completed! ==="
  where
    toLower c | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
              | otherwise = c