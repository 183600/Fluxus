{-# LANGUAGE OverloadedStrings #-}
import Fluxus.Parser.Go.Parser
import Fluxus.Parser.Go.Lexer
import Fluxus.AST.Go
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
  let goCode = T.unlines
        [ "package main"
        , ""
        , "type Person struct {"
        , "    Name string"
        , "    Age int"
        , "}"
        ]
  
  putStrLn "Testing Go struct parsing..."
  putStrLn "Code:"
  putStrLn (T.unpack goCode)
  
  case runGoLexer "test.go" goCode of
    Left err -> do
      putStrLn "Lexer failed:"
      print err
    Right tokens -> do
      putStrLn "Lexer tokens:"
      print tokens
      
      case runGoParser "test.go" tokens of
        Left err -> do
          putStrLn "Parser failed:"
          print err
        Right ast -> do
          putStrLn "Parser AST:"
          print ast