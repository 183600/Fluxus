{-# LANGUAGE OverloadedStrings #-}

import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Text.Megaparsec

main :: IO ()
main = do
  let content = "func test()"
  
  putStrLn "=== Simple Function Test Without Body ==="
  TIO.putStrLn content
  putStrLn ""
  
  putStrLn "=== Tokenizing ==="
  case runGoLexer "<test>" content of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show (length tokens) ++ " tokens found"
      mapM_ print tokens
      putStrLn ""
      
      putStrLn "=== Parsing just function declaration ==="
      case parse parseFuncDecl "<test>" tokens of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right decl -> do
          putStrLn $ "Parse success! Function: " ++ show decl