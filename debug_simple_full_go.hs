{-# LANGUAGE OverloadedStrings #-}

import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Common (Located(..))
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let filename = case args of
        [] -> "debug_for_simple.go"
        (x:_) -> x
  
  content <- TIO.readFile filename
  let filenameText = T.pack filename
  
  putStrLn "=== Go File Parsing Test ==="
  TIO.putStrLn content
  putStrLn ""
  
  putStrLn "=== Tokenizing ==="
  case runGoLexer filenameText content of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show (length tokens) ++ " tokens found"
      putStrLn "Token details:"
      mapM_ (\token -> putStrLn $ "  " ++ show token) tokens
      putStrLn ""
      
      putStrLn "=== Parsing full Go file ==="
      case runGoParser filenameText tokens of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right ast -> do
          putStrLn $ "Parse success! AST: " ++ show ast