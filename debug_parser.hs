module Main where

import System.Environment
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Fluxus.AST.Go
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- TIO.readFile filename
      case tokenizeGo filename content of
        Left err -> putStrLn $ "Tokenization error: " ++ show err
        Right tokens -> do
          putStrLn "=== TOKENS ==="
          mapM_ (putStrLn . show) tokens
          putStrLn "\n=== PARSING ==="
          case runGoParser (T.pack filename) tokens of
            Left err -> putStrLn $ "Parse error: " ++ show err
            Right ast -> do
              putStrLn "=== AST ==="
              print ast
    _ -> putStrLn "Usage: debug-parser <file.go>"
