#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- TIO.readFile filename
      case runGoLexer (T.pack filename) content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
          putStrLn $ "Tokens: " ++ show (take 20 tokens)
          case runGoParser (T.pack filename) tokens of
            Left err -> putStrLn $ "Parse error: " ++ show err
            Right ast -> putStrLn $ "AST: " ++ show ast
    _ -> putStrLn "Usage: debug_go_ast.hs <filename>"