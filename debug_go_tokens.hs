#!/usr/bin/env stack
-- stack script --resolver ghc-9.6.5

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

import Fluxus.Parser.Go.Lexer
import Fluxus.AST.Go
import Fluxus.AST.Common (Located(..))

-- Simple token inspection
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- TIO.readFile filename
      let filenameText = T.pack filename
      
      case runGoLexer filenameText content of
        Left lexErr -> putStrLn $ "Lexer error: " ++ show lexErr
        Right tokens -> do
          putStrLn $ "Total tokens: " ++ show (length tokens)
          putStrLn "\nLast 10 tokens:"
          mapM_ (putStrLn . ("  " ++) . show) (drop (length tokens - 10) tokens)
          
          putStrLn "\nLooking for 'func' keyword:"
          let funcTokens = filter (\t -> locValue t == GoTokenKeyword GoKwFunc) tokens
          putStrLn $ "Found " ++ show (length funcTokens) ++ " func tokens"
          mapM_ (putStrLn . ("  " ++) . show) funcTokens
    _ -> putStrLn "Usage: debug-go-tokens <filename>"