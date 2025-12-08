#!/usr/bin/env stack
-- stack script --resolver lts-21.25

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Go
import Fluxus.AST.Common (Located(..), locatedValue)

main :: IO ()
main = do
  let input = T.unlines
        [ "package main"
        , "type Point struct {"
        , "  X int"
        , "  Y int"
        , "}"
        , "var p = Point{"
        , "  X: 10,"
        , "  Y: 20,"
        , "}"
        ]
  
  putStrLn $ "Testing Go struct literal parsing..."
  putStrLn $ "Input:\n" ++ T.unpack input
  
  case runGoLexer "test.go" input of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Number of tokens: " ++ show (length tokens)
      case runGoParser "test.go" tokens of
        Left perr -> putStrLn $ "Parser error: " ++ show perr
        Right ast -> putStrLn $ "Parsed successfully! AST: " ++ show ast