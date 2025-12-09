#!/usr/bin/env stack
-- stack script --resolver ghc-9.6.5

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (die)

import Fluxus.Parser.Go.Parser
import Fluxus.AST.Go

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- TIO.readFile filename
      case parseGo filename content of
        Left err -> die $ "Parse error: " ++ show err
        Right ast -> print ast
    _ -> die "Usage: debug-go-parser <filename>"