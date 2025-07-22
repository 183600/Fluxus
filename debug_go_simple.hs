#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import System.Environment
import Text.Megaparsec (parseTest)

main :: IO ()
main = do
  let content = "package main\n\nfunc main() {\n    return\n}"
  case runGoLexer "test.go" content of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show tokens
      putStrLn "\nTrying to parse declarations manually:"
      parseTest parseDeclaration tokens