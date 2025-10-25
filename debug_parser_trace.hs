#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Common
import Fluxus.AST.Go
import Text.Megaparsec
import Control.Monad (void)

main :: IO ()
main = do
  let content = "package main\n\nfunc main() {\n    return\n}"
  case runGoLexer "test.go" content of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Total tokens: " ++ show (length tokens)
      let afterPackage = dropWhile (not . isFunc) tokens
      putStrLn $ "Tokens after finding func: " ++ show (take 10 afterPackage)
      case parse parseFuncDecl "test" afterPackage of
        Left err -> putStrLn $ "Function parse error: " ++ show err
        Right decl -> putStrLn $ "Parsed function: " ++ show decl
  where
    isFunc (Located _ (GoTokenKeyword GoKwFunc)) = True
    isFunc _ = False