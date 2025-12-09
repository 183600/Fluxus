#!/usr/bin/env stack
-- stack script --resolver ghc-9.6.5

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Control.Monad.Logger (runStdoutLoggingT)

import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.Parser.Go.Parser.Declarations
import Fluxus.AST.Go
import Fluxus.AST.Common (Located(..))

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
          putStrLn $ "Tokens: " ++ show (length tokens)
          
          -- Try to parse declarations directly
          result <- runStdoutLoggingT $ runGoParserWithLogger (const $ pure ()) filenameText tokens
          case result of
            Left parseErr -> putStrLn $ "Parser error: " ++ show parseErr
            Right ast -> putStrLn $ "AST: " ++ show ast
    _ -> putStrLn "Usage: debug-go-decl <filename>"