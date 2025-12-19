#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

-- | Debug utility for Python lexer
module Main (main) where

import Prelude

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Fluxus.Parser.Python.Lexer (runPythonLexer, PythonToken(..))
import Fluxus.Utils.Debug (debugLog, setDebugLevel, DebugLevel(Debug))
import Fluxus.AST.Common (Located(..))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: debug_python_lexer <filename>"
      putStrLn "  Debug Python lexer on the specified file"
    [filename] -> do
      content <- TIO.readFile filename
      putStrLn $ "Debugging Python lexer for: " ++ filename
      putStrLn $ "Content length: " ++ show (T.length content) ++ " characters"
      putStrLn ""
      
      setDebugLevel Debug
      debugLog Debug $ "Starting lexer debug for file: " <> T.pack filename
      
      case runPythonLexer (T.pack filename) content of
        Left err -> do
          putStrLn "Lexer error:"
          print err
        Right tokens -> do
          putStrLn $ "Successfully lexed " ++ show (length tokens) ++ " tokens"
          putStrLn ""
          mapM_ printToken tokens
          
          debugLog Debug $ "Lexer debug completed for file: " <> T.pack filename
    _ -> do
      putStrLn "Usage: debug_python_lexer <filename>"
      putStrLn "  Debug Python lexer on the specified file"

printToken :: Located PythonToken -> IO ()
printToken (Located tokenSpan token) = do
  putStr $ "Token at " ++ show tokenSpan ++ ": "
  case token of
    TokenKeyword kw -> putStrLn $ "Keyword " ++ show kw
    TokenIdent ident -> putStrLn $ "Identifier " ++ T.unpack ident
    TokenString str -> putStrLn $ "String \"" ++ T.unpack str ++ "\""
    TokenFString segments -> putStrLn $ "F-String with " ++ show (length segments) ++ " segments"
    TokenNumber num isFloat -> putStrLn $ "Number " ++ T.unpack num ++ " (float=" ++ show isFloat ++ ")"
    TokenBytes bytes -> putStrLn $ "Bytes b\"" ++ T.unpack bytes ++ "\""
    TokenOperator op -> putStrLn $ "Operator " ++ show op
    TokenDelimiter delim -> putStrLn $ "Delimiter " ++ show delim
    TokenNewline -> putStrLn "Newline"
    TokenIndent level -> putStrLn $ "Indent " ++ show level
    TokenDedent level -> putStrLn $ "Dedent " ++ show level
    TokenComment commentText -> putStrLn $ "Comment #" ++ T.unpack commentText
    TokenEOF -> putStrLn "EOF"
    TokenError err -> putStrLn $ "Error: " ++ T.unpack err