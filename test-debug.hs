#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

-- | General test debug utility
module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser ()
import Fluxus.Utils.Debug
import Fluxus.AST.Common (Located(..))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: test-debug <command> <args...>"
      putStrLn ""
      putStrLn "Commands:"
      putStrLn "  lexer <filename>     Debug Python lexer on the specified file"
      putStrLn "  parser <filename>    Debug Python parser on the specified file"
      putStrLn "  tokens <filename>    Show all tokens from lexer"
    ["lexer", filename] -> do
      debugLexer filename
    ["parser", filename] -> do
      debugParser filename
    ["tokens", filename] -> do
      showTokens filename
    _ -> do
      putStrLn "Invalid arguments. Use 'test-debug' for usage information."

debugLexer :: FilePath -> IO ()
debugLexer filename = do
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

debugParser :: FilePath -> IO ()
debugParser filename = do
  content <- TIO.readFile filename
  putStrLn $ "Debugging Python parser for: " ++ filename
  putStrLn $ "Content length: " ++ show (T.length content) ++ " characters"
  putStrLn ""
  
  setDebugLevel Debug
  debugLog Debug $ "Starting parser debug for file: " <> T.pack filename
  
  -- First tokenize
  case runPythonLexer (T.pack filename) content of
    Left err -> do
      putStrLn "Lexer error during parsing:"
      print err
    Right tokens -> do
      putStrLn $ "Successfully lexed " ++ show (length tokens) ++ " tokens"
      
      -- Then parse (this is a placeholder - actual parser implementation would go here)
      putStrLn "Parsing tokens..."
      debugLog Debug $ "Parser debug completed for file: " <> T.pack filename
      putStrLn "Parser debug completed (placeholder implementation)"

showTokens :: FilePath -> IO ()
showTokens filename = do
  content <- TIO.readFile filename
  putStrLn $ "Showing tokens for: " ++ filename
  putStrLn ""
  
  case runPythonLexer (T.pack filename) content of
    Left err -> do
      putStrLn "Lexer error:"
      print err
    Right tokens -> do
      putStrLn $ "Found " ++ show (length tokens) ++ " tokens:"
      putStrLn ""
      mapM_ (\(i, token) -> do
        putStr $ show (i :: Int) ++ ": "
        printToken token
        ) (zip [1..] tokens)

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