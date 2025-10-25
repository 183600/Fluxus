#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Environment (getArgs)

import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Go

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            content <- TIO.readFile filename
            putStrLn "=== Lexing ==="
            case lexGo filename content of
                Left err -> putStrLn $ "Lexer error: " ++ show err
                Right tokens -> do
                    putStrLn $ "Tokens: " ++ show (take 20 tokens)
                    putStrLn "\n=== Parsing ==="
                    case runGoParser filename tokens of
                        Left parseErr -> putStrLn $ "Parser error: " ++ show parseErr
                        Right ast -> do
                            putStrLn "Parsed AST:"
                            print ast
        _ -> putStrLn "Usage: debug_ast <go_file>"