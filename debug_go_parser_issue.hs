#!/usr/bin/env stack
{- stack script 
   --resolver lts-20.9 
   --package base 
   --package text 
   --package megaparsec 
   --package fluxus
-}

{-# LANGUAGE OverloadedStrings #-}

import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            content <- TIO.readFile filename
            let filenameText = T.pack filename
            putStrLn $ "=== File Content ==="
            TIO.putStrLn content
            putStrLn $ "=== Lexing ==="
            case runGoLexer filenameText content of
                Left err -> putStrLn $ "Lexing error: " ++ show err
                Right tokens -> do
                    putStrLn $ "Tokens: " ++ show (length tokens) ++ " tokens"
                    mapM_ print (take 20 tokens)  -- Show first 20 tokens
                    putStrLn $ "=== Parsing ==="
                    case runGoParser filenameText tokens of
                        Left err -> putStrLn $ "Parsing error: " ++ show err
                        Right ast -> do
                            putStrLn $ "Successfully parsed!"
                            print ast
        _ -> putStrLn "Usage: debug_go_parser_issue.hs <go-file>"