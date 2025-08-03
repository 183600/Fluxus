{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Go

main :: IO ()
main = do
    let input = "package main\n\nimport \"fmt\"\n\nfunc main() {\n    fmt.Println(\"Hello from fluxus!\")\n}"
    let filename = "test.go"
    putStrLn "Testing Go parser with imports..."
    putStrLn $ "Input: " ++ show input
    
    -- First test lexer
    case runGoLexer filename (T.pack input) of
        Left err -> do
            putStrLn "Lexer error:"
            print err
        Right tokens -> do
            putStrLn "Tokens produced:"
            mapM_ print tokens
            
            -- Then test parser
            putStrLn "\nTesting parser..."
            case runGoParser filename tokens of
                Left err -> do
                    putStrLn "Parser error:"
                    print err
                Right ast -> do
                    putStrLn "Parse successful!"
                    print ast