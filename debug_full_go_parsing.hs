{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Go

main :: IO ()
main = do
    content <- TIO.readFile "test_go_with_functions.go"
    putStrLn "=== Debug Full Go Parsing ==="
    putStrLn "Content:"
    TIO.putStrLn content
    putStrLn ""
    
    case runGoLexer "test.go" content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn $ "=== Tokenization successful: " ++ show (length tokens) ++ " tokens ==="
            putStrLn ""
            
            case runGoParser "test.go" tokens of
                Left err -> do
                    putStrLn $ "Parser error: " ++ show err
                    putStrLn ""
                    putStrLn "=== First 10 tokens ==="
                    mapM_ print (take 10 tokens)
                Right (GoAST package) -> do
                    putStrLn "=== Parse successful! ==="
                    putStrLn $ "Package: " ++ show (goPackageName package)
                    let files = goPackageFiles package
                    putStrLn $ "Files: " ++ show (length files)
                    mapM_ printFile files
  where
    printFile file = do
        putStrLn $ "File: " ++ show (goFileName file)
        putStrLn $ "Package: " ++ show (goFilePackage file)
        putStrLn $ "Imports: " ++ show (length $ goFileImports file)
        putStrLn $ "Declarations: " ++ show (length $ goFileDecls file)
        mapM_ printDecl (goFileDecls file)
    
    printDecl decl = putStrLn $ "  Decl: " ++ show decl