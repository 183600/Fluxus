#!/usr/bin/env runghc

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.IO

-- Import Fluxus modules
import Fluxus.Parser.Go.Lexer (runGoLexer, lexGo)
import Fluxus.Parser.Go.Parser (runGoParser, parseGo)
import Fluxus.AST.Go (GoAST(..), GoPackage(..), GoFile(..), GoDecl(..))
import Fluxus.AST.Common (Located(..))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            content <- TIO.readFile filename
            putStrLn $ "=== Debug Go Parser for: " ++ filename
            putStrLn $ "Content:\n" ++ T.unpack content
            putStrLn "\n=== Tokenization ==="
            
            -- Test lexer
            let tokens = runGoLexer filename content
            putStrLn $ "Tokens found: " ++ show (length tokens)
            mapM_ printToken tokens
            
            putStrLn "\n=== Parsing ==="
            
            -- Test parser
            let result = runGoParser filename tokens
            case result of
                Left err -> putStrLn $ "Parse error: " ++ show err
                Right ast -> do
                    putStrLn $ "Parse successful!"
                    putStrLn $ "AST: " ++ show ast
                    let goPackage = case ast of
                                        GoAST pkg -> pkg
                    putStrLn $ "Package name: " ++ show (goPackageName goPackage)
                    let files = goPackageFiles goPackage
                    putStrLn $ "Files in package: " ++ show (length files)
                    mapM_ printFileDetails files
        _ -> putStrLn "Usage: debug_go_parser.hs <filename>"

printToken :: Located Fluxus.Parser.Go.Lexer.GoToken -> IO ()
printToken (Located span token) = 
    putStrLn $ "Token at " ++ show span ++ ": " ++ show token

printFileDetails :: GoFile -> IO ()
printFileDetails file = do
    putStrLn $ "  File: " ++ goFileName file
    putStrLn $ "    Package: " ++ show (goFilePackage file)
    putStrLn $ "    Imports: " ++ show (length (goFileImports file))
    putStrLn $ "    Declarations: " ++ show (length (goFileDecls file))
    mapM_ printDeclaration (goFileDecls file)

printDeclaration :: Located GoDecl -> IO ()
printDeclaration (Located _ decl) = 
    putStrLn $ "    Declaration: " ++ show decl