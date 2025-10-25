{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Common
import Fluxus.AST.Go
import Control.Monad (when)

main :: IO ()
main = do
    content <- TIO.readFile "simple_test.go"
    putStrLn "=== Original Go Content ==="
    TIO.putStrLn content
    putStrLn ""
    
    putStrLn "=== Lexer Output ==="
    case runGoLexer "simple_test.go" content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn $ "Token count: " ++ show (length tokens)
            mapM_ printToken (take 10 tokens)  -- Show first 10 tokens
            when (length tokens > 10) $ putStrLn "..."
            putStrLn ""
            
            putStrLn "=== Parser Output ==="
            case runGoParser "simple_test.go" tokens of
                Left err -> putStrLn $ "Parser error: " ++ show err
                Right ast -> do
                    putStrLn $ "Package name: " ++ show (goPackageName (goPackage ast))
                    putStrLn $ "File count: " ++ show (length (goPackageFiles (goPackage ast)))
                    case goPackageFiles (goPackage ast) of
                        [] -> putStrLn "No files found in package"
                        (file:_) -> do
                            putStrLn $ "First file name: " ++ T.unpack (goFileName file)
                            putStrLn $ "Declaration count: " ++ show (length (goFileDecls file))
                            mapM_ printDecl (take 3 (goFileDecls file))  -- Show first 3 declarations
  where
    printToken token = putStrLn $ "  " ++ show token
    
    printDecl (Located _ decl) = putStrLn $ "  Declaration: " ++ showDeclType decl
    
    showDeclType (GoFuncDecl _) = "Function"
    showDeclType (GoImportDecl _) = "Import"
    showDeclType (GoTypeDecl _ _) = "Type"
    showDeclType (GoVarDecl _) = "Variable"
    showDeclType _ = "Other"