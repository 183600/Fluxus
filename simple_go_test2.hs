{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Common
import Fluxus.AST.Go

main :: IO ()
main = do
    content <- TIO.readFile "simple_test.go"
    putStrLn "=== Testing Go Parser Declaration Issue ==="
    
    case runGoLexer "simple_test.go" content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn $ "Total tokens: " ++ show (length tokens)
            
            -- Show tokens around the function
            let funcIndex = findFuncIndex tokens
            case funcIndex of
                Nothing -> putStrLn "No func found!"
                Just idx -> do
                    putStrLn $ "Func at index: " ++ show idx
                    putStrLn "Tokens around func:"
                    let start = max 0 (idx - 2)
                    let end = min (length tokens) (idx + 10)
                    mapM_ (\(i, token) -> putStrLn $ show i ++ ": " ++ show token) 
                           (zip [start..] (take (end - start) (drop start tokens)))
            
            -- Try the full parse
            case runGoParser "simple_test.go" tokens of
                Left err -> putStrLn $ "Parse error: " ++ show err
                Right ast -> do
                    putStrLn $ "Parse successful!"
                    putStrLn $ "Package: " ++ show (goPackageName (goPackage ast))
                    case goPackageFiles (goPackage ast) of
                        [] -> putStrLn "No files!"
                        (file:_) -> do
                            putStrLn $ "File: " ++ T.unpack (goFileName file)
                            putStrLn $ "Declarations: " ++ show (length (goFileDecls file))
                            putStrLn $ "Imports: " ++ show (length (goFileImports file))
                            
                            -- Show what the declarations actually are
                            mapM_ (\(Located _ decl) -> putStrLn $ "  Declaration: " ++ showDeclType decl) 
                                   (goFileDecls file)
  where
    findFuncIndex [] = Nothing
    findFuncIndex (Located _ (GoTokenKeyword GoKwFunc) : _) = Just 0
    findFuncIndex (_:xs) = fmap (+1) (findFuncIndex xs)
    
    showDeclType (GoFuncDecl _) = "Function"
    showDeclType (GoMethodDecl _ _) = "Method"
    showDeclType (GoTypeDecl _ _) = "Type"
    showDeclType (GoVarDecl _) = "Variable"
    showDeclType (GoConstDecl _) = "Constant"
    showDeclType (GoImportDecl _) = "Import"
    showDeclType x = "Other: " ++ show x