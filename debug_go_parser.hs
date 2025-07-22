#!/usr/bin/env runhaskell
{- cabal:
build-depends: base, text, fluxus
ghc-options: -Wall -Werror
-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Go

-- | Debug the Go parser with a specific file
debugGoParser :: FilePath -> IO ()
debugGoParser filename = do
    putStrLn $ "=== Debugging Go Parser for: " ++ filename ++ " ==="
    
    -- Read the file
    content <- TIO.readFile filename
    putStrLn "=== Input Go Code ==="
    TIO.putStrLn content
    putStrLn ""
    
    -- Step 1: Test lexer
    putStrLn "=== Lexer Phase ==="
    case runGoLexer filename content of
        Left lexErr -> do
            putStrLn $ "❌ Lexer failed: " ++ show lexErr
            return ()
        Right tokens -> do
            putStrLn $ "✅ Lexer succeeded, produced " ++ show (length tokens) ++ " tokens"
            putStrLn "First 20 tokens:"
            mapM_ (putStrLn . ("  " ++) . show) (take 20 tokens)
            if length tokens > 20
                then putStrLn "  ... (truncated)"
                else return ()
            putStrLn ""
            
            -- Step 2: Test parser
            putStrLn "=== Parser Phase ==="
            case runGoParser filename tokens of
                Left parseErr -> do
                    putStrLn $ "❌ Parser failed: " ++ show parseErr
                Right ast -> do
                    putStrLn "✅ Parser succeeded!"
                    putStrLn "=== AST Structure ==="
                    putStrLn $ show ast
                    putStrLn ""
                    
                    -- Pretty print key information
                    putStrLn "=== AST Summary ==="
                    let GoAST package_ = ast
                    putStrLn $ "Package name: " ++ show (goPackageName package_)
                    let files = goPackageFiles package_
                    putStrLn $ "Number of files: " ++ show (length files)
                    
                    -- Analyze first file
                    case files of
                        [] -> putStrLn "No files in package"
                        (file:_) -> do
                            putStrLn $ "File name: " ++ show (goFileName file)
                            let imports = goFileImports file
                            let decls = goFileDecls file
                            putStrLn $ "Number of imports: " ++ show (length imports)
                            putStrLn $ "Number of declarations: " ++ show (length decls)
                            
                            -- Show declarations
                            putStrLn "Declarations:"
                            mapM_ printDecl (zip [1..] decls)

printDecl :: (Int, Located GoDecl) -> IO ()
printDecl (i, Located _ decl) = do
    putStrLn $ "  " ++ show i ++ ". " ++ declType decl
  where
    declType :: GoDecl -> String
    declType (GoImportDecl _) = "Import declaration"
    declType (GoConstDecl _) = "Constant declaration"
    declType (GoTypeDecl name _) = "Type declaration: " ++ show name
    declType (GoVarDecl _) = "Variable declaration"
    declType (GoFuncDecl func) = case goFuncName func of
        Just name -> "Function declaration: " ++ show name
        Nothing -> "Anonymous function declaration"
    declType (GoMethodDecl _ func) = case goFuncName func of
        Just name -> "Method declaration: " ++ show name
        Nothing -> "Anonymous method declaration"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Testing with simple fibonacci example..."
            debugGoParser "examples/go/fibonacci.go"
        [filename] -> debugGoParser filename
        _ -> putStrLn "Usage: runhaskell debug_go_parser.hs [filename]"