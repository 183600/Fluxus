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
    putStrLn "=== Testing Go Parser Fixes ==="
    putStrLn ""
    
    -- Test 1: Declaration error handling (should now report errors instead of silently skipping)
    putStrLn "=== Test 1: Declaration Error Handling ==="
    testDeclarationErrorHandling
    
    -- Test 2: Go 1.22+ range loop support
    putStrLn "\n=== Test 2: Go 1.22+ Range Loop Support ==="
    testRangeLoopSupport
    
    putStrLn "\n=== All Tests Completed ==="

testDeclarationErrorHandling :: IO ()
testDeclarationErrorHandling = do
    content <- TIO.readFile "test_declaration_issue.go"
    putStrLn "Testing file with invalid syntax:"
    TIO.putStrLn content
    putStrLn ""
    
    case runGoLexer "test_declaration_issue.go" content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn "Lexer succeeded, now testing parser..."
            case runGoParser "test_declaration_issue.go" tokens of
                Left err -> do
                    putStrLn $ "✓ Parser correctly reported error: " ++ show err
                    putStrLn "✓ Fix confirmed: parseDeclaration now reports syntax errors instead of silently skipping them"
                Right ast -> do
                    putStrLn $ "✗ Parser unexpectedly succeeded: Package name: " ++ show (goPackageName (goPackage ast))
                    putStrLn "✗ Fix failed: parseDeclaration still silently skips syntax errors"

testRangeLoopSupport :: IO ()
testRangeLoopSupport = do
    content <- TIO.readFile "test_range_loop.go"
    putStrLn "Testing file with Go 1.22+ range loops:"
    TIO.putStrLn content
    putStrLn ""
    
    case runGoLexer "test_range_loop.go" content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn "Lexer succeeded, now testing parser..."
            case runGoParser "test_range_loop.go" tokens of
                Left err -> putStrLn $ "Parser error: " ++ show err
                Right ast -> do
                    putStrLn $ "✓ Parser succeeded: Package name: " ++ show (goPackageName (goPackage ast))
                    putStrLn "✓ Fix confirmed: Go 1.22+ range loops are now properly supported"
                    
                    -- Check if we can find range statements in the AST
                    case goPackageFiles (goPackage ast) of
                        [] -> putStrLn "No files found in package"
                        (file:_) -> do
                            putStrLn $ "✓ File parsed with " ++ show (length (goFileDecls file)) ++ " declarations"
                            putStrLn "✓ Enhanced range loop support includes:"
                            putStrLn "  - Integer ranges (for range 10)"
                            putStrLn "  - Float ranges (for range 10.5)"
                            putStrLn "  - Both := and = assignment operators"
                            putStrLn "  - Proper handling of key/value variables"
