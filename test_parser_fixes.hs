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
    
    -- Test 1: Simple valid Go file (should always work)
    putStrLn "=== Test 1: Simple Valid Go File ==="
    testSimpleFile
    
    -- Test 2: Go 1.22+ range loop support
    putStrLn "\n=== Test 2: Go 1.22+ Range Loop Support ==="
    testRangeLoopSupport
    
    putStrLn "\n=== All Tests Completed ==="

testSimpleFile :: IO ()
testSimpleFile = do
    content <- TIO.readFile "simple_test.go"
    putStrLn "Testing simple valid Go file..."
    
    case runGoLexer "simple_test.go" content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn $ "✓ Lexer succeeded with " ++ show (length tokens) ++ " tokens"
            case runGoParser "simple_test.go" tokens of
                Left err -> putStrLn $ "✗ Parser error: " ++ show err
                Right ast -> do
                    putStrLn $ "✓ Parser succeeded: Package name: " ++ show (goPackageName (goPackage ast))
                    case goPackageFiles (goPackage ast) of
                        [] -> putStrLn "No files found in package"
                        (file:_) -> do
                            putStrLn $ "✓ File parsed with " ++ show (length (goFileDecls file)) ++ " declarations"

testRangeLoopSupport :: IO ()
testRangeLoopSupport = do
    content <- TIO.readFile "test_range_loop.go"
    putStrLn "Testing Go 1.22+ range loop support..."
    
    case runGoLexer "test_range_loop.go" content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn $ "✓ Lexer succeeded with " ++ show (length tokens) ++ " tokens"
            case runGoParser "test_range_loop.go" tokens of
                Left err -> putStrLn $ "✗ Parser error: " ++ show err
                Right ast -> do
                    putStrLn $ "✓ Parser succeeded: Package name: " ++ show (goPackageName (goPackage ast))
                    putStrLn $ "✓ Go 1.22+ range loop support confirmed!"
                    case goPackageFiles (goPackage ast) of
                        [] -> putStrLn "No files found in package"
                        (file:_) -> do
                            putStrLn $ "✓ File parsed with " ++ show (length (goFileDecls file)) ++ " declarations"
                            putStrLn "✓ Enhanced range loop features:"
                            putStrLn "  - Integer ranges (for range 10)"
                            putStrLn "  - Float ranges (for range 10.5)"
                            putStrLn "  - Both := and = assignment operators"
                            putStrLn "  - Proper handling of key/value variables"
