{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Fluxus.CodeGen.CPP
import Fluxus.AST.Common

main :: IO ()
main = do
    putStrLn "=== Testing buildStreamingExpr Function ==="
    putStrLn ""
    
    -- Test 1: Simple format string
    putStrLn "=== Test 1: Simple format string ==="
    testSimpleFormat
    
    -- Test 2: Format with single argument
    putStrLn "\n=== Test 2: Format with single argument ==="
    testSingleArgument
    
    -- Test 3: Format with multiple arguments
    putStrLn "\n=== Test 3: Format with multiple arguments ==="
    testMultipleArguments
    
    -- Test 4: Complex format with precision and mixed types
    putStrLn "\n=== Test 4: Complex format with precision and mixed types ==="
    testComplexFormat
    
    -- Test 5: Format with newlines
    putStrLn "\n=== Test 5: Format with newlines ==="
    testFormatWithNewlines
    
    -- Test 6: Edge cases that might cause issues
    putStrLn "\n=== Test 6: Edge cases ==="
    testEdgeCases
    
    putStrLn "\n=== All Tests Completed ==="

testSimpleFormat :: IO ()
testSimpleFormat = do
    let formatStr = "Hello, World!"
        args = []
        result = buildStreamingExpr formatStr args
    putStrLn $ "Format: " ++ T.unpack formatStr
    putStrLn $ "Args: " ++ show args
    putStrLn $ "Result: " ++ show result
    putStrLn ""

testSingleArgument :: IO ()
testSingleArgument = do
    let formatStr = "Value: %d"
        args = [CppLiteral (CppIntLit 42)]
        result = buildStreamingExpr formatStr args
    putStrLn $ "Format: " ++ T.unpack formatStr
    putStrLn $ "Args: " ++ show args
    putStrLn $ "Result: " ++ show result
    putStrLn ""

testMultipleArguments :: IO ()
testMultipleArguments = do
    let formatStr = "%d + %d = %d"
        args = [CppLiteral (CppIntLit 2), CppLiteral (CppIntLit 3), CppLiteral (CppIntLit 5)]
        result = buildStreamingExpr formatStr args
    putStrLn $ "Format: " ++ T.unpack formatStr
    putStrLn $ "Args: " ++ show args
    putStrLn $ "Result: " ++ show result
    putStrLn ""

testComplexFormat :: IO ()
testComplexFormat = do
    let formatStr = "Pi = %.2f, Count = %d, Name = %s"
        args = [CppLiteral (CppFloatLit 3.14159), CppLiteral (CppIntLit 100), CppLiteral (CppStringLit "Test")]
        result = buildStreamingExpr formatStr args
    putStrLn $ "Format: " ++ T.unpack formatStr
    putStrLn $ "Args: " ++ show args
    putStrLn $ "Result: " ++ show result
    putStrLn ""

testFormatWithNewlines :: IO ()
testFormatWithNewlines = do
    let formatStr = "Line 1: %d\\nLine 2: %d\\n"
        args = [CppLiteral (CppIntLit 1), CppLiteral (CppIntLit 2)]
        result = buildStreamingExpr formatStr args
    putStrLn $ "Format: " ++ T.unpack formatStr
    putStrLn $ "Args: " ++ show args
    putStrLn $ "Result: " ++ show result
    putStrLn ""

testEdgeCases :: IO ()
testEdgeCases = do
    -- Test 1: Empty format string
    let formatStr1 = ""
        args1 = []
        result1 = buildStreamingExpr formatStr1 args1
    putStrLn "Test 6.1: Empty format string"
    putStrLn $ "Format: \"" ++ T.unpack formatStr1 ++ "\""
    putStrLn $ "Result: " ++ show result1
    putStrLn ""
    
    -- Test 2: Format string with no specifiers but has arguments
    let formatStr2 = "No specifiers"
        args2 = [CppLiteral (CppIntLit 42)]
        result2 = buildStreamingExpr formatStr2 args2
    putStrLn "Test 6.2: No specifiers but has arguments"
    putStrLn $ "Format: " ++ T.unpack formatStr2
    putStrLn $ "Args: " ++ show args2
    putStrLn $ "Result: " ++ show result2
    putStrLn ""
    
    -- Test 3: More specifiers than arguments
    let formatStr3 = "%d %d %d"
        args3 = [CppLiteral (CppIntLit 1), CppLiteral (CppIntLit 2)]
        result3 = buildStreamingExpr formatStr3 args3
    putStrLn "Test 6.3: More specifiers than arguments"
    putStrLn $ "Format: " ++ T.unpack formatStr3
    putStrLn $ "Args: " ++ show args3
    putStrLn $ "Result: " ++ show result3
    putStrLn ""
    
    -- Test 4: More arguments than specifiers
    let formatStr4 = "%d"
        args4 = [CppLiteral (CppIntLit 1), CppLiteral (CppIntLit 2)]
        result4 = buildStreamingExpr formatStr4 args4
    putStrLn "Test 6.4: More arguments than specifiers"
    putStrLn $ "Format: " ++ T.unpack formatStr4
    putStrLn $ "Args: " ++ show args4
    putStrLn $ "Result: " ++ show result4
    putStrLn ""
    
    -- Test 5: Complex nested format (potential issue case)
    let formatStr5 = "Result: %.2f%% (Status: %s)"
        args5 = [CppLiteral (CppFloatLit 85.5), CppLiteral (CppStringLit "OK")]
        result5 = buildStreamingExpr formatStr5 args5
    putStrLn "Test 6.5: Complex nested format"
    putStrLn $ "Format: " ++ T.unpack formatStr5
    putStrLn $ "Args: " ++ show args5
    putStrLn $ "Result: " ++ show result5
    putStrLn ""
