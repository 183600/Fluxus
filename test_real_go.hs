#!/usr/bin/env stack
-- stack --resolver lts-20.3 script --package text --package containers

import System.Process
import System.Exit
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    putStrLn "=== Testing Go File Processing ==="
    
    -- Test if we can process the Go file
    putStrLn "File: simple_test.go"
    content <- TIO.readFile "simple_test.go"
    TIO.putStrLn content
    
    -- Test compilation
    putStrLn "=== Testing Compilation ==="
    exitCode <- system "cabal run fluxus -- --go simple_test.go --output simple_test_compiled.cpp"
    
    case exitCode of
        ExitSuccess -> do
            putStrLn "=== Compilation Successful ==="
            
            -- Check generated file
            cppContent <- readFile "simple_test_compiled.cpp"
            putStrLn "=== Generated C++ ==="
            putStrLn cppContent
            
            -- Test C++ compilation
            putStrLn "=== Testing C++ Compilation ==="
            cppExitCode <- system "g++ -std=c++20 -o simple_test_executable simple_test_compiled.cpp"
            case cppExitCode of
                ExitSuccess -> putStrLn "C++ compilation successful!"
                ExitFailure _ -> putStrLn "C++ compilation failed"
                
        ExitFailure code -> putStrLn $ "Go compilation failed with exit code: " ++ show code