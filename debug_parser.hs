#!/usr/bin/env runhaskell

import System.Process
import System.Exit
import System.IO
import Control.Monad

testFile :: FilePath -> String -> IO ()
testFile filename lang = do
    putStrLn $ "Testing " ++ lang ++ " file: " ++ filename
    (exitCode, stdout, stderr) <- readProcessWithExitCode "cabal" ["run", "fluxus", "--", lang, filename] ""
    case exitCode of
        ExitSuccess -> putStrLn $ "✅ " ++ lang ++ " parser succeeded"
        ExitFailure code -> do
            putStrLn $ "❌ " ++ lang ++ " parser failed with code " ++ show code
            putStrLn $ "STDOUT: " ++ stdout
            putStrLn $ "STDERR: " ++ stderr

main :: IO ()
main = do
    putStrLn "=== Parser Debug Session ==="
    testFile "examples/python/fibonacci.py" "python"
    testFile "examples/go/fibonacci.go" "go"
    putStrLn "=== Debug Complete ==="