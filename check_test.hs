#!/usr/bin/env stack
-- stack script --resolver lts-23.27

import System.Process
import System.Exit

main :: IO ()
main = do
  putStrLn "Running stack test..."
  (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["test", "--test-arguments", "--color"] ""
  putStrLn "=== STDOUT ==="
  putStrLn stdout
  putStrLn "=== STDERR ==="
  putStrLn stderr
  putStrLn $ "=== EXIT CODE: " ++ show exitCode ++ " ==="
  exitWith exitCode
