#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import System.Process
import System.IO
import Text.Printf
import Control.Exception (try, SomeException)

main :: IO ()
main = do
  putStrLn "Running debug test..."
  
  -- Try to run a simple test
  result <- try $ readCreateProcessWithExitCode
    (shell "cd /home/qwe12345678/github/Fluxus && stack test --fast --test-arguments \"-m 'parses simple expressions'\"") ""
    
  case result of
    Left (e :: SomeException) -> printf "Exception: %s\n" (show e)
    Right (exitCode, stdout, stderr) -> do
      printf "Exit code: %s\n" (show exitCode)
      printf "STDOUT:\n%s\n" stdout
      printf "STDERR:\n%s\n" stderr
      
  -- Try to run parser tests
  putStrLn "\n\nRunning parser tests..."
  result2 <- try $ readCreateProcessWithExitCode
    (shell "cd /home/qwe12345678/github/Fluxus && stack test --fast --test-arguments \"-m Parser\" 2>&1") ""
    
  case result2 of
    Left (e :: SomeException) -> printf "Exception: %s\n" (show e)
    Right (exitCode, stdout, stderr) -> do
      printf "Exit code: %s\n" (show exitCode)
      printf "STDOUT:\n%s\n" stdout
      printf "STDERR:\n%s\n" stderr