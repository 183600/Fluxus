#!/bin/bash
# Test script to generate C++ source from Go code

# Rebuild the project
# stack build --fast

# Create a simple test config
# Use the existing test_concurrent_fib.go file

# Run the compiler with stop-at-codegen flag
echo "Testing Go to C++ compilation..."
echo "Input file: test_concurrent_fib.go"

# Since we don't have a command-line interface yet, let's create a simple Haskell script
cat > test_codegen.hs << 'EOF'
{-# LANGUAGE OverloadedStrings #-}
import Fluxus.Compiler.Driver
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = if null args then "test_concurrent_fib.go" else head args
  
  putStrLn $ "Generating C++ from: " ++ inputFile
  
  let config = defaultConfig { 
        ccSourceLanguage = Go,
        ccStopAtCodegen = True,
        ccKeepIntermediates = True,
        ccVerboseLevel = 3
      }
  
  result <- runCompiler config $ do
    compileFile inputFile
  
  case result of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      exitWith (ExitFailure 1)
    Right (_, state) -> do
      putStrLn $ "Successfully generated C++ source"
      putStrLn $ "Intermediate files: " ++ show (csIntermediateFiles state)
EOF

echo "Created test_codegen.hs"
echo "To run: stack runghc test_codegen.hs