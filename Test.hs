{-# LANGUAGE OverloadedStrings #-}
module Main where

import Fluxus.Compiler.Driver
import System.Environment (getArgs)

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
    Right (_, state) -> do
      putStrLn $ "Successfully generated C++ source"
      putStrLn $ "Intermediate files: " ++ show (csIntermediateFiles state)
