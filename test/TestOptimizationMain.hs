-- | Main module for testing optimization stage
module Main where

import TestOptimizationStage (testOptimizationStage)

main :: IO ()
main = do
  putStrLn "Running optimization stage tests..."
  testOptimizationStage
  putStrLn "Tests completed!"