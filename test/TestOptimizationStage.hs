{-# LANGUAGE OverloadedStrings #-}

-- | Test module for optimization stage
module TestOptimizationStage where

import Fluxus.Compiler.Driver
import Fluxus.AST.Python (PythonAST(..), PythonModule(..))

-- | Test optimization stage with different levels
testOptimizationStage :: IO ()
testOptimizationStage = do
  putStrLn "Testing optimization stage..."
  
  -- Create a simple test AST (in practice this would come from parsing)
  let testAst = Left $ PythonAST 
        { pyModule = PythonModule
            { pyModuleName = Nothing
            , pyModuleDoc = Nothing
            , pyModuleImports = []
            , pyModuleBody = []
            }
        }
  
  -- Test with O0 (no optimization)
  putStrLn "Testing O0 optimization level:"
  resultO0 <- runCompiler (defaultConfig { ccOptimizationLevel = O0 }) (optimizationStage testAst)
  case resultO0 of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (_, state) -> do
      putStrLn $ "Warnings: " ++ show (csWarnings state)
      putStrLn "O0 test completed successfully"
  
  -- Test with O1 (basic optimization)
  putStrLn "\nTesting O1 optimization level:"
  resultO1 <- runCompiler (defaultConfig { ccOptimizationLevel = O1 }) (optimizationStage testAst)
  case resultO1 of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (_, state) -> do
      putStrLn $ "Warnings: " ++ show (csWarnings state)
      putStrLn "O1 test completed successfully"
  
  -- Test with O2 (standard optimization)
  putStrLn "\nTesting O2 optimization level:"
  resultO2 <- runCompiler (defaultConfig { ccOptimizationLevel = O2 }) (optimizationStage testAst)
  case resultO2 of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (_, state) -> do
      putStrLn $ "Warnings: " ++ show (csWarnings state)
      putStrLn "O2 test completed successfully"
  
  -- Test with O3 (aggressive optimization)
  putStrLn "\nTesting O3 optimization level:"
  resultO3 <- runCompiler (defaultConfig { ccOptimizationLevel = O3 }) (optimizationStage testAst)
  case resultO3 of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (_, state) -> do
      putStrLn $ "Warnings: " ++ show (csWarnings state)
      putStrLn "O3 test completed successfully"
  
  -- Test with Os (size optimization)
  putStrLn "\nTesting Os optimization level:"
  resultOs <- runCompiler (defaultConfig { ccOptimizationLevel = Os }) (optimizationStage testAst)
  case resultOs of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (_, state) -> do
      putStrLn $ "Warnings: " ++ show (csWarnings state)
      putStrLn "Os test completed successfully"
  
  putStrLn "\nAll optimization stage tests completed!"