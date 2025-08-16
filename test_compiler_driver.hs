{-# LANGUAGE OverloadedStrings #-}
import Fluxus.Compiler.Driver
import Fluxus.AST.Common
import Fluxus.AST.Python
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  putStrLn "Testing type inference in compiler driver..."
  
  -- Create a simple Python program
  let content = "def add(x, y):\n    return x + y\n\nresult = add(1, 2)"
  
  -- Write to a temporary file
  TIO.writeFile "test_program.py" (T.pack content)
  
  -- Create compiler config
  let config = defaultConfig { 
        ccSourceLanguage = Python,
        ccEnableAnalysis = True,
        ccVerboseLevel = 2
      }
  
  -- Run the compiler
  result <- runCompiler config $ compileFile "test_program.py"
  
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (outputFile, state) -> do
      putStrLn $ "Compilation successful: " ++ outputFile
      putStrLn $ "Warnings: " ++ show (csWarnings state)
      putStrLn $ "Errors: " ++ show (csErrors state)
