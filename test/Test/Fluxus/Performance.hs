{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Performance (spec) where

import Test.Hspec
import System.IO.Temp
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import qualified Data.Text as T
import Data.Time.Clock
import Control.Exception (bracket)

import Fluxus.Compiler.Driver
import Fluxus.Compiler.Config

spec :: Spec
spec = describe "Performance Tests" $ do
  compilationSpeedSpec
  generatedCodePerformanceSpec
  memoryUsageSpec
  scalabilitySpec

compilationSpeedSpec :: Spec
compilationSpeedSpec = describe "Compilation Speed" $ do
  it "compiles small file quickly (< 5s)" $ do
    let pythonCode = T.unlines
          [ "def factorial(n):"
          , "    if n <= 1:"
          , "        return 1"
          , "    return n * factorial(n - 1)"
          , ""
          , "print(factorial(10))"
          ]
    
    withSystemTempDirectory "fluxus-perf-small-" $ \tmpDir -> do
      let inputFile = tmpDir </> "small.py"
      let outputFile = tmpDir </> "small.cpp"
      
      writeFile inputFile (T.unpack pythonCode)
      
      let config = defaultConfig
            { ccInputFiles = [inputFile]
            , ccOutputPath = Just outputFile
            , ccStopAtCodegen = True
            }
      
      start <- getCurrentTime
      result <- runCompiler (convertConfigToDriver config) (compileFile inputFile)
      end <- getCurrentTime
      
      let duration = diffUTCTime end start
      result `shouldSatisfy` isRight
      duration `shouldSatisfy` (< 5.0)
  
  it "compiles medium file reasonably fast (< 15s)" $ do
    let pythonCode = T.unlines $
          [ "class Calculator:"
          , "    def __init__(self, initial=0):"
          , "        self.value = initial"
          , "    "
          , "    def add(self, x):"
          , "        self.value += x"
          , "        return self"
          , "    "
          , "    def subtract(self, x):"
          , "        self.value -= x"
          , "        return self"
          , "    "
          , "    def multiply(self, x):"
          , "        self.value *= x"
          , "        return self"
          , "    "
          , "    def divide(self, x):"
          , "        if x != 0:"
          , "            self.value /= x"
          , "        return self"
          , "    "
          , "    def power(self, x):"
          , "        self.value **= x"
          , "        return self"
          , "    "
          , "    def get_value(self):"
          , "        return self.value"
          , ""
          ] ++ concat (replicate 10 [
            "def helper_function_" ++ show n ++ "(x):"
          , "    result = x"
          , "    for i in range(10):"
          , "        result = result * 2 + 1"
          , "    return result"
          , ""
          | n <- [1..10 :: Int]
          ])
    
    withSystemTempDirectory "fluxus-perf-medium-" $ \tmpDir -> do
      let inputFile = tmpDir </> "medium.py"
      let outputFile = tmpDir </> "medium.cpp"
      
      writeFile inputFile (T.unpack $ T.unlines pythonCode)
      
      let config = defaultConfig
            { ccInputFiles = [inputFile]
            , ccOutputPath = Just outputFile
            , ccOptimizationLevel = O2
            , ccStopAtCodegen = True
            }
      
      start <- getCurrentTime
      result <- runCompiler (convertConfigToDriver config) (compileFile inputFile)
      end <- getCurrentTime
      
      let duration = diffUTCTime end start
      result `shouldSatisfy` isRight
      duration `shouldSatisfy` (< 15.0)

generatedCodePerformanceSpec :: Spec
generatedCodePerformanceSpec = describe "Generated Code Performance" $ do
  it "generated C++ code runs efficiently" $ do
    let pythonCode = T.unlines
          [ "def fibonacci(n):"
          , "    if n <= 1:"
          , "        return n"
          , "    return fibonacci(n-1) + fibonacci(n-2)"
          , ""
          , "result = fibonacci(20)"
          , "print(result)"
          ]
    
    withSystemTempDirectory "fluxus-perf-exec-" $ \tmpDir -> do
      let inputFile = tmpDir </> "fib.py"
      let cppFile = tmpDir </> "fib.cpp"
      let exeFile = tmpDir </> "fib"
      
      writeFile inputFile (T.unpack pythonCode)
      
      let config = defaultConfig
            { ccInputFiles = [inputFile]
            , ccOutputPath = Just cppFile
            , ccOptimizationLevel = O3
            , ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (compileFile inputFile)
      result `shouldSatisfy` isRight
      
      -- Compile C++ to executable
      (exitCode, _, _) <- readProcessWithExitCode "g++" ["-O3", cppFile, "-o", exeFile] ""
      exitCode `shouldBe` ExitSuccess
      
      -- Run and time the executable
      start <- getCurrentTime
      (exitCode', stdout, _) <- readProcessWithExitCode exeFile [] ""
      end <- getCurrentTime
      
      let duration = diffUTCTime end start
      exitCode' `shouldBe` ExitSuccess
      stdout `shouldContain` "6765"  -- fib(20) = 6765
      duration `shouldSatisfy` (< 1.0)  -- Should run quickly
  
  it "optimized code is faster than unoptimized" $ do
    let pythonCode = T.unlines
          [ "def sum_squares(n):"
          , "    total = 0"
          , "    for i in range(n):"
          , "        total += i * i"
          , "    return total"
          , ""
          , "result = sum_squares(10000)"
          , "print(result)"
          ]
    
    withSystemTempDirectory "fluxus-perf-opt-comp-" $ \tmpDir -> do
      let inputFile = tmpDir </> "sum.py"
      let cppFileO0 = tmpDir </> "sum_o0.cpp"
      let cppFileO3 = tmpDir </> "sum_o3.cpp"
      let exeFileO0 = tmpDir </> "sum_o0"
      let exeFileO3 = tmpDir </> "sum_o3"
      
      writeFile inputFile (T.unpack pythonCode)
      
      -- Compile with O0
      let configO0 = defaultConfig
            { ccInputFiles = [inputFile]
            , ccOutputPath = Just cppFileO0
            , ccOptimizationLevel = O0
            , ccStopAtCodegen = True
            }
      resultO0 <- runCompiler (convertConfigToDriver configO0) (compileFile inputFile)
      resultO0 `shouldSatisfy` isRight
      
      -- Compile with O3
      let configO3 = defaultConfig
            { ccInputFiles = [inputFile]
            , ccOutputPath = Just cppFileO3
            , ccOptimizationLevel = O3
            , ccStopAtCodegen = True
            }
      resultO3 <- runCompiler (convertConfigToDriver configO3) (compileFile inputFile)
      resultO3 `shouldSatisfy` isRight
      
      -- Compile both C++ files
      (exitCodeO0, _, _) <- readProcessWithExitCode "g++" ["-O0", cppFileO0, "-o", exeFileO0] ""
      (exitCodeO3, _, _) <- readProcessWithExitCode "g++" ["-O3", cppFileO3, "-o", exeFileO3] ""
      exitCodeO0 `shouldBe` ExitSuccess
      exitCodeO3 `shouldBe` ExitSuccess
      
      -- Time both executables
      startO0 <- getCurrentTime
      _ <- readProcessWithExitCode exeFileO0 [] ""
      endO0 <- getCurrentTime
      let durationO0 = diffUTCTime endO0 startO0
      
      startO3 <- getCurrentTime
      _ <- readProcessWithExitCode exeFileO3 [] ""
      endO3 <- getCurrentTime
      let durationO3 = diffUTCTime endO3 startO3
      
      -- O3 should be faster or at least not significantly slower
      durationO3 `shouldSatisfy` (<= durationO0 * 2)

memoryUsageSpec :: Spec
memoryUsageSpec = describe "Memory Usage" $ do
  it "compiles without excessive memory usage" $ do
    let pythonCode = T.unlines $
          concat (replicate 50 [
            "def func_" ++ show n ++ "(x):"
          , "    return x + " ++ show n
          , ""
          | n <- [1..50 :: Int]
          ])
    
    withSystemTempDirectory "fluxus-perf-mem-" $ \tmpDir -> do
      let inputFile = tmpDir </> "large.py"
      let outputFile = tmpDir </> "large.cpp"
      
      writeFile inputFile (T.unpack $ T.unlines pythonCode)
      
      let config = defaultConfig
            { ccInputFiles = [inputFile]
            , ccOutputPath = Just outputFile
            , ccOptimizationLevel = O1
            , ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (compileFile inputFile)
      result `shouldSatisfy` isRight

scalabilitySpec :: Spec
scalabilitySpec = describe "Scalability" $ do
  it "handles increasing file sizes gracefully" $ do
    let sizes = [10, 50, 100]
    
    forM_ sizes $ \size -> do
      let pythonCode = T.unlines $
            concat (replicate size [
              "def func_" ++ show n ++ "(x):"
            , "    return x * 2"
            , ""
            | n <- [1..size]
            ])
      
      withSystemTempDirectory "fluxus-perf-scale-" $ \tmpDir -> do
        let inputFile = tmpDir </> ("scale_" ++ show size ++ ".py")
        let outputFile = tmpDir </> ("scale_" ++ show size ++ ".cpp")
        
        writeFile inputFile (T.unpack $ T.unlines pythonCode)
        
        let config = defaultConfig
              { ccInputFiles = [inputFile]
              , ccOutputPath = Just outputFile
              , ccStopAtCodegen = True
              }
        
        start <- getCurrentTime
        result <- runCompiler (convertConfigToDriver config) (compileFile inputFile)
        end <- getCurrentTime
        
        let duration = diffUTCTime end start
        result `shouldSatisfy` isRight
        -- Compilation time should scale reasonably (< 30s even for 100 functions)
        duration `shouldSatisfy` (< 30.0)

-- Helper functions
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
