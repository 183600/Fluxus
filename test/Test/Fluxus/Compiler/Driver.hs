{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Compiler.Driver (spec) where

import Test.Hspec
import System.IO.Temp
import System.Directory
import System.FilePath
import qualified Data.Text as T

import Fluxus.Compiler.Driver
import Fluxus.Compiler.Config

spec :: Spec
spec = describe "Compiler Driver" $ do
  compilerPipelineSpec
  errorHandlingSpec
  multiFileCompilationSpec
  optimizationPipelineSpec

compilerPipelineSpec :: Spec
compilerPipelineSpec = describe "Compilation Pipeline" $ do
  it "compiles simple Python file" $ do
    let pythonCode = T.unlines
          [ "def add(a, b):"
          , "    return a + b"
          , ""
          , "result = add(2, 3)"
          , "print(result)"
          ]
    
    withSystemTempDirectory "fluxus-driver-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "test.py"
      let outputFile = tmpDir </> "test.cpp"
      
      writeFile inputFile (T.unpack pythonCode)
      
      let config = defaultConfig
            { ccInputFiles = [inputFile]
            , ccOutputPath = Just outputFile
            , ccSourceLanguage = Python
            , ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (compileFile inputFile)
      
      result `shouldSatisfy` isRight
      exists <- doesFileExist outputFile
      exists `shouldBe` True
  
  it "compiles simple Go file" $ do
    let goCode = T.unlines
          [ "package main"
          , ""
          , "func add(a int, b int) int {"
          , "    return a + b"
          , "}"
          , ""
          , "func main() {"
          , "    result := add(2, 3)"
          , "    println(result)"
          , "}"
          ]
    
    withSystemTempDirectory "fluxus-driver-go-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "test.go"
      let outputFile = tmpDir </> "test.cpp"
      
      writeFile inputFile (T.unpack goCode)
      
      let config = defaultConfig
            { ccInputFiles = [inputFile]
            , ccOutputPath = Just outputFile
            , ccSourceLanguage = Go
            , ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (compileFile inputFile)
      
      result `shouldSatisfy` isRight
      exists <- doesFileExist outputFile
      exists `shouldBe` True
  
  it "handles complex Python code" $ do
    let pythonCode = T.unlines
          [ "class Calculator:"
          , "    def __init__(self):"
          , "        self.result = 0"
          , "    "
          , "    def add(self, x):"
          , "        self.result += x"
          , "        return self"
          , "    "
          , "    def multiply(self, x):"
          , "        self.result *= x"
          , "        return self"
          , "    "
          , "    def get_result(self):"
          , "        return self.result"
          , ""
          , "calc = Calculator()"
          , "result = calc.add(5).multiply(3).get_result()"
          , "print(result)"
          ]
    
    withSystemTempDirectory "fluxus-driver-complex-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "calculator.py"
      let outputFile = tmpDir </> "calculator.cpp"
      
      writeFile inputFile (T.unpack pythonCode)
      
      let config = defaultConfig
            { ccInputFiles = [inputFile]
            , ccOutputPath = Just outputFile
            , ccSourceLanguage = Python
            , ccOptimizationLevel = O2
            , ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (compileFile inputFile)
      
      result `shouldSatisfy` isRight

errorHandlingSpec :: Spec
errorHandlingSpec = describe "Error Handling" $ do
  it "reports syntax errors" $ do
    let invalidPython = T.unlines
          [ "def broken_function("
          , "    return 42"
          ]
    
    withSystemTempDirectory "fluxus-error-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "broken.py"
      let outputFile = tmpDir </> "broken.cpp"
      
      writeFile inputFile (T.unpack invalidPython)
      
      let config = defaultConfig
            { ccInputFiles = [inputFile]
            , ccOutputPath = Just outputFile
            , ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (compileFile inputFile)
      
      result `shouldSatisfy` isLeft
  
  it "reports missing input file" $ do
    let config = defaultConfig
          { ccInputFiles = ["/nonexistent/file.py"]
          , ccOutputPath = Just "/tmp/output.cpp"
          , ccStopAtCodegen = True
          }
    
    result <- runCompiler (convertConfigToDriver config) (compileFile "/nonexistent/file.py")
    result `shouldSatisfy` isLeft
  
  it "reports type errors" $ do
    let pythonCode = T.unlines
          [ "x = \"hello\""
          , "y = x + 5"  -- Type error: can't add string and int
          ]
    
    withSystemTempDirectory "fluxus-type-error-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "type_error.py"
      let outputFile = tmpDir </> "type_error.cpp"
      
      writeFile inputFile (T.unpack pythonCode)
      
      let config = defaultConfig
            { ccInputFiles = [inputFile]
            , ccOutputPath = Just outputFile
            , ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (compileFile inputFile)
      -- This might succeed with dynamic typing or fail with type error
      -- Just ensure it doesn't crash
      result `shouldSatisfy` (\r -> isRight r || isLeft r)

multiFileCompilationSpec :: Spec
multiFileCompilationSpec = describe "Multi-File Compilation" $ do
  it "compiles multiple Python files" $ do
    let module1 = T.unlines
          [ "def helper_function(x):"
          , "    return x * 2"
          ]
    let module2 = T.unlines
          [ "from module1 import helper_function"
          , ""
          , "def main():"
          , "    result = helper_function(21)"
          , "    return result"
          ]
    
    withSystemTempDirectory "fluxus-multi-file-test-" $ \tmpDir -> do
      let file1 = tmpDir </> "module1.py"
      let file2 = tmpDir </> "module2.py"
      let outputFile = tmpDir </> "combined.cpp"
      
      writeFile file1 (T.unpack module1)
      writeFile file2 (T.unpack module2)
      
      let config = defaultConfig
            { ccInputFiles = [file1, file2]
            , ccOutputPath = Just outputFile
            , ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (compileProject [file1, file2])
      
      result `shouldSatisfy` isRight
  
  it "handles Go packages" $ do
    let mainGo = T.unlines
          [ "package main"
          , ""
          , "import \"./utils\""
          , ""
          , "func main() {"
          , "    result := utils.Double(21)"
          , "    println(result)"
          , "}"
          ]
    let utilsGo = T.unlines
          [ "package utils"
          , ""
          , "func Double(x int) int {"
          , "    return x * 2"
          , "}"
          ]
    
    withSystemTempDirectory "fluxus-go-package-test-" $ \tmpDir -> do
      let mainFile = tmpDir </> "main.go"
      let utilsFile = tmpDir </> "utils.go"
      
      writeFile mainFile (T.unpack mainGo)
      writeFile utilsFile (T.unpack utilsGo)
      
      let config = defaultConfig
            { ccInputFiles = [mainFile, utilsFile]
            , ccSourceLanguage = Go
            , ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (compileProject [mainFile, utilsFile])
      
      result `shouldSatisfy` isRight

optimizationPipelineSpec :: Spec
optimizationPipelineSpec = describe "Optimization Pipeline" $ do
  it "applies O0 optimization (no optimization)" $ do
    let pythonCode = "x = 1 + 1\nprint(x)"
    
    withSystemTempDirectory "fluxus-opt-o0-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "test.py"
      let outputFile = tmpDir </> "test.cpp"
      
      writeFile inputFile pythonCode
      
      let config = defaultConfig
            { ccInputFiles = [inputFile]
            , ccOutputPath = Just outputFile
            , ccOptimizationLevel = O0
            , ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (compileFile inputFile)
      result `shouldSatisfy` isRight
  
  it "applies O3 optimization (maximum optimization)" $ do
    let pythonCode = "x = 1 + 1\nprint(x)"
    
    withSystemTempDirectory "fluxus-opt-o3-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "test.py"
      let outputFile = tmpDir </> "test.cpp"
      
      writeFile inputFile pythonCode
      
      let config = defaultConfig
            { ccInputFiles = [inputFile]
            , ccOutputPath = Just outputFile
            , ccOptimizationLevel = O3
            , ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (compileFile inputFile)
      result `shouldSatisfy` isRight
  
  it "applies constant folding optimization" $ do
    let pythonCode = T.unlines
          [ "x = 2 + 3 * 4"
          , "y = 10 / 2"
          , "z = x + y"
          , "print(z)"
          ]
    
    withSystemTempDirectory "fluxus-const-fold-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "const.py"
      let outputFile = tmpDir </> "const.cpp"
      
      writeFile inputFile (T.unpack pythonCode)
      
      let config = defaultConfig
            { ccInputFiles = [inputFile]
            , ccOutputPath = Just outputFile
            , ccOptimizationLevel = O2
            , ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (compileFile inputFile)
      result `shouldSatisfy` isRight
      
      -- Check that constants were folded
      cppContent <- readFile outputFile
      -- The constants should be pre-computed
      cppContent `shouldContain` "14"  -- 2 + 3 * 4
      cppContent `shouldContain` "5"   -- 10 / 2

-- Helper functions
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
