{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Integration (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Process

import Fluxus.Compiler.Driver
import Fluxus.Compiler.Config

spec :: Spec
spec = describe "Integration Tests" $ do
  pythonToCppIntegrationSpec
  goToCppIntegrationSpec
  endToEndSpec

pythonToCppIntegrationSpec :: Spec
pythonToCppIntegrationSpec = describe "Python to C++ Integration" $ do
  it "compiles simple Python function to C++" $ do
    let pythonCode = T.unlines
          [ "def add(a, b):"
          , "    return a + b"
          ]
    
    withSystemTempDirectory "fluxus-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "test.py"
      let outputFile = tmpDir </> "test.cpp"
      
      -- Write Python code to file
      writeFile inputFile (T.unpack pythonCode)
      
      -- Compile Python to C++
      let config = defaultConfig 
            { inputFiles = [inputFile]
            , outputFile = outputFile
            , sourceLanguage = Python
            , targetLanguage = Cpp
            }
      
      result <- runCompiler config
      case result of
        Right _ -> do
          -- Check that output file was created
          exists <- doesFileExist outputFile
          exists `shouldBe` True
          
          -- Check that output file contains expected C++ code
          cppContent <- readFile outputFile
          cppContent `shouldContain` "add"
          cppContent `shouldContain` "int"
          cppContent `shouldContain` "return"
        Left err -> expectationFailure $ "Compilation failed: " ++ show err
  
  it "compiles Python class to C++" $ do
    let pythonCode = T.unlines
          [ "class Calculator:"
          , "    def __init__(self, value):"
          , "        self.value = value"
          , "    "
          , "    def add(self, x):"
          , "        return self.value + x"
          , "    "
          , "    def get_value(self):"
          , "        return self.value"
          ]
    
    withSystemTempDirectory "fluxus-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "calculator.py"
      let outputFile = tmpDir </> "calculator.cpp"
      
      writeFile inputFile (T.unpack pythonCode)
      
      let config = defaultConfig 
            { inputFiles = [inputFile]
            , outputFile = outputFile
            , sourceLanguage = Python
            , targetLanguage = Cpp
            }
      
      result <- runCompiler config
      case result of
        Right _ -> do
          exists <- doesFileExist outputFile
          exists `shouldBe` True
          
          cppContent <- readFile outputFile
          cppContent `shouldContain` "Calculator"
          cppContent `shouldContain` "class"
          cppContent `shouldContain` "add"
          cppContent `shouldContain` "get_value"
        Left err -> expectationFailure $ "Compilation failed: " ++ show err
  
  it "compiles Python with control flow to C++" $ do
    let pythonCode = T.unlines
          [ "def factorial(n):"
          , "    if n <= 1:"
          , "        return 1"
          , "    else:"
          , "        return n * factorial(n - 1)"
          ]
    
    withSystemTempDirectory "fluxus-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "factorial.py"
      let outputFile = tmpDir </> "factorial.cpp"
      
      writeFile inputFile (T.unpack pythonCode)
      
      let config = defaultConfig 
            { inputFiles = [inputFile]
            , outputFile = outputFile
            , sourceLanguage = Python
            , targetLanguage = Cpp
            }
      
      result <- runCompiler config
      case result of
        Right _ -> do
          exists <- doesFileExist outputFile
          exists `shouldBe` True
          
          cppContent <- readFile outputFile
          cppContent `shouldContain` "factorial"
          cppContent `shouldContain` "if"
          cppContent `shouldContain` "else"
        Left err -> expectationFailure $ "Compilation failed: " ++ show err

goToCppIntegrationSpec :: Spec
goToCppIntegrationSpec = describe "Go to C++ Integration" $ do
  it "compiles simple Go function to C++" $ do
    let goCode = T.unlines
          [ "package main"
          , ""
          , "func add(a int, b int) int {"
          , "\treturn a + b"
          , "}"
          ]
    
    withSystemTempDirectory "fluxus-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "test.go"
      let outputFile = tmpDir </> "test.cpp"
      
      writeFile inputFile (T.unpack goCode)
      
      let config = defaultConfig 
            { inputFiles = [inputFile]
            , outputFile = outputFile
            , sourceLanguage = Go
            , targetLanguage = Cpp
            }
      
      result <- runCompiler config
      case result of
        Right _ -> do
          exists <- doesFileExist outputFile
          exists `shouldBe` True
          
          cppContent <- readFile outputFile
          cppContent `shouldContain` "add"
          cppContent `shouldContain` "int"
          cppContent `shouldContain` "return"
        Left err -> expectationFailure $ "Compilation failed: " ++ show err
  
  it "compiles Go struct to C++ class" $ do
    let goCode = T.unlines
          [ "package main"
          , ""
          , "type Person struct {"
          , "\tName string"
          , "\tAge  int"
          , "}"
          , ""
          , "func (p *Person) GetName() string {"
          , "\treturn p.Name"
          , "}"
          ]
    
    withSystemTempDirectory "fluxus-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "person.go"
      let outputFile = tmpDir </> "person.cpp"
      
      writeFile inputFile (T.unpack goCode)
      
      let config = defaultConfig 
            { inputFiles = [inputFile]
            , outputFile = outputFile
            , sourceLanguage = Go
            , targetLanguage = Cpp
            }
      
      result <- runCompiler config
      case result of
        Right _ -> do
          exists <- doesFileExist outputFile
          exists `shouldBe` True
          
          cppContent <- readFile outputFile
          cppContent `shouldContain` "Person"
          cppContent `shouldContain` "class"
          cppContent `shouldContain` "GetName"
          cppContent `shouldContain` "string"
          cppContent `shouldContain` "int"
        Left err -> expectationFailure $ "Compilation failed: " ++ show err

endToEndSpec :: Spec
endToEndSpec = describe "End-to-End Tests" $ do
  it "compiles and runs generated C++ code" $ do
    let pythonCode = T.unlines
          [ "def main():"
          , "    result = 2 + 3 * 4"
          , "    print(result)"
          , "    return result"
          ]
    
    withSystemTempDirectory "fluxus-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "main.py"
      let cppFile = tmpDir </> "main.cpp"
      let exeFile = tmpDir </> "main"
      
      writeFile inputFile (T.unpack pythonCode)
      
      -- Compile Python to C++
      let config = defaultConfig 
            { inputFiles = [inputFile]
            , outputFile = cppFile
            , sourceLanguage = Python
            , targetLanguage = Cpp
            }
      
      result <- runCompiler config
      case result of
        Right _ -> do
          -- Compile C++ to executable
          (exitCode, _, stderr) <- readProcessWithExitCode "g++" [cppFile, "-o", exeFile] ""
          exitCode `shouldBe` ExitSuccess
          
          -- Run the executable and check output
          (exitCode', stdout, stderr') <- readProcessWithExitCode exeFile [] ""
          exitCode' `shouldBe` ExitSuccess
          stdout `shouldContain` "14"  -- 2 + 3 * 4 = 14
        Left err -> expectationFailure $ "Compilation failed: " ++ show err
  
  it "handles compilation errors gracefully" $ do
    let invalidPythonCode = T.unlines
          [ "def invalid_function("
          , "    # Missing closing parenthesis"
          , "    return 42"
          ]
    
    withSystemTempDirectory "fluxus-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "invalid.py"
      let outputFile = tmpDir </> "invalid.cpp"
      
      writeFile inputFile (T.unpack invalidPythonCode)
      
      let config = defaultConfig 
            { inputFiles = [inputFile]
            , outputFile = outputFile
            , sourceLanguage = Python
            , targetLanguage = Cpp
            }
      
      result <- runCompiler config
      case result of
        Left _ -> return ()  -- Expected to fail
        Right _ -> expectationFailure "Compilation should have failed with invalid syntax"
  
  it "processes multiple input files" $ do
    let pythonCode1 = T.unlines
          [ "def util_function(x):"
          , "    return x * 2"
          ]
    let pythonCode2 = T.unlines
          [ "import util"
          , ""
          , "def main():"
          , "    result = util.util_function(5)"
          , "    return result"
          ]
    
    withSystemTempDirectory "fluxus-test-" $ \tmpDir -> do
      let inputFile1 = tmpDir </> "util.py"
      let inputFile2 = tmpDir </> "main.py"
      let outputFile = tmpDir </> "combined.cpp"
      
      writeFile inputFile1 (T.unpack pythonCode1)
      writeFile inputFile2 (T.unpack pythonCode2)
      
      let config = defaultConfig 
            { inputFiles = [inputFile1, inputFile2]
            , outputFile = outputFile
            , sourceLanguage = Python
            , targetLanguage = Cpp
            }
      
      result <- runCompiler config
      case result of
        Right _ -> do
          exists <- doesFileExist outputFile
          exists `shouldBe` True
          
          cppContent <- readFile outputFile
          cppContent `shouldContain` "util_function"
          cppContent `shouldContain` "main"
        Left err -> expectationFailure $ "Compilation failed: " ++ show err