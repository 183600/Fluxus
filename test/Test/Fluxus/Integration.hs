{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Integration (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Process
import System.Exit (ExitCode(..))

import Fluxus.Compiler.Driver (runCompiler, convertConfigToDriver)
import Fluxus.Compiler.Config as Config

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
      let config = Config.defaultConfig 
            { Config.ccInputFiles = [inputFile]
            , Config.ccOutputPath = Just outputFile
            , Config.ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (return ())
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
      
      let config = Config.defaultConfig 
            { Config.ccInputFiles = [inputFile]
            , Config.ccOutputPath = Just outputFile
            , Config.ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (return ())
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
      
      let config = Config.defaultConfig 
            { Config.ccInputFiles = [inputFile]
            , Config.ccOutputPath = Just outputFile
            , Config.ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (return ())
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
      
      let config = Config.defaultConfig 
            { Config.ccInputFiles = [inputFile]
            , Config.ccOutputPath = Just outputFile
            , Config.ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (return ())
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
      
      let config = Config.defaultConfig 
            { Config.ccInputFiles = [inputFile]
            , Config.ccOutputPath = Just outputFile
            , Config.ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (return ())
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
      let config = Config.defaultConfig 
            { Config.ccInputFiles = [inputFile]
            , Config.ccOutputPath = Just cppFile
            , Config.ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (return ())
      case result of
        Right _ -> do
          -- Compile C++ to executable
          (exitCode, _, _) <- readProcessWithExitCode "g++" [cppFile, "-o", exeFile] ""
          exitCode `shouldBe` ExitSuccess
          
          -- Run the executable and check output
          (exitCode', stdout, _) <- readProcessWithExitCode exeFile [] ""
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
      
      let config = Config.defaultConfig 
            { Config.ccInputFiles = [inputFile]
            , Config.ccOutputPath = Just outputFile
            , Config.ccStopAtCodegen = True
            }
      
      result <- runCompiler (convertConfigToDriver config) (return ())
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

      let config = Config.defaultConfig
            { Config.ccInputFiles = [inputFile1, inputFile2]
            , Config.ccOutputPath = Just outputFile
            , Config.ccStopAtCodegen = True
            }

      result <- runCompiler (convertConfigToDriver config) (return ())
      case result of
        Right _ -> do
          exists <- doesFileExist outputFile
          exists `shouldBe` True

          cppContent <- readFile outputFile
          cppContent `shouldContain` "util_function"
          cppContent `shouldContain` "main"
        Left err -> expectationFailure $ "Compilation failed: " ++ show err

  it "handles mixed Python and Go projects" $ do
    let pythonCode = T.unlines
          [ "def python_func(x):"
          , "    return x * 2"
          ]
    let goCode = T.unlines
          [ "package main"
          , ""
          , "import \"fmt\""
          , ""
          , "func go_func(x int) int {"
          , "    return x * 3"
          , "}"
          , ""
          , "func main() {"
          , "    result := go_func(5)"
          , "    fmt.Printf(\"%d\\n\", result)"
          , "}"
          ]

    withSystemTempDirectory "fluxus-mixed-test-" $ \tmpDir -> do
      let pyFile = tmpDir </> "code.py"
      let goFile = tmpDir </> "code.go"
      let pyOutput = tmpDir </> "python.cpp"
      let goOutput = tmpDir </> "go.cpp"

      writeFile pyFile (T.unpack pythonCode)
      writeFile goFile (T.unpack goCode)

      -- Test Python compilation
      let pyConfig = Config.defaultConfig
            { Config.ccInputFiles = [pyFile]
            , Config.ccOutputPath = Just pyOutput
            , Config.ccStopAtCodegen = True
            }

      pyResult <- runCompiler (convertConfigToDriver pyConfig) (return ())
      case pyResult of
        Right _ -> do
          pyExists <- doesFileExist pyOutput
          pyExists `shouldBe` True
        Left err -> expectationFailure $ "Python compilation failed: " ++ show err

      -- Test Go compilation
      let goConfig = Config.defaultConfig
            { Config.ccInputFiles = [goFile]
            , Config.ccOutputPath = Just goOutput
            , Config.ccStopAtCodegen = True
            }

      goResult <- runCompiler (convertConfigToDriver goConfig) (return ())
      case goResult of
        Right _ -> do
          goExists <- doesFileExist goOutput
          goExists `shouldBe` True
        Left err -> expectationFailure $ "Go compilation failed: " ++ show err

  it "handles complex import chains" $ do
    let moduleA = T.unlines
          [ "def func_a():"
          , "    return \"A\""
          ]
    let moduleB = T.unlines
          [ "import module_a"
          , ""
          , "def func_b():"
          , "    return \"B\" + module_a.func_a()"
          ]
    let moduleC = T.unlines
          [ "import module_b"
          , ""
          , "def func_c():"
          , "    return \"C\" + module_b.func_b()"
          , ""
          , "def main():"
          , "    result = func_c()"
          , "    return result"
          ]

    withSystemTempDirectory "fluxus-import-test-" $ \tmpDir -> do
      let fileA = tmpDir </> "module_a.py"
      let fileB = tmpDir </> "module_b.py"
      let fileC = tmpDir </> "module_c.py"
      let outputFile = tmpDir </> "combined.cpp"

      writeFile fileA (T.unpack moduleA)
      writeFile fileB (T.unpack moduleB)
      writeFile fileC (T.unpack moduleC)

      let config = Config.defaultConfig
            { Config.ccInputFiles = [fileA, fileB, fileC]
            , Config.ccOutputPath = Just outputFile
            , Config.ccStopAtCodegen = True
            }

      result <- runCompiler (convertConfigToDriver config) (return ())
      case result of
        Right _ -> do
          exists <- doesFileExist outputFile
          exists `shouldBe` True

          cppContent <- readFile outputFile
          cppContent `shouldContain` "func_a"
          cppContent `shouldContain` "func_b"
          cppContent `shouldContain` "func_c"
        Left err -> expectationFailure $ "Compilation failed: " ++ show err

  it "handles circular dependencies with fallback" $ do
    let moduleA = T.unlines
          [ "# Circular dependency"
          , "import module_b"
          , ""
          , "def func_a():"
          , "    if hasattr(module_b, 'func_b'):"
          , "        return \"A\""
          , "    else:"
          , "        return \"A_fallback\""
          ]
    let moduleB = T.unlines
          [ "# Circular dependency"
          , "import module_a"
          , ""
          , "def func_b():"
          , "    if hasattr(module_a, 'func_a'):"
          , "        return \"B\""
          , "    else:"
          , "        return \"B_fallback\""
          ]

    withSystemTempDirectory "fluxus-circular-test-" $ \tmpDir -> do
      let fileA = tmpDir </> "module_a.py"
      let fileB = tmpDir </> "module_b.py"
      let outputFile = tmpDir </> "combined.cpp"

      writeFile fileA (T.unpack moduleA)
      writeFile fileB (T.unpack moduleB)

      let config = Config.defaultConfig
            { Config.ccInputFiles = [fileA, fileB]
            , Config.ccOutputPath = Just outputFile
            , Config.ccStopAtCodegen = True
            }

      result <- runCompiler (convertConfigToDriver config) (return ())
      case result of
        Right _ -> do
          exists <- doesFileExist outputFile
          exists `shouldBe` True
        Left err -> expectationFailure $ "Circular dependency handling failed: " ++ show err

  it "integrates with external libraries" $ do
    let pythonCode = T.unlines
          [ "import json"
          , "import sys"
          , ""
          , "def main():"
          , "    data = {'name': 'Fluxus', 'version': '1.0'}"
          , "    json_str = json.dumps(data)"
          , "    print(json_str)"
          , "    return 0"
          , ""
          , "if __name__ == '__main__':"
          , "    sys.exit(main())"
          ]

    withSystemTempDirectory "fluxus-external-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "external.py"
      let outputFile = tmpDir </> "external.cpp"

      writeFile inputFile (T.unpack pythonCode)

      let config = Config.defaultConfig
            { Config.ccInputFiles = [inputFile]
            , Config.ccOutputPath = Just outputFile
            , Config.ccSourceLanguage = Config.Python
            , Config.ccStopAtCodegen = True
            }

      result <- runCompiler (convertConfigToDriver config) (return ())
      case result of
        Right _ -> do
          exists <- doesFileExist outputFile
          exists `shouldBe` True

          cppContent <- readFile outputFile
          cppContent `shouldContain` "json"
          cppContent `shouldContain` "dumps"
        Left err -> expectationFailure $ "External library integration failed: " ++ show err

  it "handles large-scale integration" $ do
    -- Generate a larger Python project with multiple modules
    let modules = [
            ("utils", T.unlines [
                "def validate_input(x):",
                "    return isinstance(x, (int, float)) and x >= 0",
                "",
                "def format_output(x):",
                "    return f\"Result: {x}\""
            ]),
            ("calculator", T.unlines [
                "import utils",
                "",
                "class Calculator:",
                "    def __init__(self):",
                "        self.result = 0",
                "    ",
                "    def add(self, x):",
                "        if utils.validate_input(x):",
                "            self.result += x",
                "        return self.result",
                "    ",
                "    def multiply(self, x):",
                "        if utils.validate_input(x):",
                "            self.result *= x",
                "        return self.result",
                "    ",
                "    def get_result(self):",
                "        return utils.format_output(self.result)"
            ]),
            ("main", T.unlines [
                "from calculator import Calculator",
                "",
                "def main():",
                "    calc = Calculator()",
                "    calc.add(10)",
                "    calc.multiply(2)",
                "    result = calc.get_result()",
                "    print(result)",
                "    return 0"
            ])
          ]

    withSystemTempDirectory "fluxus-large-integration-test-" $ \tmpDir -> do
      -- Create modules
      mapM_ (\(name, content) -> do
              let filename = tmpDir </> name ++ ".py"
              writeFile filename (T.unpack content)
            ) modules

      let inputFiles = [tmpDir </> name ++ ".py" | (name, _) <- modules]
      let outputFile = tmpDir </> "large_project.cpp"

      let config = Config.defaultConfig
            { Config.ccInputFiles = inputFiles
            , Config.ccOutputPath = Just outputFile
            , Config.ccStopAtCodegen = True
            }

      result <- runCompiler (convertConfigToDriver config) (return ())
      case result of
        Right _ -> do
          exists <- doesFileExist outputFile
          exists `shouldBe` True

          cppContent <- readFile outputFile
          cppContent `shouldContain` "Calculator"
          cppContent `shouldContain` "validate_input"
          cppContent `shouldContain` "format_output"
        Left err -> expectationFailure $ "Large-scale integration failed: " ++ show err