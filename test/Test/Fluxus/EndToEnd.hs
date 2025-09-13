{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.EndToEnd (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Process
import System.Exit

import Fluxus.Compiler.Driver
import Fluxus.Compiler.Config

spec :: Spec
spec = describe "End-to-End Production Tests" $ do
  productionCompilationSpec
  runtimeBehaviorSpec
  errorHandlingSpec
  performanceSpec

productionCompilationSpec :: Spec
productionCompilationSpec = describe "Production Compilation Tests" $ do
  it "compiles real-world Python application to C++" $ do
    let pythonApp = T.unlines
          [ "import sys"
          , "import os"
          , ""
          , "class DataProcessor:"
          , "    def __init__(self, data):"
          , "        self.data = data"
          , "        self.processed = False"
          , "    "
          , "    def process(self):"
          , "        # Simulate data processing"
          , "        result = []"
          , "        for item in self.data:"
          , "            if isinstance(item, str):"
          , "                result.append(item.upper())"
          , "            elif isinstance(item, (int, float)):"
          , "                result.append(item * 2)"
          , "            else:"
          , "                result.append(str(item))"
          , "        self.processed = True"
          , "        return result"
          , "    "
          , "    def save_to_file(self, filename):"
          , "        if not self.processed:"
          , "            raise ValueError(\"Data not processed yet\")"
          , "        with open(filename, 'w') as f:"
          , "            for item in self.data:"
          , "                f.write(str(item) + '\\n')"
          , ""
          , "def main():"
          , "    # Sample data"
          , "    sample_data = [\"hello\", 42, 3.14, \"world\", 100]"
          , "    "
          , "    # Create processor"
          , "    processor = DataProcessor(sample_data)"
          , "    "
          , "    # Process data"
          , "    processed = processor.process()"
          , "    print(f\"Processed {len(processed)} items\")"
          , "    "
          , "    # Save to file"
          , "    processor.save_to_file(\"output.txt\")"
          , "    "
          , "    return 0"
          , ""
          , "if __name__ == \"__main__\":"
          , "    sys.exit(main())"
          ]
    
    withSystemTempDirectory "fluxus-e2e-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "app.py"
      let cppFile = tmpDir </> "app.cpp"
      let exeFile = tmpDir </> "app"
      let outputFile = tmpDir </> "output.txt"
      
      writeFile inputFile (T.unpack pythonApp)
      
      -- Compile Python to C++
      let config = defaultConfig 
            { inputFiles = [inputFile]
            , outputFile = cppFile
            , sourceLanguage = Python
            , targetLanguage = Cpp
            , optimizationLevel = 3  -- Maximum optimization for production
            }
      
      result <- runCompiler config
      case result of
        Right _ -> do
          -- Check that C++ file was created
          exists <- doesFileExist cppFile
          exists `shouldBe` True
          
          -- Compile C++ to executable with production flags
          (exitCode, _, stderr) <- readProcessWithExitCode "g++" 
            [cppFile, "-o", exeFile, "-std=c++20", "-O3", "-DNDEBUG", "-Wall", "-Wextra"] ""
          exitCode `shouldBe` ExitSuccess
          
          -- Run the executable
          (exitCode', stdout, stderr') <- readProcessWithExitCode exeFile [] ""
          exitCode' `shouldBe` ExitSuccess
          stdout `shouldContain` "Processed 5 items"
          
          -- Check that output file was created
          outputExists <- doesFileExist outputFile
          outputExists `shouldBe` True
          
          -- Verify output file content
          outputContent <- readFile outputFile
          lines outputContent `shouldBe` ["hello", "42", "3.14", "world", "100"]
        Left err -> expectationFailure $ "Compilation failed: " ++ show err
  
  it "compiles multi-file Go project to C++" $ do
    let goMain = T.unlines
          [ "package main"
          , ""
          , "import ("
          , "    \"fmt\""
          , "    \"./utils\""
          , ")"
          , ""
          , "func main() {"
          , "    data := []string{\"hello\", \"world\", \"fluxus\"}"
          , "    result := utils.ProcessStrings(data)"
          , "    fmt.Printf(\"Processed %d strings\\n\", len(result))"
          , "    for i, s := range result {"
          , "        fmt.Printf(\"%d: %s\\n\", i, s)"
          , "    }"
          , "}"
          ]
    
    let goUtils = T.unlines
          [ "package utils"
          , ""
          , "import \"strings\""
          , ""
          , "type StringProcessor struct {"
          , "    prefix string"
          , "}"
          , ""
          , "func NewStringProcessor(prefix string) *StringProcessor {"
          , "    return &StringProcessor{prefix: prefix}"
          , "}"
          , ""
          , "func (sp *StringProcessor) Process(input string) string {"
          , "    return sp.prefix + strings.ToUpper(input)"
          , "}"
          , ""
          , "func ProcessStrings(inputs []string) []string {"
          , "    processor := NewStringProcessor(\"PROCESSED: \")"
          , "    result := make([]string, len(inputs))"
          , "    for i, input := range inputs {"
          , "        result[i] = processor.Process(input)"
          , "    }"
          , "    return result"
          , "}"
          ]
    
    withSystemTempDirectory "fluxus-e2e-go-test-" $ \tmpDir -> do
      let mainFile = tmpDir </> "main.go"
      let utilsFile = tmpDir </> "utils.go"
      let cppFile = tmpDir </> "combined.cpp"
      let exeFile = tmpDir </> "combined"
      
      writeFile mainFile (T.unpack goMain)
      writeFile utilsFile (T.unpack goUtils)
      
      -- Compile Go to C++
      let config = defaultConfig 
            { inputFiles = [mainFile, utilsFile]
            , outputFile = cppFile
            , sourceLanguage = Go
            , targetLanguage = Cpp
            , optimizationLevel = 3
            }
      
      result <- runCompiler config
      case result of
        Right _ -> do
          -- Check that C++ file was created
          exists <- doesFileExist cppFile
          exists `shouldBe` True
          
          -- Compile C++ to executable
          (exitCode, _, stderr) <- readProcessWithExitCode "g++" 
            [cppFile, "-o", exeFile, "-std=c++20", "-O3", "-DNDEBUG"] ""
          exitCode `shouldBe` ExitSuccess
          
          -- Run the executable
          (exitCode', stdout, stderr') <- readProcessWithExitCode exeFile [] ""
          exitCode' `shouldBe` ExitSuccess
          stdout `shouldContain` "Processed 3 strings"
          stdout `shouldContain` "PROCESSED: HELLO"
          stdout `shouldContain` "PROCESSED: WORLD"
          stdout `shouldContain` "PROCESSED: FLUXUS"
        Left err -> expectationFailure $ "Compilation failed: " ++ show err

runtimeBehaviorSpec :: Spec
runtimeBehaviorSpec = describe "Runtime Behavior Tests" $ do
  it "handles memory allocation correctly" $ do
    let memoryIntensiveCode = T.unlines
          [ "import sys"
          , ""
          , "class MemoryIntensive:"
          , "    def __init__(self, size):"
          , "        self.size = size"
          , "        self.data = [0] * size"
          , "    "
          , "    def process(self):"
          , "        # Simulate memory-intensive processing"
          , "        for i in range(self.size):"
          , "            self.data[i] = i * i"
          , "        return sum(self.data)"
          , ""
          , "def main():"
          , "    # Test with different sizes"
          , "    sizes = [1000, 10000, 100000]"
          , "    for size in sizes:"
          , "        processor = MemoryIntensive(size)"
          , "        result = processor.process()"
          , "        print(f\"Size {size}: sum of squares = {result}\")"
          , "    "
          , "    return 0"
          , ""
          , "if __name__ == \"__main__\":"
          , "    sys.exit(main())"
          ]
    
    withSystemTempDirectory "fluxus-memory-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "memory.py"
      let cppFile = tmpDir </> "memory.cpp"
      let exeFile = tmpDir </> "memory"
      
      writeFile inputFile (T.unpack memoryIntensiveCode)
      
      -- Compile Python to C++
      let config = defaultConfig 
            { inputFiles = [inputFile]
            , outputFile = cppFile
            , sourceLanguage = Python
            , targetLanguage = Cpp
            , optimizationLevel = 3
            }
      
      result <- runCompiler config
      case result of
        Right _ -> do
          -- Compile and run
          (exitCode, _, stderr) <- readProcessWithExitCode "g++" 
            [cppFile, "-o", exeFile, "-std=c++20", "-O3"] ""
          exitCode `shouldBe` ExitSuccess
          
          (exitCode', stdout, stderr') <- readProcessWithExitCode exeFile [] ""
          exitCode' `shouldBe` ExitSuccess
          stdout `shouldContain` "Size 1000:"
          stdout `shouldContain` "Size 10000:"
          stdout `shouldContain` "Size 100000:"
        Left err -> expectationFailure $ "Compilation failed: " ++ show err
  
  it "manages concurrency correctly" $ do
    let concurrentCode = T.unlines
          [ "import threading"
          , "import time"
          , "import queue"
          , ""
          , "class Worker:"
          , "    def __init__(self, worker_id, task_queue, result_queue):"
          , "        self.worker_id = worker_id"
          , "        self.task_queue = task_queue"
          , "        self.result_queue = result_queue"
          , "    "
          , "    def run(self):"
          , "        while True:"
          , "            try:"
          , "                task = self.task_queue.get_nowait()"
          , "                result = task * 2  # Simple processing"
          , "                self.result_queue.put((self.worker_id, result))"
          , "                self.task_queue.task_done()"
          , "            except queue.Empty:"
          , "                break"
          , ""
          , "def main():"
          , "    # Create queues"
          , "    task_queue = queue.Queue()"
          , "    result_queue = queue.Queue()"
          , "    "
          , "    # Add tasks"
          , "    for i in range(10):"
          , "        task_queue.put(i)
          , "    "
          , "    # Create and start workers"
          , "    workers = []"
          , "    for i in range(3):"
          , "        worker = Worker(i, task_queue, result_queue)"
          , "        thread = threading.Thread(target=worker.run)"
          , "        workers.append(thread)"
          , "        thread.start()"
          , "    "
          , "    # Wait for all tasks to be done"
          , "    task_queue.join()"
          , "    "
          , "    # Wait for workers to finish"
          , "    for thread in workers:"
          , "        thread.join()"
          , "    "
          , "    # Collect results"
          , "    results = []"
          , "    while not result_queue.empty():"
          , "        results.append(result_queue.get())"
          , "    "
          , "    # Sort results by worker ID"
          , "    results.sort()
          , "    "
          , "    print(f\"Processed {len(results)} tasks\")"
          , "    for worker_id, result in results:"
          , "        print(f\"Worker {worker_id}: {result}\")"
          , "    "
          , "    return 0"
          , ""
          , "if __name__ == \"__main__\":"
          , "    main()"
          ]
    
    withSystemTempDirectory "fluxus-concurrency-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "concurrent.py"
      let cppFile = tmpDir </> "concurrent.cpp"
      let exeFile = tmpDir </> "concurrent"
      
      writeFile inputFile (T.unpack concurrentCode)
      
      -- Compile Python to C++
      let config = defaultConfig 
            { inputFiles = [inputFile]
            , outputFile = cppFile
            , sourceLanguage = Python
            , targetLanguage = Cpp
            , optimizationLevel = 3
            }
      
      result <- runCompiler config
      case result of
        Right _ -> do
          -- Compile and run
          (exitCode, _, stderr) <- readProcessWithExitCode "g++" 
            [cppFile, "-o", exeFile, "-std=c++20", "-O3", "-pthread"] ""
          exitCode `shouldBe` ExitSuccess
          
          (exitCode', stdout, stderr') <- readProcessWithExitCode exeFile [] ""
          exitCode' `shouldBe` ExitSuccess
          stdout `shouldContain` "Processed 10 tasks"
        Left err -> expectationFailure $ "Compilation failed: " ++ show err

errorHandlingSpec :: Spec
errorHandlingSpec = describe "Error Handling Tests" $ do
  it "handles compilation errors gracefully" $ do
    let invalidPythonCode = T.unlines
          [ "def invalid_function(x, y"
          , "    # Missing closing parenthesis"
          , "    return x + y"
          ]
    
    withSystemTempDirectory "fluxus-error-test-" $ \tmpDir -> do
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
  
  it "produces helpful error messages" $ do
    let undefinedVariableCode = T.unlines
          [ "def example():"
          , "    x = undefined_variable  # This should cause an error"
          , "    return x"
          ]
    
    withSystemTempDirectory "fluxus-error-msg-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "undefined.py"
      let outputFile = tmpDir </> "undefined.cpp"
      
      writeFile inputFile (T.unpack undefinedVariableCode)
      
      let config = defaultConfig 
            { inputFiles = [inputFile]
            , outputFile = outputFile
            , sourceLanguage = Python
            , targetLanguage = Cpp
            }
      
      result <- runCompiler config
      case result of
        Left errMsg -> do
          -- Check that error message is helpful
          let errorMsg = show errMsg
          errorMsg `shouldContain` "undefined_variable"
          errorMsg `shouldContain` "undefined"
        Right _ -> expectationFailure "Compilation should have failed with undefined variable"

performanceSpec :: Spec
performanceSpec = describe "Performance Tests" $ do
  it "compiles efficiently with large codebases" $ do
    -- Generate a large Python file
    let largeCode = T.unlines $ 
          ["import sys", "import math", ""] ++
          ["def function_" ++ show i ++ "(x):" | i <- [1..100]] ++
          ["    # Function " ++ show i ++ " implementation" ++
           "    result = x * " ++ show i ++ " + math.sqrt(" ++ show i ++ ")" ++
           "    return result" | i <- [1..100]] ++
          [""] ++
          ["def main():"] ++
          ["    total = 0"] ++
          ["    for i in range(1, 101):"] ++
          ["        total += function_" ++ show i ++ "(i)" | i <- [1..100]] ++
          ["    print(f\"Total: {total}\")"] ++
          ["    return 0"] ++
          [""] ++
          ["if __name__ == \"__main__\":"]
          ["    sys.exit(main())"]
    
    withSystemTempDirectory "fluxus-large-test-" $ \tmpDir -> do
      let inputFile = tmpDir </> "large.py"
      let cppFile = tmpDir </> "large.cpp"
      let exeFile = tmpDir </> "large"
      
      writeFile inputFile (T.unpack largeCode)
      
      -- Time the compilation
      start <- getCurrentTime
      let config = defaultConfig 
            { inputFiles = [inputFile]
            , outputFile = cppFile
            , sourceLanguage = Python
            , targetLanguage = Cpp
            , optimizationLevel = 3
            }
      
      result <- runCompiler config
      end <- getCurrentTime
      
      let compilationTime = diffUTCTime end start
      
      case result of
        Right _ -> do
          -- Check that compilation completed in reasonable time
          -- (This threshold might need adjustment based on your system)
          compilationTime `shouldSatisfy` (\t -> t < 30)  -- Less than 30 seconds
          
          -- Compile and run
          (exitCode, _, stderr) <- readProcessWithExitCode "g++" 
            [cppFile, "-o", exeFile, "-std=c++20", "-O3"] ""
          exitCode `shouldBe` ExitSuccess
          
          (exitCode', stdout, stderr') <- readProcessWithExitCode exeFile [] ""
          exitCode' `shouldBe` ExitSuccess
          stdout `shouldContain` "Total:"
        Left err -> expectationFailure $ "Compilation failed: " ++ show err