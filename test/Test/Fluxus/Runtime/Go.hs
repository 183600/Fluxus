{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Runtime.Go (spec) where

import Test.Hspec
import Data.Text (unlines)
import qualified Data.Text as T

import Fluxus.Runtime.Go

spec :: Spec
spec = describe "Go Runtime Tests" $ do
  basicRuntimeSpec
  memoryManagementSpec
  concurrencySpec
  errorHandlingSpec
  ffiSpec

basicRuntimeSpec :: Spec
basicRuntimeSpec = describe "Basic Runtime Operations" $ do
  it "initializes Go runtime correctly" $ do
    result <- initGoRuntime CGoInterop
    case result of
      Right _ -> return ()
      Left err -> expectationFailure $ "Runtime initialization failed: " ++ show err

  it "executes basic Go code" $ do
    runtimeResult <- initGoRuntime CGoInterop
    case runtimeResult of
      Left err -> expectationFailure $ "Runtime initialization failed: " ++ show err
      Right runtime -> do
        let goCode = T.unlines
              [ "package main"
              , ""
              , "func main() {"
              , "}"
              ]
        result <- runGoCode runtime goCode
        case result of
          Right _ -> return ()
          Left err -> expectationFailure $ "Code execution failed: " ++ show err

  it "handles Go variables and types" $ do
    runtimeResult <- initGoRuntime CGoInterop
    case runtimeResult of
      Left err -> expectationFailure $ "Runtime initialization failed: " ++ show err
      Right runtime -> do
        let goCode = T.unlines
              [ "package main"
              , ""
              , "func add(a int, b int) int {"
              , "    return a + b"
              , "}"
              ]
        result <- runGoCode runtime goCode
        case result of
          Right output -> do
            show output `shouldContain` "add"
          Left err -> expectationFailure $ "Code execution failed: " ++ show err

memoryManagementSpec :: Spec
memoryManagementSpec = describe "Memory Management" $ do
  it "manages Go garbage collection" $ do
    runtimeResult <- initGoRuntime CGoInterop
    case runtimeResult of
      Left err -> expectationFailure $ "Runtime initialization failed: " ++ show err
      Right runtime -> do
        let goCode = T.unlines
              [ "package main"
              , ""
              , "import \"runtime\""
              , ""
              , "func createObjects() {"
              , "    for i := 0; i < 1000; i++ {"
              , "        _ = make([]byte, 1024)"
              , "    }"
              , "}"
              , ""
              , "func main() {"
              , "    createObjects()"
              , "    runtime.GC()"
              , "}"
              ]
        result <- runGoCode runtime goCode
        case result of
          Right _ -> return ()
          Left err -> expectationFailure $ "Memory management test failed: " ++ show err

  it "handles Go memory allocation" $ do
    runtimeResult <- initGoRuntime CGoInterop
    case runtimeResult of
      Left err -> expectationFailure $ "Runtime initialization failed: " ++ show err
      Right runtime -> do
        let goCode = T.unlines
              [ "package main"
              , ""
              , "import ("
              , "    \"unsafe\""
              , "    \"runtime\""
              , ")"
              , ""
              , "func main() {"
              , "    size := unsafe.Sizeof(0)"
              , "    memStats := &runtime.MemStats{}"
              , "    runtime.ReadMemStats(memStats)"
              , "    _ = size"
              , "}"
              ]
        result <- runGoCode runtime goCode
        case result of
          Right _ -> return ()
          Left err -> expectationFailure $ "Memory allocation test failed: " ++ show err

concurrencySpec :: Spec
concurrencySpec = describe "Concurrency Support" $ do
  it "handles Go goroutines" $ do
    runtimeResult <- initGoRuntime CGoInterop
    case runtimeResult of
      Left err -> expectationFailure $ "Runtime initialization failed: " ++ show err
      Right runtime -> do
        let goCode = T.unlines
              [ "package main"
              , ""
              , "import ("
              , "    \"fmt\""
              , "    \"sync\""
              , ")"
              , ""
              , "func worker(id int, wg *sync.WaitGroup) {"
              , "    defer wg.Done()"
              , "    fmt.Printf(\"Worker %d\\n\", id)"
              , "}"
              , ""
              , "func main() {"
              , "    var wg sync.WaitGroup"
              , "    for i := 1; i <= 3; i++ {"
              , "        wg.Add(1)"
              , "        go worker(i, &wg)"
              , "    }"
              , "    wg.Wait()"
              , "}"
              ]
        result <- runGoCode runtime goCode
        case result of
          Right output -> do
            show output `shouldContain` "Worker"
          Left err -> expectationFailure $ "Goroutine test failed: " ++ show err

  it "handles Go channels" $ do
    runtimeResult <- initGoRuntime CGoInterop
    case runtimeResult of
      Left err -> expectationFailure $ "Runtime initialization failed: " ++ show err
      Right runtime -> do
        let goCode = T.unlines
              [ "package main"
              , ""
              , "func main() {"
              , "    messages := make(chan string)"
              , "    go func() { messages <- \"ping\" }()"
              , "    msg := <-messages"
              , "    _ = msg"
              , "}"
              ]
        result <- runGoCode runtime goCode
        case result of
          Right _ -> return ()
          Left err -> expectationFailure $ "Channel test failed: " ++ show err

errorHandlingSpec :: Spec
errorHandlingSpec = describe "Error Handling" $ do
  it "handles Go error types" $ do
    runtimeResult <- initGoRuntime CGoInterop
    case runtimeResult of
      Left err -> expectationFailure $ "Runtime initialization failed: " ++ show err
      Right runtime -> do
        let goCode = T.unlines
              [ "package main"
              , ""
              , "import \"errors\""
              , ""
              , "func divide(a, b int) (int, error) {"
              , "    if b == 0 {"
              , "        return 0, errors.New(\"division by zero\")"
              , "    }"
              , "    return a / b, nil"
              , "}"
              , ""
              , "func main() {"
              , "    _, err := divide(10, 0)"
              , "    _ = err"
              , "}"
              ]
        result <- runGoCode runtime goCode
        case result of
          Right _ -> return ()
          Left err -> expectationFailure $ "Error handling test failed: " ++ show err

  it "handles Go panic and recover" $ do
    runtimeResult <- initGoRuntime CGoInterop
    case runtimeResult of
      Left err -> expectationFailure $ "Runtime initialization failed: " ++ show err
      Right runtime -> do
        let goCode = T.unlines
              [ "package main"
              , ""
              , "func main() {"
              , "    defer func() {"
              , "        if r := recover(); r != nil {"
              , "            _ = r"
              , "        }"
              , "    }()"
              , "    panic(\"test panic\")"
              , "}"
              ]
        result <- runGoCode runtime goCode
        case result of
          Right _ -> return ()
          Left err -> expectationFailure $ "Panic recovery test failed: " ++ show err

ffiSpec :: Spec
ffiSpec = describe "Foreign Function Interface" $ do
  it "handles Go Cgo integration" $ do
    runtimeResult <- initGoRuntime CGoInterop
    case runtimeResult of
      Left err -> expectationFailure $ "Runtime initialization failed: " ++ show err
      Right runtime -> do
        let goCode = T.unlines
              [ "package main"
              , ""
              , "/*"
              , "#include <stdio.h>"
              , "void c_function() {"
              , "    printf(\"Hello from C\\n\");"
              , "}"
              , "*/"
              , "import \"C\""
              , ""
              , "func main() {"
              , "    C.c_function()"
              , "}"
              ]
        result <- runGoCode runtime goCode
        case result of
          Right _ -> return ()
          Left err -> expectationFailure $ "Cgo integration test failed: " ++ show err

  it "handles Go system calls" $ do
    runtimeResult <- initGoRuntime CGoInterop
    case runtimeResult of
      Left err -> expectationFailure $ "Runtime initialization failed: " ++ show err
      Right runtime -> do
        let goCode = T.unlines
              [ "package main"
              , ""
              , "import ("
              , "    \"os\""
              , "    \"syscall\""
              , ")"
              , ""
              , "func main() {"
              , "    pid := syscall.Getpid()"
              , "    _ = pid"
              , "    _, err := os.Stat(\"/tmp\")"
              , "    _ = err"
              , "}"
              ]
        result <- runGoCode runtime goCode
        case result of
          Right _ -> return ()
          Left err -> expectationFailure $ "System call test failed: " ++ show err