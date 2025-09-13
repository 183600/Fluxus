{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.CodeGen.Go (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import Fluxus.CodeGen.Go

spec :: Spec
spec = describe "Go Code Generation Tests" $ do
  basicCodeGenSpec
  structCodeGenSpec
  interfaceCodeGenSpec
  functionCodeGenSpec
  errorHandlingSpec

basicCodeGenSpec :: Spec
basicCodeGenSpec = describe "Basic Code Generation" $ do
  it "generates Go code for simple functions" $ do
    let pythonCode = T.unlines
          [ "def add(a, b):"
          , "    return a + b"
          ]
    result <- generateGoCode pythonCode
    case result of
      Right goCode -> do
        goCode `shouldContain` "func add("
        goCode `shouldContain` "int"
        goCode `shouldContain` "return"
      Left err -> expectationFailure $ "Code generation failed: " ++ show err
  
  it "generates Go code for variable declarations" $ do
    let pythonCode = T.unlines
          [ "def func():"
          , "    x = 42"
          , "    y = \"hello\""
          , "    return x"
          ]
    result <- generateGoCode pythonCode
    case result of
      Right goCode -> do
        goCode `shouldContain` "var x int"
        goCode `shouldContain` "var y string"
      Left err -> expectationFailure $ "Code generation failed: " ++ show err

structCodeGenSpec :: Spec
structCodeGenSpec = describe "Struct Code Generation" $ do
  it "generates Go structs from Python classes" $ do
    let pythonCode = T.unlines
          [ "class Person:"
          , "    def __init__(self, name, age):"
          , "        self.name = name"
          , "        self.age = age"
          ]
    result <- generateGoCode pythonCode
    case result of
      Right goCode -> do
        goCode `shouldContain` "type Person struct"
        goCode `shouldContain` "Name string"
        goCode `shouldContain` "Age int"
      Left err -> expectationFailure $ "Code generation failed: " ++ show err
  
  it "generates Go struct methods" $ do
    let pythonCode = T.unlines
          [ "class Calculator:"
          , "    def __init__(self, value):"
          , "        self.value = value"
          , "    def add(self, x):"
          , "        return self.value + x"
          ]
    result <- generateGoCode pythonCode
    case result of
      Right goCode -> do
        goCode `shouldContain` "func (c *Calculator) Add("
        goCode `shouldContain` "int"
      Left err -> expectationFailure $ "Code generation failed: " ++ show err

interfaceCodeGenSpec :: Spec
interfaceCodeGenSpec = describe "Interface Code Generation" $ do
  it "generates Go interfaces from Python ABCs" $ do
    let pythonCode = T.unlines
          [ "from abc import ABC, abstractmethod"
          , ""
          , "class Shape(ABC):"
          , "    @abstractmethod"
          , "    def area(self):"
          , "        pass"
          ]
    result <- generateGoCode pythonCode
    case result of
      Right goCode -> do
        goCode `shouldContain` "type Shape interface"
        goCode `shouldContain` "Area() float64"
      Left err -> expectationFailure $ "Code generation failed: " ++ show err

functionCodeGenSpec :: Spec
functionCodeGenSpec = describe "Function Code Generation" $ do
  it "generates Go functions with multiple return values" $ do
    let pythonCode = T.unlines
          [ "def divide(a, b):"
          , "    if b == 0:"
          , "        return None, \"Division by zero\""
          , "    return a / b, None"
          ]
    result <- generateGoCode pythonCode
    case result of
      Right goCode -> do
        goCode `shouldContain` "func divide("
        goCode `shouldContain` "(float64, error)"
      Left err -> expectationFailure $ "Code generation failed: " ++ err
  
  it "generates Go functions with variadic parameters" $ do
    let pythonCode = T.unlines
          [ "def sum_all(*args):"
          , "    return sum(args)"
          ]
    result <- generateGoCode pythonCode
    case result of
      Right goCode -> do
        goCode `shouldContain` "func sumAll(args ...int)"
      Left err -> expectationFailure $ "Code generation failed: " ++ show err

errorHandlingSpec :: Spec
errorHandlingSpec = describe "Error Handling Code Generation" $ do
  it "generates Go error handling from Python exceptions" $ do
    let pythonCode = T.unlines
          [ "def process_file(filename):"
          , "    try:"
          , "        with open(filename) as f:"
          , "            return f.read()"
          , "    except FileNotFoundError:"
          , "        return \"\""
          ]
    result <- generateGoCode pythonCode
    case result of
      Right goCode -> do
        goCode `shouldContain` "if err != nil"
        goCode `shouldContain` "return"
      Left err -> expectationFailure $ "Code generation failed: " ++ show err
  
  it "generates Go panic handling" $ do
    let pythonCode = T.unlines
          [ "def risky_operation():"
          , "    try:"
          , "        result = dangerous_call()"
          , "        return result"
          , "    except Exception as e:"
          , "        return None"
          ]
    result <- generateGoCode pythonCode
    case result of
      Right goCode -> do
        goCode `shouldContain` "defer func()"
        goCode `shouldContain` "recover()"
      Left err -> expectationFailure $ "Code generation failed: " ++ show err