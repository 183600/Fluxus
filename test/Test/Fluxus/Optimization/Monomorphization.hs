{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Optimization.Monomorphization (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import Fluxus.Optimization.Monomorphization

spec :: Spec
spec = describe "Monomorphization Tests" $ do
  basicMonomorphizationSpec
  genericFunctionSpec
  genericClassSpec
  recursiveGenericSpec
  edgeCaseSpec

basicMonomorphizationSpec :: Spec
basicMonomorphizationSpec = describe "Basic Monomorphization" $ do
  it "monomorphizes simple generic functions" $ do
    let code = T.unlines
          [ "from typing import TypeVar, Generic"
          , "T = TypeVar('T')"
          , ""
          , "def identity(x: T) -> T:"
          , "    return x"
          , ""
          , "def func():"
          , "    result = identity(42)"
          , "    return result"
          ]
    result <- monomorphize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "identity_int"
        optimized `shouldContain` "return 42"
      Left err -> expectationFailure $ "Monomorphization failed: " ++ show err
  
  it "monomorphizes functions with multiple type parameters" $ do
    let code = T.unlines
          [ "from typing import TypeVar"
          , "T = TypeVar('T')"
          , "U = TypeVar('U')"
          , ""
          , "def pair(x: T, y: U) -> tuple[T, U]:"
          , "    return (x, y)"
          , ""
          , "def func():"
          , "    result = pair(42, \"hello\")"
          , "    return result"
          ]
    result <- monomorphize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "pair_int_str"
      Left err -> expectationFailure $ "Monomorphization failed: " ++ show err

genericFunctionSpec :: Spec
genericFunctionSpec = describe "Generic Function Monomorphization" $ do
  it "monomorphizes generic functions with constraints" $ do
    let code = T.unlines
          [ "from typing import TypeVar, Protocol"
          , ""
          , "class Addable(Protocol):"
          , "    def __add__(self, other):"
          , "        pass"
          , ""
          , "T = TypeVar('T', bound=Addable)"
          , ""
          , "def add(x: T, y: T) -> T:"
          , "    return x + y"
          , ""
          , "def func():"
          , "    result = add(10, 20)"
          , "    return result"
          ]
    result <- monomorphize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "add_int"
        optimized `shouldContain` "return 30"
      Left err -> expectationFailure $ "Monomorphization failed: " ++ show err
  
  it "handles multiple instantiations of the same generic" $ do
    let code = T.unlines
          [ "from typing import TypeVar"
          , "T = TypeVar('T')"
          , ""
          , "def process(x: T) -> T:"
          , "    return x"
          , ""
          , "def func():"
          , "    a = process(42)"
          , "    b = process(\"hello\")"
          , "    c = process(3.14)"
          , "    return a"
          ]
    result <- monomorphize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "process_int"
        optimized `shouldContain` "process_str"
        optimized `shouldContain` "process_float"
      Left err -> expectationFailure $ "Monomorphization failed: " ++ show err

genericClassSpec :: Spec
genericClassSpec = describe "Generic Class Monomorphization" $ do
  it "monomorphizes generic classes" $ do
    let code = T.unlines
          [ "from typing import TypeVar, Generic"
          , "T = TypeVar('T')"
          , ""
          , "class Box(Generic[T]):"
          , "    def __init__(self, value: T):"
          , "        self.value = value"
          , "    def get(self) -> T:"
          , "        return self.value"
          , ""
          , "def func():"
          , "    box = Box(42)"
          , "    return box.get()"
          ]
    result <- monomorphize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "Box_int"
        optimized `shouldContain` "return 42"
      Left err -> expectationFailure $ "Monomorphization failed: " ++ show err
  
  it "monomorphizes generic class methods" $ do
    let code = T.unlines
          [ "from typing import TypeVar, Generic"
          , "T = TypeVar('T')"
          , ""
          , "class Container(Generic[T]):"
          , "    def __init__(self):"
          , "        self.items = []"
          , "    def add(self, item: T):"
          , "        self.items.append(item)"
          , "    def get_first(self) -> T:"
          , "        return self.items[0]"
          , ""
          , "def func():"
          , "    container = Container[str]()"
          , "    container.add(\"hello\")"
          , "    return container.get_first()"
          ]
    result <- monomorphize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "Container_str"
      Left err -> expectationFailure $ "Monomorphization failed: " ++ show err

recursiveGenericSpec :: Spec
recursiveGenericSpec = describe "Recursive Generic Monomorphization" $ do
  it "handles recursive generic functions" $ do
    let code = T.unlines
          [ "from typing import TypeVar"
          , "T = TypeVar('T')"
          , ""
          , "def length(lst: list[T]) -> int:"
          , "    if not lst:"
          , "        return 0"
          , "    else:"
          , "        return 1 + length(lst[1:])"
          , ""
          , "def func():"
          , "    result = length([1, 2, 3])"
          , "    return result"
          ]
    result <- monomorphize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "length_list_int"
        optimized `shouldContain` "return 3"
      Left err -> expectationFailure $ "Monomorphization failed: " ++ show err

edgeCaseSpec :: Spec
edgeCaseSpec = describe "Edge Cases" $ do
  it "handles generic type bounds" $ do
    let code = T.unlines
          [ "from typing import TypeVar"
          , "T = TypeVar('T', bound=int)"
          , ""
          , "def square(x: T) -> T:"
          , "    return x * x"
          , ""
          , "def func():"
          , "    result = square(5)"
          , "    return result"
          ]
    result <- monomorphize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "square_int"
        optimized `shouldContain` "return 25"
      Left err -> expectationFailure $ "Monomorphization failed: " ++ show err
  
  it "handles union types in generics" $ do
    let code = T.unlines
          [ "from typing import TypeVar, Union"
          , "T = TypeVar('T')"
          , ""
          , "def process(x: Union[T, None]) -> T:"
          , "    if x is None:"
          , "        raise ValueError(\"None value\")"
          , "    return x"
          , ""
          , "def func():"
          , "    result = process(42)"
          , "    return result"
          ]
    result <- monomorphize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "process_int"
      Left err -> expectationFailure $ "Monomorphization failed: " ++ show err