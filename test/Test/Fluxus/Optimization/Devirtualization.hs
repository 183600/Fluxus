{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Optimization.Devirtualization (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import Fluxus.Optimization.Devirtualization

spec :: Spec
spec = describe "Devirtualization Tests" $ do
  basicDevirtualizationSpec
  interfaceDevirtualizationSpec
  inheritanceDevirtualizationSpec
  partialDevirtualizationSpec
  edgeCaseSpec

basicDevirtualizationSpec :: Spec
basicDevirtualizationSpec = describe "Basic Devirtualization" $ do
  it "devirtualizes direct method calls" $ do
    let code = T.unlines
          [ "class Animal:"
          , "    def speak(self):"
          , "        pass"
          , ""
          , "class Dog(Animal):"
          , "    def speak(self):"
          , "        return \"Woof!\""
          , ""
          , "def func():"
          , "    dog = Dog()"
          , "    return dog.speak()"
          ]
    result <- devirtualize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "Dog_speak"
      Left err -> expectationFailure $ "Devirtualization failed: " ++ show err
  
  it "preserves virtual calls when necessary" $ do
    let code = T.unlines
          [ "class Animal:"
          , "    def speak(self):"
          , "        pass"
          , ""
          , "class Dog(Animal):"
          , "    def speak(self):"
          , "        return \"Woof!\""
          , ""
          , "class Cat(Animal):"
          , "    def speak(self):"
          , "        return \"Meow!\""
          , ""
          , "def make_animal_speak(animal):"
          , "    return animal.speak()"
          ]
    result <- devirtualize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "animal.speak()"
      Left err -> expectationFailure $ "Devirtualization failed: " ++ show err

interfaceDevirtualizationSpec :: Spec
interfaceDevirtualizationSpec = describe "Interface Devirtualization" $ do
  it "devirtualizes interface calls with known implementations" $ do
    let code = T.unlines
          [ "from abc import ABC, abstractmethod"
          , ""
          , "class Shape(ABC):"
          , "    @abstractmethod"
          , "    def area(self):"
          , "        pass"
          , ""
          , "class Circle(Shape):"
          , "    def __init__(self, radius):"
          , "        self.radius = radius"
          , "    def area(self):"
          , "        return 3.14 * self.radius * self.radius"
          , ""
          , "def func():"
          , "    circle = Circle(5)"
          , "    return circle.area()"
          ]
    result <- devirtualize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "Circle_area"
      Left err -> expectationFailure $ "Devirtualization failed: " ++ show err
  
  it "handles multiple interface implementations" $ do
    let code = T.unlines
          [ "class Drawable:"
          , "    def draw(self):"
          , "        pass"
          , ""
          , "class Shape:"
          , "    def area(self):"
          , "        pass"
          , ""
          , "class Circle(Drawable, Shape):"
          , "    def draw(self):"
          , "        return \"Drawing circle\""
          , "    def area(self):"
          , "        return 3.14 * self.radius * self.radius"
          , ""
          , "def func():"
          , "    circle = Circle()"
          , "    return circle.draw()"
          ]
    result <- devirtualize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "Circle_draw"
      Left err -> expectationFailure $ "Devirtualization failed: " ++ show err

inheritanceDevirtualizationSpec :: Spec
inheritanceDevirtualizationSpec = describe "Inheritance Devirtualization" $ do
  it "devirtualizes calls in inheritance hierarchies" $ do
    let code = T.unlines
          [ "class Base:"
          , "    def method(self):"
          , "        return \"Base\""
          , ""
          , "class Derived(Base):"
          , "    def method(self):"
          , "        return \"Derived\""
          , ""
          , "class Final(Derived):"
          , "    pass"
          , ""
          , "def func():"
          , "    obj = Final()"
          , "    return obj.method()"
          ]
    result <- devirtualize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "Derived_method"
      Left err -> expectationFailure $ "Devirtualization failed: " ++ show err
  
  it "handles method overriding" $ do
    let code = T.unlines
          [ "class Base:"
          , "    def virtual_method(self):"
          , "        return \"Base\""
          , "    def non_virtual_method(self):"
          , "        return \"Non-virtual\""
          , ""
          , "class Derived(Base):"
          , "    def virtual_method(self):"
          , "        return \"Derived\""
          , ""
          , "def func():"
          , "    obj = Derived()"
          , "    return obj.virtual_method()"
          ]
    result <- devirtualize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "Derived_virtual_method"
      Left err -> expectationFailure $ "Devirtualization failed: " ++ show err

partialDevirtualizationSpec :: Spec
partialDevirtualizationSpec = describe "Partial Devirtualization" $ do
  it "performs partial devirtualization when possible" $ do
    let code = T.unlines
          [ "class Animal:"
          , "    def speak(self):"
          , "        pass"
          , ""
          , "class Dog(Animal):"
          , "    def speak(self):"
          , "        return \"Woof!\""
          , ""
          , "def func(is_dog):"
          , "    if is_dog:"
          , "        animal = Dog()"
          , "    else:"
          , "        animal = Animal()"
          , "    return animal.speak()"
          ]
    result <- devirtualize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "if is_dog"
        optimized `shouldContain` "Dog_speak"
      Left err -> expectationFailure $ "Devirtualization failed: " ++ show err

edgeCaseSpec :: Spec
edgeCaseSpec = describe "Edge Cases" $ do
  it "handles diamond inheritance" $ do
    let code = T.unlines
          [ "class A:"
          , "    def method(self):"
          , "        return \"A\""
          , ""
          , "class B(A):"
          , "    def method(self):"
          , "        return \"B\""
          , ""
          , "class C(A):"
          , "    def method(self):"
          , "        return \"C\""
          , ""
          , "class D(B, C):"
          , "    pass"
          , ""
          , "def func():"
          , "    obj = D()"
          , "    return obj.method()"
          ]
    result <- devirtualize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "B_method"
      Left err -> expectationFailure $ "Devirtualization failed: " ++ show err
  
  it "handles dynamic method calls" $ do
    let code = T.unlines
          [ "class Dynamic:"
          , "    def __getattr__(self, name):"
          , "        return lambda: \"Dynamic method\""
          , ""
          , "def func():"
          , "    obj = Dynamic()"
          , "    return obj.unknown_method()"
          ]
    result <- devirtualize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "__getattr__"
      Left err -> expectationFailure $ "Devirtualization failed: " ++ show err