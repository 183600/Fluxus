{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Optimization.SizeReduction (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import Fluxus.Optimization.SizeReduction

spec :: Spec
spec = describe "Size Reduction Tests" $ do
  basicSizeReductionSpec
  deadCodeEliminationSpec
  functionInliningSpec
  dataStructureOptimizationSpec
  edgeCaseSpec

basicSizeReductionSpec :: Spec
basicSizeReductionSpec = describe "Basic Size Reduction" $ do
  it "eliminates unused variables" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = 42"
          , "    y = 100"
          , "    z = 200"
          , "    return y"
          ]
    result <- reduceSize code
    case result of
      Right optimized -> do
        optimized `shouldNotContain` "x = 42"
        optimized `shouldNotContain` "z = 200"
        optimized `shouldContain` "y = 100"
      Left err -> expectationFailure $ "Size reduction failed: " ++ show err
  
  it "eliminates redundant assignments" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = 10"
          , "    x = 20"
          , "    x = 30"
          , "    return x"
          ]
    result <- reduceSize code
    case result of
      Right optimized -> do
        optimized `shouldNotContain` "x = 10"
        optimized `shouldNotContain` "x = 20"
        optimized `shouldContain` "x = 30"
      Left err -> expectationFailure $ "Size reduction failed: " ++ show err

deadCodeEliminationSpec :: Spec
deadCodeEliminationSpec = describe "Dead Code Elimination" $ do
  it "eliminates unreachable code" $ do
    let code = T.unlines
          [ "def func():"
          , "    return 42"
          , "    x = 100"
          , "    y = 200"
          , "    return x + y"
          ]
    result <- reduceSize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "return 42"
        optimized `shouldNotContain` "x = 100"
        optimized `shouldNotContain` "y = 200"
      Left err -> expectationFailure $ "Size reduction failed: " ++ show err
  
  it "eliminates dead branches" $ do
    let code = T.unlines
          [ "def func():"
          , "    if True:"
          , "        return 42"
          , "    else:"
          , "        return 100"
          ]
    result <- reduceSize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "return 42"
        optimized `shouldNotContain` "else:"
        optimized `shouldNotContain` "return 100"
      Left err -> expectationFailure $ "Size reduction failed: " ++ show err

functionInliningSpec :: Spec
functionInliningSpec = describe "Function Inlining" $ do
  it "inlines small functions" $ do
    let code = T.unlines
          [ "def helper(x):"
          , "    return x + 1"
          , ""
          , "def func():"
          , "    result = helper(41)"
          , "    return result"
          ]
    result <- reduceSize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "result = 41 + 1"
        optimized `shouldContain` "return 42"
      Left err -> expectationFailure $ "Size reduction failed: " ++ show err
  
  it "inlines functions with constant arguments" $ do
    let code = T.unlines
          [ "def multiply(a, b):"
          , "    return a * b"
          , ""
          , "def func():"
          , "    result = multiply(6, 7)"
          , "    return result"
          ]
    result <- reduceSize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "result = 42"
      Left err -> expectationFailure $ "Size reduction failed: " ++ show err

dataStructureOptimizationSpec :: Spec
dataStructureOptimizationSpec = describe "Data Structure Optimization" $ do
  it "optimizes list operations" $ do
    let code = T.unlines
          [ "def func():"
          , "    lst = []"
          , "    lst.append(1)"
          , "    lst.append(2)"
          , "    lst.append(3)"
          , "    return lst"
          ]
    result <- reduceSize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "lst = [1, 2, 3]"
      Left err -> expectationFailure $ "Size reduction failed: " ++ show err
  
  it "optimizes string concatenation" $ do
    let code = T.unlines
          [ "def func():"
          , "    result = \"\""
          , "    result += \"hello\""
          , "    result += \" \""
          , "    result += \"world\""
          , "    return result"
          ]
    result <- reduceSize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "result = \"hello world\""
      Left err -> expectationFailure $ "Size reduction failed: " ++ show err

edgeCaseSpec :: Spec
edgeCaseSpec = describe "Edge Cases" $ do
  it "preserves side effects" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = print(\"hello\")"
          , "    y = 42"
          , "    return y"
          ]
    result <- reduceSize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "print(\"hello\")"
      Left err -> expectationFailure $ "Size reduction failed: " ++ show err
  
  it "handles loops with optimizations" $ do
    let code = T.unlines
          [ "def func():"
          , "    result = 0"
          , "    for i in range(10):"
          , "        result += 1"
          , "    return result"
          ]
    result <- reduceSize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "return 10"
      Left err -> expectationFailure $ "Size reduction failed: " ++ show err