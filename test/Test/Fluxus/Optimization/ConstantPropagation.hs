{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Optimization.ConstantPropagation (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import Fluxus.Optimization.ConstantPropagation

spec :: Spec
spec = describe "Constant Propagation Tests" $ do
  basicConstantPropagationSpec
  conditionalConstantPropagationSpec
  loopConstantPropagationSpec
  interproceduralConstantPropagationSpec
  edgeCaseSpec

basicConstantPropagationSpec :: Spec
basicConstantPropagationSpec = describe "Basic Constant Propagation" $ do
  it "propagates simple constants" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = 42"
          , "    y = x"
          , "    return y"
          ]
    result <- propagateConstants code
    case result of
      Right optimized -> do
        optimized `shouldContain` "return 42"
      Left err -> expectationFailure $ "Optimization failed: " ++ show err
  
  it "propagates through arithmetic operations" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = 10"
          , "    y = 20"
          , "    z = x + y"
          , "    return z"
          ]
    result <- propagateConstants code
    case result of
      Right optimized -> do
        optimized `shouldContain` "return 30"
      Left err -> expectationFailure $ "Optimization failed: " ++ show err

conditionalConstantPropagationSpec :: Spec
conditionalConstantPropagationSpec = describe "Conditional Constant Propagation" $ do
  it "propagates constants in conditional branches" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = 42"
          , "    if x > 0:"
          , "        y = x + 1"
          , "    else:"
          , "        y = 0"
          , "    return y"
          ]
    result <- propagateConstants code
    case result of
      Right optimized -> do
        optimized `shouldContain` "return 43"
      Left err -> expectationFailure $ "Optimization failed: " ++ show err
  
  it "handles unknown conditions" $ do
    let code = T.unlines
          [ "def func(x):"
          , "    if x > 0:"
          , "        y = 42"
          , "    else:"
          , "        y = 0"
          , "    return y"
          ]
    result <- propagateConstants code
    case result of
      Right optimized -> do
        optimized `shouldContain` "if x > 0"
        optimized `shouldContain` "y = 42"
      Left err -> expectationFailure $ "Optimization failed: " ++ show err

loopConstantPropagationSpec :: Spec
loopConstantPropagationSpec = describe "Loop Constant Propagation" $ do
  it "propagates constants in simple loops" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = 0"
          , "    for i in range(5):"
          , "        x = x + 1"
          , "    return x"
          ]
    result <- propagateConstants code
    case result of
      Right optimized -> do
        optimized `shouldContain` "return 5"
      Left err -> expectationFailure $ "Optimization failed: " ++ show err
  
  it "handles loop-carried dependencies" $ do
    let code = T.unlines
          [ "def func():"
          , "    result = 1"
          , "    for i in range(5):"
          , "        result = result * 2"
          , "    return result"
          ]
    result <- propagateConstants code
    case result of
      Right optimized -> do
        optimized `shouldContain` "return 32"
      Left err -> expectationFailure $ "Optimization failed: " ++ show err

interproceduralConstantPropagationSpec :: Spec
interproceduralConstantPropagationSpec = describe "Interprocedural Constant Propagation" $ do
  it "propagates constants across function calls" $ do
    let code = T.unlines
          [ "def helper(x):"
          , "    return x + 1"
          , ""
          , "def func():"
          , "    y = helper(41)"
          , "    return y"
          ]
    result <- propagateConstants code
    case result of
      Right optimized -> do
        optimized `shouldContain` "return 42"
      Left err -> expectationFailure $ "Optimization failed: " ++ show err
  
  it "handles constant arguments" $ do
    let code = T.unlines
          [ "def multiply(a, b):"
          , "    return a * b"
          , ""
          , "def func():"
          , "    result = multiply(6, 7)"
          , "    return result"
          ]
    result <- propagateConstants code
    case result of
      Right optimized -> do
        optimized `shouldContain` "return 42"
      Left err -> expectationFailure $ "Optimization failed: " ++ show err

edgeCaseSpec :: Spec
edgeCaseSpec = describe "Edge Cases" $ do
  it "handles global constants" $ do
    let code = T.unlines
          [ "GLOBAL_CONST = 100"
          , ""
          , "def func():"
          , "    x = GLOBAL_CONST"
          , "    return x"
          ]
    result <- propagateConstants code
    case result of
      Right optimized -> do
        optimized `shouldContain` "return 100"
      Left err -> expectationFailure $ "Optimization failed: " ++ show err
  
  it "handles constant folding combined with propagation" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = 10"
          , "    y = 20"
          , "    z = x + y * 2"
          , "    return z"
          ]
    result <- propagateConstants code
    case result of
      Right optimized -> do
        optimized `shouldContain` "return 50"
      Left err -> expectationFailure $ "Optimization failed: " ++ show err