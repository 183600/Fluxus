{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Optimization.Vectorization (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import Fluxus.Optimization.Vectorization

spec :: Spec
spec = describe "Vectorization Tests" $ do
  basicVectorizationSpec
  loopVectorizationSpec
  arrayOperationSpec
  mathematicalVectorizationSpec
  edgeCaseSpec

basicVectorizationSpec :: Spec
basicVectorizationSpec = describe "Basic Vectorization" $ do
  it "vectorizes simple arithmetic operations" $ do
    let code = T.unlines
          [ "def func():"
          , "    a = [1, 2, 3, 4]"
          , "    b = [5, 6, 7, 8]"
          , "    c = []"
          , "    for i in range(len(a)):"
          , "        c.append(a[i] + b[i])"
          , "    return c"
          ]
    result <- vectorize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "vector_add"
      Left err -> expectationFailure $ "Vectorization failed: " ++ show err
  
  it "vectorizes element-wise operations" $ do
    let code = T.unlines
          [ "def func():"
          , "    data = [1, 2, 3, 4, 5]"
          , "    result = []"
          , "    for x in data:"
          , "        result.append(x * 2)"
          , "    return result"
          ]
    result <- vectorize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "vector_mul"
      Left err -> expectationFailure $ "Vectorization failed: " ++ show err

loopVectorizationSpec :: Spec
loopVectorizationSpec = describe "Loop Vectorization" $ do
  it "vectorizes simple for loops" $ do
    let code = T.unlines
          [ "def func():"
          , "    result = [0] * 100"
          , "    for i in range(100):"
          , "        result[i] = i * i"
          , "    return result"
          ]
    result <- vectorize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "vectorized_loop"
      Left err -> expectationFailure $ "Vectorization failed: " ++ show err
  
  it "vectorizes loops with multiple operations" $ do
    let code = T.unlines
          [ "def func():"
          , "    a = [1, 2, 3, 4]"
          , "    b = [5, 6, 7, 8]"
          , "    c = [0] * 4"
          , "    for i in range(4):"
          , "        c[i] = a[i] * 2 + b[i]"
          , "    return c"
          ]
    result <- vectorize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "vector_fma"
      Left err -> expectationFailure $ "Vectorization failed: " ++ show err

arrayOperationSpec :: Spec
arrayOperationSpec = describe "Array Operation Vectorization" $ do
  it "vectorizes array reductions" $ do
    let code = T.unlines
          [ "def func():"
          , "    data = [1, 2, 3, 4, 5]"
          , "    sum = 0"
          , "    for x in data:"
          , "        sum += x"
          , "    return sum"
          ]
    result <- vectorize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "vector_reduce_sum"
      Left err -> expectationFailure $ "Vectorization failed: " ++ show err
  
  it "vectorizes array transformations" $ do
    let code = T.unlines
          [ "def func():"
          , "    data = [1, 2, 3, 4, 5]"
          , "    result = []"
          , "    for x in data:"
          , "        result.append(x + 1)"
          , "    return result"
          ]
    result <- vectorize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "vector_map"
      Left err -> expectationFailure $ "Vectorization failed: " ++ show err

mathematicalVectorizationSpec :: Spec
mathematicalVectorizationSpec = describe "Mathematical Vectorization" $ do
  it "vectorizes mathematical functions" $ do
    let code = T.unlines
          [ "import math"
          , ""
          , "def func():"
          , "    angles = [0, 1, 2, 3]"
          , "    result = []"
          , "    for x in angles:"
          , "        result.append(math.sin(x))"
          , "    return result"
          ]
    result <- vectorize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "vector_sin"
      Left err -> expectationFailure $ "Vectorization failed: " ++ show err
  
  it "vectorizes complex mathematical expressions" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = [1, 2, 3, 4]"
          , "    y = [5, 6, 7, 8]"
          , "    result = []"
          , "    for i in range(len(x)):"
          , "        result.append(x[i] * x[i] + y[i] * y[i])"
          , "    return result"
          ]
    result <- vectorize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "vector_magnitude_squared"
      Left err -> expectationFailure $ "Vectorization failed: " ++ show err

edgeCaseSpec :: Spec
edgeCaseSpec = describe "Edge Cases" $ do
  it "handles non-vectorizable loops gracefully" $ do
    let code = T.unlines
          [ "def func():"
          , "    result = []"
          , "    for i in range(10):"
          , "        if i % 2 == 0:"
          , "            result.append(i)"
          , "    return result"
          ]
    result <- vectorize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "for i in range(10)"
      Left err -> expectationFailure $ "Vectorization failed: " ++ show err
  
  it "handles loops with dependencies" $ do
    let code = T.unlines
          [ "def func():"
          , "    fib = [0] * 10"
          , "    fib[0] = 0"
          , "    fib[1] = 1"
          , "    for i in range(2, 10):"
          , "        fib[i] = fib[i-1] + fib[i-2]"
          , "    return fib"
          ]
    result <- vectorize code
    case result of
      Right optimized -> do
        optimized `shouldContain` "for i in range(2, 10)"
      Left err -> expectationFailure $ "Vectorization failed: " ++ show err