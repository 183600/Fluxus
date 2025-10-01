{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Analysis.SmartFallback (spec) where

import Test.Hspec
import qualified Data.Text as T

import Fluxus.Analysis.SmartFallback

spec :: Spec
spec = describe "Smart Fallback Tests" $ do
  basicFallbackSpec
  optimizationFallbackSpec
  errorRecoverySpec
  edgeCaseSpec

basicFallbackSpec :: Spec
basicFallbackSpec = describe "Basic Fallback Mechanisms" $ do
  it "falls back to interpreter for unsupported constructs" $ do
    let code = T.unlines
          [ "def func():"
          , "    exec('print(42)')"
          , "    return 42"
          ]
    let result = analyzeWithFallback code
    case result of
      Right strategy -> do
        let fallbackPoints = getFallbackPoints strategy
        fallbackPoints `shouldNotBe` []
      Left err -> expectationFailure $ "Analysis failed: " ++ show err
  
  it "identifies safe optimization regions" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = 1 + 2"
          , "    y = x * 3"
          , "    return y"
          ]
    let result = analyzeWithFallback code
    case result of
      Right strategy -> do
        let safeRegions = getSafeOptimizationRegions strategy
        safeRegions `shouldNotBe` []
      Left err -> expectationFailure $ "Analysis failed: " ++ show err

optimizationFallbackSpec :: Spec
optimizationFallbackSpec = describe "Optimization Fallback" $ do
  it "handles optimization failures gracefully" $ do
    let code = T.unlines
          [ "def func():"
          , "    try:"
          , "        # Complex optimization that might fail"
          , "        result = complex_operation()"
          , "    except Exception:"
          , "        result = fallback_operation()"
          , "    return result"
          ]
    let result = analyzeWithFallback code
    case result of
      Right strategy -> do
        let hasFallback = hasOptimizationFallback strategy
        hasFallback `shouldBe` True
      Left err -> expectationFailure $ "Analysis failed: " ++ show err
  
  it "preserves semantics when falling back" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = [1, 2, 3]"
          , "    # This might be optimized or fall back"
          , "    y = map(lambda i: i * 2, x)"
          , "    return list(y)"
          ]
    let result = analyzeWithFallback code
    case result of
      Right strategy -> do
        let semanticsPreserved = checkSemanticPreservation strategy
        semanticsPreserved `shouldBe` True
      Left err -> expectationFailure $ "Analysis failed: " ++ show err

errorRecoverySpec :: Spec
errorRecoverySpec = describe "Error Recovery" $ do
  it "recovers from analysis errors" $ do
    let code = T.unlines
          [ "def func():"
          , "    # Potentially problematic code"
          , "    x = unknown_function()"
          , "    return x"
          ]
    let result = analyzeWithFallback code
    case result of
      Right strategy -> do
        let canRecover = canRecoverFromErrors strategy
        canRecover `shouldBe` True
      Left err -> expectationFailure $ "Analysis failed: " ++ show err
  
  it "provides meaningful error messages" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = 1 / 0"
          , "    return x"
          ]
    let result = analyzeWithFallback code
    case result of
      Right strategy -> do
        let errors = getAnalysisErrors strategy
        errors `shouldNotBe` []
      Left err -> expectationFailure $ "Analysis failed: " ++ show err

edgeCaseSpec :: Spec
edgeCaseSpec = describe "Edge Cases" $ do
  it "handles recursive functions with fallback" $ do
    let code = T.unlines
          [ "def factorial(n):"
          , "    if n <= 1:"
          , "        return 1"
          , "    else:"
          , "        return n * factorial(n - 1)"
          ]
    let result = analyzeWithFallback code
    case result of
      Right strategy -> do
        let canHandle = canHandleRecursion strategy
        canHandle `shouldBe` True
      Left err -> expectationFailure $ "Analysis failed: " ++ show err
  
  it "handles dynamic code execution" $ do
    let code = T.unlines
          [ "def func():"
          , "    code = \"print('dynamic')\""
          , "    exec(code)"
          , "    return 42"
          ]
    let result = analyzeWithFallback code
    case result of
      Right strategy -> do
        let needsFallback = needsInterpreterFallback strategy
        needsFallback `shouldBe` True
      Left err -> expectationFailure $ "Analysis failed: " ++ show err