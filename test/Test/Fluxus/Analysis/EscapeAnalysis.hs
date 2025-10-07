{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Analysis.EscapeAnalysis (spec) where

import Test.Hspec
import Data.Text ()
import qualified Data.Text as T

import Fluxus.Analysis.EscapeAnalysis
import Fluxus.AST.Common (Identifier(..))

spec :: Spec
spec = describe "Escape Analysis Tests" $ do
  basicEscapeAnalysisSpec
  complexEscapeAnalysisSpec
  edgeCaseSpec

basicEscapeAnalysisSpec :: Spec
basicEscapeAnalysisSpec = describe "Basic Escape Analysis" $ do
  it "identifies non-escaping local variables" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = 42"
          , "    return x + 1"
          ]
    result <- analyzeEscape code
    case result of
      Right analysis -> do
        escapes <- getEscapingVariables analysis
        escapes `shouldBe` []
      Left err -> expectationFailure $ "Analysis failed: " ++ show err
  
  it "identifies escaping variables" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = [1, 2, 3]"
          , "    return x"
          ]
    result <- analyzeEscape code
    case result of
      Right analysis -> do
        escapes <- getEscapingVariables analysis
        escapes `shouldContain` [Identifier "x"]
      Left err -> expectationFailure $ "Analysis failed: " ++ show err

complexEscapeAnalysisSpec :: Spec
complexEscapeAnalysisSpec = describe "Complex Escape Analysis" $ do
  it "analyzes escape behavior in nested functions" $ do
    let code = T.unlines
          [ "def outer():"
          , "    x = 42"
          , "    def inner():"
          , "        return x + 1"
          , "    return inner"
          ]
    result <- analyzeEscape code
    case result of
      Right analysis -> do
        escapes <- getEscapingVariables analysis
        escapes `shouldContain` [Identifier "x"]
      Left err -> expectationFailure $ "Analysis failed: " ++ show err
  
  it "analyzes escape behavior in data structures" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = 42"
          , "    y = {'value': x}"
          , "    return y"
          ]
    result <- analyzeEscape code
    case result of
      Right analysis -> do
        escapes <- getEscapingVariables analysis
        escapes `shouldContain` [Identifier "y"]
        indirectEscapes <- getIndirectlyEscapingVariables analysis
        indirectEscapes `shouldContain` [Identifier "x"]
      Left err -> expectationFailure $ "Analysis failed: " ++ show err

edgeCaseSpec :: Spec
edgeCaseSpec = describe "Edge Cases" $ do
  it "handles empty functions" $ do
    let code = T.unlines
          [ "def func():"
          , "    pass"
          ]
    result <- analyzeEscape code
    case result of
      Right analysis -> do
        escapes <- getEscapingVariables analysis
        escapes `shouldBe` []
      Left err -> expectationFailure $ "Analysis failed: " ++ show err
  
  it "handles recursive functions" $ do
    let code = T.unlines
          [ "def factorial(n):"
          , "    if n <= 1:"
          , "        return 1"
          , "    else:"
          , "        return n * factorial(n - 1)"
          ]
    result <- analyzeEscape code
    case result of
      Right _ -> return ()
      Left err -> expectationFailure $ "Analysis failed: " ++ show err