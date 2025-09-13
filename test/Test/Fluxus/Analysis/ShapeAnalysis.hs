{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Analysis.ShapeAnalysis (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import Fluxus.Analysis.ShapeAnalysis

spec :: Spec
spec = describe "Shape Analysis Tests" $ do
  basicShapeAnalysisSpec
  containerShapeSpec
  functionShapeSpec
  edgeCaseSpec

basicShapeAnalysisSpec :: Spec
basicShapeAnalysisSpec = describe "Basic Shape Analysis" $ do
  it "infers shape of primitive values" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = 42"
          , "    y = \"hello\""
          , "    z = True"
          , "    return x"
          ]
    result <- analyzeShape code
    case result of
      Right analysis -> do
        shapeX <- getVariableShape analysis "x"
        shapeY <- getVariableShape analysis "y"
        shapeZ <- getVariableShape analysis "z"
        shapeX `shouldBe` ScalarShape
        shapeY `shouldBe` ScalarShape
        shapeZ `shouldBe` ScalarShape
      Left err -> expectationFailure $ "Analysis failed: " ++ show err
  
  it "infers shape of lists" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = [1, 2, 3]"
          , "    y = []"
          , "    return x"
          ]
    result <- analyzeShape code
    case result of
      Right analysis -> do
        shapeX <- getVariableShape analysis "x"
        shapeY <- getVariableShape analysis "y"
        shapeX `shouldBe` ListShape
        shapeY `shouldBe` ListShape
      Left err -> expectationFailure $ "Analysis failed: " ++ show err

containerShapeSpec :: Spec
containerShapeSpec = describe "Container Shape Analysis" $ do
  it "infers shape of dictionaries" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = {'a': 1, 'b': 2}"
          , "    y = {}"
          , "    return x"
          ]
    result <- analyzeShape code
    case result of
      Right analysis -> do
        shapeX <- getVariableShape analysis "x"
        shapeY <- getVariableShape analysis "y"
        shapeX `shouldBe` DictShape
        shapeY `shouldBe` DictShape
      Left err -> expectationFailure $ "Analysis failed: " ++ show err
  
  it "infers nested container shapes" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = {'items': [1, 2, 3], 'meta': {}}"
          , "    return x"
          ]
    result <- analyzeShape code
    case result of
      Right analysis -> do
        shapeX <- getVariableShape analysis "x"
        shapeX `shouldBe` DictShape
      Left err -> expectationFailure $ "Analysis failed: " ++ show err

functionShapeSpec :: Spec
functionShapeSpec = describe "Function Shape Analysis" $ do
  it "infers shape of function objects" $ do
    let code = T.unlines
          [ "def func():"
          , "    def inner(x):"
          , "        return x + 1"
          , "    return inner"
          ]
    result <- analyzeShape code
    case result of
      Right analysis -> do
        shapeInner <- getVariableShape analysis "inner"
        shapeInner `shouldBe` FunctionShape
      Left err -> expectationFailure $ "Analysis failed: " ++ show err
  
  it "infers shape of lambda functions" $ do
    let code = T.unlines
          [ "def func():"
          , "    f = lambda x: x + 1"
          , "    return f"
          ]
    result <- analyzeShape code
    case result of
      Right analysis -> do
        shapeF <- getVariableShape analysis "f"
        shapeF `shouldBe` FunctionShape
      Left err -> expectationFailure $ "Analysis failed: " ++ show err

edgeCaseSpec :: Spec
edgeCaseSpec = describe "Edge Cases" $ do
  it "handles dynamic typing scenarios" $ do
    let code = T.unlines
          [ "def func(x):"
          , "    if isinstance(x, list):"
          , "        return len(x)"
          , "    else:"
          , "        return x"
          ]
    result <- analyzeShape code
    case result of
      Right analysis -> do
        shapeX <- getVariableShape analysis "x"
        shapeX `shouldBe` DynamicShape
      Left err -> expectationFailure $ "Analysis failed: " ++ show err
  
  it "handles None values" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = None"
          , "    return x"
          ]
    result <- analyzeShape code
    case result of
      Right analysis -> do
        shapeX <- getVariableShape analysis "x"
        shapeX `shouldBe` ScalarShape
      Left err -> expectationFailure $ "Analysis failed: " ++ show err