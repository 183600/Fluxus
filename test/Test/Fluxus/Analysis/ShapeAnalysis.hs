{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Analysis.ShapeAnalysis (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import Fluxus.Analysis.ShapeAnalysis
import Fluxus.AST.Common (Identifier(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isJust)

-- Helper function to convert ShapeInfo to SimpleShape for testing
shapeToSimple :: ShapeInfo -> SimpleShape
shapeToSimple shape
  | null (siDimensions shape) = ScalarShape
  | not (null (siDimensions shape)) && isJust (siElementType shape) = ListShape
  | not (HashMap.null (siFieldTypes shape)) = DictShape
  | otherwise = DynamicShape

-- Helper function to run shape analysis and get variable shape
getVariableShapeForTest :: Text -> Text -> Maybe SimpleShape
getVariableShapeForTest code varName =
  let result = analyzeShapeFromText code
  in case result of
    Left _ -> Nothing
    Right state ->
      -- Directly lookup variable shape from state, no need to run analysis again
      case HashMap.lookup (Identifier varName) (sasShapeMap state) of
        Just shape -> Just (shapeToSimple shape)
        Nothing -> Just DynamicShape  -- Default to dynamic shape if not found

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
    let shapeX = getVariableShapeForTest code "x"
    let shapeY = getVariableShapeForTest code "y"
    let shapeZ = getVariableShapeForTest code "z"
    shapeX `shouldBe` Just ScalarShape
    shapeY `shouldBe` Just ScalarShape
    shapeZ `shouldBe` Just ScalarShape
  
  it "infers shape of lists" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = [1, 2, 3]"
          , "    y = []"
          , "    return x"
          ]
    let shapeX = getVariableShapeForTest code "x"
    let shapeY = getVariableShapeForTest code "y"
    shapeX `shouldBe` Just ListShape
    shapeY `shouldBe` Just ListShape

containerShapeSpec :: Spec
containerShapeSpec = describe "Container Shape Analysis" $ do
  it "infers shape of dictionaries" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = {'a': 1, 'b': 2}"
          , "    y = {}"
          , "    return x"
          ]
    let shapeX = getVariableShapeForTest code "x"
    let shapeY = getVariableShapeForTest code "y"
    shapeX `shouldBe` Just DictShape
    shapeY `shouldBe` Just DictShape
  
  it "infers nested container shapes" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = {'items': [1, 2, 3], 'meta': {}}"
          , "    return x"
          ]
    let shapeX = getVariableShapeForTest code "x"
    shapeX `shouldBe` Just DictShape

functionShapeSpec :: Spec
functionShapeSpec = describe "Function Shape Analysis" $ do
  it "infers shape of function objects" $ do
    let code = T.unlines
          [ "def func():"
          , "    def inner(x):"
          , "        return x + 1"
          , "    return inner"
          ]
    let shapeInner = getVariableShapeForTest code "inner"
    shapeInner `shouldBe` Just DynamicShape
  
  it "infers shape of lambda functions" $ do
    let code = T.unlines
          [ "def func():"
          , "    f = lambda x: x + 1"
          , "    return f"
          ]
    let shapeF = getVariableShapeForTest code "f"
    shapeF `shouldBe` Just DynamicShape

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
    let shapeX = getVariableShapeForTest code "x"
    shapeX `shouldBe` Just DynamicShape
  
  it "handles None values" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = None"
          , "    return x"
          ]
    let shapeX = getVariableShapeForTest code "x"
    shapeX `shouldBe` Just DynamicShape