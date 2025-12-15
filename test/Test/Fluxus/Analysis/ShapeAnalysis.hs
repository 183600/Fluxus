{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Analysis.ShapeAnalysis (spec) where

import Control.Monad.State (modify)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Test.Hspec

import Fluxus.AST.Common
import Fluxus.Analysis.ShapeAnalysis

intLiteral :: Int -> CommonExpr
intLiteral n = CELiteral (LInt (fromIntegral (n :: Int)))

listOfLength :: Int -> CommonExpr
listOfLength n = CEList (map (noLoc . intLiteral) [1 .. n])

spec :: Spec
spec = describe "Fluxus.Analysis.ShapeAnalysis" $ do
  describe "analyzeShape" $ do
    it "combines list dimensions when concatenating" $ do
      let leftExpr = listOfLength 2
          rightExpr = listOfLength 3
          expr = CEBinaryOp OpConcat (noLoc leftExpr) (noLoc rightExpr)
      case runShapeAnalysis (analyzeShape expr) of
        Left err -> expectationFailure $ "analysis failed: " <> T.unpack err
        Right (shape, _) -> do
          Vector.toList (siDimensions shape) `shouldBe` [5]
          siAccessPattern shape `shouldBe` SequentialAccess
          siIsHomogeneous shape `shouldBe` True

    it "extracts element shape when indexing into a known list" $ do
      let numbersId = Identifier "numbers"
          expr = CEIndex (noLoc (CEVar numbersId)) (noLoc (CELiteral (LInt 0)))
          action = do
            modify $ \st -> st { sasShapeMap = HashMap.singleton numbersId (inferShape (TList (TInt 32))) }
            analyzeShape expr
      case runShapeAnalysis action of
        Left err -> expectationFailure $ "analysis failed: " <> T.unpack err
        Right (shape, _) -> shape `shouldBe` inferShape (TInt 32)

    it "marks slices as having unknown size" $ do
      let numbersId = Identifier "numbers"
          expr = CESlice (noLoc (CEVar numbersId)) Nothing Nothing
          action = do
            modify $ \st -> st { sasShapeMap = HashMap.singleton numbersId (inferShape (TList (TInt 32))) }
            analyzeShape expr
      case runShapeAnalysis action of
        Left err -> expectationFailure $ "analysis failed: " <> T.unpack err
        Right (shape, _) -> do
          siIsKnown shape `shouldBe` False
          Vector.null (siDimensions shape) `shouldBe` True
          siAccessPattern shape `shouldBe` SequentialAccess

    it "treats list comprehensions as sequential access" $ do
      let clause = CommonCompClause
            { cccBindings = [Identifier "x"]
            , cccIter = noLoc (CEVar (Identifier "xs"))
            , cccFilters = []
            , cccIsAsync = False
            }
          expr = CEListComp (noLoc (CEVar (Identifier "x"))) [clause]
      case runShapeAnalysis (analyzeShape expr) of
        Left err -> expectationFailure $ "analysis failed: " <> T.unpack err
        Right (shape, _) -> do
          Vector.toList (siDimensions shape) `shouldBe` [-1]
          siIsKnown shape `shouldBe` False
          siAccessPattern shape `shouldBe` SequentialAccess

    it "treats set comprehensions as random access" $ do
      let clause = CommonCompClause
            { cccBindings = [Identifier "x"]
            , cccIter = noLoc (CEVar (Identifier "xs"))
            , cccFilters = []
            , cccIsAsync = False
            }
          expr = CESetComp (noLoc (CEVar (Identifier "x"))) [clause]
      case runShapeAnalysis (analyzeShape expr) of
        Left err -> expectationFailure $ "analysis failed: " <> T.unpack err
        Right (shape, _) -> do
          Vector.toList (siDimensions shape) `shouldBe` [-1]
          siIsKnown shape `shouldBe` False
          siAccessPattern shape `shouldBe` RandomAccess

  describe "inferShape" $ do
    it "captures tuple element metadata" $ do
      let shape = inferShape (TTuple [TInt 32, TString])
      Vector.toList (siDimensions shape) `shouldBe` [2]
      HashMap.lookup "0" (siFieldTypes shape) `shouldBe` Just (TInt 32)
      HashMap.lookup "1" (siFieldTypes shape) `shouldBe` Just TString
      siIsHomogeneous shape `shouldBe` False

    it "captures dictionary value information" $ do
      let shape = inferShape (TDict TString (TInt 32))
      siElementType shape `shouldBe` Just (TInt 32)
      siIsKnown shape `shouldBe` False
      siAccessPattern shape `shouldBe` RandomAccess

  describe "analyzeStructure" $ do
    it "produces defaults for struct types" $ do
      let structType = TStruct (QualifiedName [ModuleName "pkg"] (Identifier "Point")) []
      case runShapeAnalysis (analyzeStructure structType) of
        Left err -> expectationFailure $ "analysis failed: " <> T.unpack err
        Right (structShape, _) -> do
          ssFields structShape `shouldBe` HashMap.empty
          ssAlignment structShape `shouldBe` 8
          ssSize structShape `shouldBe` 0
          ssIsPackable structShape `shouldBe` True

    it "fails for non-struct types" $ do
      case runShapeAnalysis (analyzeStructure TBool) of
        Left err -> err `shouldBe` "Not a struct type"
        Right _ -> expectationFailure "Expected analyzeStructure to fail for non-struct"

  describe "inferContainerShape" $ do
    it "extracts container metadata from known variables" $ do
      let varId = Identifier "values"
          expr = CEVar varId
          action = do
            modify $ \st -> st { sasShapeMap = HashMap.singleton varId (inferShape (TList (TFloat 32))) }
            inferContainerShape expr
      case runShapeAnalysis action of
        Left err -> expectationFailure $ "analysis failed: " <> T.unpack err
        Right (container, _) -> do
          csElementType container `shouldBe` TFloat 32
          csIsResizable container `shouldBe` True
          csAccessPattern container `shouldBe` SequentialAccess
          csCapacity container `shouldBe` Nothing

    it "reports errors for non-container expressions" $ do
      case runShapeAnalysis (inferContainerShape (CELiteral (LInt 1))) of
        Left err -> err `shouldBe` "Expression does not represent a container"
        Right _ -> expectationFailure "Expected inferContainerShape to fail for scalars"

  describe "analyzeDictShape" $ do
    it "reflects homogeneous dictionary shapes" $ do
      let valueId = Identifier "numbers"
          dictExpr = HashMap.fromList
            [ ("first", CEVar valueId)
            , ("second", CEVar valueId)
            ]
          action = do
            modify $ \st -> st { sasShapeMap = HashMap.singleton valueId (inferShape (TList (TInt 32))) }
            analyzeDictShape dictExpr
      case runShapeAnalysis action of
        Left err -> expectationFailure $ "analysis failed: " <> T.unpack err
        Right (shape, _) -> do
          Vector.toList (siDimensions shape) `shouldBe` [2]
          siElementType shape `shouldBe` Just (TInt 32)
          siIsHomogeneous shape `shouldBe` True

  describe "analyzeObjectShape" $ do
    it "captures field layouts" $ do
      let fields = HashMap.fromList
            [ ("x", TInt 32)
            , ("y", TInt 32)
            ]
      case runShapeAnalysis (analyzeObjectShape fields) of
        Left err -> expectationFailure $ "analysis failed: " <> T.unpack err
        Right (shape, _) -> do
          siFieldTypes shape `shouldBe` fields
          siIsHomogeneous shape `shouldBe` True

  describe "generateCppStructure" $ do
    it "uses std::array for small fixed-size lists" $ do
      let expr = listOfLength 3
          action = do
            shape <- analyzeShape expr
            generateCppStructure shape
      case runShapeAnalysis action of
        Left err -> expectationFailure $ "analysis failed: " <> T.unpack err
        Right (mapping, _) -> do
          cmType mapping `shouldBe` "std::array"
          cmMemoryLayout mapping `shouldBe` ContiguousLayout

    it "uses std::vector for large dynamic lists" $ do
      let expr = listOfLength 70
          action = do
            shape <- analyzeShape expr
            generateCppStructure shape
      case runShapeAnalysis action of
        Left err -> expectationFailure $ "analysis failed: " <> T.unpack err
        Right (mapping, _) -> do
          cmType mapping `shouldBe` "std::vector"
          cmMemoryLayout mapping `shouldBe` ContiguousLayout

    it "uses std::unordered_map for random access containers" $ do
      let shape = inferShape (TDict TString (TInt 32))
      case runShapeAnalysis (generateCppStructure shape) of
        Left err -> expectationFailure $ "analysis failed: " <> T.unpack err
        Right (mapping, _) -> do
          cmType mapping `shouldBe` "std::unordered_map"
          cmMemoryLayout mapping `shouldBe` HashBasedLayout

    it "uses struct mappings for object shapes" $ do
      let action = do
            shape <- analyzeObjectShape (HashMap.fromList [("field", TInt 32)])
            generateCppStructure shape
      case runShapeAnalysis action of
        Left err -> expectationFailure $ "analysis failed: " <> T.unpack err
        Right (mapping, _) -> do
          cmType mapping `shouldBe` "struct"
          cmMemoryLayout mapping `shouldBe` ContiguousLayout

    it "falls back to std::variant for unknown shapes" $ do
      let action = do
            shape <- analyzeShape (CEVar (Identifier "mystery"))
            generateCppStructure shape
      case runShapeAnalysis action of
        Left err -> expectationFailure $ "analysis failed: " <> T.unpack err
        Right (mapping, _) -> do
          cmType mapping `shouldBe` "std::variant"
          cmMemoryLayout mapping `shouldBe` CustomLayout "variant"

  describe "optimizeDataStructures" $ do
    it "returns mappings for each expression" $ do
      let dictId = Identifier "mapping"
          exprs =
            [ CEVar dictId
            , listOfLength 4
            ]
          action = do
            modify $ \st -> st { sasShapeMap = HashMap.singleton dictId (inferShape (TDict TString (TInt 32))) }
            optimizeDataStructures exprs
      case runShapeAnalysis action of
        Left err -> expectationFailure $ "analysis failed: " <> T.unpack err
        Right (mappings, _) -> do
          length mappings `shouldBe` 2
          case mappings of
            ((_, firstMapping) : _) -> do
              let (_, secondMapping) = mappings !! 1
              cmType firstMapping `shouldBe` "std::unordered_map"
              cmType secondMapping `shouldBe` "std::array"
            [] -> expectationFailure "mappings list is empty"
