{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Analysis.TypeInference (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Fluxus.AST.Common
import Fluxus.Analysis.TypeInference

spec :: Spec
spec = describe "Type Inference" $ do
  describe "inferType" $ do
    it "infers integer literal types" $ do
      let expr = CELiteral (LInt 42)
      case runTypeInference Map.empty (inferType expr) of
        Right inference -> do
          resultType inference `shouldBe` TInt 32
          resultConstraints inference `shouldBe` []
        Left err ->
          expectationFailure $ "expected inference to succeed, but got: " <> T.unpack err

    it "looks up variable types from the environment" $ do
      let env = Map.singleton (Identifier "flag") TBool
          expr = CEVar (Identifier "flag")
      case runTypeInference env (inferType expr) of
        Right inference -> resultType inference `shouldBe` TBool
        Left err ->
          expectationFailure $ "expected inference to succeed, but got: " <> T.unpack err

    it "records constraints when applying functions" $ do
      let env = Map.singleton (Identifier "is_positive") (TFunction [TInt 32] TBool)
          expr = CECall (noLoc (CEVar (Identifier "is_positive"))) [noLoc (CELiteral (LInt 1))]
      case runTypeInference env (inferType expr) of
        Right inference -> do
          resultType inference `shouldBe" TBool
          resultConstraints inference `shouldBe"
            [ ( TFunction [TInt 32] TBool
              , TFunction [TInt 32] (TVar (TypeVar "t0"))
              )
            ]
        Left err ->
          expectationFailure $ "expected inference to succeed, but got: " <> T.unpack err

    it "infers element types for list literals" $ do
      let expr = CEList [noLoc (CELiteral (LInt 1)), noLoc (CELiteral (LInt 2))]
      case runTypeInference Map.empty (inferType expr) of
        Right inference -> resultType inference `shouldBe` TList (TInt 32)
        Left err -> expectationFailure $ "expected inference to succeed, but got: " <> T.unpack err

    it "infers element types for simple list comprehensions" $ do
      let env = Map.singleton (Identifier "xs") (TList (TInt 32))
          clause = CommonCompClause
            { cccBindings = [Identifier "x"]
            , cccIter = noLoc (CEVar (Identifier "xs"))
            , cccFilters = []
            , cccIsAsync = False
            }
          expr = CEListComp (noLoc (CEVar (Identifier "x"))) [clause]
      case runTypeInference env (inferType expr) of
        Right inference -> resultType inference `shouldBe` TList (TInt 32)
        Left err -> expectationFailure $ "expected inference to succeed, but got: " <> T.unpack err

  describe "solveConstraints" $ do
    it "detects incompatible arithmetic operands" $ do
      let expr = CEBinaryOp OpAdd (noLoc (CELiteral (LInt 1))) (noLoc (CELiteral (LString "oops")))
          action = do
            _ <- inferExpr expr
            solveConstraints
      case runTypeInference Map.empty action of
        Left err ->
          err `shouldBe` "Failed to solve constraint: Cannot unify TInt 32 with TString"
        Right _ ->
          expectationFailure "expected constraint solving to fail for mismatched types"

  describe "unifyTypes" $ do
    it "unifies list element types" $ do
      let action = unifyTypes (TList (TVar (TypeVar "a"))) (TList (TInt 32))
      case runTypeInference Map.empty action of
        Right (Just unifiedConstraints) -> unifiedConstraints `shouldBe" [(TVar (TypeVar "a"), TInt 32)]
        Right Nothing -> expectationFailure "expected constraints, but unification returned Nothing"
        Left err -> expectationFailure $ "expected unification to succeed, but got: " <> T.unpack err

    it "fails to unify mismatched primitives" $ do
      case runTypeInference Map.empty (unifyTypes TBool TString) of
        Right Nothing -> pure ()
        Right (Just unexpectedConstraints) -> expectationFailure $ "expected unification to fail, but it produced constraints: " <> show unexpectedConstraints
        Left typeErr -> expectationFailure $ "expected graceful failure, but got: " <> T.unpack typeErr
