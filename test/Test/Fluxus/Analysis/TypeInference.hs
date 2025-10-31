{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Analysis.TypeInference (spec) where

import Test.Hspec
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T

import Fluxus.AST.Common
import Fluxus.Analysis.TypeInference

spec :: Spec
spec = describe "Type Inference" $ do
  describe "inferType" $ do
    it "infers integer literal types" $ do
      let expr = CELiteral (LInt 42)
      case runTypeInference HashMap.empty (inferType expr) of
        Right inference -> do
          resultType inference `shouldBe` TInt 32
          resultConstraints inference `shouldBe` []
        Left err ->
          expectationFailure $ "expected inference to succeed, but got: " <> T.unpack err

    it "looks up variable types from the environment" $ do
      let env = HashMap.singleton (Identifier "flag") TBool
          expr = CEVar (Identifier "flag")
      case runTypeInference env (inferType expr) of
        Right inference -> resultType inference `shouldBe` TBool
        Left err ->
          expectationFailure $ "expected inference to succeed, but got: " <> T.unpack err

    it "records constraints when applying functions" $ do
      let env = HashMap.singleton (Identifier "is_positive") (TFunction [TInt 32] TBool)
          expr = CECall (noLoc (CEVar (Identifier "is_positive"))) [noLoc (CELiteral (LInt 1))]
      case runTypeInference env (inferType expr) of
        Right inference -> do
          resultType inference `shouldBe` TVar (TypeVar "t0")
          resultConstraints inference `shouldBe`
            [ ( TFunction [TInt 32] TBool
              , TFunction [TInt 32] (TVar (TypeVar "t0"))
              )
            ]
        Left err ->
          expectationFailure $ "expected inference to succeed, but got: " <> T.unpack err

  describe "solveConstraints" $ do
    it "detects incompatible arithmetic operands" $ do
      let expr = CEBinaryOp OpAdd (noLoc (CELiteral (LInt 1))) (noLoc (CELiteral (LString "oops")))
          action = do
            _ <- inferExpr expr
            solveConstraints
      case runTypeInference HashMap.empty action of
        Left err ->
          err `shouldBe` "Failed to solve constraint: Cannot unify TInt 32 with TString"
        Right _ ->
          expectationFailure "expected constraint solving to fail for mismatched types"

  describe "unifyTypes" $ do
    it "unifies list element types" $ do
      let action = unifyTypes (TList (TVar (TypeVar "a"))) (TList (TInt 32))
      case runTypeInference HashMap.empty action of
        Right (Just constraints) -> constraints `shouldBe` [(TVar (TypeVar "a"), TInt 32)]
        Right Nothing -> expectationFailure "expected constraints, but unification returned Nothing"
        Left err -> expectationFailure $ "expected unification to succeed, but got: " <> T.unpack err

    it "fails to unify mismatched primitives" $ do
      case runTypeInference HashMap.empty (unifyTypes TBool TString) of
        Right Nothing -> pure ()
        Right (Just _) -> expectationFailure "expected unification to fail, but it produced constraints"
        Left err -> expectationFailure $ "expected graceful failure, but got: " <> T.unpack err
