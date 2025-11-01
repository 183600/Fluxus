{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Analysis.OwnershipInference (spec) where

import Test.Hspec

import Fluxus.AST.Common
import Fluxus.Analysis.OwnershipInference

spec :: Spec
spec = describe "Ownership inference" $ do
  it "assigns stack ownership strategy for literals" $ do
    case runOwnershipInference (inferOwnership (CELiteral (LString "fluxus"))) of
      Right (result, _) -> do
        ownsMemory (orOwnership result) `shouldBe` True
        memLocation (orOwnership result) `shouldBe` Stack
        orStrategy result `shouldBe` StackOwned
        orCppType result `shouldBe` "std::string"
      Left err ->
        expectationFailure $ "expected ownership inference to succeed, but got: " <> show err

  it "infers unique ownership for function calls that escape to the heap" $ do
    let callExpr = CECall (noLoc (CEVar (Identifier "make_vector"))) []
    case runOwnershipInference (inferOwnership callExpr) of
      Right (result, _) -> do
        memLocation (orOwnership result) `shouldBe` Heap
        escapes (orOwnership result) `shouldBe` EscapeToHeap
        orStrategy result `shouldBe` UniqueOwnership
      Left err ->
        expectationFailure $ "expected ownership inference to succeed, but got: " <> show err

  it "persists ownership information set in the environment" $ do
    let identifier = Identifier "resource"
        explicitInfo = OwnershipInfo
          { ownsMemory = False
          , canMove = False
          , refCount = Nothing
          , escapes = NoEscape
          , memLocation = Stack
          }
        action = do
          setOwnershipInfo identifier explicitInfo
          getOwnershipInfo identifier
    case runOwnershipInference action of
      Right (info, _) -> info `shouldBe` explicitInfo
      Left err -> expectationFailure $ "expected ownership info lookup to succeed, but got: " <> show err
