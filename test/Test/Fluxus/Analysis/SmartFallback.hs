{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Analysis.SmartFallback (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Fluxus.AST.Common
import Fluxus.Analysis.SmartFallback

spec :: Spec
spec = describe "Smart fallback analysis" $ do
  it "classifies literals as fully static" $ do
    let expr = CELiteral (LInt 0)
        (level, _) = runSmartFallback (analyzeDynamism expr)
    level `shouldBe` FullyStatic

  it "requests runtime fallback for highly dynamic calls" $ do
    let callExpr = CECall (noLoc (CEVar (Identifier "invoke"))) []
        (shouldFallback, _) = runSmartFallback (shouldFallbackToRuntime callExpr)
    shouldFallback `shouldBe` True

  prop "binary expressions escalate to the maximum dynamism level" $ \leftCall rightCall ->
    let leftExpr = if leftCall then dynamicCall else staticLiteral
        rightExpr = if rightCall then dynamicCall else staticLiteral
        expr = CEBinaryOp OpAdd (noLoc leftExpr) (noLoc rightExpr)
        (level, _) = runSmartFallback (analyzeDynamism expr)
        expected = maximum [levelFor leftCall, levelFor rightCall]
    in level === expected
  where
    dynamicCall = CECall (noLoc (CEVar (Identifier "callee"))) []
    staticLiteral = CELiteral (LInt 1)
    levelFor True = SemiDynamic
    levelFor False = FullyStatic
