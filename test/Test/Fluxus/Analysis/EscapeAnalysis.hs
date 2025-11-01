{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Analysis.EscapeAnalysis (spec) where

import Control.Monad.Reader (local)
import qualified Data.Set as Set
import qualified Data.Text as T
import Test.Hspec

import Fluxus.AST.Common
import Fluxus.Analysis.EscapeAnalysis

spec :: Spec
spec = describe "Escape analysis" $ do
  it "treats literals as non-escaping stack allocations" $ do
    let expr = CELiteral (LInt 1)
        (info, _) = runEscapeAnalysis (analyzeExpression expr)
    info `shouldBe` NoEscape

  it "marks and retrieves escape information" $ do
    let identifier = Identifier "captured"
        action = do
          markEscape identifier EscapeToGlobal
          getEscapeInfo identifier
        (info, state) = runEscapeAnalysis action
    info `shouldBe` EscapeToGlobal
    Set.member identifier (easGlobalEscapes state) `shouldBe` True

  it "propagates return escapes through function calls" $ do
    let callExpr = CECall (noLoc (CEVar (Identifier "allocate"))) []
        (escapeInfo, _) = runEscapeAnalysis $
          local (\ctx -> ctx { ecInReturn = True }) (analyzeExpression callExpr)
    escapeInfo `shouldBe` EscapeToReturn

  it "identifies heap escaping calls as non stack-allocatable" $ do
    let callExpr = CECall (noLoc (CEVar (Identifier "builder"))) []
        (isStack, _) = runEscapeAnalysis (isStackAllocatable callExpr)
    isStack `shouldBe` False

  it "suggests heap-specific optimizations for escaping calls" $ do
    let callExpr = CECall (noLoc (CEVar (Identifier "builder"))) []
        ((_, hints), _) = runEscapeAnalysis (optimizeMemoryAllocation callExpr)
        expected = map T.pack
          [ "Consider std::shared_ptr for shared ownership"
          , "Use move semantics to avoid unnecessary copies"
          ]
    mapM_ (\hintText -> hints `shouldSatisfy` elem hintText) expected
