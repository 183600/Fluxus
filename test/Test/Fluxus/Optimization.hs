{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Optimization (spec) where

import Test.Hspec

import qualified Test.Fluxus.Optimization.ConstantFolding as ConstantFoldingTests
import qualified Test.Fluxus.Optimization.DeadCodeElimination as DeadCodeEliminationTests
import qualified Test.Fluxus.Optimization.Inlining as InliningTests

spec :: Spec
spec = describe "Optimization Tests" $ do
  ConstantFoldingTests.spec
  DeadCodeEliminationTests.spec
  InliningTests.spec