module Main (main) where

import Test.Hspec

import qualified Test.Fluxus.Analysis.TypeInference as TypeInferenceTests

main :: IO ()
main = hspec $ do
  describe "HyperStatic Compiler Test Suite" $ do
    TypeInferenceTests.spec