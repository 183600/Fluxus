module Main (main) where

import Test.Hspec

import qualified Test.Fluxus.Analysis.TypeInference as TypeInferenceTests
import qualified Test.Fluxus.ConvertCommand as ConvertCommandTests

main :: IO ()
main = hspec $ do
  describe "HyperStatic Compiler Test Suite" $ do
    TypeInferenceTests.spec
    ConvertCommandTests.spec