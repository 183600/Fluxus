module Main (main) where

import Test.Hspec

import qualified Test.Fluxus.Parser.Python as PythonTests
import qualified Test.Fluxus.Parser.Go as GoTests
import qualified Test.Fluxus.Analysis.TypeInference as TypeInferenceTests
import qualified Test.Fluxus.CodeGen.CPP as CodeGenTests
import qualified Test.Fluxus.Optimization as OptimizationTests
import qualified Test.Fluxus.Integration as IntegrationTests
import qualified Test.Fluxus.EndToEnd as EndToEndTests

main :: IO ()
main = hspec $ do
  describe "HyperStatic Compiler Test Suite" $ do
    PythonTests.spec
    GoTests.spec
    TypeInferenceTests.spec
    CodeGenTests.spec
    OptimizationTests.spec
    IntegrationTests.spec
    EndToEndTests.spec