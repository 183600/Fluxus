module Main (main) where

import Test.Hspec

import qualified Test.Fluxus.Parser.Python as PythonTests
import qualified Test.Fluxus.Parser.Go as GoTests
import qualified Test.Fluxus.Analysis.TypeInference as TypeInferenceTests
import qualified Test.Fluxus.CodeGen.CPP as CodeGenTests

main :: IO ()
main = hspec $ do
  describe "HyperStatic Compiler Test Suite" $ do
    PythonTests.spec
    GoTests.spec
    TypeInferenceTests.spec
    CodeGenTests.spec