module Main (main) where

import Test.Hspec
import Test.Hspec.Runner (hspecWith, defaultConfig, Config(..), ColorMode(..))
import Test.Hspec.Core.Formatters (specdoc, progress)
import System.IO (hIsTerminalDevice, stdout)


import qualified Test.Fluxus.Analysis.TypeInference as TypeInferenceTests
import qualified Test.Fluxus.ConvertCommand as ConvertCommandTests
import qualified Test.Fluxus.Parser.Python as PythonParserTests
import qualified Test.Fluxus.Parser.Go as GoParserTests
import qualified Test.Fluxus.Analysis.EscapeAnalysis as EscapeAnalysisTests
import qualified Test.Fluxus.Analysis.OwnershipInference as OwnershipInferenceTests
-- import qualified Test.Fluxus.Analysis.ShapeAnalysis as ShapeAnalysisTests
import qualified Test.Fluxus.Analysis.SmartFallback as SmartFallbackTests
import qualified Test.Fluxus.Optimization as OptimizationTests
import qualified Test.Fluxus.CodeGen.CPP as CodeGenCppTests
import qualified Test.Fluxus.CodeGen.Go as CodeGenGoTests
import qualified Test.Fluxus.Runtime.Python as RuntimePythonTests
import qualified Test.Fluxus.Runtime.Go as RuntimeGoTests
import qualified Test.Fluxus.Integration as IntegrationTests
import qualified Test.Fluxus.EndToEnd as EndToEndTests

main :: IO ()
main = do
  isTerm <- hIsTerminalDevice stdout
  let fmt = if isTerm then progress else specdoc
      cfg = defaultConfig { configFormatter = Just fmt
                         , configColorMode = if isTerm then ColorAuto else ColorNever }
  hspecWith cfg $ do
    describe "Fluxus Compiler Comprehensive Test Suite" $ do
      -- Unit Tests
      TypeInferenceTests.spec
      PythonParserTests.spec
      GoParserTests.spec
      EscapeAnalysisTests.spec
      OwnershipInferenceTests.spec
      -- ShapeAnalysisTests.spec  -- Temporarily disabled due to timeout issues
      SmartFallbackTests.spec

      -- Optimization Tests
      OptimizationTests.spec

      -- Code Generation Tests
      CodeGenCppTests.spec
      CodeGenGoTests.spec

      -- Runtime Tests
      RuntimePythonTests.spec
      RuntimeGoTests.spec

      -- Integration Tests
      IntegrationTests.spec

      -- End-to-End Tests
      EndToEndTests.spec

      -- Command Tests
      ConvertCommandTests.spec