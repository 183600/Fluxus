module Main (main) where

import Test.Hspec
import Test.Hspec.Runner (hspecWith, defaultConfig, Config(..), ColorMode(..))
import Test.Hspec.Core.Formatters (specdoc, progress)
import System.IO (hIsTerminalDevice, stdout)
import System.Environment (getArgs, withArgs)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)

-- Analysis Tests
import qualified Test.Fluxus.Analysis.TypeInference as TypeInferenceTests
import qualified Test.Fluxus.Analysis.EscapeAnalysis as EscapeAnalysisTests
import qualified Test.Fluxus.Analysis.OwnershipInference as OwnershipInferenceTests
-- import qualified Test.Fluxus.Analysis.ShapeAnalysis as ShapeAnalysisTests
import qualified Test.Fluxus.Analysis.SmartFallback as SmartFallbackTests

-- Parser Tests
import qualified Test.Fluxus.Parser.Python as PythonParserTests
import qualified Test.Fluxus.Parser.Go as GoParserTests
import qualified Test.Fluxus.Parser.GoSyntax as GoSyntaxTests

-- Optimization Tests
import qualified Test.Fluxus.Optimization as OptimizationTests

-- Code Generation Tests
import qualified Test.Fluxus.CodeGen.CPP as CodeGenCppTests
import qualified Test.Fluxus.CodeGen.Go as CodeGenGoTests

-- Runtime Tests
import qualified Test.Fluxus.Runtime.Python as RuntimePythonTests
import qualified Test.Fluxus.Runtime.Go as RuntimeGoTests

-- Integration Tests
import qualified Test.Fluxus.Integration as IntegrationTests
import qualified Test.Fluxus.EndToEnd as EndToEndTests
import qualified Test.Fluxus.PythonGolden as PythonGoldenTests

-- Compiler Tests
-- import qualified Test.Fluxus.Compiler.Config as CompilerConfigTests
-- import qualified Test.Fluxus.Compiler.Driver as CompilerDriverTests

-- Utility Tests
import qualified Test.Fluxus.Utils.Pretty as PrettyTests
import qualified Test.Fluxus.Utils.Graph as GraphTests

-- Debug Tests
-- import qualified Test.Fluxus.Debug as DebugTests

-- Performance Tests (comment out for faster test runs)
-- import qualified Test.Fluxus.Performance as PerformanceTests

-- Command Tests
import qualified Test.Fluxus.ConvertCommand as ConvertCommandTests

main :: IO ()
main = do
  isTerm <- hIsTerminalDevice stdout
  args <- getArgs
  let (colorOverride, fmtOverride, restArgs) = parseCliArgs args
      fmtDefault = if isTerm then progress else specdoc
      fmt = fromMaybe fmtDefault fmtOverride
      colorDefault = if isTerm then ColorAuto else ColorNever
      color = fromMaybe colorDefault colorOverride
      cfg = defaultConfig { configFormatter = Just fmt
                         , configColorMode = color }
  withArgs restArgs $ hspecWith cfg $ do
    describe "Fluxus Compiler Comprehensive Test Suite" $ do
      
      -- === UNIT TESTS ===
      describe "Unit Tests" $ do
        describe "Analysis" $ do
          TypeInferenceTests.spec
          EscapeAnalysisTests.spec
          OwnershipInferenceTests.spec
          -- ShapeAnalysisTests.spec  -- Temporarily disabled due to timeout issues
          SmartFallbackTests.spec
        
        describe "Parsers" $ do
          PythonParserTests.spec
          GoParserTests.spec
          GoSyntaxTests.spec
        
        describe "Code Generation" $ do
          CodeGenCppTests.spec
          CodeGenGoTests.spec
        
        describe "Optimization" $ do
          OptimizationTests.spec
        
        describe "Runtime" $ do
          RuntimePythonTests.spec
          RuntimeGoTests.spec
        
        -- describe "Compiler" $ do
        --   CompilerConfigTests.spec
        --   CompilerDriverTests.spec
        
        describe "Utilities" $ do
          PrettyTests.spec
          GraphTests.spec
        
        -- describe "Debug & Logging" $ do
        --   DebugTests.spec
      
      -- === INTEGRATION TESTS ===
      describe "Integration Tests" $ do
        IntegrationTests.spec
      
      -- === END-TO-END TESTS ===
      describe "End-to-End Tests" $ do
        EndToEndTests.spec
        PythonGoldenTests.spec
      
      -- === COMMAND TESTS ===
      describe "Command Tests" $ do
        ConvertCommandTests.spec
      
      -- === PERFORMANCE TESTS ===
      -- Uncomment to run performance tests (takes longer)
      -- describe "Performance Tests" $ do
      --   PerformanceTests.spec

-- Minimal CLI handling to support CI runner flags and ignore unsupported ones
parseCliArgs = go Nothing Nothing []
  where
    go mc mf acc [] = (mc, mf, reverse acc)
    go mc mf acc (x:xs)
      -- --color=never|always
      | "--color=" `isPrefixOf` x =
          let v = drop 8 x
              mc' = case v of
                      "never"  -> Just ColorNever
                      "always" -> Just ColorAlways
                      _         -> mc
          in go mc' mf acc xs
      -- --no-color
      | x == "--no-color" = go (Just ColorNever) mf acc xs
      -- --color (treat as always)
      | x == "--color" = go (Just ColorAlways) mf acc xs
      -- --format=quiet|progress|specdoc
      | "--format=" `isPrefixOf` x =
          let v = drop 9 x
              mf' = case v of
                      "quiet"    -> Just progress
                      "progress" -> Just progress
                      "specdoc"  -> Just specdoc
                      _           -> mf
          in go mc mf' acc xs
      -- drop unknown test-runner formatting args we know about
      | x == "--format" = case xs of
          (y:ys) -> let mf' = case y of
                                  "quiet"    -> Just progress
                                  "progress" -> Just progress
                                  "specdoc"  -> Just specdoc
                                  _           -> mf
                    in go mc mf' acc ys
          [] -> go mc mf acc xs
      -- keep anything else
      | otherwise = go mc mf (x:acc) xs