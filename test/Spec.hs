{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getArgs, withArgs)
import Test.Hspec
import Control.Monad (when)

import qualified Test.Fluxus.Parser.Python as PythonTests
import qualified Test.Fluxus.Parser.Go as GoTests
import qualified Test.Fluxus.Analysis.TypeInference as TypeInferenceTests
import qualified Test.Fluxus.Analysis.EscapeAnalysis as EscapeAnalysisTests
import qualified Test.Fluxus.Analysis.OwnershipInference as OwnershipInferenceTests
import qualified Test.Fluxus.Analysis.SmartFallback as SmartFallbackTests
import qualified Test.Fluxus.Analysis.CommonExprLowering as CommonExprLoweringTests
import qualified Test.Fluxus.Analysis.ShapeAnalysis as ShapeAnalysisTests
import qualified Test.Fluxus.CodeGen.CPP as CodeGenTests
import qualified Test.Fluxus.Utils.GraphSpec as GraphUtilsTests
import qualified Test.Fluxus.Compiler.ConfigSpec as ConfigTests
import qualified Test.Fluxus.Compiler.DriverSpec as DriverTests

import Fluxus.Utils.Debug (DebugLevel(..), setDebugLevel, getDebugLevel, debugLog, debugBreak)

main :: IO ()
main = do
  -- Check for debug environment variable
  debugLevel <- getDebugLevel
  when (debugLevel >= Info) $ do
    debugLog Info "Starting Fluxus test suite"
    debugLog Info $ "Debug level set to: " <> T.pack (show debugLevel)
  
  -- Check for breakpoint request
  args <- getArgs
  when ("--break" `elem` args) $ do
    debugBreak "Test suite breakpoint - press Enter to continue"
  
  let args' = ensureProgressFormat args
  withArgs args' $ hspec fullSpec

fullSpec :: Spec
fullSpec =
  describe "Fluxus Compiler Test Suite" $ do
    PythonTests.spec
    GoTests.spec
    TypeInferenceTests.spec
    EscapeAnalysisTests.spec
    OwnershipInferenceTests.spec
    SmartFallbackTests.spec
    CommonExprLoweringTests.spec
    ShapeAnalysisTests.spec
    CodeGenTests.spec
    GraphUtilsTests.spec
    ConfigTests.spec
    DriverTests.spec

ensureProgressFormat :: [String] -> [String]
ensureProgressFormat args
  | hasFormatOption args = args
  | otherwise = "--format=progress" : args

hasFormatOption :: [String] -> Bool
hasFormatOption [] = False
hasFormatOption (opt:rest)
  | opt == "--format" = True
  | opt == "-f" = True
  | "--format=" `isPrefixOf` opt = True
  | "-f" `isPrefixOf` opt && opt /= "-f" = True
  | otherwise = hasFormatOption rest
