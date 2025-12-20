{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.List (isPrefixOf)
import qualified Data.Text as T
import System.Environment (getArgs, withArgs)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, hspec, parallel)
import Control.Monad (when, filterM)
import Control.Exception (catch, SomeException)

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
import qualified Test.Fluxus.QuickCheckProperties as QuickCheckTests

import Fluxus.Utils.Debug (DebugLevel(..), getDebugLevel, debugLog, debugBreak)

main :: IO ()
main = do
  ensureTestLogDirectory
  
  debugLevel <- getDebugLevel
  when (debugLevel >= Info) $ do
    debugLog Info "Starting Fluxus test suite"
    debugLog Info $ "Debug level set to: " <> T.pack (show debugLevel)
  
  args <- getArgs
  when ("--break" `elem` args) $ do
    debugBreak "Test suite breakpoint - press Enter to continue"
  
  let args' = ensureProgressFormat args
  withArgs args' $ hspec fullSpec

ensureTestLogDirectory :: IO ()
ensureTestLogDirectory = catch findAndCreateLogDir $ \(_ :: SomeException) -> return ()
  where
    findAndCreateLogDir = do
      let buildDir = "." </> "dist-newstyle" </> "build"
      exists <- doesDirectoryExist buildDir
      when exists $ do
        platforms <- listDirectory buildDir
        platformDirs <- filterM (\p -> doesDirectoryExist (buildDir </> p)) platforms
        mapM_ createLogDirForPlatform platformDirs
    
    createLogDirForPlatform platform = do
      let platformDir = "." </> "dist-newstyle" </> "build" </> platform
      ghcVersions <- listDirectory platformDir
      ghcDirs <- filterM (\g -> doesDirectoryExist (platformDir </> g)) ghcVersions
      mapM_ (createLogDirForGhc platform) ghcDirs
    
    createLogDirForGhc platform ghcVer = do
      let ghcDir = "." </> "dist-newstyle" </> "build" </> platform </> ghcVer
      packages <- listDirectory ghcDir
      packageDirs <- filterM (\p -> doesDirectoryExist (ghcDir </> p)) packages
      mapM_ (createLogDirForPackage platform ghcVer) packageDirs
    
    createLogDirForPackage platform ghcVer pkg = do
      let testDir = "." </> "dist-newstyle" </> "build" </> platform </> ghcVer </> pkg </> "t" </> "fluxus-test" </> "test"
      createDirectoryIfMissing True testDir

fullSpec :: Spec
fullSpec = describe "Fluxus Compiler Test Suite" $ do
  parallel $ do
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
    DriverTests.spec
    QuickCheckTests.spec
  -- Run config tests serially to avoid environment variable interference
  ConfigTests.spec

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
