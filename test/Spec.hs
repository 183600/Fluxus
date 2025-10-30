module Main (main) where

import Data.List (isPrefixOf)
import System.Environment (getArgs, withArgs)
import Test.Hspec

import qualified Test.Fluxus.Parser.Python as PythonTests
import qualified Test.Fluxus.Parser.Go as GoTests
import qualified Test.Fluxus.Analysis.TypeInference as TypeInferenceTests
import qualified Test.Fluxus.CodeGen.CPP as CodeGenTests

main :: IO ()
main = do
  args <- getArgs
  let args' = ensureProgressFormat args
  withArgs args' $ hspec fullSpec

fullSpec :: Spec
fullSpec =
  describe "HyperStatic Compiler Test Suite" $ do
    PythonTests.spec
    GoTests.spec
    TypeInferenceTests.spec
    CodeGenTests.spec

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
