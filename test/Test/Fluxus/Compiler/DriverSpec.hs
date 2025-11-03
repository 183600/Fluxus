{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Compiler.DriverSpec (spec) where

import qualified Data.Text as T
import Data.Either (isRight)
import Test.Hspec

import Fluxus.Compiler.Driver
  ( CompilerConfig(..)
  , CompilerError(..)
  , defaultConfig
  , runCompiler
  , setupCompilerEnvironment
  )

spec :: Spec
spec = describe "setupCompilerEnvironment" $ do
  it "fails when the configured compiler is missing by default" $ do
    let config = defaultConfig
          { ccCppCompiler = "fluxus-nonexistent-compiler"
          , ccVerboseLevel = 0
          }
    result <- runCompiler config setupCompilerEnvironment
    case result of
      Left (ConfigurationError msg) ->
        T.isInfixOf "C++ compiler not found" msg `shouldBe` True
      other -> expectationFailure $ "expected configuration error, got " ++ show other

  it "succeeds when skipping the compiler check" $ do
    let config = defaultConfig
          { ccCppCompiler = "fluxus-nonexistent-compiler"
          , ccSkipCompilerCheck = True
          , ccVerboseLevel = 0
          }
    result <- runCompiler config setupCompilerEnvironment
    result `shouldSatisfy` isRight

  it "succeeds when stopping at code generation" $ do
    let config = defaultConfig
          { ccCppCompiler = "fluxus-nonexistent-compiler"
          , ccStopAtCodegen = True
          , ccVerboseLevel = 0
          }
    result <- runCompiler config setupCompilerEnvironment
    result `shouldSatisfy` isRight
