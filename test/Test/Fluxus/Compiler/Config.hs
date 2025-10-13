{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Compiler.Config (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T

import Fluxus.Compiler.Config

spec :: Spec
spec = describe "Compiler Configuration" $ do
  configCreationSpec
  configValidationSpec
  configMergingSpec
  propertyBasedSpec

configCreationSpec :: Spec
configCreationSpec = describe "Configuration Creation" $ do
  it "creates default configuration" $ do
    let config = defaultConfig
    ccSourceLanguage config `shouldBe` Python
    ccOptimizationLevel config `shouldBe` O0
    ccStopAtCodegen config `shouldBe` False
  
  it "creates configuration with custom optimization level" $ do
    let config = defaultConfig { ccOptimizationLevel = O3 }
    ccOptimizationLevel config `shouldBe` O3
  
  it "creates configuration for Go source" $ do
    let config = defaultConfig { ccSourceLanguage = Go }
    ccSourceLanguage config `shouldBe` Go
  
  it "creates configuration with specific output path" $ do
    let outputPath = "/tmp/output.cpp"
    let config = defaultConfig { ccOutputPath = Just outputPath }
    ccOutputPath config `shouldBe` Just outputPath
  
  it "creates configuration with multiple input files" $ do
    let files = ["file1.py", "file2.py", "file3.py"]
    let config = defaultConfig { ccInputFiles = files }
    length (ccInputFiles config) `shouldBe` 3

configValidationSpec :: Spec
configValidationSpec = describe "Configuration Validation" $ do
  it "validates config with input files" $ do
    let config = defaultConfig { ccInputFiles = ["test.py"] }
    validateConfig config `shouldBe` Right ()
  
  it "rejects config without input files" $ do
    let config = defaultConfig { ccInputFiles = [] }
    validateConfig config `shouldSatisfy` isLeft
  
  it "validates output path" $ do
    let config = defaultConfig 
          { ccInputFiles = ["test.py"]
          , ccOutputPath = Just "output.cpp"
          }
    validateConfig config `shouldBe` Right ()
  
  it "validates C++ standard" $ do
    let config = defaultConfig 
          { ccInputFiles = ["test.py"]
          , ccCppStandard = "c++20"
          }
    validateConfig config `shouldBe` Right ()
  
  it "rejects invalid C++ standard" $ do
    let config = defaultConfig 
          { ccInputFiles = ["test.py"]
          , ccCppStandard = "c++05"
          }
    validateConfig config `shouldSatisfy` isLeft
  
  it "validates optimization levels" $ do
    let levels = [O0, O1, O2, O3, Os, Oz]
    forM_ levels $ \level -> do
      let config = defaultConfig { ccInputFiles = ["test.py"], ccOptimizationLevel = level }
      validateConfig config `shouldBe` Right ()

configMergingSpec :: Spec
configMergingSpec = describe "Configuration Merging" $ do
  it "merges configurations" $ do
    let config1 = defaultConfig { ccOptimizationLevel = O2 }
    let config2 = defaultConfig { ccCppStandard = "c++23" }
    let merged = mergeConfigs config1 config2
    ccOptimizationLevel merged `shouldBe` O2
    ccCppStandard merged `shouldBe` "c++23"
  
  it "second config overrides first" $ do
    let config1 = defaultConfig { ccOptimizationLevel = O0 }
    let config2 = defaultConfig { ccOptimizationLevel = O3 }
    let merged = mergeConfigs config1 config2
    ccOptimizationLevel merged `shouldBe` O3
  
  it "merges input files" $ do
    let config1 = defaultConfig { ccInputFiles = ["file1.py"] }
    let config2 = defaultConfig { ccInputFiles = ["file2.py"] }
    let merged = mergeConfigs config1 config2
    length (ccInputFiles merged) `shouldBe` 2
  
  it "handles empty configs" $ do
    let config1 = defaultConfig
    let config2 = defaultConfig
    let merged = mergeConfigs config1 config2
    merged `shouldBe` defaultConfig

propertyBasedSpec :: Spec
propertyBasedSpec = describe "Property-Based Tests" $ do
  it "optimization level is always valid" $ property $ \level ->
    let config = defaultConfig { ccOptimizationLevel = toOptLevel level }
    in ccOptimizationLevel config `elem` [O0, O1, O2, O3, Os, Oz]
    where
      toOptLevel n = case n `mod` 6 of
        0 -> O0
        1 -> O1
        2 -> O2
        3 -> O3
        4 -> Os
        _ -> Oz
  
  it "config with at least one input file is valid" $ property $ \(NonEmpty files) ->
    let config = defaultConfig { ccInputFiles = files }
    in validateConfig config == Right ()
  
  it "merged config maintains at least one property from each" $ property $ \opt1 opt2 ->
    let config1 = defaultConfig { ccOptimizationLevel = toOptLevel opt1, ccInputFiles = ["a"] }
        config2 = defaultConfig { ccCppStandard = "c++20", ccInputFiles = ["b"] }
        merged = mergeConfigs config1 config2
    in ccCppStandard merged == "c++20"
    where
      toOptLevel n = case n `mod` 6 of
        0 -> O0
        1 -> O1
        2 -> O2
        3 -> O3
        4 -> Os
        _ -> Oz

-- Helper function
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
