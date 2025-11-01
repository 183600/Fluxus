{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.CodeGen.CPP.TypeMapping (spec) where

import Test.Hspec

import Fluxus.AST.Common
import Fluxus.CodeGen.CPP

spec :: Spec
spec = describe "Type Mapping" $ do
  it "maps basic types correctly" $ do
    mapCommonTypeToCpp (TInt 32) `shouldBe` CppInt
    mapCommonTypeToCpp TBool `shouldBe` CppBool
    mapCommonTypeToCpp TString `shouldBe` CppString
    mapCommonTypeToCpp (TFloat 64) `shouldBe` CppDouble

  it "maps container types correctly" $ do
    mapCommonTypeToCpp (TList (TInt 32)) `shouldBe` CppVector CppInt
    mapCommonTypeToCpp (TDict TString (TInt 32)) `shouldBe` CppUnorderedMap CppString CppInt
    mapCommonTypeToCpp (TOptional TString) `shouldBe` CppOptional CppString

  it "maps smart pointer types correctly" $ do
    mapCommonTypeToCpp (TOwned (TInt 32)) `shouldBe` CppUniquePtr CppInt
    mapCommonTypeToCpp (TShared TString) `shouldBe` CppSharedPtr CppString

  it "maps complex types conservatively" $ do
    let complexType = TFunction [TInt 32, TString] TBool
    mapCommonTypeToCpp complexType `shouldBe` CppAuto -- Fallback to auto for complex types
