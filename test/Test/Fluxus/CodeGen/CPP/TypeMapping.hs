{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.CodeGen.CPP.TypeMapping (spec) where

import Test.Hspec

import Fluxus.AST.Common
import Fluxus.AST.Go
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

  describe "Go type mapping" $ do
    it "maps struct types to inline struct literals" $ do
      let goStruct = GoStructType
            [ GoField
                { goFieldNames = [Identifier "x"]
                , goFieldType = noLoc (GoBasicType (Identifier "int"))
                , goFieldTag = Nothing
                }
            , GoField
                { goFieldNames = []
                , goFieldType = noLoc (GoBasicType (Identifier "string"))
                , goFieldTag = Nothing
                }
            ]
      mapGoTypeToCpp goStruct `shouldBe`
        CppStructLiteral [("x", CppInt), ("field", CppString)]

    it "maps fixed-size arrays to std::array" $ do
      let goArray = GoArrayType (noLoc (GoLiteral (GoInt 4))) (noLoc (GoBasicType (Identifier "int")))
      mapGoTypeToCpp goArray `shouldBe` CppStdArray CppInt 4

    it "falls back to std::vector when array size is not a literal" $ do
      let dynamicArray = GoArrayType (noLoc (GoIdent (Identifier "n"))) (noLoc (GoBasicType (Identifier "int")))
      mapGoTypeToCpp dynamicArray `shouldBe` CppVector CppInt

    it "maps function signatures to std::function" $ do
      let params =
            [ GoField
                { goFieldNames = [Identifier "x", Identifier "y"]
                , goFieldType = noLoc (GoBasicType (Identifier "int"))
                , goFieldTag = Nothing
                }
            ]
          results =
            [ GoField
                { goFieldNames = []
                , goFieldType = noLoc (GoBasicType (Identifier "bool"))
                , goFieldTag = Nothing
                }
            ]
          expected = CppTemplateType "std::function" [CppFunctionType [CppInt, CppInt] CppBool]
      mapGoTypeToCpp (GoFuncType params results) `shouldBe` expected

    it "maps named Go types to qualified C++ class types" $ do
      let qualified = QualifiedName [ModuleName "pkg"] (Identifier "Custom")
      mapGoTypeToCpp (GoNamedType qualified) `shouldBe` CppClassType "pkg::Custom" []

    it "maps variadic types to std::vector" $ do
      mapGoTypeToCpp (GoEllipsisType (noLoc (GoBasicType (Identifier "string"))))
        `shouldBe` CppVector CppString

    it "maps interfaces to std::any" $ do
      mapGoTypeToCpp (GoInterfaceType []) `shouldBe` CppTemplateType "std::any" []

    it "recognizes unsigned byte aliases" $ do
      mapGoTypeToCpp (GoBasicType (Identifier "uint8"))
        `shouldBe` CppClassType "std::uint8_t" []
      mapGoTypeToCpp (GoBasicType (Identifier "any"))
        `shouldBe` CppTemplateType "std::any" []
