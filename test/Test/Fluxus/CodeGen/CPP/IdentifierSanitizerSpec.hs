{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.CodeGen.CPP.IdentifierSanitizerSpec (spec) where

import Test.Hspec
import qualified Data.Text as T

import Fluxus.CodeGen.CPP.IdentifierSanitizer (sanitizeIdentifier, sanitizeCppUnit)
import Fluxus.CodeGen.CPP.AST
  ( CppUnit(..)
  , CppDecl(..)
  , CppExpr(..)
  , CppStmt(..)
  , CppType(..)
  , CppLiteral(..)
  , CppParam(..)
  )

spec :: Spec
spec = describe "Fluxus.CodeGen.CPP.IdentifierSanitizer" $ do
  describe "sanitizeIdentifier" $ do
    it "leaves safe identifiers unchanged" $ do
      sanitizeIdentifier "foo" `shouldBe` "foo"
      sanitizeIdentifier "bar_baz" `shouldBe` "bar_baz"
      sanitizeIdentifier "camelCase" `shouldBe` "camelCase"

    it "suffixes reserved C++ keywords with _fluxus" $ do
      sanitizeIdentifier "class" `shouldBe` "class_fluxus"
      sanitizeIdentifier "return" `shouldBe` "return_fluxus"
      sanitizeIdentifier "int" `shouldBe` "int_fluxus"
      sanitizeIdentifier "namespace" `shouldBe` "namespace_fluxus"

    it "prefixes identifiers starting with digit" $ do
      sanitizeIdentifier "123abc" `shouldBe` "fluxus_123abc"

    it "returns fallback for empty string" $ do
      sanitizeIdentifier "" `shouldBe` "fluxus_symbol"

    it "leaves qualified names (::) unchanged" $ do
      sanitizeIdentifier "std::vector" `shouldBe` "std::vector"

    it "leaves names with spaces unchanged" $ do
      sanitizeIdentifier "has space" `shouldBe` "has space"

    it "leaves Python dunder names unchanged" $ do
      sanitizeIdentifier "__len__" `shouldBe` "__len__"
      sanitizeIdentifier "__init__" `shouldBe` "__init__"

    it "prefixes leading double underscore or underscore+uppercase" $ do
      sanitizeIdentifier "__internal" `shouldBe` "fluxus__internal"
      sanitizeIdentifier "_Uppercase" `shouldBe` "fluxus_Uppercase"

  describe "sanitizeCppUnit" $ do
    it "sanitizes namespace names" $ do
      let unit = CppUnit [] ["class"] []
      cppNamespaces (sanitizeCppUnit unit) `shouldBe` ["class_fluxus"]

    it "sanitizes function names in declarations" $ do
      let unit = CppUnit [] [] [CppFunction "return" CppVoid [] []]
      let decls = cppDeclarations (sanitizeCppUnit unit)
      case decls of
        [CppFunction name _ _ _] -> name `shouldBe` "return_fluxus"
        _ -> expectationFailure "expected single CppFunction"

    it "sanitizes variable names in CppVar expressions" $ do
      let unit = CppUnit [] [] [CppVariable "int" CppInt (Just (CppVar "class"))]
      let decls = cppDeclarations (sanitizeCppUnit unit)
      case decls of
        [CppVariable vname _ (Just (CppVar ename))] -> do
          vname `shouldBe` "int_fluxus"
          ename `shouldBe` "class_fluxus"
        _ -> expectationFailure "expected CppVariable with CppVar init"
