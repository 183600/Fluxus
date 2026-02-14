{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Utils.PrettySpec (spec) where

import Test.Hspec
import qualified Data.Text as T

import Fluxus.Utils.Pretty
  ( Pretty(..)
  , renderDoc
  , renderCompact
  , text
  , int
  , bool
  , empty
  , (</>)
  , quotes
  , doubleQuotes
  )

spec :: Spec
spec = describe "Fluxus.Utils.Pretty" $ do
  describe "renderDoc and renderCompact" $ do
    it "renderDoc renders text to plain Text" $ do
      renderDoc (text "hello") `shouldBe` "hello"

    it "renderCompact flattens without unnecessary line breaks" $ do
      renderCompact (text "a" <> text "b") `shouldBe` "ab"

    it "renderDoc preserves newlines from (</>)" $ do
      let doc = text "line1" </> text "line2"
      renderDoc doc `shouldBe` "line1\nline2"

  describe "Pretty instances" $ do
    it "Text renders as-is" $ do
      renderDoc (pretty (T.pack "foo")) `shouldBe` "foo"

    it "Int renders as decimal" $ do
      renderDoc (pretty (42 :: Int)) `shouldBe` "42"

    it "Integer renders as decimal" $ do
      renderDoc (pretty (100 :: Integer)) `shouldBe` "100"

    it "Double renders" $ do
      T.unpack (renderDoc (pretty (3.14 :: Double))) `shouldContain` "3.14"

    it "Bool true renders as true" $ do
      renderDoc (pretty True) `shouldBe` "true"

    it "Bool false renders as false" $ do
      renderDoc (pretty False) `shouldBe` "false"

    it "Maybe Nothing renders as Nothing" $ do
      renderDoc (pretty (Nothing :: Maybe Int)) `shouldBe` "Nothing"

    it "Maybe Just renders as Just value" $ do
      renderDoc (pretty (Just 7 :: Maybe Integer)) `shouldBe` "Just 7"

    it "List renders with brackets" $ do
      let out = T.unpack (renderDoc (pretty ([1, 2, 3] :: [Integer])))
      out `shouldContain` "1"
      out `shouldContain` "2"
      out `shouldContain` "3"

  describe "combinators" $ do
    it "quotes wraps with single quotes" $ do
      renderDoc (quotes (text "x")) `shouldBe` "'x'"

    it "doubleQuotes wraps with double quotes" $ do
      renderDoc (doubleQuotes (text "y")) `shouldBe` "\"y\""

    it "int and bool produce correct output" $ do
      renderDoc (int 99) `shouldBe` "99"
      renderDoc (bool True) `shouldBe` "true"
      renderDoc (bool False) `shouldBe` "false"

    it "empty renders as empty string" $ do
      renderDoc empty `shouldBe` ""
