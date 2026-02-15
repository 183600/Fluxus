{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Utils.CommonSpec (spec) where

import Test.Hspec
import qualified Data.Text as T

import Fluxus.Utils.Common
  ( SourcePos(..)
  , SourceSpan(..)
  , mergeSpans
  , textShow
  , defaultSpan
  , zeroWidthSpan
  )

spec :: Spec
spec = describe "Fluxus.Utils.Common" $ do
  describe "SourcePos" $ do
    it "orders by line then column" $ do
      let a = SourcePos 1 0
          b = SourcePos 1 5
          c = SourcePos 2 0
      a < b `shouldBe` True
      b < c `shouldBe` True
      a < c `shouldBe` True

    it "orders by column when lines are equal" $ do
      let a = SourcePos 3 0
          b = SourcePos 3 10
      a < b `shouldBe` True
      a <= b `shouldBe` True
      b > a `shouldBe` True

    it "equates same line and column" $ do
      SourcePos 2 5 `shouldBe` SourcePos 2 5

    it "orders with > and >=" $ do
      let a = SourcePos 1 0
          b = SourcePos 2 0
      b > a `shouldBe` True
      a >= a `shouldBe` True
      b >= a `shouldBe` True
      a >= b `shouldBe` False

  describe "SourceSpan" $ do
    it "stores filename and start/end positions" $ do
      let span' = SourceSpan "file.py" (SourcePos 1 0) (SourcePos 1 10)
      spanFilename span' `shouldBe` "file.py"
      spanStart span' `shouldBe` SourcePos 1 0
      spanEnd span' `shouldBe` SourcePos 1 10

  describe "mergeSpans" $ do
    it "merges two spans to the range from first start to second end" $ do
      let file = "x.py"
          s1 = SourceSpan file (SourcePos 1 0) (SourcePos 1 5)
          s2 = SourceSpan file (SourcePos 2 10) (SourcePos 3 0)
          merged = mergeSpans s1 s2
      spanFilename merged `shouldBe` file
      spanStart merged `shouldBe` SourcePos 1 0
      spanEnd merged `shouldBe` SourcePos 3 0

    it "uses filename from the first span" $ do
      let s1 = SourceSpan "first.py" (SourcePos 0 0) (SourcePos 0 0)
          s2 = SourceSpan "second.py" (SourcePos 1 0) (SourcePos 1 1)
      spanFilename (mergeSpans s1 s2) `shouldBe` "first.py"

    it "mergeSpans with same span yields span with same start and end" $ do
      let s = SourceSpan "f" (SourcePos 2 1) (SourcePos 2 8)
          merged = mergeSpans s s
      spanStart merged `shouldBe` SourcePos 2 1
      spanEnd merged `shouldBe` SourcePos 2 8

    it "mergeSpans is not symmetric in filename" $ do
      let s1 = SourceSpan "a" (SourcePos 1 0) (SourcePos 1 1)
          s2 = SourceSpan "b" (SourcePos 2 0) (SourcePos 2 1)
      spanFilename (mergeSpans s1 s2) `shouldBe` "a"

  describe "textShow" $ do
    it "converts Int to Text" $ do
      textShow (42 :: Int) `shouldBe` T.pack "42"

    it "converts Bool to Text" $ do
      textShow True `shouldBe` T.pack "True"

    it "converts list to Text" $ do
      textShow [1, 2, 3] `shouldBe` T.pack "[1,2,3]"

    it "converts String to Text" $ do
      textShow ("hello" :: String) `shouldBe` T.pack "\"hello\""

    it "converts Maybe to Text" $ do
      textShow (Nothing :: Maybe Int) `shouldBe` T.pack "Nothing"
      textShow (Just 7 :: Maybe Int) `shouldBe` T.pack "Just 7"

  describe "defaultSpan" $ do
    it "produces zero-width span at line 0 column 0" $ do
      let s = defaultSpan "test.py"
      spanFilename s `shouldBe` "test.py"
      spanStart s `shouldBe` SourcePos 0 0
      spanEnd s `shouldBe` SourcePos 0 0

  describe "zeroWidthSpan" $ do
    it "collapses span to start position only" $ do
      let s = SourceSpan "f" (SourcePos 5 3) (SourcePos 10 0)
          z = zeroWidthSpan s
      spanFilename z `shouldBe` "f"
      spanStart z `shouldBe` SourcePos 5 3
      spanEnd z `shouldBe` SourcePos 5 3
