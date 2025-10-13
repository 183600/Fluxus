{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Utils.Pretty (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import qualified Prettyprinter as PP

import Fluxus.Utils.Pretty

spec :: Spec
spec = describe "Pretty Printer" $ do
  basicPrettyPrintSpec
  renderingSpec
  colorSpec
  propertyBasedSpec

basicPrettyPrintSpec :: Spec
basicPrettyPrintSpec = describe "Basic Pretty Printing" $ do
  it "pretty prints text" $ do
    let doc = text "hello"
    T.unpack (renderDoc doc) `shouldContain` "hello"
  
  it "pretty prints integers" $ do
    let doc = int 42
    T.unpack (renderDoc doc) `shouldContain` "42"
  
  it "pretty prints doubles" $ do
    let doc = double 3.14
    T.unpack (renderDoc doc) `shouldContain` "3.14"
  
  it "pretty prints booleans" $ do
    let doc1 = bool True
    let doc2 = bool False
    T.unpack (renderDoc doc1) `shouldContain` "true"
    T.unpack (renderDoc doc2) `shouldContain` "false"
  
  it "handles empty documents" $ do
    let doc = empty
    renderDoc doc `shouldBe` ""
  
  it "concatenates documents vertically" $ do
    let doc = text "line1" </> text "line2"
    let output = T.unpack (renderDoc doc)
    output `shouldContain` "line1"
    output `shouldContain` "line2"

renderingSpec :: Spec
renderingSpec = describe "Document Rendering" $ do
  it "renders documents to text" $ do
    let doc = text "hello" PP.<+> text "world"
    let rendered = renderDoc doc
    T.length rendered `shouldSatisfy` (> 0)
    rendered `shouldSatisfy` (T.isInfixOf "hello")
  
  it "renders compact documents" $ do
    let doc = text "line1" </> text "line2"
    let rendered = renderCompact doc
    T.length rendered `shouldSatisfy` (> 0)
  
  it "handles quotes" $ do
    let doc = quotes (text "quoted")
    let rendered = renderDoc doc
    T.unpack rendered `shouldContain` "'"
    T.unpack rendered `shouldContain` "quoted"
  
  it "handles double quotes" $ do
    let doc = doubleQuotes (text "quoted")
    let rendered = renderDoc doc
    T.unpack rendered `shouldContain` "\""
    T.unpack rendered `shouldContain` "quoted"

colorSpec :: Spec
colorSpec = describe "Color and Styling" $ do
  it "applies red color" $ do
    let doc = red (text "error")
    renderDoc doc `shouldSatisfy` ((> 0) . T.length)
  
  it "applies green color" $ do
    let doc = green (text "success")
    renderDoc doc `shouldSatisfy` ((> 0) . T.length)
  
  it "applies blue color" $ do
    let doc = blue (text "info")
    renderDoc doc `shouldSatisfy` ((> 0) . T.length)
  
  it "applies yellow color" $ do
    let doc = yellow (text "warning")
    renderDoc doc `shouldSatisfy` ((> 0) . T.length)
  
  it "applies underline style" $ do
    let doc = underline (text "emphasized")
    renderDoc doc `shouldSatisfy` ((> 0) . T.length)

propertyBasedSpec :: Spec
propertyBasedSpec = describe "Property-Based Tests" $ do
  it "text rendering preserves content" $ property $ \s ->
    not (null s) ==>
    let doc = text (T.pack s)
        rendered = T.unpack (renderDoc doc)
    in any (`elem` s) rendered
  
  it "empty document renders to empty text" $ do
    let doc = empty
    renderDoc doc `shouldBe` ""
  
  it "vertical composition creates multi-line output" $ do
    let doc = text "a" </> text "b"
    let rendered = T.unpack (renderDoc doc)
    length (lines rendered) `shouldSatisfy` (>= 2)
  
  it "pretty instances work for common types" $ do
    T.length (renderDoc (Fluxus.Utils.Pretty.pretty (42 :: Int))) `shouldSatisfy` (> 0)
    T.length (renderDoc (Fluxus.Utils.Pretty.pretty (3.14 :: Double))) `shouldSatisfy` (> 0)
    T.length (renderDoc (Fluxus.Utils.Pretty.pretty True)) `shouldSatisfy` (> 0)
    T.length (renderDoc (text "hello")) `shouldSatisfy` (> 0)
