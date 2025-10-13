{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Parser.GoSyntax (spec) where

import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, takeFileName)
import Test.Hspec

import Fluxus.Parser.Go.Lexer (runGoLexer)
import Fluxus.Parser.Go.Parser (runGoParser)

spec :: Spec
spec = describe "Go Comprehensive Syntax Coverage" $ do
  let dir = "test/go250923"
  it "parses all Go examples in test/go250923" $ do
    exists <- doesDirectoryExist dir
    exists `shouldBe` True
    files <- filter ((== ".go") . takeExtension) <$> listDirectory dir
    files `shouldSatisfy` (not . null)
    forM_ files $ \f -> do
      let fp = dir </> f
      src <- TIO.readFile fp
      case runGoLexer (T.pack (takeFileName fp)) src of
        Left err -> expectationFailure $ "Lexer failed for " ++ fp ++ ": " ++ show err
        Right toks -> case runGoParser (T.pack (takeFileName fp)) toks of
          Left perr -> expectationFailure $ "Parser failed for " ++ fp ++ ": " ++ show perr
          Right _ -> return ()
