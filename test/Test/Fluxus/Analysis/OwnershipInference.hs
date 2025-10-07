{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Analysis.OwnershipInference (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (void)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Fluxus.Analysis.OwnershipInference

-- Helper function to get variable ownership from analysis
getVariableOwnership' :: OwnershipAnalysis -> Text -> IO OwnershipStatus
getVariableOwnership' analysis var = return $ case HashMap.lookup var (oaVariableOwnership analysis) of
  Just status -> status
  Nothing -> Owned  -- Default fallback

-- Helper function to get variable ownership at specific line
getVariableOwnershipAt' :: OwnershipAnalysis -> Text -> Int -> IO OwnershipStatus
getVariableOwnershipAt' analysis var line = return $
  case HashMap.lookup var (oaVariableOwnershipByLine analysis) of
    Just lineMap -> case HashMap.lookup line lineMap of
      Just status -> status
      Nothing -> Owned  -- Default fallback
    Nothing -> Owned  -- Default fallback

spec :: Spec
spec = describe "Ownership Inference Tests" $ do
  basicOwnershipSpec
  transferOwnershipSpec
  borrowingSpec
  edgeCaseSpec

basicOwnershipSpec :: Spec
basicOwnershipSpec = describe "Basic Ownership Inference" $ do
  it "infers ownership of local variables" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = 42"
          , "    return x"
          ]
    result <- inferOwnershipFromTextIO code
    case result of
      Right analysis -> do
        ownership <- getVariableOwnership' analysis "x"
        ownership `shouldBe` Owned
      Left err -> expectationFailure $ "Inference failed: " ++ show err
  
  it "identifies shared references" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = [1, 2, 3]"
          , "    y = x"
          , "    return y"
          ]
    result <- inferOwnershipFromTextIO code
    case result of
      Right analysis -> do
        ownershipX <- getVariableOwnership' analysis "x"
        ownershipY <- getVariableOwnership' analysis "y"
        ownershipX `shouldBe` Owned
        ownershipY `shouldBe` Shared
      Left err -> expectationFailure $ "Inference failed: " ++ show err

transferOwnershipSpec :: Spec
transferOwnershipSpec = describe "Ownership Transfer" $ do
  it "detects ownership transfer in function calls" $ do
    let code = T.unlines
          [ "def consume(x):"
          , "    pass"
          , ""
          , "def func():"
          , "    x = [1, 2, 3]"
          , "    consume(x)"
          , "    return 42"
          ]
    result <- inferOwnershipFromTextIO code
    case result of
      Right analysis -> do
        ownershipBefore <- getVariableOwnershipAt' analysis "x" 4
        ownershipAfter <- getVariableOwnershipAt' analysis "x" 6
        ownershipBefore `shouldBe` Owned
        ownershipAfter `shouldBe` Moved
      Left err -> expectationFailure $ "Inference failed: " ++ show err

borrowingSpec :: Spec
borrowingSpec = describe "Borrowing Analysis" $ do
  it "identifies immutable borrows" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = [1, 2, 3]"
          , "    y = len(x)"
          , "    return y"
          ]
    result <- inferOwnershipFromTextIO code
    case result of
      Right analysis -> do
        ownership <- getVariableOwnership' analysis "x"
        ownership `shouldBe` Borrowed Immutable
      Left err -> expectationFailure $ "Inference failed: " ++ show err
  
  it "identifies mutable borrows" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = [1, 2, 3]"
          , "    x.append(4)"
          , "    return x"
          ]
    result <- inferOwnershipFromTextIO code
    case result of
      Right analysis -> do
        ownership <- getVariableOwnership' analysis "x"
        ownership `shouldBe` Borrowed Mutable
      Left err -> expectationFailure $ "Inference failed: " ++ show err

edgeCaseSpec :: Spec
edgeCaseSpec = describe "Edge Cases" $ do
  it "handles ownership in loops" $ do
    let code = T.unlines
          [ "def func():"
          , "    result = []"
          , "    for i in range(10):"
          , "        result.append(i)"
          , "    return result"
          ]
    result <- inferOwnershipFromTextIO code
    case result of
      Right analysis -> do
        ownership <- getVariableOwnership' analysis "result"
        ownership `shouldBe` Owned
      Left err -> expectationFailure $ "Inference failed: " ++ show err
  
  it "handles ownership in exception handling" $ do
    let code = T.unlines
          [ "def func():"
          , "    try:"
          , "        x = [1, 2, 3]"
          , "        return x"
          , "    except:"
          , "        return []"
          ]
    result <- inferOwnershipFromTextIO code
    case result of
      Right analysis -> do
        ownership <- getVariableOwnership' analysis "x"
        ownership `shouldBe` Owned
      Left err -> expectationFailure $ "Inference failed: " ++ show err