{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.PythonGolden (spec) where

import Test.Hspec
import System.Process
import System.Exit (ExitCode(..))
import System.Environment (setEnv)

spec :: Spec
spec = describe "Python golden end-to-end suite (via Node runner)" $ do
  it "runs test/test-runner.js and succeeds" $ do
    -- Route the runner to use the local wrapper by default to avoid path checks failing
    -- The Node runner accepts either a file path or a command string; using the local wrapper is reliable in CI
    setEnv "FLUXUS_BINARY" "./bin/fluxus"
    -- Execute the Node-based E2E runner; let it print its own progress
    (code, _out, err) <- readProcessWithExitCode "node" ["test/test-runner.js"] ""
    case code of
      ExitSuccess   -> return ()
      ExitFailure _ -> expectationFailure ("Node test runner failed: " <> take 2000 err)
