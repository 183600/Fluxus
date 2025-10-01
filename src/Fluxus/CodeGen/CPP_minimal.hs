{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Fluxus.CodeGen.CPP
  ( CppCodeGen
  , CppGenState(..)
  , CppGenConfig(..)
  ) where

import Control.Monad.State
import Data.Text (Text)

-- Minimal implementation for testing
type CppCodeGen = State CppGenState

data CppGenState = CppGenState
  { cgIndentLevel :: Int
  , cgOutput :: Text
  }

data CppGenConfig = CppGenConfig
  { cgUseModernCpp :: Bool
  }

-- Placeholder functions
instance Show CppGenState where
  show _ = "CppGenState"

instance Show CppGenConfig where
  show _ = "CppGenConfig"