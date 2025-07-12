{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module Fluxus.Analysis.ShapeAnalysis
  ( ShapeAnalysisM
  , ShapeInfo(..)
  , analyzeShape
  , inferShape
  ) where

import Fluxus.AST.Common
import Control.Monad.State
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

type ShapeAnalysisM = State ShapeAnalysisState

data ShapeAnalysisState = ShapeAnalysisState
  { shapeConstraints :: [(Identifier, ShapeInfo)]
  } deriving (Show, Generic)

data ShapeInfo = ShapeInfo
  { dimensions :: Vector Int
  , isKnown :: Bool
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

analyzeShape :: CommonExpr -> ShapeAnalysisM ShapeInfo
analyzeShape _ = pure $ ShapeInfo mempty False

inferShape :: Type -> ShapeInfo
inferShape _ = ShapeInfo mempty False
