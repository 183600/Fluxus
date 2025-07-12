{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module Fluxus.Analysis.OwnershipInference
  ( OwnershipInferenceM
  , OwnershipEnv
  , inferOwnership
  , analyzeOwnership
  ) where

import Fluxus.AST.Common
import Control.Monad.Reader
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)

type OwnershipInferenceM = Reader OwnershipEnv
type OwnershipEnv = HashMap Identifier OwnershipInfo

inferOwnership :: CommonExpr -> OwnershipInferenceM OwnershipInfo
inferOwnership _ = pure defaultOwnership
  where
    defaultOwnership = OwnershipInfo
      { ownsMemory = False
      , canMove = False
      , refCount = Nothing
      , escapes = NoEscape
      , memLocation = Unknown
      }

analyzeOwnership :: [CommonExpr] -> OwnershipInferenceM [OwnershipInfo]
analyzeOwnership exprs = mapM inferOwnership exprs
