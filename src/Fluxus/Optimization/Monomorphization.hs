{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module Fluxus.Optimization.Monomorphization
  ( MonomorphizationM
  , MonomorphizationEnv
  , monomorphize
  , specializeFunction
  ) where

import Fluxus.AST.Common
import Control.Monad.Reader
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)

type MonomorphizationM = Reader MonomorphizationEnv
type MonomorphizationEnv = HashMap QualifiedName [Type]

monomorphize :: CommonExpr -> MonomorphizationM CommonExpr
monomorphize expr = pure expr

specializeFunction :: QualifiedName -> [Type] -> MonomorphizationM QualifiedName
specializeFunction name _ = pure name
