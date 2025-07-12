{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module Fluxus.Optimization.Devirtualization
  ( DevirtualizationM
  , DevirtualizationEnv
  , devirtualize
  , resolveVirtualCall
  ) where

import Fluxus.AST.Common
import Control.Monad.Reader
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)

type DevirtualizationM = Reader DevirtualizationEnv
type DevirtualizationEnv = HashMap QualifiedName Type

devirtualize :: CommonExpr -> DevirtualizationM CommonExpr
devirtualize expr = pure expr

resolveVirtualCall :: QualifiedName -> Type -> DevirtualizationM (Maybe QualifiedName)
resolveVirtualCall _ _ = pure Nothing
