{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module Fluxus.Analysis.TypeInference
  ( TypeInferenceM
  , TypeConstraints
  , InferenceResult(..)
  , inferType
  , unifyTypes
  , instantiate
  , generalize
  ) where

import Fluxus.AST.Common
import Control.Monad.State
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

type TypeInferenceM = State TypeInferenceState
type TypeConstraints = [(Type, Type)]

data TypeInferenceState = TypeInferenceState
  { nextTyVar :: Int
  , constraints :: TypeConstraints
  , substitutions :: HashMap TypeVar Type
  } deriving (Show, Generic)

data InferenceResult = InferenceResult
  { resultType :: Type
  , resultConstraints :: TypeConstraints
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

inferType :: CommonExpr -> TypeInferenceM InferenceResult
inferType expr = pure $ InferenceResult TAny []

unifyTypes :: Type -> Type -> TypeInferenceM (Maybe TypeConstraints)
unifyTypes _ _ = pure $ Just []

instantiate :: Type -> TypeInferenceM Type
instantiate t = pure t

generalize :: Type -> Type
generalize = id
