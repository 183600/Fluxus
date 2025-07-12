{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module Fluxus.Internal.Types
  ( CompilerM
  , CompilerState(..)
  , CompilerEnv(..)
  , CompilerError(..)
  , SymbolTable
  , TypeTable
  ) where

import Fluxus.AST.Common
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

type CompilerM = ReaderT CompilerEnv (StateT CompilerState (Except CompilerError))
type SymbolTable = HashMap Identifier Type
type TypeTable = HashMap QualifiedName Type

data CompilerState = CompilerState
  { csSymbolTable :: SymbolTable
  , csTypeTable :: TypeTable
  , csNextId :: Int
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

data CompilerEnv = CompilerEnv
  { ceSourceFile :: Text
  , ceOptimizationLevel :: Int
  , ceTargetCppVersion :: Text
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

data CompilerError
  = ParseError Text
  | TypeError Text
  | AnalysisError Text
  | CodeGenError Text
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)
