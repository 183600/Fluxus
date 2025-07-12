{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module Fluxus.Analysis.EscapeAnalysis
  ( EscapeAnalysisM
  , EscapeEnv
  , analyzeEscape
  , getEscapeInfo
  ) where

import Fluxus.AST.Common
import Control.Monad.Reader
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)

type EscapeAnalysisM = Reader EscapeEnv
type EscapeEnv = HashMap Identifier EscapeInfo

analyzeEscape :: CommonExpr -> EscapeAnalysisM EscapeInfo
analyzeEscape _ = pure NoEscape

getEscapeInfo :: Identifier -> EscapeAnalysisM EscapeInfo
getEscapeInfo _ = pure NoEscape
