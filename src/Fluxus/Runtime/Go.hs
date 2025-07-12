{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module Fluxus.Runtime.Go
  ( GoRuntime
  , GoValue(..)
  , initGoRuntime
  , callGoFunction
  , convertToGo
  , convertFromGo
  ) where

import Fluxus.AST.Common
import Data.Text (Text)
import Data.Int (Int64)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data GoRuntime = GoRuntime
  { goCompiler :: Text
  , goModules :: [Text]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data GoValue
  = GVInt Int64
  | GVFloat Double
  | GVString Text
  | GVBool Bool
  | GVNil
  | GVInterface Text  -- interface{} value
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

initGoRuntime :: IO GoRuntime
initGoRuntime = pure $ GoRuntime "go" []

callGoFunction :: GoRuntime -> Text -> [GoValue] -> IO GoValue
callGoFunction _ _ _ = pure GVNil

convertToGo :: Literal -> GoValue
convertToGo (LInt i) = GVInt i
convertToGo (LFloat f) = GVFloat f
convertToGo (LString s) = GVString s
convertToGo (LBool b) = GVBool b
convertToGo _ = GVNil

convertFromGo :: GoValue -> Literal
convertFromGo (GVInt i) = LInt i
convertFromGo (GVFloat f) = LFloat f
convertFromGo (GVString s) = LString s
convertFromGo (GVBool b) = LBool b
convertFromGo _ = LNone
