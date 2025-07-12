{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module Fluxus.Runtime.Python
  ( PythonRuntime
  , RuntimeValue(..)
  , initPythonRuntime
  , callPythonFunction
  , convertToPython
  , convertFromPython
  ) where

import Fluxus.AST.Common
import Data.Text (Text)
import Data.Int (Int64)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data PythonRuntime = PythonRuntime
  { pythonInterpreter :: Text
  , pythonGlobals :: [(Text, RuntimeValue)]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data RuntimeValue
  = RVInt Int64
  | RVFloat Double
  | RVString Text
  | RVBool Bool
  | RVNone
  | RVObject Text  -- Opaque object reference
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

initPythonRuntime :: IO PythonRuntime
initPythonRuntime = pure $ PythonRuntime "python3" []

callPythonFunction :: PythonRuntime -> Text -> [RuntimeValue] -> IO RuntimeValue
callPythonFunction _ _ _ = pure RVNone

convertToPython :: Literal -> RuntimeValue
convertToPython (LInt i) = RVInt i
convertToPython (LFloat f) = RVFloat f
convertToPython (LString s) = RVString s
convertToPython (LBool b) = RVBool b
convertToPython _ = RVNone

convertFromPython :: RuntimeValue -> Literal
convertFromPython (RVInt i) = LInt i
convertFromPython (RVFloat f) = LFloat f
convertFromPython (RVString s) = LString s
convertFromPython (RVBool b) = LBool b
convertFromPython _ = LNone
