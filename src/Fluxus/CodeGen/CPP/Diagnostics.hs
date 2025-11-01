{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Fluxus.CodeGen.CPP.Diagnostics
  ( DiagnosticSeverity(..)
  , CppDiagnostic(..)
  , cppInfo
  , cppWarning
  , cppError
  , CppCodeGenError(..)
  , renderCppCodeGenError
  ) where

import Data.Text (Text)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- | Severity levels for diagnostics emitted during code generation.
data DiagnosticSeverity = SeverityInfo | SeverityWarning | SeverityError
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- | Structured diagnostic emitted by the C++ code generator.
data CppDiagnostic = CppDiagnostic
  { diagSeverity :: !DiagnosticSeverity
  , diagMessage  :: !Text
  , diagContext  :: !(Maybe Text)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Convenient constructors for diagnostics.
cppInfo :: Text -> CppDiagnostic
cppInfo msg = CppDiagnostic SeverityInfo msg Nothing

cppWarning :: Text -> CppDiagnostic
cppWarning msg = CppDiagnostic SeverityWarning msg Nothing

cppError :: Text -> CppDiagnostic
cppError msg = CppDiagnostic SeverityError msg Nothing

-- | Structured error used to abort code generation when unsupported constructs are encountered.
data CppCodeGenError
  = CppNotImplemented !Text
  | CppUnsupported !Text
  | CppInternalError !Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

renderCppCodeGenError :: CppCodeGenError -> Text
renderCppCodeGenError = \case
  CppNotImplemented msg -> "not implemented: " <> msg
  CppUnsupported msg   -> "unsupported: " <> msg
  CppInternalError msg -> "internal error: " <> msg
