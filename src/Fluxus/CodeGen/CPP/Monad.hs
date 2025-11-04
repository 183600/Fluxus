{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Shared configuration, state, diagnostics and helper routines for the C++
--   code generation pipeline. This module deliberately keeps language-neutral
--   utilities so that the Python and Go backends can build on the same core
--   infrastructure without sharing incidental details.
module Fluxus.CodeGen.CPP.Monad
  ( -- * Core code generation monad
    CppCodeGen
  , CppGenState(..)
  , CppGenConfig(..)
  , CppCodeGenResult(..)
  , CppCodeGenFailure(..)
  , defaultCppGenConfig
  , initialCppGenState
  , runCppCodeGen
  , runCppCodeGenWithAnnotations
    -- * Diagnostics helpers
  , emitDiagnostic
  , emitInfo
  , emitWarning
  , emitError
  , reportNotImplemented
  , reportUnsupported
  , reportFatalNotImplemented
  , reportFatalUnsupported
  , reportInternalError
    -- * State manipulation helpers
  , cppNoop
  , addInclude
  , addDeclaration
  , ensureHelperFunction
  , ensureRuntimeAbortHelper
  , runtimeAbortCall
  , runtimeAbortStmt
  , recordHoistedGlobal
  , enterNamespace
  , exitNamespace
  , generateTempVar
  , addStatement
  , addComment
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (unless, when)
import Control.Monad.State (StateT, gets, modify, runStateT)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Fluxus.AST.Common
  ( AnalysisAnnotations
  , emptyAnnotations
  )
import Fluxus.CodeGen.CPP.AST
  ( CppDecl(..)
  , CppExpr(..)
  , CppStmt(..)
  , CppType(..)
  , CppLiteral(..)
  , CppParam(..)
  , CppUnit(..)
  )
import Fluxus.CodeGen.CPP.Diagnostics
  ( CppCodeGenError(..)
  , CppDiagnostic(..)
  , DiagnosticSeverity(..)
  )

-------------------------------------------------------------------------------
-- Configuration and state ----------------------------------------------------
-------------------------------------------------------------------------------

data CppGenConfig = CppGenConfig
  { cgcOptimizationLevel :: !Int
  , cgcEnableInterop     :: !Bool
  , cgcTargetCppStd      :: !Text
  , cgcUseSmartPointers  :: !Bool
  , cgcEnableParallel    :: !Bool
  , cgcEnableCoroutines  :: !Bool
  , cgcNamespace         :: !Text
  , cgcHeaderGuard       :: !Text
  , cgcStrictMode        :: !Bool
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

data CppGenState = CppGenState
  { cgsIncludes            :: ![Text]
  , cgsDeclarations        :: ![CppDecl]
  , cgsNamespaces          :: ![Text]
  , cgsTempVarCount        :: !Int
  , cgsSymbolTable         :: !(HashMap Text CppType)
  , cgsHoistedGlobals      :: ![Text]
  , cgsFatalErrors         :: ![CppCodeGenError]
  , cgsConfig              :: !CppGenConfig
  , cgsAnalysisAnnotations :: !AnalysisAnnotations
  , cgsLoggedAnnotationMiss :: !Bool
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Core code generation monad.
type CppCodeGen = StateT CppGenState (Writer [CppDiagnostic])

-- | Result bundle produced when code generation succeeds.
data CppCodeGenResult = CppCodeGenResult
  { cgrUnit        :: !CppUnit
  , cgrDiagnostics :: ![CppDiagnostic]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Failure bundle returned when strict mode aborts code generation.
data CppCodeGenFailure = CppCodeGenFailure
  { cgfErrors      :: ![CppCodeGenError]
  , cgfDiagnostics :: ![CppDiagnostic]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Default configuration used when callers do not provide overrides.
defaultCppGenConfig :: CppGenConfig
defaultCppGenConfig = CppGenConfig
  { cgcOptimizationLevel = 2
  , cgcEnableInterop = True
  , cgcTargetCppStd = "c++20"
  , cgcUseSmartPointers = True
  , cgcEnableParallel = True
  , cgcEnableCoroutines = True
  , cgcNamespace = "hyperstatic"
  , cgcHeaderGuard = "HYPERSTATIC_GENERATED"
  , cgcStrictMode = True
  }

initialCppGenState :: CppGenConfig -> CppGenState
initialCppGenState config = CppGenState
  { cgsIncludes = []
  , cgsDeclarations = []
  , cgsNamespaces = []
  , cgsTempVarCount = 0
  , cgsSymbolTable = HM.empty
  , cgsHoistedGlobals = []
  , cgsFatalErrors = []
  , cgsConfig = config
  , cgsAnalysisAnnotations = emptyAnnotations
  , cgsLoggedAnnotationMiss = False
  }

runCppCodeGenWithAnnotations :: CppGenConfig -> AnalysisAnnotations -> CppCodeGen a -> (a, CppGenState, [CppDiagnostic])
runCppCodeGenWithAnnotations config annotations action =
  let initialState = (initialCppGenState config) { cgsAnalysisAnnotations = annotations }
      ((result, finalState), diagnostics) = runWriter (runStateT action initialState)
  in (result, finalState, diagnostics)

runCppCodeGen :: CppGenConfig -> CppCodeGen a -> (a, CppGenState, [CppDiagnostic])
runCppCodeGen config = runCppCodeGenWithAnnotations config emptyAnnotations

-------------------------------------------------------------------------------
-- Diagnostics ----------------------------------------------------------------
-------------------------------------------------------------------------------

emitDiagnostic :: DiagnosticSeverity -> Text -> CppCodeGen ()
emitDiagnostic severity msg = tell [CppDiagnostic severity msg Nothing]

emitInfo, emitWarning, emitError :: Text -> CppCodeGen ()
emitInfo = emitDiagnostic SeverityInfo
emitWarning = emitDiagnostic SeverityWarning
emitError = emitDiagnostic SeverityError

recordFatalError :: CppCodeGenError -> CppCodeGen ()
recordFatalError err =
  modify $ \s -> s { cgsFatalErrors = cgsFatalErrors s ++ [err] }

reportNotImplemented :: Text -> CppCodeGen ()
reportNotImplemented msg = do
  strict <- gets (cgcStrictMode . cgsConfig)
  if strict
    then do
      emitError msg
      recordFatalError (CppNotImplemented msg)
    else emitWarning msg

reportUnsupported :: Text -> CppCodeGen ()
reportUnsupported msg = do
  strict <- gets (cgcStrictMode . cgsConfig)
  if strict
    then do
      emitError msg
      recordFatalError (CppUnsupported msg)
    else emitWarning msg

reportFatalNotImplemented :: Text -> CppCodeGen ()
reportFatalNotImplemented msg = do
  emitError msg
  recordFatalError (CppNotImplemented msg)

reportFatalUnsupported :: Text -> CppCodeGen ()
reportFatalUnsupported msg = do
  emitError msg
  recordFatalError (CppUnsupported msg)

reportInternalError :: Text -> CppCodeGen ()
reportInternalError msg = do
  emitError msg
  strict <- gets (cgcStrictMode . cgsConfig)
  when strict $ recordFatalError (CppInternalError msg)

cppNoop :: CppStmt
cppNoop = CppStmtSeq []

-------------------------------------------------------------------------------
-- Helpers shared by codegen frontends ---------------------------------------
-------------------------------------------------------------------------------

addInclude :: Text -> CppCodeGen ()
addInclude inc = do
  currentIncludes <- gets cgsIncludes
  unless (inc `elem` currentIncludes) $
    modify $ \s -> s { cgsIncludes = inc : currentIncludes }

addDeclaration :: CppDecl -> CppCodeGen ()
addDeclaration decl =
  modify $ \s -> s { cgsDeclarations = decl : cgsDeclarations s }

ensureHelperFunction :: Text -> CppDecl -> CppCodeGen ()
ensureHelperFunction helperName decl = do
  existingDecls <- gets cgsDeclarations
  let alreadyDefined = any matches existingDecls
  unless alreadyDefined $ addDeclaration decl
  where
    matches (CppFunction name _ _ _) = name == helperName
    matches _ = False

runtimeAbortHelperName :: Text
runtimeAbortHelperName = "fluxus_runtime_abort"

ensureRuntimeAbortHelper :: CppCodeGen ()
ensureRuntimeAbortHelper = do
  addInclude "<stdexcept>"
  ensureHelperFunction runtimeAbortHelperName helperDecl
  where
    helperDecl =
      CppFunction runtimeAbortHelperName CppVoid
        [CppParam "message" (CppPointer (CppConst CppChar)) Nothing]
        [CppThrow (Just (CppCall (CppVar "std::runtime_error") [CppVar "message"]))]

runtimeAbortCall :: Text -> CppCodeGen CppExpr
runtimeAbortCall message = do
  ensureRuntimeAbortHelper
  pure $ CppCall (CppVar runtimeAbortHelperName) [CppLiteral (CppStringLit message)]

runtimeAbortStmt :: Text -> CppCodeGen CppStmt
runtimeAbortStmt message = CppExprStmt <$> runtimeAbortCall message

recordHoistedGlobal :: Text -> CppCodeGen ()
recordHoistedGlobal name =
  modify $ \s ->
    let globals = cgsHoistedGlobals s
    in if name `elem` globals
         then s
         else s { cgsHoistedGlobals = globals ++ [name] }

enterNamespace :: Text -> CppCodeGen ()
enterNamespace ns = modify $ \s -> s { cgsNamespaces = ns : cgsNamespaces s }

exitNamespace :: CppCodeGen ()
exitNamespace = modify $ \s -> s { cgsNamespaces = drop 1 (cgsNamespaces s) }

generateTempVar :: CppCodeGen Text
generateTempVar = do
  count <- gets cgsTempVarCount
  modify $ \s -> s { cgsTempVarCount = count + 1 }
  pure $ "temp_" <> T.pack (show count)

addStatement :: CppStmt -> CppCodeGen ()
addStatement _ = pure ()

addComment :: Text -> CppCodeGen ()
addComment = emitInfo
