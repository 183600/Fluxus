{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Fluxus.CodeGen.CPP.Monad
  ( CppGenConfig(..)
  , CppGenState(..)
  , CppCodeGen
  , CppCodeGenOutput(..)
  , defaultCppGenConfig
  , initialCppGenState
  , runCppCodeGen
  , emitDiagnostic
  , emitInfo
  , emitWarning
  , emitError
  , addInclude
  , addDeclaration
  , addStatement
  , addCommentDecl
  , addComment
  , recordHoistedGlobal
  , enterNamespace
  , exitNamespace
  , generateTempVar
  , withScopedSymbol
  , lookupSymbolType
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.DeepSeq (NFData)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Fluxus.CodeGen.CPP.Diagnostics
import Fluxus.CodeGen.CPP.Types

-- | Code generation configuration passed to the C++ backend.
data CppGenConfig = CppGenConfig
  { cgcOptimizationLevel :: !Int
  , cgcEnableInterop     :: !Bool
  , cgcTargetCppStd      :: !Text
  , cgcUseSmartPointers  :: !Bool
  , cgcEnableParallel    :: !Bool
  , cgcEnableCoroutines  :: !Bool
  , cgcNamespace         :: !Text
  , cgcHeaderGuard       :: !Text
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Mutable state threaded through the code generator.
data CppGenState = CppGenState
  { cgsIncludes        :: ![Text]
  , cgsDeclarations    :: ![CppDecl]
  , cgsNamespaces      :: ![Text]
  , cgsTempVarCount    :: !Int
  , cgsSymbolTable     :: !(HashMap Text CppType)
  , cgsHoistedGlobals  :: ![Text]
  , cgsPendingStmts    :: ![CppStmt]
  , cgsConfig          :: !CppGenConfig
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Code generation monad with diagnostics and failure information.
type CppCodeGen = ExceptT CppCodeGenError (StateT CppGenState (Writer [CppDiagnostic]))

-- | Output bundle produced by running a code generation action.
data CppCodeGenOutput a = CppCodeGenOutput
  { cgoResult       :: !(Either CppCodeGenError a)
  , cgoFinalState   :: !CppGenState
  , cgoDiagnostics  :: ![CppDiagnostic]
  } deriving stock (Generic)
    deriving anyclass (NFData)

-- | Default configuration used by tests and CLI entrypoints.
defaultCppGenConfig :: CppGenConfig
defaultCppGenConfig = CppGenConfig
  { cgcOptimizationLevel = 2
  , cgcEnableInterop     = True
  , cgcTargetCppStd      = "c++20"
  , cgcUseSmartPointers  = True
  , cgcEnableParallel    = True
  , cgcEnableCoroutines  = True
  , cgcNamespace         = "hyperstatic"
  , cgcHeaderGuard       = "HYPERSTATIC_GENERATED"
  }

-- | Initial generator state.
initialCppGenState :: CppGenConfig -> CppGenState
initialCppGenState config = CppGenState
  { cgsIncludes       = []
  , cgsDeclarations   = []
  , cgsNamespaces     = []
  , cgsTempVarCount   = 0
  , cgsSymbolTable    = HM.empty
  , cgsHoistedGlobals = []
  , cgsPendingStmts   = []
  , cgsConfig         = config
  }

-- | Execute a code generation action, yielding diagnostics and final state.
runCppCodeGen :: CppGenConfig -> CppCodeGen a -> CppCodeGenOutput a
runCppCodeGen config action =
  let initialState = initialCppGenState config
      ((result, finalState), diagnostics) = runWriter (runStateT (runExceptT action) initialState)
  in CppCodeGenOutput
       { cgoResult = result
       , cgoFinalState = finalState
       , cgoDiagnostics = diagnostics
       }

-- | Emit a diagnostic message.
emitDiagnostic :: DiagnosticSeverity -> Text -> CppCodeGen ()
emitDiagnostic severity msg = tell [CppDiagnostic severity msg Nothing]

emitInfo, emitWarning, emitError :: Text -> CppCodeGen ()
emitInfo = emitDiagnostic SeverityInfo
emitWarning = emitDiagnostic SeverityWarning
emitError = emitDiagnostic SeverityError

-- | Include management ------------------------------------------------------
addInclude :: Text -> CppCodeGen ()
addInclude inc = modify' $ \s ->
  if inc `elem` cgsIncludes s
    then s
    else s { cgsIncludes = inc : cgsIncludes s }

-- | Declaration management --------------------------------------------------
addDeclaration :: CppDecl -> CppCodeGen ()
addDeclaration decl = modify' $ \s -> s { cgsDeclarations = decl : cgsDeclarations s }

addStatement :: CppStmt -> CppCodeGen ()
addStatement stmt = modify' $ \s -> s { cgsPendingStmts = stmt : cgsPendingStmts s }

addCommentDecl :: Text -> CppCodeGen ()
addCommentDecl = addDeclaration . CppCommentDecl

addComment :: Text -> CppCodeGen ()
addComment msg = addCommentDecl msg >> emitInfo msg

recordHoistedGlobal :: Text -> CppCodeGen ()
recordHoistedGlobal name = modify' $ \s ->
  if name `elem` cgsHoistedGlobals s
    then s
    else s { cgsHoistedGlobals = cgsHoistedGlobals s ++ [name] }

-- | Namespace management ----------------------------------------------------
enterNamespace :: Text -> CppCodeGen ()
enterNamespace ns = modify' $ \s -> s { cgsNamespaces = ns : cgsNamespaces s }

exitNamespace :: CppCodeGen ()
exitNamespace = modify' $ \s -> s { cgsNamespaces = drop 1 (cgsNamespaces s) }

-- | Symbol table helpers ----------------------------------------------------
withScopedSymbol :: Text -> CppType -> CppCodeGen a -> CppCodeGen a
withScopedSymbol name ty action = do
  original <- gets cgsSymbolTable
  modify' $ \s -> s { cgsSymbolTable = HM.insert name ty (cgsSymbolTable s) }
  result <- action
  modify' $ \s -> s { cgsSymbolTable = original }
  pure result

lookupSymbolType :: Text -> CppCodeGen (Maybe CppType)
lookupSymbolType name = HM.lookup name <$> gets cgsSymbolTable

-- | Temporary variable creation --------------------------------------------
generateTempVar :: CppCodeGen Text
generateTempVar = do
  count <- gets cgsTempVarCount
  modify' $ \s -> s { cgsTempVarCount = count + 1 }
  pure $ "temp_" <> T.pack (show count)
