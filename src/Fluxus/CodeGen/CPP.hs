{-# LANGUAGE OverloadedStrings #-}

module Fluxus.CodeGen.CPP
  ( -- * Code generation types
    CppCodeGen
  , CppGenState(..)
  , CppGenConfig(..)
  , CppCodeGenResult(..)
  , CppCodeGenFailure(..)
    -- * Main code generation functions
  , generateCpp
  , generateCppWithAnnotations
  , generateCppFromPython
  , generateCppFromGo
    -- * C++ AST types
  , CppUnit(..)
  , CppDecl(..)
  , CppStmt(..)
  , CppExpr(..)
  , CppType(..)
  , CppLiteral(..)
  , CppParam(..)
  , CppCase(..)
    -- * Code generation utilities
  , runCppCodeGen
    -- * Type mapping helpers
  , mapPythonTypeToCpp
  , mapGoTypeToCpp
  , mapCommonTypeToCpp
  ) where

import Fluxus.AST.Common (AnalysisAnnotations, emptyAnnotations)
import Fluxus.AST.Go (GoAST)
import Fluxus.AST.Python (PythonAST)
import Fluxus.CodeGen.CPP.AST
  ( CppCase(..)
  , CppDecl(..)
  , CppExpr(..)
  , CppLiteral(..)
  , CppParam(..)
  , CppStmt(..)
  , CppType(..)
  , CppUnit(..)
  )
import Fluxus.CodeGen.CPP.Go (generateCppFromGo)
import Fluxus.CodeGen.CPP.IdentifierSanitizer (sanitizeCppUnit)
import Fluxus.CodeGen.CPP.Monad
  ( CppCodeGen
  , CppGenConfig(..)
  , CppGenState(..)
  , CppCodeGenFailure(..)
  , CppCodeGenResult(..)
  , runCppCodeGen
  , runCppCodeGenWithAnnotations
  )
import Fluxus.CodeGen.CPP.Python (generateCppFromPython)
import Fluxus.CodeGen.CPP.Shared (mapCommonTypeToCpp, mapGoTypeToCpp, mapPythonTypeToCpp)

-- | Run code generation for either a Python or Go AST using the default analysis annotations.
generateCpp :: CppGenConfig -> Either PythonAST GoAST -> Either CppCodeGenFailure CppCodeGenResult
generateCpp config = generateCppWithAnnotations config emptyAnnotations

-- | Run code generation with externally supplied analysis annotations.
generateCppWithAnnotations
  :: CppGenConfig
  -> AnalysisAnnotations
  -> Either PythonAST GoAST
  -> Either CppCodeGenFailure CppCodeGenResult
generateCppWithAnnotations config annotations ast =
  let (unit, finalState, diagnostics) =
        runCppCodeGenWithAnnotations config annotations $
          case ast of
            Left pyAst -> generateCppFromPython pyAst
            Right goAst -> generateCppFromGo goAst
      sanitizedUnit = sanitizeCppUnit unit
      fatalErrors = cgsFatalErrors finalState
  in if null fatalErrors
       then Right (CppCodeGenResult sanitizedUnit diagnostics)
       else Left (CppCodeGenFailure fatalErrors diagnostics)
