{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Helpers that are shared between the Python and Go C++ code generators.
--   These utilities provide type mapping, common annotation plumbing and small
--   AST manipulation helpers that are independent from any specific frontend.
module Fluxus.CodeGen.CPP.Shared
  ( -- * Type mapping helpers
    mapPythonTypeToCpp
  , mapGoTypeToCpp
  , mapCommonTypeToCpp
    -- * Analysis annotation helpers
  , lookupExprAnnotations
  , applyOwnershipToType
  , applyExprAnnotations
  , lookupAndApplyAnnotations
    -- * AST combinators
  , streamChain
  , spaceSeparate
  ) where

import Control.Monad (when)
import Control.Monad.State (gets, modify)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Fluxus.AST.Common
  ( ExprAnnotations(..)
  , OwnershipInfo(..)
  , Type(..)
  , lookupAnnotations
  )
import Fluxus.AST.Go
import Fluxus.CodeGen.CPP.AST
  ( CppExpr(..)
  , CppLiteral(..)
  , CppType(..)
  )
import Fluxus.CodeGen.CPP.Monad
  ( CppCodeGen
  , CppGenState(..)
  , emitInfo
  )

-------------------------------------------------------------------------------
-- Type mapping ---------------------------------------------------------------
-------------------------------------------------------------------------------

mapPythonTypeToCpp :: Type -> CppType
mapPythonTypeToCpp = \case
  TInt _ -> CppInt
  TFloat _ -> CppDouble
  TBool -> CppBool
  TString -> CppString
  TList t -> CppVector (mapPythonTypeToCpp t)
  TDict k v -> CppUnorderedMap (mapPythonTypeToCpp k) (mapPythonTypeToCpp v)
  TOptional t -> CppOptional (mapPythonTypeToCpp t)
  TOwned t -> CppUniquePtr (mapPythonTypeToCpp t)
  TShared t -> CppSharedPtr (mapPythonTypeToCpp t)
  TVoid -> CppVoid
  _ -> CppAuto

mapGoTypeToCpp :: GoType -> CppType
mapGoTypeToCpp = \case
  GoBasicType (Identifier "int") -> CppInt
  GoBasicType (Identifier "float64") -> CppDouble
  GoBasicType (Identifier "bool") -> CppBool
  GoBasicType (Identifier "string") -> CppString
  GoSliceType (Located _ elemType) -> CppVector (mapGoTypeToCpp elemType)
  GoMapType (Located _ keyType) (Located _ valueType) ->
    CppUnorderedMap (mapGoTypeToCpp keyType) (mapGoTypeToCpp valueType)
  GoPointerType (Located _ baseType) -> CppPointer (mapGoTypeToCpp baseType)
  GoChanType _ (Located _ elemType) -> CppTemplateType "Channel" [mapGoTypeToCpp elemType]
  _ -> CppAuto

mapCommonTypeToCpp :: Type -> CppType
mapCommonTypeToCpp = mapPythonTypeToCpp

-------------------------------------------------------------------------------
-- Analysis annotation helpers ------------------------------------------------
-------------------------------------------------------------------------------

lookupExprAnnotations :: Text -> CppCodeGen (Maybe ExprAnnotations)
lookupExprAnnotations exprKey = do
  annotations <- gets cgsAnalysisAnnotations
  pure $ lookupAnnotations exprKey annotations

applyOwnershipToType :: OwnershipInfo -> CppType -> CppType
applyOwnershipToType ownership cppType =
  case memLocation ownership of
    Stack -> cppType
    Heap ->
      if ownsMemory ownership
        then if canMove ownership
          then CppUniquePtr cppType
          else CppSharedPtr cppType
        else CppPointer cppType
    Global -> cppType
    Unknown -> cppType

applyExprAnnotations :: CppType -> ExprAnnotations -> (CppType, Bool)
applyExprAnnotations defaultType anns =
  let typeFromAnalysis = fmap mapCommonTypeToCpp (eaInferredType anns)
      baseType = fromMaybe defaultType typeFromAnalysis
      withOwnership = maybe baseType (`applyOwnershipToType` baseType) (eaOwnership anns)
      typeChanged = maybe False (/= defaultType) typeFromAnalysis
      ownershipChanged = maybe False (\info -> applyOwnershipToType info baseType /= baseType) (eaOwnership anns)
  in (withOwnership, typeChanged || ownershipChanged)

lookupAndApplyAnnotations :: Text -> Text -> CppType -> CppCodeGen CppType
lookupAndApplyAnnotations context exprKey defaultType = do
  mAnns <- lookupExprAnnotations exprKey
  case mAnns of
    Nothing -> do
      alreadyLogged <- gets cgsLoggedAnnotationMiss
      when (not alreadyLogged) $ do
        emitInfo $ context <> ": no analysis annotations for expression " <> exprKey
        modify $ \s -> s { cgsLoggedAnnotationMiss = True }
      pure defaultType
    Just anns -> do
      let (refinedType, changed) = applyExprAnnotations defaultType anns
      when changed $
        emitInfo $ context <> ": refined type for " <> exprKey <> " -> " <> T.pack (show refinedType)
      when (not (null (eaOptimizationNotes anns))) $
        emitInfo $ context <> ": analysis notes - " <> T.intercalate ", " (eaOptimizationNotes anns)
      pure refinedType

-------------------------------------------------------------------------------
-- AST helpers ----------------------------------------------------------------
-------------------------------------------------------------------------------

streamChain :: CppExpr -> [CppExpr] -> CppExpr
streamChain = foldl (\acc expr -> CppBinary "<<" acc expr)

spaceSeparate :: [CppExpr] -> [CppExpr]
spaceSeparate [] = []
spaceSeparate (x:xs) = x : concatMap (\arg -> [CppLiteral (CppStringLit " "), arg]) xs
