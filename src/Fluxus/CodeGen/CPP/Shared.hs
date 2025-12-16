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
  , MemoryLocation(..)
  , Type(..)
  , lookupAnnotations
  )
import Fluxus.CodeGen.CPP.AST 
  ( CppExpr(..)
  , CppType(..)
  , CppLiteral(..)
  )
import Fluxus.CodeGen.CPP.Go.TypeMapping (mapGoTypeToCpp)
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
      let baseType = dropRefQualifiers cppType
          prefersValue = prefersValueSemantics (stripConst baseType)
          pointerLike = isPointerLike cppType
      in if ownsMemory ownership
           then if pointerLike
                  then cppType
                  else if canMove ownership
                         then CppUniquePtr baseType
                         else CppSharedPtr baseType
           else if prefersValue || pointerLike
                  then cppType
                  else CppPointer baseType
    Global -> cppType
    Unknown -> cppType

dropRefQualifiers :: CppType -> CppType
dropRefQualifiers (CppReference inner) = dropRefQualifiers inner
dropRefQualifiers (CppRvalueRef inner) = dropRefQualifiers inner
dropRefQualifiers (CppConst inner) = CppConst (dropRefQualifiers inner)
dropRefQualifiers other = other

stripConst :: CppType -> CppType
stripConst (CppConst inner) = stripConst inner
stripConst other = other

prefersValueSemantics :: CppType -> Bool
prefersValueSemantics ty = case ty of
  CppConst inner -> prefersValueSemantics inner
  CppBool -> True
  CppChar -> True
  CppUChar -> True
  CppShort -> True
  CppUShort -> True
  CppInt -> True
  CppUInt -> True
  CppLong -> True
  CppULong -> True
  CppLongLong -> True
  CppULongLong -> True
  CppFloat -> True
  CppDouble -> True
  CppLongDouble -> True
  CppSizeT -> True
  CppAuto -> True
  CppString -> True
  CppStdArray inner _ -> prefersValueSemantics inner
  CppArray inner _ -> prefersValueSemantics inner
  CppOptional inner -> prefersValueSemantics inner
  CppVariant inners -> all prefersValueSemantics inners
  CppPair a b -> prefersValueSemantics a && prefersValueSemantics b
  CppTuple inners -> all prefersValueSemantics inners
  CppFunctionType args ret -> all prefersValueSemantics args && prefersValueSemantics ret
  _ -> False

isPointerLike :: CppType -> Bool
isPointerLike ty = case ty of
  CppConst inner -> isPointerLike inner
  CppPointer _ -> True
  CppReference _ -> True
  CppRvalueRef _ -> True
  CppUniquePtr _ -> True
  CppSharedPtr _ -> True
  _ -> False

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
