{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Fluxus.CodeGen.CPP.Emit
  ( mapPythonTypeToCpp
  , mapGoTypeToCpp
  , mapCommonTypeToCpp
  , mapPythonLiteral
  , mapGoLiteral
  , mapPythonBinaryOp
  , mapGoBinaryOp
  , mapComparisonOp
  , buildPrintExpr
  , buildPrintlnExpr
  , streamChain
  , spaceSeparate
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Fluxus.AST.Common
import Fluxus.AST.Go
import Fluxus.AST.Python
import Fluxus.CodeGen.CPP.Types

mapPythonTypeToCpp :: Type -> CppType
mapPythonTypeToCpp = \case
  TInt _     -> CppInt
  TFloat _   -> CppDouble
  TBool      -> CppBool
  TString    -> CppString
  TList t    -> CppVector (mapPythonTypeToCpp t)
  TDict k v  -> CppUnorderedMap (mapPythonTypeToCpp k) (mapPythonTypeToCpp v)
  TOptional t -> CppOptional (mapPythonTypeToCpp t)
  TOwned t    -> CppUniquePtr (mapPythonTypeToCpp t)
  TShared t   -> CppSharedPtr (mapPythonTypeToCpp t)
  TVoid       -> CppVoid
  _           -> CppAuto

mapGoTypeToCpp :: GoType -> CppType
mapGoTypeToCpp = \case
  GoBasicType (Identifier "int")      -> CppInt
  GoBasicType (Identifier "float64")  -> CppDouble
  GoBasicType (Identifier "bool")     -> CppBool
  GoBasicType (Identifier "string")   -> CppString
  GoSliceType (Located _ elemType)     -> CppVector (mapGoTypeToCpp elemType)
  GoMapType (Located _ keyType) (Located _ valueType) ->
    CppUnorderedMap (mapGoTypeToCpp keyType) (mapGoTypeToCpp valueType)
  GoPointerType (Located _ baseType)   -> CppPointer (mapGoTypeToCpp baseType)
  GoChanType _ (Located _ elemType)    -> CppTemplateType "Channel" [mapGoTypeToCpp elemType]
  _                                    -> CppAuto

mapCommonTypeToCpp :: Type -> CppType
mapCommonTypeToCpp = mapPythonTypeToCpp

mapPythonLiteral :: PythonLiteral -> CppLiteral
mapPythonLiteral = \case
  PyInt i    -> CppIntLit i
  PyFloat f  -> CppFloatLit f
  PyBool b   -> CppBoolLit b
  PyString s -> CppStringLit s
  PyFString s _ -> CppStringLit s
  PyNone     -> CppNullPtr
  _          -> CppIntLit 0

mapGoLiteral :: GoLiteral -> CppLiteral
mapGoLiteral = \case
  GoInt i    -> CppIntLit i
  GoFloat f  -> CppFloatLit f
  GoBool b   -> CppBoolLit b
  GoString s -> CppStringLit s
  GoNil      -> CppNullPtr
  _          -> CppIntLit 0

mapPythonBinaryOp :: BinaryOp -> Text
mapPythonBinaryOp = \case
  OpAdd     -> "+"
  OpSub     -> "-"
  OpMul     -> "*"
  OpDiv     -> "/"
  OpFloorDiv -> "/"
  OpMod     -> "%"
  OpBitAnd  -> "&"
  OpBitOr   -> "|"
  OpBitXor  -> "^"
  OpShiftL  -> "<<"
  OpShiftR  -> ">>"
  OpAnd     -> "&&"
  OpOr      -> "||"
  OpConcat  -> "+"
  _         -> "+"

mapGoBinaryOp :: BinaryOp -> Text
mapGoBinaryOp = mapPythonBinaryOp

mapComparisonOp :: ComparisonOp -> Text
mapComparisonOp = \case
  OpEq    -> "=="
  OpNe    -> "!="
  OpLt    -> "<"
  OpLe    -> "<="
  OpGt    -> ">"
  OpGe    -> ">="
  OpIs    -> "=="
  OpIsNot -> "!="

buildPrintExpr :: [CppExpr] -> CppExpr
buildPrintExpr args =
  let components = if null args then [CppLiteral (CppStringLit "")] else spaceSeparate args
  in streamChain (CppVar "std::cout") components

buildPrintlnExpr :: [CppExpr] -> CppExpr
buildPrintlnExpr args =
  let components =
        if null args then [CppVar "std::endl"] else spaceSeparate args ++ [CppVar "std::endl"]
  in streamChain (CppVar "std::cout") components

streamChain :: CppExpr -> [CppExpr] -> CppExpr
streamChain = foldl (CppBinary "<<")

spaceSeparate :: [CppExpr] -> [CppExpr]
spaceSeparate [] = []
spaceSeparate (x:xs) = x : concatMap (\expr -> [CppLiteral (CppStringLit " "), expr]) xs
