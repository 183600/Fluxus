{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Fluxus.CodeGen.CPP.Go.TypeMapping
  ( mapGoTypeToCpp
  , collectCppTypeIncludes
  ) where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set

import Fluxus.AST.Common
  ( Identifier(..)
  , Located(..)
  , ModuleName(..)
  , QualifiedName(..)
  )
import Fluxus.AST.Go
  ( GoType(..)
  , GoField(..)
  , GoExpr(..)
  , GoLiteral(..)
  )
import Fluxus.CodeGen.CPP.AST
  ( CppType(..)
  )

-- | Map a parsed Go type to its C++ counterpart.
mapGoTypeToCpp :: GoType -> CppType
mapGoTypeToCpp = \case
  GoBasicType ident -> mapGoBasicType ident
  GoArrayType sizeExpr elemType -> mapGoArrayType sizeExpr elemType
  GoSliceType elemType -> CppVector (mapLocatedType elemType)
  GoMapType keyType valueType ->
    CppUnorderedMap (mapLocatedType keyType) (mapLocatedType valueType)
  GoChanType _ elemType -> CppTemplateType "Channel" [mapLocatedType elemType]
  GoPointerType baseType -> CppPointer (mapLocatedType baseType)
  GoFuncType params results -> mapGoFuncType params results
  GoStructType fields -> mapGoStructType fields
  GoInterfaceType _ -> CppTemplateType "std::any" []
  GoNamedType name -> mapGoNamedType name
  GoGenericType name args -> mapGoGenericType name args
  GoTypeParam ident _ -> mapGoTypeParam ident
  GoEllipsisType inner -> CppVector (mapLocatedType inner)
  -- Fallback for currently unsupported forms such as type switches or embedded unions
  _ -> CppTemplateType "std::any" []

-- | Collect standard library includes required for the given C++ type.
collectCppTypeIncludes :: CppType -> [Text]
collectCppTypeIncludes = go
  where
    go ty = case ty of
      CppString -> ["<string>"]
      CppVector inner -> "<vector>" : go inner
      CppStdArray inner _ -> "<array>" : go inner
      CppArray inner _ -> go inner
      CppPointer inner -> go inner
      CppReference inner -> go inner
      CppRvalueRef inner -> go inner
      CppConst inner -> go inner
      CppVolatile inner -> go inner
      CppSizeT -> ["<cstddef>"]
      CppOptional inner -> "<optional>" : go inner
      CppUniquePtr inner -> "<memory>" : go inner
      CppSharedPtr inner -> "<memory>" : go inner
      CppVariant inners -> "<variant>" : concatMap go inners
      CppPair lhs rhs -> "<utility>" : (go lhs ++ go rhs)
      CppTuple inners -> "<tuple>" : concatMap go inners
      CppMap k v -> "<map>" : (go k ++ go v)
      CppUnorderedMap k v -> "<unordered_map>" : (go k ++ go v)
      CppFunctionType args ret -> go ret ++ concatMap go args
      CppClassType name args -> classTypeIncludes name ++ concatMap go args
      CppTemplateType name args -> templateIncludes name ++ concatMap go args
      CppStructLiteral fields -> concatMap (go . snd) fields
      _ -> []

    templateIncludes name
      | name == "std::function" = ["<functional>"]
      | name == "std::any" = ["<any>"]
      | name == "std::variant" = ["<variant>"]
      | name == "std::complex" = ["<complex>"]
      | name == "std::array" = ["<array>"]
      | otherwise = []

    classTypeIncludes name
      | name `Set.member` numericAliasNames = ["<cstdint>"]
      | name == "std::byte" = ["<cstddef>"]
      | name == "std::error_code" = ["<system_error>"]
      | otherwise = []

    numericAliasNames = Set.fromList
      [ "std::int8_t"
      , "std::int16_t"
      , "std::int32_t"
      , "std::int64_t"
      , "std::uint8_t"
      , "std::uint16_t"
      , "std::uint32_t"
      , "std::uint64_t"
      , "std::uintptr_t"
      ]

mapGoBasicType :: Identifier -> CppType
mapGoBasicType (Identifier name) = case name of
  "bool" -> CppBool
  "string" -> CppString
  "int" -> CppInt
  "int8" -> CppClassType "std::int8_t" []
  "int16" -> CppClassType "std::int16_t" []
  "int32" -> CppInt
  "int64" -> CppLongLong
  "uint" -> CppUInt
  "uint8" -> CppClassType "std::uint8_t" []
  "uint16" -> CppClassType "std::uint16_t" []
  "uint32" -> CppUInt
  "uint64" -> CppULongLong
  "uintptr" -> CppClassType "std::uintptr_t" []
  "byte" -> CppClassType "std::uint8_t" []
  "rune" -> CppInt
  "float32" -> CppFloat
  "float64" -> CppDouble
  "complex64" -> CppTemplateType "std::complex" [CppFloat]
  "complex128" -> CppTemplateType "std::complex" [CppDouble]
  "error" -> CppTemplateType "std::any" []
  "any" -> CppTemplateType "std::any" []
  _ -> CppClassType name []

mapGoArrayType :: Located GoExpr -> Located GoType -> CppType
mapGoArrayType sizeExpr elemType =
  case literalArrayLength sizeExpr of
    Just n -> CppStdArray (mapLocatedType elemType) n
    Nothing -> CppVector (mapLocatedType elemType)

mapGoFuncType :: [GoField] -> [GoField] -> CppType
mapGoFuncType params results =
  let paramTypes = concatMap expandField params
      resultTypes = concatMap expandField results
      returnType = case resultTypes of
        [] -> CppVoid
        [single] -> single
        _ -> CppTuple resultTypes
  in CppTemplateType "std::function" [CppFunctionType paramTypes returnType]

mapGoStructType :: [GoField] -> CppType
mapGoStructType fields = CppStructLiteral (snd (foldl' accumulate (Set.empty, []) fields))
  where
    accumulate (used, acc) field =
      let fieldType = mapLocatedType (goFieldType field)
          (names, used') = allocateNames used (goFieldNames field)
          newEntries = [(name, fieldType) | name <- names]
      in (used', acc ++ newEntries)

    allocateNames used fieldNames
      | null fieldNames =
          let (generated, used') = ensureFresh used "field"
          in ([generated], used')
      | otherwise = foldl' collect ([], used) fieldNames

    collect (accNames, usedNames) (Identifier rawName) =
      let (freshName, used') = ensureFresh usedNames rawName
      in (accNames ++ [freshName], used')

mapGoNamedType :: QualifiedName -> CppType
mapGoNamedType name = CppClassType (qualifiedNameToText name) []

mapGoGenericType :: QualifiedName -> [Located GoType] -> CppType
mapGoGenericType name args =
  CppTemplateType (qualifiedNameToText name) (map mapLocatedType args)

mapGoTypeParam :: Identifier -> CppType
mapGoTypeParam (Identifier name) = CppClassType name []

mapLocatedType :: Located GoType -> CppType
mapLocatedType (Located _ inner) = mapGoTypeToCpp inner

literalArrayLength :: Located GoExpr -> Maybe Int
literalArrayLength (Located _ expr) = case expr of
  GoLiteral (GoInt n)
    | n >= 0 && n <= fromIntegral (maxBound :: Int) -> Just (fromIntegral n)
    | otherwise -> Nothing
  _ -> Nothing

qualifiedNameToText :: QualifiedName -> Text
qualifiedNameToText (QualifiedName modules (Identifier name)) =
  T.intercalate "::" (map moduleNameText modules ++ [name])

moduleNameText :: ModuleName -> Text
moduleNameText (ModuleName name) = name

ensureFresh :: Set.Set Text -> Text -> (Text, Set.Set Text)
ensureFresh used base
  | base `Set.member` used = ensureFresh used (base <> "_")
  | otherwise = (base, Set.insert base used)

expandField :: GoField -> [CppType]
expandField field =
  let occurrences = case goFieldNames field of
        [] -> 1
        names -> length names
      fieldType = mapLocatedType (goFieldType field)
  in replicate occurrences fieldType
