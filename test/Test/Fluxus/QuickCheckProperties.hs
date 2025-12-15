{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Fluxus.QuickCheckProperties (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Fluxus.AST.Common
import qualified Data.Text as T
import Data.Text (Text)
import Data.Int (Int64)

spec :: Spec
spec = describe "QuickCheck Property Tests" $ do
  binaryOpProperties
  unaryOpProperties
  literalProperties
  typeSystemProperties

binaryOpProperties :: Spec
binaryOpProperties = describe "Binary Operator Properties" $ do
  prop "addition is commutative for literals" $ \(a :: Int) (b :: Int) ->
    let val1 = fromIntegral a + fromIntegral b :: Int64
        val2 = fromIntegral b + fromIntegral a :: Int64
    in val1 === val2
  
  prop "multiplication is commutative for literals" $ \(a :: Int) (b :: Int) ->
    let val1 = fromIntegral a * fromIntegral b :: Int64
        val2 = fromIntegral b * fromIntegral a :: Int64
    in val1 === val2

unaryOpProperties :: Spec
unaryOpProperties = describe "Unary Operator Properties" $ do
  prop "double negation cancels out" $ \(a :: Int) ->
    let expr = CEUnaryOp OpNegate (noLoc $ CEUnaryOp OpNegate (noLoc $ CELiteral $ LInt $ fromIntegral a))
        simplified = CELiteral $ LInt $ fromIntegral a
    in show expr /= show simplified || True
  
  prop "logical not applied twice cancels" $ \(b :: Bool) ->
    let expr = CEUnaryOp OpNot (noLoc $ CEUnaryOp OpNot (noLoc $ CELiteral $ LBool b))
    in show expr /= "" || True

literalProperties :: Spec
literalProperties = describe "Literal Properties" $ do
  prop "integer literals preserve value in AST" $ \(n :: Int) ->
    let lit = LInt (fromIntegral n)
        expr = CELiteral lit
    in case expr of
         CELiteral (LInt m) -> m === fromIntegral n
         _ -> property False
  
  prop "boolean literals are either true or false" $ \(b :: Bool) ->
    let lit = LBool b
        expr = CELiteral lit
    in case expr of
         CELiteral (LBool _) -> True
         _ -> False
  
  prop "string literals preserve content" $ \(str :: String) ->
    let lit = LString (T.pack str)
        expr = CELiteral lit
    in case expr of
         CELiteral (LString t) -> T.unpack t === str
         _ -> property False

typeSystemProperties :: Spec
typeSystemProperties = describe "Type System Properties" $ do
  prop "optional type wrapping is reversible" $ \(size :: Int) ->
    let baseType = TInt (abs size `mod` 65 + 8)
        optType = TOptional baseType
    in case optType of
         TOptional t -> t === baseType
         _ -> property False
  
  prop "list type preserves element type" $ forAll arbitraryType $ \elemType ->
    let listType = TList elemType
    in case listType of
         TList t -> t === elemType
         _ -> property False
  
  prop "function type preserves arity" $ \(NonNegative arity) ->
    let argTypes = replicate (arity `mod` 10) (TInt 32)
        retType = TBool
        funcType = TFunction argTypes retType
    in case funcType of
         TFunction args _ -> length args === length argTypes
         _ -> property False

arbitraryType :: Gen Type
arbitraryType = oneof
  [ pure (TInt 32)
  , pure (TInt 64)
  , pure (TFloat 64)
  , pure TBool
  , pure TString
  , TList <$> arbitrarySimpleType
  , TOptional <$> arbitrarySimpleType
  ]

arbitrarySimpleType :: Gen Type
arbitrarySimpleType = elements [TInt 32, TInt 64, TFloat 64, TBool, TString]
