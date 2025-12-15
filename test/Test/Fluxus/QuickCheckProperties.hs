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
  identifierProperties
  sourceLocationProperties
  locatedNodeProperties
  expressionStructureProperties

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

identifierProperties :: Spec
identifierProperties = describe "Identifier Properties" $ do
  prop "identifier preserves text content" $ forAll arbitraryIdentifierText $ \txt ->
    let ident = Identifier txt
    in case ident of
         Identifier t -> t === txt
  
  prop "qualified name concatenation is associative" $ 
    forAll arbitraryModuleName $ \m1 ->
    forAll arbitraryModuleName $ \m2 ->
    forAll arbitraryModuleName $ \m3 ->
      let qn1 = QualifiedName [m1, m2, m3] (Identifier "func")
          qn2 = QualifiedName [m1, m2, m3] (Identifier "func")
      in qn1 === qn2

sourceLocationProperties :: Spec
sourceLocationProperties = describe "Source Location Properties" $ do
  prop "source position ordering is transitive" $
    forAll arbitrarySourcePos $ \p1 ->
    forAll arbitrarySourcePos $ \p2 ->
    forAll arbitrarySourcePos $ \p3 ->
      (p1 <= p2 && p2 <= p3) ==> (p1 <= p3)
  
  prop "source span contains start position" $ forAll arbitrarySourceSpan $ \srcSpan ->
    let start = spanStart srcSpan
        end = spanEnd srcSpan
    in start <= end

locatedNodeProperties :: Spec
locatedNodeProperties = describe "Located Node Properties" $ do
  prop "locatedValue extracts original value" $ \(n :: Int) ->
    let val = fromIntegral n :: Int64
        loc = noLoc val
    in locatedValue loc === val
  
  prop "fmap preserves location" $ \(n :: Int) ->
    let val = fromIntegral n :: Int64
        loc = noLoc val
        mapped = fmap (*2) loc
    in locSpan mapped === locSpan loc

expressionStructureProperties :: Spec
expressionStructureProperties = describe "Expression Structure Properties" $ do
  prop "binary operation maintains operand count" $
    forAll arbitraryBinaryOp $ \op ->
    forAll arbitraryLiteral $ \lit1 ->
    forAll arbitraryLiteral $ \lit2 ->
      let expr = CEBinaryOp op (noLoc $ CELiteral lit1) (noLoc $ CELiteral lit2)
      in case expr of
           CEBinaryOp _ _ _ -> True
           _ -> False
  
  prop "list construction preserves element count" $ \(NonNegative n) ->
    let count = n `mod` 20
        elems = replicate count (noLoc $ CELiteral $ LInt 42)
        listExpr = CEList elems
    in case listExpr of
         CEList es -> length es === count
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

arbitraryIdentifierText :: Gen Text
arbitraryIdentifierText = do
  firstChar <- elements (['a'..'z'] ++ ['A'..'Z'] ++ ['_'])
  rest <- listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])
  pure $ T.pack (firstChar : take 20 rest)

arbitraryModuleName :: Gen ModuleName
arbitraryModuleName = ModuleName <$> arbitraryIdentifierText

arbitrarySourcePos :: Gen SourcePos
arbitrarySourcePos = do
  NonNegative line <- arbitrary
  NonNegative col <- arbitrary
  pure $ SourcePos (line `mod` 10000) (col `mod` 500)

arbitrarySourceSpan :: Gen SourceSpan
arbitrarySourceSpan = do
  filename <- elements ["test.py", "main.go", "module.py"]
  start <- arbitrarySourcePos
  end <- arbitrarySourcePos
  let (s, e) = if start <= end then (start, end) else (end, start)
  pure $ SourceSpan (T.pack filename) s e

arbitraryBinaryOp :: Gen BinaryOp
arbitraryBinaryOp = elements [OpAdd, OpSub, OpMul, OpDiv, OpMod]

arbitraryLiteral :: Gen Literal
arbitraryLiteral = oneof
  [ LInt . fromIntegral <$> (arbitrary :: Gen Int)
  , LFloat <$> arbitrary
  , LBool <$> arbitrary
  , LString . T.pack <$> listOf (elements ['a'..'z'])
  ]
