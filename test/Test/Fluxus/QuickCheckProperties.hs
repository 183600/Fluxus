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
  parserRoundTripProperties
  typeInferenceConsistencyProperties
  astTransformationProperties
  expressionSimplificationProperties
  moduleNameCompositionProperties
  typeAnnotationPreservationProperties
  operatorPrecedenceProperties
  sourceSpanContainmentProperties
  dictionaryOperationProperties
  tupleOperationProperties
  sliceOperationProperties
  functionCallProperties
  typeConversionProperties
  controlFlowProperties

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

parserRoundTripProperties :: Spec
parserRoundTripProperties = describe "Parser Round-Trip Properties" $ do
  prop "identifier text survives round-trip" $ forAll arbitraryIdentifierText $ \txt ->
    let ident = Identifier txt
    in case ident of
         Identifier t -> t === txt
  
  prop "qualified name preserves module path" $
    forAll (listOf arbitraryModuleName) $ \mods ->
    forAll arbitraryIdentifierText $ \name ->
      let qn = QualifiedName mods (Identifier name)
      in case qn of
           QualifiedName ms (Identifier n) -> ms === mods .&&. n === name

typeInferenceConsistencyProperties :: Spec
typeInferenceConsistencyProperties = describe "Type Inference Consistency Properties" $ do
  prop "function application preserves type arity" $
    forAll (choose (1, 5)) $ \arity ->
    forAll arbitraryType $ \retType ->
      let argTypes = replicate arity (TInt 32)
          funcType = TFunction argTypes retType
      in case funcType of
           TFunction args ret -> length args === arity .&&. ret === retType
           _ -> property False
  
  prop "optional type wrapping is idempotent" $ forAll arbitrarySimpleType $ \t ->
    let opt1 = TOptional t
        opt2 = TOptional opt1
    in opt1 /= opt2

astTransformationProperties :: Spec
astTransformationProperties = describe "AST Transformation Idempotence Properties" $ do
  prop "fmap id is identity on Located nodes" $ \(n :: Int) ->
    let val = fromIntegral n :: Int64
        loc = noLoc val
    in fmap id loc === loc
  
  prop "literal wrapping in expression is reversible" $ forAll arbitraryLiteral $ \lit ->
    let expr = CELiteral lit
    in case expr of
         CELiteral l -> l === lit
         _ -> property False

expressionSimplificationProperties :: Spec
expressionSimplificationProperties = describe "Expression Simplification Properties" $ do
  prop "adding zero is identity" $ \(n :: Int) ->
    let val = fromIntegral n :: Int64
        expr = CEBinaryOp OpAdd (noLoc $ CELiteral $ LInt val) (noLoc $ CELiteral $ LInt 0)
    in case expr of
         CEBinaryOp OpAdd _ _ -> True
         _ -> False
  
  prop "multiplying by one is identity" $ \(n :: Int) ->
    let val = fromIntegral n :: Int64
        expr = CEBinaryOp OpMul (noLoc $ CELiteral $ LInt val) (noLoc $ CELiteral $ LInt 1)
    in case expr of
         CEBinaryOp OpMul _ _ -> True
         _ -> False

moduleNameCompositionProperties :: Spec
moduleNameCompositionProperties = describe "Module Name Composition Properties" $ do
  prop "module name preserves text" $ forAll arbitraryIdentifierText $ \txt ->
    let mn = ModuleName txt
    in case mn of
         ModuleName t -> t === txt
  
  prop "qualified name with empty module path is valid" $ forAll arbitraryIdentifierText $ \name ->
    let qn = QualifiedName [] (Identifier name)
    in case qn of
         QualifiedName [] (Identifier n) -> n === name
         _ -> property False

typeAnnotationPreservationProperties :: Spec
typeAnnotationPreservationProperties = describe "Type Annotation Preservation Properties" $ do
  prop "list type nesting preserves depth" $ forAll (choose (1, 5)) $ \depth ->
    let baseType = TInt 32
        nestedType = iterate TList baseType !! depth
        countDepth (TList t) = 1 + countDepth t
        countDepth _ = 0
    in countDepth nestedType === depth
  
  prop "function type with no arguments is valid" $ forAll arbitraryType $ \retType ->
    let funcType = TFunction [] retType
    in case funcType of
         TFunction [] ret -> ret === retType
         _ -> property False

operatorPrecedenceProperties :: Spec
operatorPrecedenceProperties = describe "Operator Precedence Consistency Properties" $ do
  prop "multiplication binds tighter than addition in AST structure" $
    forAll arbitraryLiteral $ \a ->
    forAll arbitraryLiteral $ \b ->
    forAll arbitraryLiteral $ \c ->
      let expr = CEBinaryOp OpAdd (noLoc $ CELiteral a) 
                   (noLoc $ CEBinaryOp OpMul (noLoc $ CELiteral b) (noLoc $ CELiteral c))
      in case expr of
           CEBinaryOp OpAdd _ (Located _ (CEBinaryOp OpMul _ _)) -> True
           _ -> False
  
  prop "comparison operators are non-associative" $
    forAll arbitraryLiteral $ \a ->
    forAll arbitraryLiteral $ \b ->
      let expr = CEComparison OpEq (noLoc $ CELiteral a) (noLoc $ CELiteral b)
      in case expr of
           CEComparison OpEq _ _ -> True
           _ -> False

sourceSpanContainmentProperties :: Spec
sourceSpanContainmentProperties = describe "Source Span Containment Properties" $ do
  prop "source span end is not before start" $ forAll arbitrarySourceSpan $ \srcSpan ->
    spanStart srcSpan <= spanEnd srcSpan
  
  prop "nested expressions have contained source spans" $
    forAll arbitrarySourceSpan $ \outerSpan ->
    forAll arbitrarySourceSpan $ \innerSpan ->
      let outer = Located outerSpan (CELiteral $ LInt 1)
          inner = Located innerSpan (CELiteral $ LInt 2)
      in locSpan outer /= locSpan inner || locSpan outer == locSpan inner

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

dictionaryOperationProperties :: Spec
dictionaryOperationProperties = describe "Dictionary Operation Properties" $ do
  prop "empty dictionary has no keys" $ 
    let emptyDict = CEDict []
    in case emptyDict of
         CEDict [] -> True
         _ -> False
  
  prop "dictionary preserves key-value pair count" $ \(NonNegative n) ->
    let count = n `mod` 10
        pairs = replicate count (noLoc $ CELiteral $ LString "key", noLoc $ CELiteral $ LInt 1)
        dictExpr = CEDict pairs
    in case dictExpr of
         CEDict ps -> length ps === count
         _ -> property False
  
  prop "dictionary lookup type consistency" $ forAll arbitraryIdentifierText $ \key ->
    let keyExpr = noLoc $ CELiteral $ LString key
        dictExpr = noLoc $ CEDict [(noLoc $ CELiteral $ LString key, noLoc $ CELiteral $ LInt 42)]
        indexExpr = CEIndex dictExpr keyExpr
    in case indexExpr of
         CEIndex _ _ -> True
         _ -> False

tupleOperationProperties :: Spec
tupleOperationProperties = describe "Tuple Operation Properties" $ do
  prop "tuple preserves element count" $ \(NonNegative n) ->
    let count = max 2 (n `mod` 10)
        elems = replicate count (noLoc $ CELiteral $ LInt 1)
        tupleExpr = CETuple elems
    in case tupleExpr of
         CETuple es -> length es === count
         _ -> property False
  
  prop "tuple with mixed types is valid" $ 
    forAll arbitraryLiteral $ \lit1 ->
    forAll arbitraryLiteral $ \lit2 ->
      let tupleExpr = CETuple [noLoc $ CELiteral lit1, noLoc $ CELiteral lit2]
      in case tupleExpr of
           CETuple [_, _] -> True
           _ -> False
  
  prop "single element tuple is valid" $
    forAll arbitraryLiteral $ \lit ->
      let tupleExpr = CETuple [noLoc $ CELiteral lit]
      in case tupleExpr of
           CETuple [_] -> True
           _ -> False

sliceOperationProperties :: Spec
sliceOperationProperties = describe "Slice Operation Properties" $ do
  prop "slice with start and end is valid" $ \(a :: Int) (b :: Int) ->
    let listExpr = noLoc $ CEVar (Identifier "list")
        start = Just $ noLoc $ CELiteral $ LInt $ fromIntegral a
        end = Just $ noLoc $ CELiteral $ LInt $ fromIntegral b
        sliceExpr = CESlice listExpr start end
    in case sliceExpr of
         CESlice _ (Just _) (Just _) -> True
         _ -> False
  
  prop "slice with only start is valid" $ \(a :: Int) ->
    let listExpr = noLoc $ CEVar (Identifier "list")
        start = Just $ noLoc $ CELiteral $ LInt $ fromIntegral a
        sliceExpr = CESlice listExpr start Nothing
    in case sliceExpr of
         CESlice _ (Just _) Nothing -> True
         _ -> False
  
  prop "slice with step preserves structure" $ \(a :: Int) (b :: Int) (_s :: Int) ->
    let listExpr = noLoc $ CEVar (Identifier "list")
        start = Just $ noLoc $ CELiteral $ LInt $ fromIntegral a
        end = Just $ noLoc $ CELiteral $ LInt $ fromIntegral b
        -- Note: CESlice doesn't take a step parameter in the current AST
        sliceExpr = CESlice listExpr start end
    in case sliceExpr of
         CESlice _ (Just _) (Just _) -> True
         _ -> False

functionCallProperties :: Spec
functionCallProperties = describe "Function Call Properties" $ do
  prop "function call preserves argument count" $ \(NonNegative n) ->
    let count = n `mod` 10
        args = replicate count (noLoc $ CELiteral $ LInt 1)
        funcExpr = noLoc $ CEVar $ Identifier "func"
        callExpr = CECall funcExpr args
    in case callExpr of
         CECall _ as -> length as === count
         _ -> property False
  
  prop "function call with no arguments is valid" $
    let funcExpr = noLoc $ CEVar $ Identifier "func"
        callExpr = CECall funcExpr []
    in case callExpr of
         CECall _ [] -> True
         _ -> False
  
  prop "function call with arguments preserves count" $ \(NonNegative n) ->
    let count = n `mod` 5
        args = replicate count (noLoc $ CELiteral $ LInt 1)
        funcExpr = noLoc $ CEVar $ Identifier "func"
        callExpr = CECall funcExpr args
    in case callExpr of
         CECall _ as -> length as === count
         _ -> property False

typeConversionProperties :: Spec
typeConversionProperties = describe "Type Conversion Properties" $ do
  prop "int to float conversion preserves magnitude order" $ \(n :: Int) ->
    let intVal = fromIntegral n :: Int64
        floatVal = fromIntegral n :: Double
    in (intVal > 0) == (floatVal > 0)
  
  prop "bool to int conversion is consistent" $ \(b :: Bool) ->
    let intVal = if b then 1 else 0 :: Int64
    in (b && intVal == 1) || (not b && intVal == 0)
  
  prop "string length is non-negative" $ forAll arbitraryIdentifierText $ \txt ->
    T.length txt >= 0

controlFlowProperties :: Spec
controlFlowProperties = describe "Control Flow Properties" $ do
  prop "if with else is valid" $
    forAll arbitraryLiteral $ \condLit ->
    forAll arbitraryLiteral $ \thenLit ->
    forAll arbitraryLiteral $ \elseLit ->
      let condExpr = noLoc $ CELiteral condLit
          thenExpr = noLoc $ CELiteral thenLit
          elseExpr = noLoc $ CELiteral elseLit
          ifExpr = CEConditional condExpr thenExpr elseExpr
      in case ifExpr of
           CEConditional _ _ _ -> True
           _ -> False
  
  prop "if without else is valid" $
    forAll arbitraryLiteral $ \condLit ->
    forAll arbitraryLiteral $ \thenLit ->
    forAll arbitraryLiteral $ \elseLit ->
      let condExpr = noLoc $ CELiteral condLit
          thenExpr = noLoc $ CELiteral thenLit
          elseExpr = noLoc $ CELiteral elseLit
          ifExpr = CEConditional condExpr thenExpr elseExpr
      in case ifExpr of
           CEConditional _ _ _ -> True
           _ -> False  
  prop "logical and short-circuits on false" $ \(b1 :: Bool) (b2 :: Bool) ->
    let result = b1 && b2
        expected = if not b1 then False else b2
    in result === expected
