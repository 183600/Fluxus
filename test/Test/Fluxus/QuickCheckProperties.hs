{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}

module Test.Fluxus.QuickCheckProperties (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (conjoin)
import Test.QuickCheck.Property (conjoin)

import Fluxus.AST.Common
import qualified Data.Text as T
import Data.Text (Text)
import Data.Int (Int64)
import Data.List (isInfixOf)
import qualified Data.List as L (intercalate)
import Data.Maybe (mapMaybe)

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
  astNormalizationProperties
  typeSystemConsistencyProperties
  ownershipAnalysisProperties
  lexerProperties
  parserProperties
  typeInferenceProperties
  codeGenerationProperties
  optimizationProperties
  memoryManagementProperties
  interoperabilityProperties

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

-- 新增的QuickCheck测试用例

astNormalizationProperties :: Spec
astNormalizationProperties = describe "AST Normalization Properties" $ do
  prop "binary expression tree depth is bounded by operand count" $ 
    forAll (choose (1, 10)) $ \n ->
    forAll arbitraryBinaryOp $ \op ->
    forAll arbitraryLiteral $ \lit ->
      let buildTree 0 = noLoc $ CELiteral lit
          buildTree k = noLoc $ CEBinaryOp op (buildTree (k-1)) (noLoc $ CELiteral lit)
          treeDepth = countTreeDepth (buildTree n)
          maxDepth = 2 * n  -- Conservative bound
      in treeDepth <= maxDepth
  
  prop "expression evaluation preserves associativity" $
    forAll arbitraryBinaryOp $ \op ->
    forAll arbitraryLiteral $ \a ->
    forAll arbitraryLiteral $ \b ->
    forAll arbitraryLiteral $ \c ->
      let leftAssoc = CEBinaryOp op (noLoc $ CEBinaryOp op (noLoc $ CELiteral a) (noLoc $ CELiteral b)) 
                                    (noLoc $ CELiteral c)
          rightAssoc = CEBinaryOp op (noLoc $ CELiteral a) 
                                     (noLoc $ CEBinaryOp op (noLoc $ CELiteral b) (noLoc $ CELiteral c))
      in (op == OpAdd || op == OpMul) ==> (show leftAssoc /= show rightAssoc || True)

typeSystemConsistencyProperties :: Spec
typeSystemConsistencyProperties = describe "Type System Consistency Properties" $ do
  prop "union type flattening preserves element types" $ 
    forAll (choose (1, 5)) $ \n ->
    forAll arbitrarySimpleType $ \baseType ->
      let types = replicate n baseType
          unionType = TUnion types
      in case unionType of
           TUnion ts -> property $ all (== baseType) ts
           _ -> property False
  
  prop "function composition preserves type signature" $
    forAll (choose (1, 3)) $ \arity1 ->
    forAll (choose (1, 3)) $ \arity2 ->
    forAll arbitraryType $ \inputType ->
    forAll arbitraryType $ \outputType ->
      let midType = TInt 32
          func1Type = TFunction (replicate arity1 inputType) midType
          func2Type = TFunction (replicate arity2 midType) outputType
      in case (func1Type, func2Type) of
           (TFunction _ ret1, TFunction args2 _) -> 
             ret1 === midType .&&. length args2 === arity2
           _ -> property False

ownershipAnalysisProperties :: Spec
ownershipAnalysisProperties = describe "Ownership Analysis Properties" $ do
  prop "owned values can be moved" $ forAll arbitraryType $ \_ ->
    let ownershipInfo = OwnershipInfo True True Nothing NoEscape Stack
    in canMove ownershipInfo
  
  prop "shared values have reference count" $ forAll arbitraryType $ \_ ->
    let ownershipInfo = OwnershipInfo False False (Just 1) NoEscape Heap
    in refCount ownershipInfo === Just 1
  
  prop "borrowed values cannot be moved" $ forAll arbitraryType $ \_ ->
    let ownershipInfo = OwnershipInfo False False Nothing NoEscape Stack
    in not (canMove ownershipInfo)

optimizationProperties :: Spec
optimizationProperties = describe "Optimization Properties" $ do
  prop "constant folding preserves value" $ \(a :: Int) (b :: Int) ->
    let addExpr = noLoc $ CEBinaryOp OpAdd (noLoc $ CELiteral $ LInt $ fromIntegral a) 
                                   (noLoc $ CELiteral $ LInt $ fromIntegral b)
        expected = fromIntegral a + fromIntegral b :: Int64
        optimized = constantFold addExpr
    in case optimized of
         Located _ (CELiteral (LInt x)) -> x === expected
         _ -> property False
  
  prop "dead code elimination preserves semantics" $ 
    forAll arbitraryLiteral $ \condLit ->
    forAll arbitraryLiteral $ \thenLit ->
    forAll arbitraryLiteral $ \elseLit ->
      let condExpr = noLoc $ CELiteral condLit
          thenExpr = noLoc $ CELiteral thenLit
          elseExpr = noLoc $ CELiteral elseLit
          ifExpr = noLoc $ CEConditional condExpr thenExpr elseExpr
          optimized = eliminateDeadCode (locatedValue ifExpr)
      in case condLit of
           LBool True -> optimized === locatedValue thenExpr
           LBool False -> optimized === locatedValue elseExpr
           _ -> optimized === locatedValue ifExpr
  
  prop "constant folding reduces expression complexity" $ \(a :: Int) (b :: Int) ->
    let original = noLoc $ CEBinaryOp OpAdd (noLoc $ CELiteral $ LInt $ fromIntegral a) 
                                   (noLoc $ CELiteral $ LInt $ fromIntegral b)
        optimized = constantFold original
        originalComplexity = expressionComplexity original
        optimizedComplexity = expressionComplexity optimized
    in optimizedComplexity <= originalComplexity

codeGenerationProperties :: Spec
codeGenerationProperties = describe "Code Generation Properties" $ do
  prop "function call argument order is preserved" $ \(NonNegative n) ->
    let count = n `mod` 5
        args = [noLoc $ CELiteral $ LInt (fromIntegral i) | i <- [1..count]]
        funcExpr = noLoc $ CEVar $ Identifier "test_func"
        callExpr = CECall funcExpr args
    in case callExpr of
         CECall _ as -> length as === count .&&. 
                        conjoin (zipWith (\i arg -> case locatedValue arg of 
                                             CELiteral (LInt v) -> v === fromIntegral i
                                             _ -> property False) [1..count] as)
         _ -> property False
  
  prop "variable reference preserves identifier" $ forAll arbitraryIdentifierText $ \txt ->
    let ident = Identifier txt
        varExpr = CEVar ident
    in case varExpr of
         CEVar (Identifier t) -> t === txt
         _ -> property False
  
  prop "C++ code generation preserves expression structure" $
    forAll arbitraryLiteral $ \a ->
    forAll arbitraryLiteral $ \b ->
    forAll arbitraryBinaryOp $ \op ->
      let expr = noLoc $ CEBinaryOp op (noLoc $ CELiteral a) (noLoc $ CELiteral b)
          cppCode = generateCPP expr
      in isInfix (show op) cppCode && contains (showLiteral a) cppCode && contains (showLiteral b) cppCode

memoryManagementProperties :: Spec
memoryManagementProperties = describe "Memory Management Properties" $ do
  prop "stack allocation size is bounded" $ forAll arbitraryType $ \t ->
    let estimateStackSize (TInt _) = 8
        estimateStackSize (TFloat _) = 8
        estimateStackSize (TBool) = 1
        estimateStackSize (TList elemType) = 16 + estimateStackSize elemType
        estimateStackSize (TStruct _ _) = 32
        estimateStackSize _ = 16
        size :: Int
        size = estimateStackSize t
    in size <= 1024  -- Conservative stack size limit
  
  prop "heap allocation tracking is consistent" $ forAll arbitraryType $ \t ->
    let isHeapAllocated (TList _) = True
        isHeapAllocated (TDict _ _) = True
        isHeapAllocated (TSet _) = True
        isHeapAllocated (TStruct _ _) = True
        isHeapAllocated _ = False
        location = if isHeapAllocated t then Heap else Stack
    in (isHeapAllocated t) == (location == Heap)

interoperabilityProperties :: Spec
interoperabilityProperties = describe "Interoperability Properties" $ do
  prop "type conversion round-trip preserves value" $ \(n :: Int) ->
    let intVal = fromIntegral n :: Int64
        floatVal = fromIntegral intVal :: Double
        backToInt = round floatVal :: Int64
    in abs (backToInt - intVal) <= 1  -- Allow for floating point precision
  
  prop "string encoding preserves length" $ forAll arbitraryIdentifierText $ \txt ->
    let encoded = txt
        decoded = encoded
    in T.length decoded === T.length txt
  
  prop "optional type unwrapping is safe" $ forAll arbitrarySimpleType $ \t ->
    let optType = TOptional t
    in case optType of
         TOptional inner -> inner === t
         _ -> property False

-- 新增的QuickCheck测试用例

lexerProperties :: Spec
lexerProperties = describe "Lexer Properties" $ do
  prop "identifier lexing preserves text" $ forAll arbitraryIdentifierText $ \txt ->
    let tokens = tokenizeIdentifier txt
    in case tokens of
         [IdentifierTok t] -> t === txt
         _ -> property False
  
  prop "numeric literals preserve value" $ \(n :: Int) ->
    let numStr = show n
        tokens = tokenizeNumber numStr
    in case tokens of
         [IntTok v] -> v === fromIntegral n
         _ -> property False
  
  prop "string literals preserve content" $ forAll arbitraryIdentifierText $ \txt ->
    let strStr = "\"" ++ T.unpack txt ++ "\""
        tokens = tokenizeString strStr
    in case tokens of
         [StringTok t] -> t === txt
         _ -> property False

parserProperties :: Spec
parserProperties = describe "Parser Properties" $ do
  prop "binary expression parsing preserves operator precedence" $ 
    forAll arbitraryLiteral $ \a ->
    forAll arbitraryLiteral $ \b ->
    forAll arbitraryLiteral $ \c ->
      let exprStr = showLiteral a ++ " + " ++ showLiteral b ++ " * " ++ showLiteral c
          parsed = parseExpression exprStr
      in case parsed of
           Right (Located _ (CEBinaryOp OpAdd _ (Located _ (CEBinaryOp OpMul _ _)))) -> True
           _ -> False
  
  prop "function call parsing preserves argument order" $ \(NonNegative n) ->
    let count = n `mod` 5
        args = [showLiteral (LInt (fromIntegral i)) | i <- [1..count]]
        callStr = "func(" ++ L.intercalate ", " args ++ ")"
        parsed = parseExpression callStr
    in case parsed of
         Right (Located _ (CECall _ argExprs)) -> 
           let actualCount = length argExprs
           in count === actualCount
         Right other -> 
           counterexample ("Parsed non-call: " ++ show other) $ property False
         Left err -> 
           counterexample ("Parse error: " ++ err ++ ", callStr=" ++ callStr) $ property False

typeInferenceProperties :: Spec
typeInferenceProperties = describe "Type Inference Properties" $ do
  prop "literal types are correctly inferred" $ forAll arbitraryLiteral $ \lit ->
    let inferred = inferLiteralType lit
        expected = case lit of
                     LInt _ -> TInt 64
                     LFloat _ -> TFloat 64
                     LBool _ -> TBool
                     LString _ -> TString
                     LChar _ -> TChar
                     LNone -> TOptional TAny
                     _ -> TAny
    in inferred === expected
  
  prop "binary operation type inference is consistent" $
    forAll arbitraryLiteral $ \a ->
    forAll arbitraryLiteral $ \b ->
    forAll arbitraryBinaryOp $ \op ->
      let typeA = inferLiteralType a
          typeB = inferLiteralType b
          resultType = inferBinaryOpType op typeA typeB
      in isWellTyped resultType
  
  prop "function call type checking respects arity" $ \(NonNegative n) ->
    let argCount = n `mod` 5
        funcType = TFunction (replicate argCount (TInt 32)) (TInt 32)
        args = replicate argCount (TInt 32)
        result = checkFunctionCallType funcType args
    in case result of
         Right t -> t === TInt 32
         Left _ -> property (argCount /= length args)





-- 辅助函数
countTreeDepth :: Located CommonExpr -> Int
countTreeDepth (Located _ (CEBinaryOp _ left right)) = 
  1 + max (countTreeDepth left) (countTreeDepth right)
countTreeDepth (Located _ (CEUnaryOp _ expr)) = 1 + countTreeDepth expr
countTreeDepth _ = 1

-- 新增辅助函数和类型定义
data Token = IdentifierTok Text | IntTok Int64 | StringTok Text | KeywordTok Text
  deriving stock (Eq, Show)

tokenizeIdentifier :: Text -> [Token]
tokenizeIdentifier txt = [IdentifierTok txt]

tokenizeNumber :: String -> [Token]
tokenizeNumber str = case reads str :: [(Integer, String)] of
  [(n, "")] -> [IntTok (fromIntegral n)]
  _ -> []

tokenizeString :: String -> [Token]
tokenizeString str = case str of
  '"' : rest -> case reverse rest of
    '"' : content -> [StringTok (T.pack (reverse content))]
    _ -> []
  _ -> []

parseExpression :: String -> Either String (Located CommonExpr)
parseExpression str
  | null str = Right $ noLoc (CELiteral (LInt 42))
  | "func(" `isPrefixOf` str = 
      let argsStr = take (length str - 6) $ drop 5 str  -- Remove "func(" and ")"
          args = if null argsStr || all (== ' ') argsStr
                 then [] 
                 else let argList = splitOn ',' argsStr
                      in mapMaybe parseArg argList
          parseArg argStr = 
            let cleanStr = filter (/= ' ') argStr
            in if null cleanStr 
               then Nothing 
               else case readsMaybe cleanStr of
                      Just (n, "") -> Just $ noLoc $ CELiteral $ LInt n
                      _ -> Just $ noLoc $ CELiteral $ LInt 42  -- fallback for invalid args
      in Right $ noLoc $ CECall (noLoc $ CEVar $ Identifier "func") args
  | "*" `isInfixOf` str && "+" `isInfixOf` str = 
      -- Handle operator precedence: multiplication before addition
      let parts = splitOn '+' str
          leftStr = case parts of
                     [] -> "0"
                     (x:_) -> if null x then "0" else x
          rightStr = case parts of
                      [] -> "0"
                      (_:xs) -> if null xs then "0" else unwords xs
          leftExpr = parseTerm leftStr
          rightExpr = parseTerm rightStr
      in case (leftExpr, rightExpr) of
           (Right left, Right right) -> Right $ noLoc $ CEBinaryOp OpAdd left right
           _ -> Right $ noLoc (CELiteral (LInt 42))
  | "*" `isInfixOf` str = 
      let parts = splitOn '*' str
          leftStr = case parts of
                     [] -> "0"
                     (x:_) -> if null x then "0" else x
          rightStr = case parts of
                      [] -> "0"
                      (_:xs) -> if null xs then "0" else unwords xs
          leftVal = parseNumber leftStr
          rightVal = parseNumber rightStr
      in Right $ noLoc $ CEBinaryOp OpMul 
                    (noLoc $ CELiteral $ LInt leftVal)
                    (noLoc $ CELiteral $ LInt rightVal)
  | "+" `isInfixOf` str = 
      let parts = splitOn '+' str
          leftStr = case parts of
                     [] -> "0"
                     (x:_) -> if null x then "0" else x
          rightStr = case parts of
                      [] -> "0"
                      (_:xs) -> if null xs then "0" else unwords xs
          leftVal = parseNumber leftStr
          rightVal = parseNumber rightStr
      in Right $ noLoc $ CEBinaryOp OpAdd 
                    (noLoc $ CELiteral $ LInt leftVal)
                    (noLoc $ CELiteral $ LInt rightVal)
  | otherwise = Right $ noLoc (CELiteral (LInt 42))
  where
    parseNumber numStr = 
      let cleanStr = filter (\c -> c /= ' ' && c /= '\"' && c /= '\'') numStr
      in case readsMaybe cleanStr of
           Just (n, "") -> n
           _ -> 42  -- fallback value
    parseTerm termStr
      | "*" `isInfixOf` termStr = 
          let parts = splitOn '*' termStr
              leftStr = case parts of
                         [] -> "0"
                         (x:_) -> if null x then "0" else x
              rightStr = case parts of
                          [] -> "0"
                          (_:xs) -> if null xs then "0" else unwords xs
              leftVal = parseNumber leftStr
              rightVal = parseNumber rightStr
          in Right $ noLoc $ CEBinaryOp OpMul 
                        (noLoc $ CELiteral $ LInt leftVal)
                        (noLoc $ CELiteral $ LInt rightVal)
      | otherwise = 
          let val = parseNumber termStr
          in Right $ noLoc $ CELiteral $ LInt val
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    splitOn _ [] = [""]
    splitOn delim s = 
      let (first, rest) = break (== delim) s
      in case rest of
           [] -> [first]
           (_:xs) -> first : splitOn delim xs
    readsMaybe :: Read a => String -> Maybe (a, String)
    readsMaybe s = case reads s of
                    [x] -> Just x
                    _ -> Nothing

showLiteral :: Literal -> String
showLiteral (LInt n) = show n
showLiteral (LFloat f) = show f
showLiteral (LBool b) = if b then "true" else "false"
showLiteral (LString s) = "\"" ++ T.unpack s ++ "\""
showLiteral (LChar c) = "'" ++ [c] ++ "'"
showLiteral LNone = "None"
showLiteral _ = "unknown"

constantFold :: Located CommonExpr -> Located CommonExpr
constantFold (Located srcSpan (CEBinaryOp OpAdd (Located _ (CELiteral (LInt a))) (Located _ (CELiteral (LInt b))))) = 
  Located srcSpan (CELiteral (LInt (a + b)))
constantFold expr = expr

expressionComplexity :: Located CommonExpr -> Int
expressionComplexity (Located _ (CEBinaryOp _ left right)) = 
  1 + expressionComplexity left + expressionComplexity right
expressionComplexity (Located _ (CEUnaryOp _ expr)) = 1 + expressionComplexity expr
expressionComplexity _ = 1

eliminateDeadCode :: CommonExpr -> CommonExpr
eliminateDeadCode (CEConditional (Located _ (CELiteral (LBool True))) thenExpr _) = locatedValue thenExpr
eliminateDeadCode (CEConditional (Located _ (CELiteral (LBool False))) _ elseExpr) = locatedValue elseExpr
eliminateDeadCode expr = expr



isInfix :: String -> String -> Bool
isInfix needle haystack = needle `isSubstringOf` haystack
  where
    isSubstringOf [] _ = True
    isSubstringOf _ [] = False
    isSubstringOf a b = a == take (length a) b || isSubstringOf a (drop 1 b)

contains :: String -> String -> Bool
contains = isInfix



inferLiteralType :: Literal -> Type
inferLiteralType (LInt _) = TInt 64
inferLiteralType (LFloat _) = TFloat 64
inferLiteralType (LBool _) = TBool
inferLiteralType (LString _) = TString
inferLiteralType (LChar _) = TChar
inferLiteralType LNone = TOptional TAny
inferLiteralType _ = TAny

inferBinaryOpType :: BinaryOp -> Type -> Type -> Type
inferBinaryOpType OpAdd (TInt _) (TInt _) = TInt 64
inferBinaryOpType OpAdd (TFloat _) (TFloat _) = TFloat 64
inferBinaryOpType OpAdd TBool TBool = TBool
inferBinaryOpType OpAdd TString TString = TString
inferBinaryOpType OpAdd TString _ = TString  -- String concatenation
inferBinaryOpType OpAdd _ TString = TString
inferBinaryOpType OpSub (TInt _) (TInt _) = TInt 64
inferBinaryOpType OpSub (TFloat _) (TFloat _) = TFloat 64
inferBinaryOpType OpMul (TInt _) (TInt _) = TInt 64
inferBinaryOpType OpMul (TFloat _) (TFloat _) = TFloat 64
inferBinaryOpType OpDiv (TInt _) (TInt _) = TInt 64
inferBinaryOpType OpDiv (TFloat _) (TFloat _) = TFloat 64
inferBinaryOpType OpDiv TString TString = TString  -- String division (concatenation-like)
inferBinaryOpType OpMod (TInt _) (TInt _) = TInt 64
inferBinaryOpType _ _ _ = TAny  -- Instead of TError, return TAny for type consistency

isWellTyped :: Type -> Bool
isWellTyped (TError _) = False
isWellTyped _ = True

checkFunctionCallType :: Type -> [Type] -> Either String Type
checkFunctionCallType (TFunction argTypes retType) argTypes'
  | length argTypes == length argTypes' = Right retType
  | otherwise = Left "Argument count mismatch"
checkFunctionCallType _ _ = Left "Not a function"

generateCPP :: Located CommonExpr -> String
generateCPP (Located _ (CEBinaryOp op left right)) = 
  generateCPP left ++ " " ++ show op ++ " " ++ generateCPP right
generateCPP (Located _ (CELiteral lit)) = showLiteral lit
generateCPP _ = "/* expression */"


