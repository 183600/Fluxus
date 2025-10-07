{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Fluxus.Analysis.ShapeAnalysis
  ( ShapeAnalysisM
  , ShapeAnalysisState(..)
  , ShapeInfo(..)
  , StructShape(..)
  , ContainerShape(..)
  , CppMapping(..)
  , FunctionSignature(..)
  , ScopeInfo(..)
  , runShapeAnalysis
  , analyzeProgram
  , analyzeStatement
  , analyzeShape
  , inferShape
  , analyzeStructure
  , optimizeDataStructures
  , generateCppStructure
  , inferContainerShape
  , analyzeDictShape
  , analyzeObjectShape
  ) where

import Fluxus.AST.Common
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad (void, when, foldM)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Control.DeepSeq (NFData)
import Data.Maybe (fromMaybe, catMaybes, listToMaybe, isJust)
import Data.List (foldl', sortOn)

type ShapeAnalysisM = ReaderT ShapeContext (StateT ShapeAnalysisState (Except Text))

-- | Context for shape analysis
data ShapeContext = ShapeContext
  { scOptimizeForMemory :: !Bool        -- Optimize for memory usage vs access speed
  , scTargetCppVersion :: !Text         -- Target C++ version for optimizations
  , scMaxInlineSize :: !Int             -- Maximum size for inline optimization
  , scCurrentScope :: !ScopeInfo        -- Current scope information
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Scope information for variable tracking
data ScopeInfo = ScopeInfo
  { siScopeId :: !Int                   -- Unique scope identifier
  , siParentScope :: !(Maybe Int)       -- Parent scope ID
  , siScopeType :: !ScopeType           -- Type of scope
  , siVariables :: !(Set Identifier)    -- Variables declared in this scope
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data ScopeType 
  = GlobalScope
  | FunctionScope Text
  | BlockScope
  | LoopScope
  | ClassScope Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | State for shape analysis
data ShapeAnalysisState = ShapeAnalysisState
  { sasShapeMap :: !(HashMap Identifier ShapeInfo)           -- Variable shape information
  , sasStructShapes :: !(HashMap Text StructShape)           -- Known struct shapes
  , sasContainerShapes :: !(HashMap Identifier ContainerShape) -- Container shape info
  , sasCppMappings :: !(HashMap ShapeInfo CppMapping)        -- Shape to C++ mappings
  , sasOptimizations :: ![Text]                              -- Optimization suggestions
  , sasFunctionSignatures :: !(HashMap Text FunctionSignature) -- Function signatures
  , sasScopes :: !(HashMap Int ScopeInfo)                    -- All scopes
  , sasCurrentScopeId :: !Int                                -- Current scope ID
  , sasNextScopeId :: !Int                                   -- Next available scope ID
  , sasAccessCounts :: !(HashMap Identifier Int)             -- Variable access frequency
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- | Function signature for shape analysis
data FunctionSignature = FunctionSignature
  { fsParameterShapes :: ![ShapeInfo]   -- Parameter shapes
  , fsReturnShape :: !ShapeInfo         -- Return value shape
  , fsIsVariadic :: !Bool               -- Accepts variable arguments
  , fsSideEffects :: !Bool              -- Has side effects on shapes
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Comprehensive shape information
data ShapeInfo = ShapeInfo
  { siDimensions :: !(Vector Int)        -- Dimensions for arrays/tensors
  , siIsKnown :: !Bool                   -- Whether shape is known at compile time
  , siElementType :: !(Maybe Type)       -- Element type for containers
  , siFieldTypes :: !(HashMap Text Type) -- Field types for objects/structs
  , siSize :: !(Maybe Int)               -- Size in bytes if known
  , siAlignment :: !(Maybe Int)          -- Alignment requirements
  , siIsHomogeneous :: !Bool             -- All elements have same type
  , siAccessPattern :: !AccessPattern    -- How data is typically accessed
  , siIsConstant :: !Bool                -- Whether value is constant
  , siOrigin :: !ShapeOrigin             -- Where shape info came from
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData, Hashable)

-- | Origin of shape information
data ShapeOrigin
  = UserAnnotation      -- From type annotation
  | InferredFromValue   -- Inferred from literal/expression
  | PropagatedFrom Text -- Propagated from another variable
  | FunctionReturn Text -- From function return
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)

-- | Access patterns for optimization
data AccessPattern
  = SequentialAccess     -- Sequential iteration
  | RandomAccess         -- Random access by key/index
  | WriteOnceReadMany    -- Written once, read many times
  | ReadOnceWriteMany    -- Read once, written many times
  | StreamingAccess      -- Data flows through without storage
  | UnknownAccess        -- Cannot determine access pattern
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

-- | Structure shapes for objects/classes
data StructShape = StructShape
  { ssFields :: !(HashMap Text Type)     -- Field name to type mapping
  , ssFieldOrder :: ![Text]              -- Field ordering for layout optimization
  , ssSize :: !Int                       -- Total size in bytes
  , ssAlignment :: !Int                  -- Alignment requirement
  , ssPadding :: ![Int]                  -- Padding between fields
  , ssIsPackable :: !Bool                -- Can be packed without padding
  , ssHotFields :: !(Set Text)           -- Frequently accessed fields
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Container shapes for collections
data ContainerShape = ContainerShape
  { csElementType :: !Type               -- Type of elements
  , csCapacity :: !(Maybe Int)           -- Known capacity if static
  , csGrowthPattern :: !GrowthPattern    -- How container grows
  , csIsResizable :: !Bool               -- Whether container can resize
  , csAccessPattern :: !AccessPattern    -- How elements are accessed
  , csKeyType :: !(Maybe Type)           -- Key type for associative containers
  , csAverageSize :: !(Maybe Int)        -- Average size from analysis
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Growth patterns for containers
data GrowthPattern
  = FixedSize              -- Fixed size, no growth
  | LinearGrowth Int       -- Grows by fixed amount
  | ExponentialGrowth      -- Doubles in size
  | LogarithmicGrowth      -- Grows logarithmically
  | UnknownGrowth          -- Cannot determine growth pattern
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | C++ mapping strategies
data CppMapping = CppMapping
  { cmType :: !Text                      -- C++ type to use
  , cmHeaders :: ![Text]                 -- Required headers
  , cmOptimizations :: ![Text]           -- Specific optimizations
  , cmMemoryLayout :: !MemoryLayout      -- Memory layout strategy
  , cmInitializerHint :: !(Maybe Text)   -- Initialization hint
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData, Hashable)

-- | Memory layout strategies
data MemoryLayout
  = ContiguousLayout       -- std::vector, std::array
  | NodeBasedLayout        -- std::list, std::map
  | HashBasedLayout        -- std::unordered_map
  | TreeBasedLayout        -- std::map, std::set
  | StackLayout            -- Stack-allocated
  | HeapLayout             -- Heap-allocated
  | CustomLayout Text      -- Custom structure
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)

-- | Initial shape context
initialContext :: ShapeContext
initialContext = ShapeContext
  { scOptimizeForMemory = False
  , scTargetCppVersion = "c++20"
  , scMaxInlineSize = 64
  , scCurrentScope = initialScope
  }

-- | Initial scope
initialScope :: ScopeInfo
initialScope = ScopeInfo
  { siScopeId = 0
  , siParentScope = Nothing
  , siScopeType = GlobalScope
  , siVariables = Set.empty
  }

-- | Initial shape analysis state
initialState :: ShapeAnalysisState
initialState = ShapeAnalysisState
  { sasShapeMap = HashMap.empty
  , sasStructShapes = HashMap.empty
  , sasContainerShapes = HashMap.empty
  , sasCppMappings = HashMap.empty
  , sasOptimizations = []
  , sasFunctionSignatures = initializeBuiltinFunctions
  , sasScopes = HashMap.singleton 0 initialScope
  , sasCurrentScopeId = 0
  , sasNextScopeId = 1
  , sasAccessCounts = HashMap.empty
  }

-- | Initialize built-in function signatures
initializeBuiltinFunctions :: HashMap Text FunctionSignature
initializeBuiltinFunctions = HashMap.fromList
  [ ("len", FunctionSignature [anyContainerShape] intShape False False)
  , ("append", FunctionSignature [anyContainerShape, unknownShape] voidShape False True)
  , ("extend", FunctionSignature [anyContainerShape, anyContainerShape] voidShape False True)
  , ("push", FunctionSignature [anyContainerShape, unknownShape] voidShape False True)
  , ("pop", FunctionSignature [anyContainerShape] unknownShape False True)
  , ("zeros", FunctionSignature [intShape] arrayShape False False)
  , ("ones", FunctionSignature [intShape] arrayShape False False)
  , ("range", FunctionSignature [intShape, intShape] listShape False False)
  , ("sum", FunctionSignature [anyContainerShape] floatShape False False)
  , ("mean", FunctionSignature [anyContainerShape] floatShape False False)
  , ("max", FunctionSignature [anyContainerShape] unknownShape False False)
  , ("min", FunctionSignature [anyContainerShape] unknownShape False False)
  , ("sort", FunctionSignature [anyContainerShape] anyContainerShape False False)
  , ("reverse", FunctionSignature [anyContainerShape] anyContainerShape False False)
  ]

-- | Run shape analysis
runShapeAnalysis :: ShapeAnalysisM a -> Either Text (a, ShapeAnalysisState)
runShapeAnalysis m = runExcept $ runStateT (runReaderT m initialContext) initialState

-- | Analyze entire program
analyzeProgram :: [Statement] -> ShapeAnalysisM ()
analyzeProgram stmts = do
  enterScope GlobalScope
  mapM_ analyzeStatement stmts
  exitScope

-- | Analyze a statement and update shape information
analyzeStatement :: Statement -> ShapeAnalysisM ()
analyzeStatement (SAssignment pattern expr) = do
  exprShape <- analyzeShape expr
  case pattern of
    PVariable var -> updateVariableShape var exprShape
    PTuple patterns -> do
      case siFieldTypes exprShape of
        fields | not (HashMap.null fields) -> do
          -- Unpack tuple elements
          forM_ (zip patterns [0..]) $ KATEX_INLINE_OPENpat, idx) -> do
            let fieldName = T.pack (show idx)
            case HashMap.lookup fieldName fields of
              Just fieldType -> do
                let elemShape = inferShape fieldType
                case pat of
                  PVariable v -> updateVariableShape v elemShape
                  _ -> return ()
              Nothing -> return ()
        _ -> return ()
    _ -> return ()

analyzeStatement (SExpression expr) = void $ analyzeShape expr

analyzeStatement (SReturn (Just expr)) = do
  shape <- analyzeShape expr
  -- Store return shape for current function if in function scope
  scope <- asks scCurrentScope
  case siScopeType scope of
    FunctionScope funcName -> do
      modify $ \s -> s {
        sasFunctionSignatures = HashMap.adjust
          (\sig -> sig { fsReturnShape = shape })
          funcName
          (sasFunctionSignatures s)
      }
    _ -> return ()

analyzeStatement (SReturn Nothing) = return ()

analyzeStatement (SIf cond thenBlock elseBlock) = do
  void $ analyzeShape cond
  enterScope BlockScope
  mapM_ analyzeStatement thenBlock
  exitScope
  case elseBlock of
    Just elseStmts -> do
      enterScope BlockScope
      mapM_ analyzeStatement elseStmts
      exitScope
    Nothing -> return ()

analyzeStatement (SWhile cond body) = do
  enterScope LoopScope
  void $ analyzeShape cond
  -- Analyze body multiple times to detect growth patterns
  mapM_ analyzeStatement body
  mapM_ analyzeStatement body  -- Second pass for pattern detection
  exitScope

analyzeStatement (SFor pattern iterable body) = do
  enterScope LoopScope
  iterShape <- analyzeShape iterable
  case pattern of
    PVariable var -> do
      case siElementType iterShape of
        Just elemType -> updateVariableShape var (inferShape elemType)
        Nothing -> updateVariableShape var unknownShape
    _ -> return ()
  mapM_ analyzeStatement body
  exitScope

analyzeStatement (SFunctionDef name params _ body) = do
  enterScope (FunctionScope name)
  -- Initialize parameter shapes
  paramShapes <- forM params $ \param -> do
    let shape = unknownShape  -- Could be improved with type annotations
    updateVariableShape param shape
    return shape
  -- Analyze function body
  mapM_ analyzeStatement body
  -- Extract return shape (if any return statements were analyzed)
  sigs <- gets sasFunctionSignatures
  let returnShape = case HashMap.lookup name sigs of
        Just sig -> fsReturnShape sig
        Nothing -> unknownShape
  -- Store complete function signature
  modify $ \s -> s {
    sasFunctionSignatures = HashMap.insert name
      (FunctionSignature paramShapes returnShape False False)
      (sasFunctionSignatures s)
  }
  exitScope

analyzeStatement _ = return ()  -- Handle other statement types

-- | Enter a new scope
enterScope :: ScopeType -> ShapeAnalysisM ()
enterScope scopeType = do
  currentId <- gets sasCurrentScopeId
  nextId <- gets sasNextScopeId
  let newScope = ScopeInfo
        { siScopeId = nextId
        , siParentScope = Just currentId
        , siScopeType = scopeType
        , siVariables = Set.empty
        }
  modify $ \s -> s
    { sasScopes = HashMap.insert nextId newScope (sasScopes s)
    , sasCurrentScopeId = nextId
    , sasNextScopeId = nextId + 1
    }
  local (\ctx -> ctx { scCurrentScope = newScope }) $ return ()

-- | Exit current scope
exitScope :: ShapeAnalysisM ()
exitScope = do
  currentScope <- gets sasCurrentScopeId
  scopes <- gets sasScopes
  case HashMap.lookup currentScope scopes >>= siParentScope of
    Just parentId -> modify $ \s -> s { sasCurrentScopeId = parentId }
    Nothing -> return ()  -- Already at global scope

-- | Update variable shape and track it in current scope
updateVariableShape :: Identifier -> ShapeInfo -> ShapeAnalysisM ()
updateVariableShape var shape = do
  -- Update shape map
  modify $ \s -> s { sasShapeMap = HashMap.insert var shape (sasShapeMap s) }
  -- Track variable in current scope
  scopeId <- gets sasCurrentScopeId
  modify $ \s -> s {
    sasScopes = HashMap.adjust
      (\scope -> scope { siVariables = Set.insert var (siVariables scope) })
      scopeId
      (sasScopes s)
  }
  -- Update access count
  modify $ \s -> s {
    sasAccessCounts = HashMap.insertWith (+) var 0 (sasAccessCounts s)
  }

-- | Increment access count for a variable
incrementAccessCount :: Identifier -> ShapeAnalysisM ()
incrementAccessCount var = modify $ \s -> s {
  sasAccessCounts = HashMap.insertWith (+) var 1 (sasAccessCounts s)
}

-- | Analyze expression and infer its shape
analyzeShape :: CommonExpr -> ShapeAnalysisM ShapeInfo
analyzeShape (CELiteral lit) = return $ inferLiteralShape lit

analyzeShape (CEVar var) = do
  incrementAccessCount var
  shapeMap <- gets sasShapeMap
  case HashMap.lookup var shapeMap of
    Just shape -> return shape
    Nothing -> return unknownShape { siOrigin = PropagatedFrom (unIdentifier var) }

analyzeShape (CEBinaryOp op left right) = do
  leftShape <- analyzeShape (locatedValue left)
  rightShape <- analyzeShape (locatedValue right)
  combineBinaryShapes op leftShape rightShape

analyzeShape (CEUnaryOp op operand) = do
  operandShape <- analyzeShape (locatedValue operand)
  return $ transformUnaryShape op operandShape

analyzeShape (CEComparison _ left right) = do
  void $ analyzeShape (locatedValue left)
  void $ analyzeShape (locatedValue right)
  return booleanShape

analyzeShape (CECall func args) = do
  funcShape <- analyzeShape (locatedValue func)
  argShapes <- mapM (analyzeShape . locatedValue) args
  -- Look up function signature
  case locatedValue func of
    CEVar (Identifier funcName) -> do
      sigs <- gets sasFunctionSignatures
      case HashMap.lookup funcName sigs of
        Just sig -> return $ fsReturnShape sig
        Nothing -> return unknownShape { siOrigin = FunctionReturn funcName }
    _ -> return unknownShape

analyzeShape (CEIndex container index) = do
  containerShape <- analyzeShape (locatedValue container)
  void $ analyzeShape (locatedValue index)
  return $ extractElementShape containerShape

analyzeShape (CESlice container start end) = do
  containerShape <- analyzeShape (locatedValue container)
  -- Analyze slice bounds if present
  startVal <- case start of
    Just startExpr -> do
      shape <- analyzeShape (locatedValue startExpr)
      case locatedValue startExpr of
        CELiteral (LInt n) -> return $ Just n
        _ -> return Nothing
    Nothing -> return $ Just 0
  
  endVal <- case end of
    Just endExpr -> do
      shape <- analyzeShape (locatedValue endExpr)
      case locatedValue endExpr of
        CELiteral (LInt n) -> return $ Just n
        _ -> return Nothing
    Nothing -> return Nothing
  
  return $ createSliceShape containerShape startVal endVal

analyzeShape (CEAttribute obj attr) = do
  objShape <- analyzeShape (locatedValue obj)
  return $ extractFieldShape objShape attr

analyzeShape (CEList elements) = do
  elementShapes <- mapM (analyzeShape . locatedValue) elements
  let elementTypes = catMaybes $ map (siElementType . inferShape . extractCommonType) elementShapes
  let isHomogeneous = allSame elementTypes
  let elemType = if isHomogeneous then listToMaybe elementTypes else Nothing
  return ShapeInfo
    { siDimensions = Vector.singleton (length elements)
    , siIsKnown = True
    , siElementType = elemType
    , siFieldTypes = HashMap.empty
    , siSize = Just $ length elements * maybe 8 getTypeSize elemType
    , siAlignment = Just 8
    , siIsHomogeneous = isHomogeneous
    , siAccessPattern = SequentialAccess
    , siIsConstant = all siIsConstant elementShapes
    , siOrigin = InferredFromValue
    }

analyzeShape (CEDict pairs) = do
  let dict = HashMap.fromList [(k, v) | (Located _ (CELiteral (LString k)), v) <- pairs]
  analyzeDictShape dict

analyzeShape (CETuple elements) = do
  elementShapes <- mapM (analyzeShape . locatedValue) elements
  let fieldTypes = HashMap.fromList $ zipWith (\i s -> (T.pack (show i), extractCommonType s)) [0::Int ..] elementShapes
  let totalSize = sum $ map (fromMaybe 8 . siSize) elementShapes
  let maxAlign = maximum $ map (fromMaybe 8 . siAlignment) elementShapes
  return ShapeInfo
    { siDimensions = Vector.singleton (length elements)
    , siIsKnown = True
    , siElementType = Nothing
    , siFieldTypes = fieldTypes
    , siSize = Just totalSize
    , siAlignment = Just maxAlign
    , siIsHomogeneous = allSame elementShapes
    , siAccessPattern = RandomAccess
    , siIsConstant = all siIsConstant elementShapes
    , siOrigin = InferredFromValue
    }

analyzeShape (CELambda params body) = do
  -- Lambda analysis would require more context
  return unknownShape

analyzeShape (CEIf cond thenExpr elseExpr) = do
  void $ analyzeShape (locatedValue cond)
  thenShape <- analyzeShape (locatedValue thenExpr)
  elseShape <- analyzeShape (locatedValue elseExpr)
  -- Merge shapes from both branches
  mergeShapes thenShape elseShape

-- | Merge shapes from different control flow paths
mergeShapes :: ShapeInfo -> ShapeInfo -> ShapeAnalysisM ShapeInfo
mergeShapes shape1 shape2
  | shape1 == shape2 = return shape1
  | siElementType shape1 == siElementType shape2 = return ShapeInfo
    { siDimensions = if siDimensions shape1 == siDimensions shape2
                     then siDimensions shape1
                     else Vector.empty
    , siIsKnown = siIsKnown shape1 && siIsKnown shape2
    , siElementType = siElementType shape1
    , siFieldTypes = HashMap.intersection (siFieldTypes shape1) (siFieldTypes shape2)
    , siSize = case (siSize shape1, siSize shape2) of
        (Just s1, Just s2) | s1 == s2 -> Just s1
        _ -> Nothing
    , siAlignment = max <$> siAlignment shape1 <*> siAlignment shape2
    , siIsHomogeneous = siIsHomogeneous shape1 && siIsHomogeneous shape2
    , siAccessPattern = if siAccessPattern shape1 == siAccessPattern shape2
                        then siAccessPattern shape1
                        else UnknownAccess
    , siIsConstant = siIsConstant shape1 && siIsConstant shape2
    , siOrigin = InferredFromValue
    }
  | otherwise = return unknownShape

-- | Extract common type from shape
extractCommonType :: ShapeInfo -> Type
extractCommonType shape
  | Just elemType <- siElementType shape = elemType
  | not (HashMap.null (siFieldTypes shape)) = TTuple $ HashMap.elems (siFieldTypes shape)
  | Vector.length (siDimensions shape) > 0 = TList TVoid
  | otherwise = TVoid

-- | Infer shape from type information
inferShape :: Type -> ShapeInfo
inferShape (TInt size) = ShapeInfo
  { siDimensions = Vector.empty
  , siIsKnown = True
  , siElementType = Nothing
  , siFieldTypes = HashMap.empty
  , siSize = Just (size `div` 8)
  , siAlignment = Just (min 8 (size `div` 8))
  , siIsHomogeneous = True
  , siAccessPattern = RandomAccess
  , siIsConstant = False
  , siOrigin = UserAnnotation
  }

inferShape (TFloat size) = ShapeInfo
  { siDimensions = Vector.empty
  , siIsKnown = True
  , siElementType = Nothing
  , siFieldTypes = HashMap.empty
  , siSize = Just (size `div` 8)
  , siAlignment = Just (min 8 (size `div` 8))
  , siIsHomogeneous = True
  , siAccessPattern = RandomAccess
  , siIsConstant = False
  , siOrigin = UserAnnotation
  }

inferShape TBool = booleanShape
inferShape TString = stringShape
inferShape TBytes = bytesShape
inferShape TChar = charShape

inferShape (TList elemType) = ShapeInfo
  { siDimensions = Vector.singleton (-1)  -- Unknown size
  , siIsKnown = False
  , siElementType = Just elemType
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Just 8  -- Pointer alignment
  , siIsHomogeneous = True
  , siAccessPattern = SequentialAccess
  , siIsConstant = False
  , siOrigin = UserAnnotation
  }

inferShape (TTuple types) = 
  let sizes = map getTypeSize types
      offsets = scanl1 (\acc size -> align acc 8 + size) sizes
      totalSize = if null sizes then 0 else align (last offsets) 8
      fields = HashMap.fromList $ zipWith (\i t -> (T.pack $ show (i :: Int), t)) [0..] types
  in ShapeInfo
    { siDimensions = Vector.singleton (length types)
    , siIsKnown = True
    , siElementType = Nothing
    , siFieldTypes = fields
    , siSize = Just totalSize
    , siAlignment = Just 8
    , siIsHomogeneous = allSame types
    , siAccessPattern = RandomAccess
    , siIsConstant = False
    , siOrigin = UserAnnotation
    }

inferShape (TDict keyType valueType) = ShapeInfo
  { siDimensions = Vector.empty
  , siIsKnown = False
  , siElementType = Just valueType
  , siFieldTypes = HashMap.singleton "key" keyType
  , siSize = Nothing
  , siAlignment = Just 8
  , siIsHomogeneous = True
  , siAccessPattern = RandomAccess
  , siIsConstant = False
  , siOrigin = UserAnnotation
  }

inferShape (TSet elemType) = ShapeInfo
  { siDimensions = Vector.empty
  , siIsKnown = False
  , siElementType = Just elemType
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Just 8
  , siIsHomogeneous = True
  , siAccessPattern = RandomAccess
  , siIsConstant = False
  , siOrigin = UserAnnotation
  }

inferShape _ = unknownShape

-- | Get size of a type in bytes
getTypeSize :: Type -> Int
getTypeSize (TInt size) = size `div` 8
getTypeSize (TFloat size) = size `div` 8
getTypeSize TBool = 1
getTypeSize TChar = 1
getTypeSize TString = 8  -- Pointer size
getTypeSize TBytes = 8   -- Pointer size
getTypeSize (TList _) = 8  -- Pointer size
getTypeSize (TDict _ _) = 8  -- Pointer size
getTypeSize (TSet _) = 8  -- Pointer size
getTypeSize (TTuple types) = align (sum (map getTypeSize types)) 8
getTypeSize _ = 8  -- Default pointer size

-- | Align value to boundary
align :: Int -> Int -> Int
align value boundary = ((value + boundary - 1) `div` boundary) * boundary

-- | Analyze structure for optimization opportunities
analyzeStructure :: Type -> ShapeAnalysisM StructShape
analyzeStructure (TStruct name fieldDefs) = do
  -- Sort fields by size for better packing
  let sortedFields = sortOn (negate . getTypeSize . snd) fieldDefs
  let (fieldOrder, sizes, alignments) = unzip3 
        [(fname, getTypeSize ftype, getTypeAlignment ftype) | (fname, ftype) <- sortedFields]
  
  -- Calculate layout with padding
  let (offsets, paddings) = calculateLayout sizes alignments
  let totalSize = if null offsets then 0 else last offsets + last sizes
  let structAlign = if null alignments then 8 else maximum alignments
  
  -- Detect hot fields based on access patterns
  accessCounts <- gets sasAccessCounts
  let hotFields = Set.fromList 
        [f | (f, _) <- fieldDefs, 
         maybe 0 id (HashMap.lookup (Identifier f) accessCounts) > 10]
  
  return StructShape
    { ssFields = HashMap.fromList fieldDefs
    , ssFieldOrder = fieldOrder
    , ssSize = align totalSize structAlign
    , ssAlignment = structAlign
    , ssPadding = paddings
    , ssIsPackable = sum paddings == 0
    , ssHotFields = hotFields
    }

analyzeStructure (TClass name methods fields) = 
  analyzeStructure (TStruct name fields)  -- Treat class as struct for layout

analyzeStructure _ = throwError "Not a struct or class type"

-- | Calculate field layout with padding
calculateLayout :: [Int] -> [Int] -> ([Int], [Int])
calculateLayout sizes alignments = 
  let calcOffset (offset, paddings) (size, alignment) =
        let padding = (alignment - (offset `mod` alignment)) `mod` alignment
            newOffset = offset + padding + size
        in (newOffset, paddings ++ [padding])
      (_, paddings) = foldl' calcOffset (0, []) (zip sizes alignments)
      offsets = scanl1 (+) (zipWith (+) (0 : init paddings) (0 : init sizes))
  in (offsets, paddings)

-- | Get alignment requirement for a type
getTypeAlignment :: Type -> Int
getTypeAlignment (TInt size) = min 8 (size `div` 8)
getTypeAlignment (TFloat size) = min 8 (size `div` 8)
getTypeAlignment TBool = 1
getTypeAlignment TChar = 1
getTypeAlignment _ = 8  -- Default to pointer alignment

-- | Infer container shape from usage patterns
inferContainerShape :: CommonExpr -> ShapeAnalysisM ContainerShape
inferContainerShape expr = do
  shape <- analyzeShape expr
  -- Analyze usage in current scope to determine growth pattern
  growth <- analyzeGrowthPattern expr
  avgSize <- analyzeAverageSize expr
  
  case siElementType shape of
    Just elemType -> return ContainerShape
      { csElementType = elemType
      , csCapacity = case siDimensions shape of
          dims | Vector.length dims > 0 -> 
            case Vector.head dims of
              firstDim | firstDim >= 0 -> Just firstDim
              _ -> Nothing
          _ -> Nothing
      , csGrowthPattern = growth
      , csIsResizable = growth /= FixedSize
      , csAccessPattern = siAccessPattern shape
      , csKeyType = HashMap.lookup "key" (siFieldTypes shape)
      , csAverageSize = avgSize
      }
    Nothing -> throwError "Expression does not represent a container"

-- | Analyze growth pattern of a container
analyzeGrowthPattern :: CommonExpr -> ShapeAnalysisM GrowthPattern
analyzeGrowthPattern expr = do
  -- This would analyze loop bodies and function calls to detect patterns
  -- For now, return a reasonable default
  case expr of
    CECall func _ -> case locatedValue func of
      CEVar (Identifier fname)
        | fname `elem` ["append", "push", "extend"] -> return ExponentialGrowth
        | fname `elem` ["zeros", "ones", "range"] -> return FixedSize
      _ -> return UnknownGrowth
    _ -> return UnknownGrowth

-- | Analyze average size of containers
analyzeAverageSize :: CommonExpr -> ShapeAnalysisM (Maybe Int)
analyzeAverageSize expr = do
  shape <- analyzeShape expr
  case siDimensions shape of
    dims | Vector.length dims > 0 -> 
      case Vector.head dims of
        firstDim | firstDim >= 0 -> return $ Just firstDim
        _ -> return Nothing
    _ -> return Nothing

-- | Analyze dictionary shape for C++ map optimization
analyzeDictShape :: HashMap Text CommonExpr -> ShapeAnalysisM ShapeInfo
analyzeDictShape dict = do
  if HashMap.null dict
    then return $ unknownShape { siElementType = Just TVoid }
    else do
      -- Analyze all values to determine homogeneity
      valueShapes <- mapM analyzeShape (HashMap.elems dict)
      let valueTypes = map extractCommonType valueShapes
      let isHomogeneous = allSame valueTypes
      let valueType = if isHomogeneous then listToMaybe valueTypes else Nothing
      
      return ShapeInfo
        { siDimensions = Vector.singleton (HashMap.size dict)
        , siIsKnown = True
        , siElementType = valueType
        , siFieldTypes = HashMap.singleton "key" TString  -- Keys are strings
        , siSize = Just $ HashMap.size dict * 16  -- Rough estimate
        , siAlignment = Just 8
        , siIsHomogeneous = isHomogeneous
        , siAccessPattern = RandomAccess
        , siIsConstant = all siIsConstant valueShapes
        , siOrigin = InferredFromValue
        }

-- | Analyze object shape for struct optimization
analyzeObjectShape :: HashMap Text Type -> ShapeAnalysisM ShapeInfo
analyzeObjectShape fields = do
  let fieldSizes = map getTypeSize (HashMap.elems fields)
  let totalSize = sum fieldSizes
  let maxAlign = maximum $ map getTypeAlignment (HashMap.elems fields)
  
  return ShapeInfo
    { siDimensions = Vector.empty
    , siIsKnown = True
    , siElementType = Nothing
    , siFieldTypes = fields
    , siSize = Just (align totalSize maxAlign)
    , siAlignment = Just maxAlign
    , siIsHomogeneous = allSame (HashMap.elems fields)
    , siAccessPattern = RandomAccess
    , siIsConstant = False
    , siOrigin = InferredFromValue
    }

-- | Generate optimized C++ data structure
generateCppStructure :: ShapeInfo -> ShapeAnalysisM CppMapping
generateCppStructure shape = do
  context <- ask
  existing <- gets sasCppMappings
  
  -- Check if we already have a mapping
  case HashMap.lookup shape existing of
    Just mapping -> return mapping
    Nothing -> do
      mapping <- generateNewMapping shape context
      modify $ \s -> s { sasCppMappings = HashMap.insert shape mapping existing }
      return mapping

generateNewMapping :: ShapeInfo -> ShapeContext -> ShapeAnalysisM CppMapping
generateNewMapping shape context
  -- Fixed-size array
  | Vector.length (siDimensions shape) == 1 
    && siIsKnown shape 
    && siIsHomogeneous shape
    && isJust (siElementType shape) = do
    let size = case siDimensions shape of
          dims | Vector.length dims > 0 -> Vector.head dims
          _ -> 0  -- Safe fallback, though condition should prevent this
    if size > 0 && size <= scMaxInlineSize context
      then return CppMapping
        { cmType = T.concat ["std::array<", cppType (fromJust (siElementType shape)), ", ", T.pack (show size), ">"]
        , cmHeaders = ["<array>"]
        , cmOptimizations = ["Stack-allocated for small fixed size", "Zero-cost abstraction"]
        , cmMemoryLayout = StackLayout
        , cmInitializerHint = Just "Use brace initialization"
        }
      else return CppMapping
        { cmType = T.concat ["std::vector<", cppType (fromJust (siElementType shape)), ">"]
        , cmHeaders = ["<vector>"]
        , cmOptimizations = ["Reserve capacity: " <> T.pack (show size), "Use shrink_to_fit after filling"]
        , cmMemoryLayout = ContiguousLayout
        , cmInitializerHint = Just $ "Reserve " <> T.pack (show size)
        }
  
  -- Dynamic array/list
  | Vector.length (siDimensions shape) >= 1 
    && siIsHomogeneous shape
    && isJust (siElementType shape) = do
    let elemType = cppType (fromJust (siElementType shape))
    case siAccessPattern shape of
      SequentialAccess -> return CppMapping
        { cmType = T.concat ["std::vector<", elemType, ">"]
        , cmHeaders = ["<vector>"]
        , cmOptimizations = ["Contiguous memory for cache efficiency", "Consider std::deque for front insertion"]
        , cmMemoryLayout = ContiguousLayout
        , cmInitializerHint = Nothing
        }
      RandomAccess | siIsConstant shape -> return CppMapping
        { cmType = T.concat ["std::vector<", elemType, ">"]
        , cmHeaders = ["<vector>"]
        , cmOptimizations = ["Mark as const for optimization", "Consider std::array if size becomes known"]
        , cmMemoryLayout = ContiguousLayout
        , cmInitializerHint = Just "Initialize with std::initializer_list"
        }
      _ -> return CppMapping
        { cmType = T.concat ["std::vector<", elemType, ">"]
        , cmHeaders = ["<vector>"]
        , cmOptimizations = ["General purpose container"]
        , cmMemoryLayout = ContiguousLayout
        , cmInitializerHint = Nothing
        }
  
  -- Dictionary/map
  | siAccessPattern shape == RandomAccess 
    && isJust (siElementType shape)
    && HashMap.member "key" (siFieldTypes shape) = do
    let keyType = cppType (siFieldTypes shape HashMap.! "key")
    let valueType = cppType (fromJust (siElementType shape))
    if siIsConstant shape
      then return CppMapping
        { cmType = T.concat ["std::unordered_map<", keyType, ", ", valueType, ">"]
        , cmHeaders = ["<unordered_map>"]
        , cmOptimizations = ["O(1) average lookup", "Consider perfect hash if keys are known", "Mark as const"]
        , cmMemoryLayout = HashBasedLayout
        , cmInitializerHint = Just "Use initializer list"
        }
      else return CppMapping
        { cmType = T.concat ["std::unordered_map<", keyType, ", ", valueType, ">"]
        , cmHeaders = ["<unordered_map>"]
        , cmOptimizations = ["Reserve bucket count if size is predictable", "Consider robin_hood hash map"]
        , cmMemoryLayout = HashBasedLayout
        , cmInitializerHint = Nothing
        }
  
  -- Struct/object
  | not (HashMap.null (siFieldTypes shape)) = do
    let fields = siFieldTypes shape
    if scOptimizeForMemory context
      then return CppMapping
        { cmType = "struct /* packed */"
        , cmHeaders = []
        , cmOptimizations = ["Pack struct with __attribute__((packed))", "Reorder fields by size"]
        , cmMemoryLayout = CustomLayout "packed_struct"
        , cmInitializerHint = Just "Use designated initializers (C++20)"
        }
      else return CppMapping
        { cmType = "struct"
        , cmHeaders = []
        , cmOptimizations = ["Align fields for performance", "Group frequently accessed fields"]
        , cmMemoryLayout = CustomLayout "aligned_struct"
        , cmInitializerHint = Just "Use aggregate initialization"
        }
  
  -- Tuple
  | Vector.length (siDimensions shape) == 1 && not (HashMap.null (siFieldTypes shape)) = do
    let types = [cppType t | (_, t) <- sortOn fst (HashMap.toList (siFieldTypes shape))]
    return CppMapping
      { cmType = T.concat ["std::tuple<", T.intercalate ", " types, ">"]
      , cmHeaders = ["<tuple>"]
      , cmOptimizations = ["Use structured bindings (C++17)", "Consider std::pair for 2 elements"]
      , cmMemoryLayout = ContiguousLayout
      , cmInitializerHint = Just "Use std::make_tuple"
      }
  
  -- Set
  | siElementType shape == Just TBool = return CppMapping
    { cmType = "std::bitset<N> /* or std::vector<bool> */"
    , cmHeaders = ["<bitset>", "<vector>"]
    , cmOptimizations = ["Use bitset for fixed size", "Space-efficient bool storage"]
    , cmMemoryLayout = CustomLayout "bitset"
    , cmInitializerHint = Nothing
    }
  
  -- Generic/unknown
  | otherwise = do
    if scTargetCppVersion context >= "c++17"
      then return CppMapping
        { cmType = "std::any"
        , cmHeaders = ["<any>"]
        , cmOptimizations = ["Type erasure for maximum flexibility", "Consider std::variant if types are known"]
        , cmMemoryLayout = HeapLayout
        , cmInitializerHint = Just "Use std::make_any"
        }
      else return CppMapping
        { cmType = "void*"
        , cmHeaders = []
        , cmOptimizations = ["Manual type management required", "Consider boost::any"]
        , cmMemoryLayout = HeapLayout
        , cmInitializerHint = Nothing
        }

-- | Convert Fluxus type to C++ type
cppType :: Type -> Text
cppType (TInt 8) = "int8_t"
cppType (TInt 16) = "int16_t"
cppType (TInt 32) = "int32_t"
cppType (TInt 64) = "int64_t"
cppType (TInt _) = "int"
cppType (TFloat 32) = "float"
cppType (TFloat 64) = "double"
cppType (TFloat _) = "double"
cppType TBool = "bool"
cppType TChar = "char"
cppType TString = "std::string"
cppType TBytes = "std::vector<uint8_t>"
cppType (TList t) = T.concat ["std::vector<", cppType t, ">"]
cppType (TDict k v) = T.concat ["std::unordered_map<", cppType k, ", ", cppType v, ">"]
cppType (TSet t) = T.concat ["std::unordered_set<", cppType t, ">"]
cppType _ = "std::any"

-- | Optimize data structures based on shape analysis
optimizeDataStructures :: [CommonExpr] -> ShapeAnalysisM [(CommonExpr, CppMapping)]
optimizeDataStructures exprs = do
  shapes <- mapM analyzeShape exprs
  mappings <- mapM generateCppStructure shapes
  
  -- Add optimization suggestions
  forM_ (zip3 exprs shapes mappings) $ KATEX_INLINE_OPENexpr, shape, mapping) -> do
    when (siIsConstant shape) $
      addOptimization "Consider marking as const for better optimization"
    
    when (siAccessPattern shape == WriteOnceReadMany) $
      addOptimization "Consider read-only data structure or immutable design"
    
    case siDimensions shape of
      dims | Vector.length dims > 0 && Vector.head dims > 1000 ->
        addOptimization "Large container detected - consider memory pooling"
      _ -> return ()
  
  return $ zip exprs mappings

-- | Add optimization suggestion
addOptimization :: Text -> ShapeAnalysisM ()
addOptimization opt = modify $ \s -> s { sasOptimizations = opt : sasOptimizations s }

-- | Binary operator shape combination with broadcasting
combineBinaryShapes :: BinaryOp -> ShapeInfo -> ShapeInfo -> ShapeAnalysisM ShapeInfo
combineBinaryShapes op left right = case op of
  -- Arithmetic operations with broadcasting
  OpAdd -> broadcastShapes left right
  OpSubtract -> broadcastShapes left right
  OpMultiply -> broadcastShapes left right
  OpDivide -> broadcastShapes left right
  OpModulo -> broadcastShapes left right
  OpPower -> broadcastShapes left right
  
  -- Bitwise operations
  OpBitwiseAnd -> broadcastShapes left right
  OpBitwiseOr -> broadcastShapes left right
  OpBitwiseXor -> broadcastShapes left right
  OpLeftShift -> broadcastShapes left right
  OpRightShift -> broadcastShapes left right
  
  -- String/list concatenation
  OpConcat -> return ShapeInfo
    { siDimensions = case (siDimensions left, siDimensions right) of
        (ld, rd) | Vector.length ld == 1 && Vector.length rd == 1 ->
          Vector.singleton $ Vector.head ld + Vector.head rd
        _ -> Vector.empty
    , siIsKnown = siIsKnown left && siIsKnown right
    , siElementType = if siElementType left == siElementType right 
                      then siElementType left 
                      else Nothing
    , siFieldTypes = HashMap.empty
    , siSize = liftA2 (+) (siSize left) (siSize right)
    , siAlignment = max <$> siAlignment left <*> siAlignment right
    , siIsHomogeneous = siIsHomogeneous left && siIsHomogeneous right
    , siAccessPattern = SequentialAccess
    , siIsConstant = siIsConstant left && siIsConstant right
    , siOrigin = InferredFromValue
    }
  
  -- Logical operations return boolean
  OpAnd -> return booleanShape
  OpOr -> return booleanShape
  
  -- Matrix multiplication (simplified)
  OpMatrixMultiply -> case (siDimensions left, siDimensions right) of
    (ld, rd) | Vector.length ld == 2 && Vector.length rd == 2 ->
      let (m, k1) = (ld Vector.! 0, ld Vector.! 1)
          (k2, n) = (rd Vector.! 0, rd Vector.! 1)
      in if k1 == k2 
         then return left { siDimensions = Vector.fromList [m, n] }
         else throwError "Matrix dimensions incompatible for multiplication"
    _ -> return unknownShape

-- | Broadcast shapes according to NumPy-like rules
broadcastShapes :: ShapeInfo -> ShapeInfo -> ShapeAnalysisM ShapeInfo
broadcastShapes left right
  | siDimensions left == siDimensions right = 
    -- Same shape, no broadcasting needed
    return left { 
      siIsConstant = siIsConstant left && siIsConstant right,
      siOrigin = InferredFromValue 
    }
  | Vector.null (siDimensions left) = 
    -- Left is scalar, broadcast to right's shape
    return right { 
      siElementType = siElementType left <|> siElementType right,
      siIsConstant = siIsConstant left && siIsConstant right,
      siOrigin = InferredFromValue
    }
  | Vector.null (siDimensions right) = 
    -- Right is scalar, broadcast to left's shape
    return left { 
      siIsConstant = siIsConstant left && siIsConstant right,
      siOrigin = InferredFromValue
    }
  | otherwise = do
    -- General broadcasting
    let leftDims = Vector.toList (siDimensions left)
    let rightDims = Vector.toList (siDimensions right)
    broadcastedDims <- broadcastDimensions leftDims rightDims
    return ShapeInfo
      { siDimensions = Vector.fromList broadcastedDims
      , siIsKnown = siIsKnown left && siIsKnown right && all (>= 0) broadcastedDims
      , siElementType = siElementType left <|> siElementType right
      , siFieldTypes = HashMap.empty
      , siSize = Nothing  -- Size calculation would be complex
      , siAlignment = max <$> siAlignment left <*> siAlignment right
      , siIsHomogeneous = siIsHomogeneous left && siIsHomogeneous right
      , siAccessPattern = if siAccessPattern left == siAccessPattern right
                          then siAccessPattern left
                          else UnknownAccess
      , siIsConstant = siIsConstant left && siIsConstant right
      , siOrigin = InferredFromValue
      }

-- | Broadcast dimension lists
broadcastDimensions :: [Int] -> [Int] -> ShapeAnalysisM [Int]
broadcastDimensions dims1 dims2 = do
  let n1 = length dims1
  let n2 = length dims2
  let maxLen = max n1 n2
  let padded1 = replicate (maxLen - n1) 1 ++ dims1
  let padded2 = replicate (maxLen - n2) 1 ++ dims2
  zipWithM broadcastDim padded1 padded2
  where
    broadcastDim d1 d2
      | d1 == d2 = return d1
      | d1 == 1 = return d2
      | d2 == 1 = return d1
      | d1 == -1 || d2 == -1 = return (-1)  -- Unknown dimension
      | otherwise = throwError $ T.concat 
          ["Cannot broadcast dimensions ", T.pack (show d1), " and ", T.pack (show d2)]

-- | Transform shape for unary operations
transformUnaryShape :: UnaryOp -> ShapeInfo -> ShapeInfo
transformUnaryShape op shape = case op of
  OpNot -> booleanShape
  OpNegate -> shape { siIsConstant = siIsConstant shape }
  OpBitwiseNot -> shape { siIsConstant = siIsConstant shape }
  OpSplat -> shape  -- Splat doesn't change shape in this context

-- | Extract element shape from container
extractElementShape :: ShapeInfo -> ShapeInfo
extractElementShape shape = case siElementType shape of
  Just elemType -> inferShape elemType
  Nothing -> 
    -- For multi-dimensional arrays, reduce dimensionality
    if Vector.length (siDimensions shape) > 1
    then shape { siDimensions = Vector.tail (siDimensions shape) }
    else unknownShape

-- | Create slice shape with known bounds
createSliceShape :: ShapeInfo -> Maybe Int -> Maybe Int -> ShapeInfo
createSliceShape shape startVal endVal = 
  let dims = siDimensions shape
  in if Vector.null dims
     then shape  -- Can't slice scalar
     else case (startVal, endVal, Vector.head dims) of
       (Just s, Just e, d) | d >= 0 -> 
         -- Known slice of known dimension
         shape { siDimensions = Vector.cons (e - s) (Vector.tail dims) }
       _ -> 
         -- Unknown slice size
         shape { 
           siDimensions = Vector.cons (-1) (Vector.tail dims),
           siIsKnown = False 
         }

-- | Extract field shape from object
extractFieldShape :: ShapeInfo -> Identifier -> ShapeInfo
extractFieldShape shape (Identifier fieldName) = 
  case HashMap.lookup fieldName (siFieldTypes shape) of
    Just fieldType -> inferShape fieldType
    Nothing -> unknownShape { siOrigin = PropagatedFrom fieldName }

-- | Infer literal shape
inferLiteralShape :: Literal -> ShapeInfo
inferLiteralShape (LInt _) = (inferShape (TInt 64)) { siIsConstant = True }
inferLiteralShape (LFloat _) = (inferShape (TFloat 64)) { siIsConstant = True }
inferLiteralShape (LBool _) = booleanShape { siIsConstant = True }
inferLiteralShape (LString _) = stringShape { siIsConstant = True }
inferLiteralShape (LBytes _) = bytesShape { siIsConstant = True }
inferLiteralShape (LChar _) = charShape { siIsConstant = True }
inferLiteralShape LNone = unknownShape { siIsConstant = True }

-- Helper shapes

unknownShape :: ShapeInfo
unknownShape = ShapeInfo
  { siDimensions = Vector.empty
  , siIsKnown = False
  , siElementType = Nothing
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Nothing
  , siIsHomogeneous = True
  , siAccessPattern = UnknownAccess
  , siIsConstant = False
  , siOrigin = InferredFromValue
  }

booleanShape :: ShapeInfo
booleanShape = ShapeInfo
  { siDimensions = Vector.empty
  , siIsKnown = True
  , siElementType = Nothing
  , siFieldTypes = HashMap.empty
  , siSize = Just 1
  , siAlignment = Just 1
  , siIsHomogeneous = True
  , siAccessPattern = RandomAccess
  , siIsConstant = False
  , siOrigin = UserAnnotation
  }

stringShape :: ShapeInfo
stringShape = ShapeInfo
  { siDimensions = Vector.empty
  , siIsKnown = False
  , siElementType = Just TChar
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Just 8
  , siIsHomogeneous = True
  , siAccessPattern = SequentialAccess
  , siIsConstant = False
  , siOrigin = UserAnnotation
  }

bytesShape :: ShapeInfo
bytesShape = ShapeInfo
  { siDimensions = Vector.empty
  , siIsKnown = False
  , siElementType = Just (TInt 8)
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Just 1
  , siIsHomogeneous = True
  , siAccessPattern = SequentialAccess
  , siIsConstant = False
  , siOrigin = UserAnnotation
  }

charShape :: ShapeInfo
charShape = ShapeInfo
  { siDimensions = Vector.empty
  , siIsKnown = True
  , siElementType = Nothing
  , siFieldTypes = HashMap.empty
  , siSize = Just 1
  , siAlignment = Just 1
  , siIsHomogeneous = True
  , siAccessPattern = RandomAccess
  , siIsConstant = False
  , siOrigin = UserAnnotation
  }

intShape :: ShapeInfo
intShape = inferShape (TInt 64)

floatShape :: ShapeInfo
floatShape = inferShape (TFloat 64)

voidShape :: ShapeInfo
voidShape = unknownShape { siIsKnown = True }

arrayShape :: ShapeInfo
arrayShape = ShapeInfo
  { siDimensions = Vector.singleton (-1)
  , siIsKnown = False
  , siElementType = Just TFloat
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Just 8
  , siIsHomogeneous = True
  , siAccessPattern = RandomAccess
  , siIsConstant = False
  , siOrigin = UserAnnotation
  }

listShape :: ShapeInfo
listShape = ShapeInfo
  { siDimensions = Vector.singleton (-1)
  , siIsKnown = False
  , siElementType = Just (TInt 64)
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Just 8
  , siIsHomogeneous = True
  , siAccessPattern = SequentialAccess
  , siIsConstant = False
  , siOrigin = UserAnnotation
  }

anyContainerShape :: ShapeInfo
anyContainerShape = ShapeInfo
  { siDimensions = Vector.singleton (-1)
  , siIsKnown = False
  , siElementType = Nothing
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Just 8
  , siIsHomogeneous = False
  , siAccessPattern = UnknownAccess
  , siIsConstant = False
  , siOrigin = UserAnnotation
  }

-- Utility functions

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (== x) xs