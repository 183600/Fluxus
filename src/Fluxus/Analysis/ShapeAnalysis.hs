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
import Fluxus.AST.Python
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad (void, when, foldM, forM_)
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
import Data.Maybe (fromMaybe, catMaybes, listToMaybe, isJust, fromJust)
import Data.List (foldl', sortOn)

type ShapeAnalysisM = ReaderT ShapeContext (StateT ShapeAnalysisState (Except Text))

-- | C++ mapping information
data CppMapping = CppMapping
  { cmType :: !Text
  , cmHeaders :: ![Text]
  , cmOptimizations :: ![Text]
  , cmMemoryLayout :: !MemoryLayout
  , cmInitializerHint :: !(Maybe Text)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Memory layout types
data MemoryLayout
  = StackLayout
  | HeapLayout
  | ContiguousLayout
  | HashBasedLayout
  | CustomLayout !Text
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Shape context with optimization settings
data ShapeContext = ShapeContext
  { scCurrentScope :: !ScopeInfo
  , scFunctionContext :: !(Maybe FunctionSignature)
  , scOptimizationLevel :: !Int
  , scMaxInlineSize :: !Int
  , scTargetCppVersion :: !Text
  , scOptimizeForMemory :: !Bool
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- | Shape analysis state
data ShapeAnalysisState = ShapeAnalysisState
  { sasShapeMap :: !(HashMap Identifier ShapeInfo)
  , sasTypeMap :: !(HashMap Type ShapeInfo)
  , sasStructMap :: !(HashMap QualifiedName StructShape)
  , sasFunctionMap :: !(HashMap Identifier FunctionSignature)
  , sasCppMapping :: !(HashMap ShapeInfo Text)
  , sasWarnings :: ![Text]
  , sasOptimizations :: ![Text]
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- | Scope information
data ScopeInfo = ScopeInfo
  { siVariables :: !(HashMap Identifier ShapeInfo)
  , siParentScope :: !(Maybe ScopeInfo)
  , siScopeDepth :: !Int
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- | Function signature
data FunctionSignature = FunctionSignature
  { fsName :: !Identifier
  , fsParameters :: ![(Identifier, Type)]
  , fsReturnType :: !Type
  , fsIsPure :: !Bool
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- | Unknown shape
unknownShape :: ShapeInfo
unknownShape = ShapeInfo
  { siDimensions = Vector.empty
  , siIsKnown = False
  , siElementType = Nothing
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Nothing
  , siIsHomogeneous = False
  , siAccessPattern = UnknownAccess
  , siIsConstant = False
  , siOrigin = UnknownOrigin
  }

-- | Boolean shape
booleanShape :: ShapeInfo
booleanShape = (inferShape TBool) { siIsConstant = False }

-- | String shape
stringShape :: ShapeInfo
stringShape = (inferShape TString) { siIsConstant = False }

-- | Access pattern
data AccessPattern
  = SequentialAccess
  | RandomAccess
  | StridedAccess
  | UnknownAccess
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData, Hashable)

-- | Shape origin
data ShapeOrigin
  = InferredFromValue
  | InferredFromType
  | PropagatedFrom Identifier
  | UnknownOrigin
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData, Hashable)

-- | Growth pattern
data GrowthPattern
  = FixedSize
  | GrowingLinear
  | GrowingExponential
  | UnknownGrowth
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData, Hashable)

-- | Analyze shape of Python expressions
analyzeShape :: Located PythonExpr -> ShapeAnalysisM ShapeInfo
analyzeShape expr = case locatedValue expr of
  PyVar _ -> return unknownShape
  PyLiteral lit -> return $ inferLiteralShape lit
  PyBinaryOp op l r -> do
    leftShape <- analyzeShape l
    rightShape <- analyzeShape r
    combineBinaryShapes op leftShape rightShape
  PyUnaryOp op e -> do
    shape <- analyzeShape e
    return $ transformUnaryShape op shape
  PyComparison _ exprs -> do
    shapes <- mapM analyzeShape exprs
    return $ booleanShape { siIsConstant = all siIsConstant shapes }
  PyBoolOp _ exprs -> do
    shapes <- mapM analyzeShape exprs
    return $ booleanShape { siIsConstant = all siIsConstant shapes }
  PyCall func args -> do
    funcShape <- analyzeShape func
    argShapes <- mapM analyzeShape args
    -- Function calls typically return unknown shape
    return unknownShape { siIsConstant = all siIsConstant (funcShape : argShapes) }
  PySubscript container idx -> do
    containerShape <- analyzeShape container
    idxShape <- analyzeShape idx
    return $ extractElementShape containerShape
  PySlice container start end -> do
    containerShape <- analyzeShape container
    startVal <- case start of
      Just s -> do
        sShape <- analyzeShape s
        return $ if siIsConstant sShape && Vector.length (siDimensions sShape) == 0
                 then Just 0  -- Simplified: would need actual value extraction
                 else Nothing
      Nothing -> return Nothing
    endVal <- case end of
      Just e -> do
        eShape <- analyzeShape e
        return $ if siIsConstant eShape && Vector.length (siDimensions eShape) == 0
                 then Just 0  -- Simplified: would need actual value extraction
                 else Nothing
      Nothing -> return Nothing
    return $ createSliceShape containerShape startVal endVal
  PyAttribute obj attr -> do
    objShape <- analyzeShape obj
    return $ extractFieldShape objShape attr

-- | Function signature with shape information
data FunctionSignatureWithShape = FunctionSignatureWithShape
  { fswsName :: !Identifier
  , fswsParameters :: ![(Identifier, Type)]
  , fswsReturnType :: !Type
  , fswsReturnShape :: !ShapeInfo         -- Return value shape
  , fswsIsVariadic :: !Bool               -- Accepts variable arguments
  , fswsSideEffects :: !Bool              -- Has side effects on shapes
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Comprehensive shape information
data ShapeInfo = ShapeInfo
  { siDimensions :: ![Int]               -- Dimensions for arrays/tensors
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

-- | Origin of shape information (alternative version)
data ShapeOriginAlt
  = UserAnnotationAlt      -- From type annotation
  | InferredFromValueAlt   -- Inferred from literal/expression
  | PropagatedFromAlt Text -- Propagated from another variable
  | FunctionReturnAlt Text -- From function return
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)

-- | Access patterns for optimization
data AccessPatternOpt
  = SequentialAccessOpt     -- Sequential iteration
  | RandomAccessOpt         -- Random access by key/index
  | WriteOnceReadManyOpt    -- Written once, read many times
  | ReadOnceWriteManyOpt    -- Read once, written many times
  | StreamingAccessOpt      -- Data flows through without storage
  | UnknownAccessOpt        -- Cannot determine access pattern
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

-- | Initial shape context
initialContext :: ShapeContext
initialContext = ShapeContext
  { scCurrentScope = initialScope
  , scFunctionContext = Nothing
  , scOptimizationLevel = 2
  , scMaxInlineSize = 64
  , scTargetCppVersion = "c++17"
  , scOptimizeForMemory = False
  }

-- | Global scope identifier
type ScopeType = Int

-- | Scope types
globalScope :: ScopeType
globalScope = 0

-- | Initial scope
initialScope :: ScopeInfo
initialScope = ScopeInfo
  { siVariables = HashMap.empty
  , siParentScope = Nothing
  , siScopeDepth = 0
  }

-- | Initial shape analysis state
initialState :: ShapeAnalysisState
initialState = ShapeAnalysisState
  { sasShapeMap = HashMap.empty
  , sasTypeMap = HashMap.empty
  , sasStructMap = HashMap.empty
  , sasFunctionMap = HashMap.empty
  , sasCppMapping = HashMap.empty
  , sasWarnings = []
  , sasOptimizations = []
  }

-- | Initialize built-in function signatures
initializeBuiltinFunctions :: HashMap Text FunctionSignature
initializeBuiltinFunctions = HashMap.fromList
  [ ("len", FunctionSignature [anyunknownShape] intShape False False)
  , ("append", FunctionSignature [anyunknownShape, unknownShape] voidShape False True)
  , ("extend", FunctionSignature [anyunknownShape, anyunknownShape] voidShape False True)
  , ("push", FunctionSignature [anyunknownShape, unknownShape] voidShape False True)
  , ("pop", FunctionSignature [anyunknownShape] unknownShape False True)
  , ("zeros", FunctionSignature [intShape] arrayShape False False)
  , ("ones", FunctionSignature [intShape] arrayShape False False)
  , ("range", FunctionSignature [intShape, intShape] listShape False False)
  , ("sum", FunctionSignature [anyunknownShape] floatShape False False)
  , ("mean", FunctionSignature [anyunknownShape] floatShape False False)
  , ("max", FunctionSignature [anyunknownShape] unknownShape False False)
  , ("min", FunctionSignature [anyunknownShape] unknownShape False False)
  , ("sort", FunctionSignature [anyunknownShape] anyunknownShape False False)
  , ("reverse", FunctionSignature [anyunknownShape] anyunknownShape False False)
  ]

-- | Run shape analysis
runShapeAnalysis :: ShapeAnalysisM a -> Either Text (a, ShapeAnalysisState)
runShapeAnalysis m = runExcept $ runStateT (runReaderT m initialContext) initialState

-- | Analyze entire program
analyzeProgram :: [CommonExpr] -> ShapeAnalysisM ()
analyzeProgram stmts = do
  enterScope globalScope
  mapM_ analyzeStatementExpr stmts
  exitScope

analyzeStatementExpr :: CommonExpr -> ShapeAnalysisM ()
analyzeStatementExpr expr = void $ analyzeShape expr

-- | Enter a new scope
enterScope :: Int -> ShapeAnalysisM ()
enterScope _scopeType = do
  currentScope <- asks scCurrentScope
  let newScope = ScopeInfo
        { siVariables = HashMap.empty
        , siParentScope = Just currentScope
        , siScopeDepth = siScopeDepth currentScope + 1
        }
  modify $ \s -> s
    { sasShapeMap = HashMap.empty  -- Reset for new scope
    }
  local (\ctx -> ctx { scCurrentScope = newScope }) $ return ()

-- | Exit current scope
exitScope :: ShapeAnalysisM ()
exitScope = do
  currentScope <- asks scCurrentScope
  case siParentScope currentScope of
    Just parentScope -> modify $ \s -> s { sasShapeMap = HashMap.empty }  -- Reset for parent scope
    Nothing -> return ()  -- Already at global scope

-- | Update variable shape and track it in current scope
updateVariableShape :: Identifier -> ShapeInfo -> ShapeAnalysisM ()
updateVariableShape var shape = do
  -- Update shape map
  modify $ \s -> s { sasShapeMap = HashMap.insert var shape (sasShapeMap s) }

-- | Increment access count for a variable (simplified)
incrementAccessCount :: Identifier -> ShapeAnalysisM ()
incrementAccessCount _var = return ()

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
  , siOrigin = UserAnnotationAlt
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
  , siOrigin = UserAnnotationAlt
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
  , siOrigin = UserAnnotationAlt
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
    , siOrigin = UserAnnotationAlt
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
  , siOrigin = UserAnnotationAlt
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
  , siOrigin = UserAnnotationAlt
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
analyzeStructure (TStruct name fieldTypes) = do
  -- Create field names as indices since TStruct doesn't provide them
  let fieldDefs = zipWith (\i ftype -> (T.pack ("field" ++ show i), ftype)) [0..] fieldTypes

  -- Sort fields by size for better packing
  let sortedFields = sortOn (negate . getTypeSize . snd) fieldDefs
  let (fieldOrder, sizes, alignments) = unzip3
        [(fname, getTypeSize ftype, getTypeAlignment ftype) | (fname, ftype) <- sortedFields]

  -- Calculate layout with padding
  let (offsets, paddings) = calculateLayout sizes alignments
  let totalSize = if null offsets then 0 else last offsets + last sizes
  let structAlign = if null alignments then 8 else maximum alignments

  -- Detect hot fields based on access patterns (simplified)
  let hotFields = Set.empty

  return StructShape
    { ssFields = HashMap.fromList fieldDefs
    , ssFieldOrder = fieldOrder
    , ssSize = align totalSize structAlign
    , ssAlignment = structAlign
    , ssPadding = paddings
    , ssIsPackable = sum paddings == 0
    , ssHotFields = hotFields
    }


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
inferunknownShape :: CommonExpr -> ShapeAnalysisM ShapeInfo
inferunknownShape expr = do
  shape <- analyzeShape expr
  -- Analyze usage in current scope to determine growth pattern
  growth <- analyzeGrowthPattern expr
  avgSize <- analyzeAverageSize expr
  
  case siElementType shape of
    Just elemType -> return unknownShape
      { siElementType = Just elemType
      , siDimensions = case siDimensions shape of
          dims | Vector.length dims > 0 && Vector.head dims >= 0 -> Vector.singleton (Vector.head dims)
          _ -> Vector.empty
      , siIsConstant = siIsConstant shape
      , siAccessPattern = siAccessPattern shape
      , siFieldTypes = HashMap.insert "size" TInt (siFieldTypes shape)
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
        | fname `elem` ["append", "push", "extend"] -> return GrowingExponential
        | fname `elem` ["zeros", "ones", "range"] -> return FixedSize
      _ -> return UnknownGrowth
    _ -> return UnknownGrowth

-- | Analyze average size of containers
analyzeAverageSize :: CommonExpr -> ShapeAnalysisM (Maybe Int)
analyzeAverageSize expr = do
  shape <- analyzeShape expr
  case siDimensions shape of
    dims | Vector.length dims > 0 && Vector.head dims >= 0 -> 
      return $ Just (Vector.head dims)
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
generateCppStructure :: ShapeInfo -> ShapeAnalysisM Text
generateCppStructure shape = do
  context <- ask
  existing <- gets sasCppMapping
  
  -- Check if we already have a mapping
  case HashMap.lookup shape existing of
    Just mapping -> return mapping
    Nothing -> do
      mapping <- generateNewMapping shape context
      modify $ \s -> s { sasCppMapping = HashMap.insert shape mapping existing }
      return mapping

generateNewMapping :: ShapeInfo -> ShapeContext -> ShapeAnalysisM Text
generateNewMapping shape context
  -- Fixed-size array
  | Vector.length (siDimensions shape) == 1 
    && siIsKnown shape 
    && siIsHomogeneous shape
    && isJust (siElementType shape) = do
    let size = head (siDimensions shape)
    if size > 0 && size <= scMaxInlineSize context
      then return "cpp_mapping"
        { cmType = T.concat ["std::array<", cppType (fromJust (siElementType shape)), ", ", T.pack (show size), ">"]
        , cmHeaders = ["<array>"]
        , cmOptimizations = ["Stack-allocated for small fixed size", "Zero-cost abstraction"]
        , cmMemoryLayout = StackLayout
        , cmInitializerHint = Just "Use brace initialization"
        }
      else return "cpp_mapping"
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
      SequentialAccess -> return "cpp_mapping"
        { cmType = T.concat ["std::vector<", elemType, ">"]
        , cmHeaders = ["<vector>"]
        , cmOptimizations = ["Contiguous memory for cache efficiency", "Consider std::deque for front insertion"]
        , cmMemoryLayout = ContiguousLayout
        , cmInitializerHint = Nothing
        }
      RandomAccess | siIsConstant shape -> return "cpp_mapping"
        { cmType = T.concat ["std::vector<", elemType, ">"]
        , cmHeaders = ["<vector>"]
        , cmOptimizations = ["Mark as const for optimization", "Consider std::array if size becomes known"]
        , cmMemoryLayout = ContiguousLayout
        , cmInitializerHint = Just "Initialize with std::initializer_list"
        }
      _ -> return "cpp_mapping"
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
      then return "cpp_mapping"
        { cmType = T.concat ["std::unordered_map<", keyType, ", ", valueType, ">"]
        , cmHeaders = ["<unordered_map>"]
        , cmOptimizations = ["O(1) average lookup", "Consider perfect hash if keys are known", "Mark as const"]
        , cmMemoryLayout = HashBasedLayout
        , cmInitializerHint = Just "Use initializer list"
        }
      else return "cpp_mapping"
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
      then return "cpp_mapping"
        { cmType = "struct /* packed */"
        , cmHeaders = []
        , cmOptimizations = ["Pack struct with __attribute__((packed))", "Reorder fields by size"]
        , cmMemoryLayout = CustomLayout "packed_struct"
        , cmInitializerHint = Just "Use designated initializers (C++20)"
        }
      else return "cpp_mapping"
        { cmType = "struct"
        , cmHeaders = []
        , cmOptimizations = ["Align fields for performance", "Group frequently accessed fields"]
        , cmMemoryLayout = CustomLayout "aligned_struct"
        , cmInitializerHint = Just "Use aggregate initialization"
        }
  
  -- Tuple
  | Vector.length (siDimensions shape) == 1 && not (HashMap.null (siFieldTypes shape)) = do
    let types = [cppType t | (_, t) <- sortOn fst (HashMap.toList (siFieldTypes shape))]
    return "cpp_mapping"
      { cmType = T.concat ["std::tuple<", T.intercalate ", " types, ">"]
      , cmHeaders = ["<tuple>"]
      , cmOptimizations = ["Use structured bindings (C++17)", "Consider std::pair for 2 elements"]
      , cmMemoryLayout = ContiguousLayout
      , cmInitializerHint = Just "Use std::make_tuple"
      }
  
  -- Set
  | siElementType shape == Just TBool = return "cpp_mapping"
    { cmType = "std::bitset<N> /* or std::vector<bool> */"
    , cmHeaders = ["<bitset>", "<vector>"]
    , cmOptimizations = ["Use bitset for fixed size", "Space-efficient bool storage"]
    , cmMemoryLayout = CustomLayout "bitset"
    , cmInitializerHint = Nothing
    }
  
  -- Generic/unknown
  | otherwise = do
    if scTargetCppVersion context >= "c++17"
      then return "cpp_mapping"
        { cmType = "std::any"
        , cmHeaders = ["<any>"]
        , cmOptimizations = ["Type erasure for maximum flexibility", "Consider std::variant if types are known"]
        , cmMemoryLayout = HeapLayout
        , cmInitializerHint = Just "Use std::make_any"
        }
      else return "cpp_mapping"
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
optimizeDataStructures :: [CommonExpr] -> ShapeAnalysisM [(CommonExpr, Text)]
optimizeDataStructures exprs = do
  shapes <- mapM analyzeShape exprs
  mappings <- mapM generateCppStructure shapes
  
  -- Add optimization suggestions
  forM_ (zip3 exprs shapes mappings) $ \(expr, shape, mapping) -> do
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
  OpSub -> broadcastShapes left right
  OpMul -> broadcastShapes left right
  OpDiv -> broadcastShapes left right
  OpMod -> broadcastShapes left right
  OpPow -> broadcastShapes left right
  
  -- Bitwise operations
  OpBitAnd -> broadcastShapes left right
  OpBitOr -> broadcastShapes left right
  OpBitXor -> broadcastShapes left right
  OpShiftL -> broadcastShapes left right
  OpShiftR -> broadcastShapes left right
  
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
  _ -> case (siDimensions left, siDimensions right) of
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
  OpBitNot -> shape { siIsConstant = siIsConstant shape }

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

-- Removed duplicate helper shapes

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
  , siOrigin = UserAnnotationAlt
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
  , siOrigin = UserAnnotationAlt
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
  , siOrigin = UserAnnotationAlt
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
  , siOrigin = UserAnnotationAlt
  }

anyunknownShape :: ShapeInfo
anyunknownShape = ShapeInfo
  { siDimensions = Vector.singleton (-1)
  , siIsKnown = False
  , siElementType = Nothing
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Just 8
  , siIsHomogeneous = False
  , siAccessPattern = UnknownAccess
  , siIsConstant = False
  , siOrigin = UserAnnotationAlt
  }

-- Utility functions

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (== x) xs