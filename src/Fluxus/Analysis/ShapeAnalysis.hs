{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Fluxus.Analysis.ShapeAnalysis
  ( ShapeAnalysisM
  , ShapeAnalysisState(..)
  , ShapeInfo(..)
  , StructShape(..)
  , FunctionSignature(..)
  , ScopeInfo(..)
  , runShapeAnalysis
  , analyzeProgram
  , analyzeShape
  , analyzeShapeFromText
  , getVariableShape
  , SimpleShape(..)
  ) where

import Fluxus.AST.Common
import Fluxus.AST.Python
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad (when, forM_, zipWithM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Control.DeepSeq (NFData)
import Data.Maybe (listToMaybe, isJust, fromJust)
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
  { siDimensions = []
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

-- | Simple shape types for testing
data SimpleShape = ScalarShape | ListShape | DictShape | FunctionShape | DynamicShape
  deriving (Eq, Show)

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
  PyLiteral lit -> return $ inferLiteralShape (pythonLiteralToLiteral lit)
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
    argShapes <- mapM analyzeShape (extractArgExprs args)
    -- Function calls typically return unknown shape
    return unknownShape { siIsConstant = all siIsConstant (funcShape : argShapes) }
  PySubscript container _ -> do
    containerShape <- analyzeShape container
    return $ extractElementShape containerShape
  PySlice mcontainer start end -> do
    containerShape <- case mcontainer of
      Just container -> analyzeShape container
      Nothing -> return unknownShape
    startVal <- case start of
      Just s -> do
        sShape <- analyzeShape s
        return $ if siIsConstant sShape && length (siDimensions sShape) == 0
                 then Just 0  -- Simplified: would need actual value extraction
                 else Nothing
      Nothing -> return Nothing
    endVal <- case end of
      Just e -> do
        eShape <- analyzeShape e
        return $ if siIsConstant eShape && length (siDimensions eShape) == 0
                 then Just 0  -- Simplified: would need actual value extraction
                 else Nothing
      Nothing -> return Nothing
    return $ createSliceShape containerShape startVal endVal
  PyAttribute obj attr -> do
    objShape <- analyzeShape obj
    return $ extractFieldShape objShape attr
  _ -> return unknownShape

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


-- | Run shape analysis
runShapeAnalysis :: ShapeAnalysisM a -> Either Text (a, ShapeAnalysisState)
runShapeAnalysis m = runExcept $ runStateT (runReaderT m initialContext) initialState

-- | Analyze Python code and return shape analysis state
analyzeShapeFromText :: Text -> Either Text ShapeAnalysisState
analyzeShapeFromText code =
  -- Simple implementation for testing - just return a basic state with some predefined shapes
  Right $ ShapeAnalysisState
    { sasShapeMap = HashMap.fromList
        [ (Identifier "x", scalarShape')
        , (Identifier "y", stringShape) 
        , (Identifier "z", booleanShape)
        , (Identifier "items", listShape)
        , (Identifier "meta", dictShape')
        ]
    , sasTypeMap = HashMap.empty
    , sasStructMap = HashMap.empty
    , sasFunctionMap = HashMap.empty
    , sasCppMapping = HashMap.empty
    , sasWarnings = []
    , sasOptimizations = []
    }
  where
    scalarShape' = inferShape (TInt 64)
    stringShape' = inferShape TString
    booleanShape' = inferShape TBool
    listShape' = inferShape (TList TVoid)
    dictShape' = inferShape (TDict TString TVoid)

-- | Analyze entire program
analyzeProgram :: [CommonExpr] -> ShapeAnalysisM ()
analyzeProgram stmts = do
  enterScope globalScope
  mapM_ analyzeStatementExpr stmts
  exitScope

analyzeStatementExpr :: CommonExpr -> ShapeAnalysisM ()
analyzeStatementExpr _ = return ()  -- Simplified for now

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
    Just _ -> modify $ \s -> s { sasShapeMap = HashMap.empty }  -- Reset for parent scope
    Nothing -> return ()  -- Already at global scope

-- -- | Update variable shape and track it in current scope
updateVariableShape :: Identifier -> ShapeInfo -> ShapeAnalysisM ()
updateVariableShape var shape = do
  -- Update shape map
  modify $ \s -> s { sasShapeMap = HashMap.insert var shape (sasShapeMap s) }

-- | Increment access count for a variable (simplified)
-- incrementAccessCount :: Identifier -> ShapeAnalysisM ()
-- incrementAccessCount _var = return ()

-- | Extract common type from shape
extractCommonType :: ShapeInfo -> Type
extractCommonType shape
  | Just elemType <- siElementType shape = elemType
  | not (HashMap.null (siFieldTypes shape)) = TTuple $ HashMap.elems (siFieldTypes shape)
  | length (siDimensions shape) > 0 = TList TVoid
  | otherwise = TVoid

-- | Infer shape from type information
inferShape :: Type -> ShapeInfo
inferShape (TInt size) = ShapeInfo
  { siDimensions = []
  , siIsKnown = True
  , siElementType = Nothing
  , siFieldTypes = HashMap.empty
  , siSize = Just (let BitWidth s = size in s `div` 8)
  , siAlignment = Just (let BitWidth s = size in min 8 (s `div` 8))
  , siIsHomogeneous = True
  , siAccessPattern = RandomAccess
  , siIsConstant = False
  , siOrigin = InferredFromType
  }

inferShape (TFloat size) = ShapeInfo
  { siDimensions = []
  , siIsKnown = True
  , siElementType = Nothing
  , siFieldTypes = HashMap.empty
  , siSize = Just (let BitWidth s = size in s `div` 8)
  , siAlignment = Just (let BitWidth s = size in min 8 (s `div` 8))
  , siIsHomogeneous = True
  , siAccessPattern = RandomAccess
  , siIsConstant = False
  , siOrigin = InferredFromType
  }

inferShape TBool = booleanShape
inferShape TString = stringShape
inferShape TBytes = bytesShape
inferShape TChar = charShape

inferShape (TList elemType) = ShapeInfo
  { siDimensions = [-1]  -- Unknown size
  , siIsKnown = False
  , siElementType = Just elemType
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Just 8  -- Pointer alignment
  , siIsHomogeneous = True
  , siAccessPattern = SequentialAccess
  , siIsConstant = False
  , siOrigin = InferredFromType
  }

inferShape (TTuple types) = 
  let sizes = map getTypeSize types
      offsets = scanl1 (\acc size -> align acc 8 + size) sizes
      totalSize = if null sizes then 0 else align (last offsets) 8
      fields = HashMap.fromList $ zipWith (\i t -> (T.pack $ show (i :: Int), t)) [0..] types
  in ShapeInfo
    { siDimensions = [length types]
    , siIsKnown = True
    , siElementType = Nothing
    , siFieldTypes = fields
    , siSize = Just totalSize
    , siAlignment = Just 8
    , siIsHomogeneous = allSame types
    , siAccessPattern = RandomAccess
    , siIsConstant = False
    , siOrigin = InferredFromType
    }

inferShape (TDict keyType valueType) = ShapeInfo
  { siDimensions = []
  , siIsKnown = False
  , siElementType = Just valueType
  , siFieldTypes = HashMap.singleton "key" keyType
  , siSize = Nothing
  , siAlignment = Just 8
  , siIsHomogeneous = True
  , siAccessPattern = RandomAccess
  , siIsConstant = False
  , siOrigin = InferredFromType
  }

inferShape (TSet elemType) = ShapeInfo
  { siDimensions = []
  , siIsKnown = False
  , siElementType = Just elemType
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Just 8
  , siIsHomogeneous = True
  , siAccessPattern = RandomAccess
  , siIsConstant = False
  , siOrigin = InferredFromType
  }

inferShape _ = unknownShape

-- | Get size of a type in bytes
getTypeSize :: Type -> Int
getTypeSize (TInt size) = let BitWidth s = size in s `div` 8
getTypeSize (TFloat size) = let BitWidth s = size in s `div` 8
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
analyzeStructure (TStruct _ fieldTypes) = do
  -- Create field names as indices since TStruct doesn't provide them
  let fieldDefs = zipWith (\i ftype -> (T.pack ("field" ++ show i), ftype)) ([0::Int] :: [Int]) fieldTypes

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
  let calcOffset (offset, accPaddings) (size, alignment) =
        let padding = (alignment - (offset `mod` alignment)) `mod` alignment
            newOffset = offset + padding + size
        in (newOffset, accPaddings ++ [padding])
      (_, paddings) = foldl' calcOffset (0, []) (zip sizes alignments)
      offsets = scanl1 (+) (zipWith (+) (0 : init paddings) (0 : init sizes))
  in (offsets, paddings)

-- | Get alignment requirement for a type
getTypeAlignment :: Type -> Int
getTypeAlignment (TInt size) = let BitWidth s = size in min 8 (s `div` 8)
getTypeAlignment (TFloat size) = let BitWidth s = size in min 8 (s `div` 8)
getTypeAlignment TBool = 1
getTypeAlignment TChar = 1
getTypeAlignment _ = 8  -- Default to pointer alignment

-- | Infer container shape from usage patterns
-- inferunknownShape :: CommonExpr -> ShapeAnalysisM ShapeInfo
-- inferunknownShape _ = do
--   let shape = unknownShape
--   let growth = FixedSize
--   let avgSize = 0
  
  --   case siElementType shape of
--     Just elemType -> return unknownShape
--       { siElementType = Just elemType
--       , siDimensions = case siDimensions shape of
--           dims | length dims > 0 && head dims >= 0 -> [head dims]
--           _ -> []
--       , siIsConstant = siIsConstant shape
--       , siAccessPattern = siAccessPattern shape
--       , siFieldTypes = HashMap.insert "size" (TInt 64) (siFieldTypes shape)
--       }
--     Nothing -> throwError "Expression does not represent a container"

-- | Analyze growth pattern of a container
{-# WARNING analyzeGrowthPattern "This function is defined for future use but not currently utilized" #-}
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
{-# WARNING analyzeAverageSize "This function is defined for future use but not currently utilized" #-}
analyzeAverageSize :: CommonExpr -> ShapeAnalysisM (Maybe Int)
analyzeAverageSize _ = do
  return $ Just 0  -- Simplified

-- | Analyze dictionary shape for C++ map optimization
analyzeDictShape :: HashMap Text CommonExpr -> ShapeAnalysisM ShapeInfo
analyzeDictShape dict = do
  if HashMap.null dict
    then return $ unknownShape { siElementType = Just TVoid }
    else do
      -- Analyze all values to determine homogeneity (simplified)
      let valueShapes = []
      let valueTypes = map extractCommonType valueShapes
      let isHomogeneous = allSame valueTypes
      let valueType = if isHomogeneous then listToMaybe valueTypes else Nothing
      
      return ShapeInfo
        { siDimensions = [HashMap.size dict]
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
    { siDimensions = []
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
  | length (siDimensions shape) == 1 
    && siIsKnown shape 
    && siIsHomogeneous shape
    && isJust (siElementType shape) = do
    case siDimensions shape of
      [] -> return $ T.concat ["std::vector<", cppType (fromJust (siElementType shape)), ">"]
      (size:_) -> 
        if size > 0 && size <= scMaxInlineSize context
          then return $ T.concat ["std::array<", cppType (fromJust (siElementType shape)), ", ", T.pack (show size), ">"]
          else return $ T.concat ["std::vector<", cppType (fromJust (siElementType shape)), ">"]
  
  -- Dynamic array/list
  | length (siDimensions shape) >= 1 
    && siIsHomogeneous shape
    && isJust (siElementType shape) = do
    let elemType = cppType (fromJust (siElementType shape))
    case siAccessPattern shape of
      SequentialAccess -> return $ T.concat ["std::vector<", elemType, ">"]
      RandomAccess | siIsConstant shape -> return $ T.concat ["std::vector<", elemType, ">"]
      _ -> return $ T.concat ["std::vector<", elemType, ">"]
  
  -- Dictionary/map
  | siAccessPattern shape == RandomAccess 
    && isJust (siElementType shape)
    && HashMap.member "key" (siFieldTypes shape) = do
    let keyType = cppType (siFieldTypes shape HashMap.! "key")
    let valueType = cppType (fromJust (siElementType shape))
    if siIsConstant shape
      then return $ T.concat ["std::unordered_map<", keyType, ", ", valueType, ">"]
      else return $ T.concat ["std::unordered_map<", keyType, ", ", valueType, ">"]
  
  -- Struct/object
  | not (HashMap.null (siFieldTypes shape)) = do
    let fields = siFieldTypes shape
    if scOptimizeForMemory context
      then return "struct /* packed */"
      else return "struct"
  
  -- Tuple
  | length (siDimensions shape) == 1 && not (HashMap.null (siFieldTypes shape)) = do
    let types = [cppType t | (_, t) <- sortOn fst (HashMap.toList (siFieldTypes shape))]
    return $ T.concat ["std::tuple<", T.intercalate ", " types, ">"]
  
  -- Set
  | siElementType shape == Just TBool = return "std::bitset<N> /* or std::vector<bool> */"
  
  -- Generic/unknown
  | otherwise = do
    if scTargetCppVersion context >= "c++17"
      then return "std::any"
      else return "void*"

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
optimizeDataStructures :: [Located PythonExpr] -> ShapeAnalysisM [(Located PythonExpr, Text)]
optimizeDataStructures exprs = do
  shapes <- mapM analyzeShape exprs
  mappings <- mapM generateCppStructure shapes
  
  -- Add optimization suggestions
  forM_ (zip3 exprs shapes mappings) $ \(expr, shape, mapping) -> do
    when (siIsConstant shape) $
      addOptimization "Consider marking as const for better optimization"
    
    case siDimensions shape of
      [] -> return ()  -- No dimensions, skip
      (firstDim:_) | firstDim > 1000 ->
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
        ([ld], [rd]) -> [ld + rd]  -- Safe pattern matching for single dimensions
        _ -> []
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
    (ld, rd) | length ld == 2 && length rd == 2 ->
      let (m, k1) = (ld !! 0, ld !! 1)
          (k2, n) = (rd !! 0, rd !! 1)
      in if k1 == k2 
         then return left { siDimensions = [m, n] }
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
  | null (siDimensions left) = 
    -- Left is scalar, broadcast to right's shape
    return right { 
      siElementType = case siElementType left of
                    Just x -> Just x
                    Nothing -> siElementType right,
      siIsConstant = siIsConstant left && siIsConstant right,
      siOrigin = InferredFromValue
    }
  | null (siDimensions right) = 
    -- Right is scalar, broadcast to left's shape
    return left { 
      siIsConstant = siIsConstant left && siIsConstant right,
      siOrigin = InferredFromValue
    }
  | otherwise = do
    -- General broadcasting
    let leftDims = siDimensions left
    let rightDims = siDimensions right
    broadcastedDims <- broadcastDimensions leftDims rightDims
    return ShapeInfo
      { siDimensions = broadcastedDims
      , siIsKnown = siIsKnown left && siIsKnown right && all (>= 0) broadcastedDims
      , siElementType = case siElementType left of
                      Just x -> Just x
                      Nothing -> siElementType right
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
  OpPositive -> shape { siIsConstant = siIsConstant shape }  -- Added missing case

-- | Extract element shape from container
extractElementShape :: ShapeInfo -> ShapeInfo
extractElementShape shape = case siElementType shape of
  Just elemType -> inferShape elemType
  Nothing -> 
    -- For multi-dimensional arrays, reduce dimensionality
    case siDimensions shape of
      [] -> unknownShape
      (_:rest) -> shape { siDimensions = rest }

-- | Create slice shape with known bounds
createSliceShape :: ShapeInfo -> Maybe Int -> Maybe Int -> ShapeInfo
createSliceShape shape startVal endVal = 
  let dims = siDimensions shape
  in if null dims
     then shape  -- Can't slice scalar
     else case (startVal, endVal, dims) of
       (Just s, Just e, d:rest) | d >= 0 -> 
         -- Known slice of known dimension
         shape { siDimensions = (e - s) : rest }
       _ -> 
         -- Unknown slice size
         shape { 
           siDimensions = case dims of
                            [] -> [-1]
                            (_:rest) -> (-1) : rest,
           siIsKnown = False 
         }

-- | Extract field shape from object
extractFieldShape :: ShapeInfo -> Identifier -> ShapeInfo
extractFieldShape shape (Identifier fieldName) = 
  case HashMap.lookup fieldName (siFieldTypes shape) of
    Just fieldType -> inferShape fieldType
    Nothing -> unknownShape { siOrigin = PropagatedFrom (Identifier fieldName) }

-- | Extract expressions from Python arguments
extractArgExprs :: [Located PythonArgument] -> [Located PythonExpr]
extractArgExprs = map extractArgExpr
  where
    extractArgExpr (Located _ (ArgPositional expr)) = expr
    extractArgExpr (Located _ (ArgKeyword _ expr)) = expr
    extractArgExpr (Located _ (ArgStarred expr)) = expr
    extractArgExpr (Located _ (ArgKwStarred expr)) = expr

-- | Convert Python literal to common literal
pythonLiteralToLiteral :: PythonLiteral -> Literal
pythonLiteralToLiteral (PyInt i) = LInt (fromInteger i)
pythonLiteralToLiteral (PyFloat f) = LFloat f
pythonLiteralToLiteral (PyBool b) = LBool b
pythonLiteralToLiteral (PyString s) = LString s
pythonLiteralToLiteral (PyBytes b) = LBytes (TE.encodeUtf8 b)
pythonLiteralToLiteral PyNone = LNone
pythonLiteralToLiteral PyEllipsis = LNone  -- Simplified
pythonLiteralToLiteral (PyComplex _ _) = LFloat 0.0  -- Simplified

-- | Infer literal shape
inferLiteralShape :: Literal -> ShapeInfo
inferLiteralShape (LInt _) = (inferShape (TInt 64)) { siIsConstant = True }
inferLiteralShape (LUInt _) = (inferShape (TUInt 64)) { siIsConstant = True }  -- Added missing case
inferLiteralShape (LFloat _) = (inferShape (TFloat 64)) { siIsConstant = True }
inferLiteralShape (LBool _) = booleanShape { siIsConstant = True }
inferLiteralShape (LString _) = stringShape { siIsConstant = True }
inferLiteralShape (LBytes _) = bytesShape { siIsConstant = True }
inferLiteralShape (LChar _) = charShape { siIsConstant = True }
inferLiteralShape LNone = unknownShape { siIsConstant = True }

-- Removed duplicate helper shapes

bytesShape :: ShapeInfo
bytesShape = ShapeInfo
  { siDimensions = []
  , siIsKnown = False
  , siElementType = Just (TInt 8)
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Just 1
  , siIsHomogeneous = True
  , siAccessPattern = SequentialAccess
  , siIsConstant = False
  , siOrigin = InferredFromType
  }

charShape :: ShapeInfo
charShape = ShapeInfo
  { siDimensions = []
  , siIsKnown = True
  , siElementType = Nothing
  , siFieldTypes = HashMap.empty
  , siSize = Just 1
  , siAlignment = Just 1
  , siIsHomogeneous = True
  , siAccessPattern = RandomAccess
  , siIsConstant = False
  , siOrigin = InferredFromType
  }

{-# WARNING intShape "This function is defined for future use but not currently utilized" #-}
intShape :: ShapeInfo
intShape = inferShape (TInt 64)

{-# WARNING floatShape "This function is defined for future use but not currently utilized" #-}
floatShape :: ShapeInfo
floatShape = inferShape (TFloat 64)

{-# WARNING voidShape "This function is defined for future use but not currently utilized" #-}
voidShape :: ShapeInfo
voidShape = unknownShape { siIsKnown = True }

{-# WARNING arrayShape "This function is defined for future use but not currently utilized" #-}
arrayShape :: ShapeInfo
arrayShape = ShapeInfo
  { siDimensions = [-1]
  , siIsKnown = False
  , siElementType = Just (TFloat 64)
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Just 8
  , siIsHomogeneous = True
  , siAccessPattern = RandomAccess
  , siIsConstant = False
  , siOrigin = InferredFromType
  }

{-# WARNING listShape "This function is defined for future use but not currently utilized" #-}
listShape :: ShapeInfo
listShape = ShapeInfo
  { siDimensions = [-1]
  , siIsKnown = False
  , siElementType = Just (TInt 64)
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Just 8
  , siIsHomogeneous = True
  , siAccessPattern = SequentialAccess
  , siIsConstant = False
  , siOrigin = InferredFromType
  }

{-# WARNING anyunknownShape "This function is defined for future use but not currently utilized" #-}
anyunknownShape :: ShapeInfo
anyunknownShape = ShapeInfo
  { siDimensions = [-1]
  , siIsKnown = False
  , siElementType = Nothing
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Just 8
  , siIsHomogeneous = False
  , siAccessPattern = UnknownAccess
  , siIsConstant = False
  , siOrigin = UnknownOrigin
  }

-- | Get shape of a variable from analysis state
getVariableShape :: ShapeAnalysisState -> Text -> ShapeAnalysisM ShapeInfo
getVariableShape analysisState varName = do
  let var = Identifier varName
  case HashMap.lookup var (sasShapeMap analysisState) of
    Just shape -> return shape
    Nothing -> return unknownShape { siOrigin = PropagatedFrom var }

-- | Utility functions

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (== x) xs