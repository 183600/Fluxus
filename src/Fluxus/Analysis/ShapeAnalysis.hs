{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Fluxus.Analysis.ShapeAnalysis
  ( ShapeAnalysisM
  , ShapeAnalysisState(..)
  , ShapeInfo(..)
  , StructShape(..)
  , ContainerShape(..)
  , CppMapping(..)
  , runShapeAnalysis
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
import Control.Monad (void)
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

type ShapeAnalysisM = ReaderT ShapeContext (StateT ShapeAnalysisState (Except Text))

-- | Context for shape analysis
data ShapeContext = ShapeContext
  { scOptimizeForMemory :: !Bool        -- Optimize for memory usage vs access speed
  , scTargetCppVersion :: !Text         -- Target C++ version for optimizations
  , scMaxInlineSize :: !Int             -- Maximum size for inline optimization
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | State for shape analysis
data ShapeAnalysisState = ShapeAnalysisState
  { sasShapeMap :: !(HashMap Identifier ShapeInfo)           -- Variable shape information
  , sasStructShapes :: !(HashMap Text StructShape)           -- Known struct shapes
  , sasContainerShapes :: !(HashMap Identifier ContainerShape) -- Container shape info
  , sasCppMappings :: !(HashMap ShapeInfo CppMapping)        -- Shape to C++ mappings
  , sasOptimizations :: ![Text]                              -- Optimization suggestions
  } deriving stock (Show, Generic)
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
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Access patterns for optimization
data AccessPattern
  = SequentialAccess     -- Sequential iteration
  | RandomAccess         -- Random access by key/index
  | WriteOnceReadMany    -- Written once, read many times
  | ReadOnceWriteMany    -- Read once, written many times
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
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Growth patterns for containers
data GrowthPattern
  = FixedSize              -- Fixed size, no growth
  | LinearGrowth Int       -- Grows by fixed amount
  | ExponentialGrowth      -- Doubles in size
  | UnknownGrowth          -- Cannot determine growth pattern
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | C++ mapping strategies
data CppMapping = CppMapping
  { cmType :: !Text                      -- C++ type to use
  , cmHeaders :: ![Text]                 -- Required headers
  , cmOptimizations :: ![Text]           -- Specific optimizations
  , cmMemoryLayout :: !MemoryLayout      -- Memory layout strategy
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Memory layout strategies
data MemoryLayout
  = ContiguousLayout       -- std::vector, std::array
  | NodeBasedLayout        -- std::list, std::map
  | HashBasedLayout        -- std::unordered_map
  | TreeBasedLayout        -- std::map, std::set
  | CustomLayout Text      -- Custom structure
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Initial shape context
initialContext :: ShapeContext
initialContext = ShapeContext
  { scOptimizeForMemory = False
  , scTargetCppVersion = "c++20"
  , scMaxInlineSize = 64
  }

-- | Initial shape analysis state
initialState :: ShapeAnalysisState
initialState = ShapeAnalysisState
  { sasShapeMap = HashMap.empty
  , sasStructShapes = HashMap.empty
  , sasContainerShapes = HashMap.empty
  , sasCppMappings = HashMap.empty
  , sasOptimizations = []
  }

-- | Run shape analysis
runShapeAnalysis :: ShapeAnalysisM a -> Either Text (a, ShapeAnalysisState)
runShapeAnalysis m = runExcept $ runStateT (runReaderT m initialContext) initialState

-- | Analyze expression and infer its shape
analyzeShape :: CommonExpr -> ShapeAnalysisM ShapeInfo
analyzeShape (CELiteral lit) = return $ inferLiteralShape lit

analyzeShape (CEVar var) = do
  shapeMap <- gets sasShapeMap
  case HashMap.lookup var shapeMap of
    Just shape -> return shape
    Nothing -> return unknownShape

analyzeShape (CEBinaryOp op left right) = do
  leftShape <- analyzeShape (locatedValue left)
  rightShape <- analyzeShape (locatedValue right)
  combineBinaryShapes op leftShape rightShape

analyzeShape (CEUnaryOp op operand) = do
  operandShape <- analyzeShape (locatedValue operand)
  return $ transformUnaryShape op operandShape

analyzeShape (CEComparison _ left right) = do
  _ <- analyzeShape (locatedValue left)
  _ <- analyzeShape (locatedValue right)
  return booleanShape

analyzeShape (CECall func args) = do
  _funcShape <- analyzeShape (locatedValue func)
  _argShapes <- mapM (analyzeShape . locatedValue) args
  -- Function calls typically return unknown shapes unless we have more info
  return unknownShape

analyzeShape (CEIndex container index) = do
  containerShape <- analyzeShape (locatedValue container)
  _ <- analyzeShape (locatedValue index)
  return $ extractElementShape containerShape

analyzeShape (CESlice container start end) = do
  containerShape <- analyzeShape (locatedValue container)
  -- Analyze slice bounds if present
  case start of
    Just startExpr -> void $ analyzeShape (locatedValue startExpr)
    Nothing -> return ()
  case end of
    Just endExpr -> void $ analyzeShape (locatedValue endExpr)
    Nothing -> return ()
  return $ createSliceShape containerShape

analyzeShape (CEAttribute obj attr) = do
  objShape <- analyzeShape (locatedValue obj)
  return $ extractFieldShape objShape attr

-- | Infer shape from type information
inferShape :: Type -> ShapeInfo
inferShape (TInt size) = ShapeInfo
  { siDimensions = Vector.empty
  , siIsKnown = True
  , siElementType = Nothing
  , siFieldTypes = HashMap.empty
  , siSize = Just (size `div` 8)
  , siAlignment = Just (size `div` 8)
  , siIsHomogeneous = True
  , siAccessPattern = RandomAccess
  }

inferShape (TFloat size) = ShapeInfo
  { siDimensions = Vector.empty
  , siIsKnown = True
  , siElementType = Nothing
  , siFieldTypes = HashMap.empty
  , siSize = Just (size `div` 8)
  , siAlignment = Just (size `div` 8)
  , siIsHomogeneous = True
  , siAccessPattern = RandomAccess
  }

inferShape TBool = booleanShape
inferShape TString = stringShape
inferShape TBytes = bytesShape

inferShape (TList elemType) = ShapeInfo
  { siDimensions = Vector.singleton (-1)  -- Unknown size
  , siIsKnown = False
  , siElementType = Just elemType
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Nothing
  , siIsHomogeneous = True
  , siAccessPattern = SequentialAccess
  }

inferShape (TTuple types) = ShapeInfo
  { siDimensions = Vector.singleton (length types)
  , siIsKnown = True
  , siElementType = Nothing
  , siFieldTypes = HashMap.fromList $ zipWith (\i t -> (T.pack $ show (i :: Int), t)) [0..] types
  , siSize = Nothing  -- Would need to sum element sizes
  , siAlignment = Nothing
  , siIsHomogeneous = allSame types
  , siAccessPattern = RandomAccess
  }

inferShape (TDict _keyType valueType) = ShapeInfo
  { siDimensions = Vector.empty
  , siIsKnown = False
  , siElementType = Just valueType
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Nothing
  , siIsHomogeneous = True
  , siAccessPattern = RandomAccess
  }

inferShape _ = unknownShape

-- | Analyze structure for optimization opportunities
analyzeStructure :: Type -> ShapeAnalysisM StructShape
analyzeStructure (TStruct _name _fieldTypes) = do
  let fields = HashMap.empty  -- Would extract from TStruct definition
  let fieldOrder = []         -- Would analyze access patterns
  let totalSize = 0          -- Would calculate based on field types
  return StructShape
    { ssFields = fields
    , ssFieldOrder = fieldOrder
    , ssSize = totalSize
    , ssAlignment = 8  -- Default alignment
    , ssPadding = []
    , ssIsPackable = True
    , ssHotFields = Set.empty
    }

analyzeStructure _ = throwError "Not a struct type"

-- | Infer container shape from usage patterns
inferContainerShape :: CommonExpr -> ShapeAnalysisM ContainerShape
inferContainerShape expr = do
  shape <- analyzeShape expr
  case siElementType shape of
    Just elemType -> return ContainerShape
      { csElementType = elemType
      , csCapacity = Nothing
      , csGrowthPattern = UnknownGrowth
      , csIsResizable = True
      , csAccessPattern = siAccessPattern shape
      , csKeyType = Nothing
      }
    Nothing -> throwError "Expression does not represent a container"

-- | Analyze dictionary shape for C++ map optimization
analyzeDictShape :: HashMap Text CommonExpr -> ShapeAnalysisM ShapeInfo
analyzeDictShape dict = do
  if HashMap.null dict
    then return unknownShape
    else do
      -- Analyze all values to determine homogeneity
      valueShapes <- mapM analyzeShape (HashMap.elems dict)
      let valueTypes = map siElementType valueShapes
      let isHomogeneous = allSame (catMaybes valueTypes)
      
      return ShapeInfo
        { siDimensions = Vector.singleton (HashMap.size dict)
        , siIsKnown = True
        , siElementType = if isHomogeneous then listToMaybe (catMaybes valueTypes) else Nothing
        , siFieldTypes = HashMap.empty
        , siSize = Nothing
        , siAlignment = Nothing
        , siIsHomogeneous = isHomogeneous
        , siAccessPattern = RandomAccess
        }

-- | Analyze object shape for struct optimization
analyzeObjectShape :: HashMap Text Type -> ShapeAnalysisM ShapeInfo
analyzeObjectShape fields = do
  return ShapeInfo
    { siDimensions = Vector.empty
    , siIsKnown = True
    , siElementType = Nothing
    , siFieldTypes = fields
    , siSize = Nothing  -- Would calculate based on field sizes
    , siAlignment = Nothing
    , siIsHomogeneous = allSame (HashMap.elems fields)
    , siAccessPattern = RandomAccess
    }

-- | Generate optimized C++ data structure
generateCppStructure :: ShapeInfo -> ShapeAnalysisM CppMapping
generateCppStructure shape
  | Vector.null (siDimensions shape) && not (HashMap.null (siFieldTypes shape)) = do
    -- Object/struct type
    return CppMapping
      { cmType = "struct"
      , cmHeaders = ["<memory>"]
      , cmOptimizations = ["Pack struct fields for better cache locality"]
      , cmMemoryLayout = ContiguousLayout
      }
  
  | Vector.length (siDimensions shape) == 1 && siIsHomogeneous shape = do
    -- Array/vector type
    context <- ask
    let useArray = case Vector.head (siDimensions shape) of
          size | size > 0 && size <= scMaxInlineSize context -> True
          _ -> False
    
    if useArray
      then return CppMapping
        { cmType = "std::array"
        , cmHeaders = ["<array>"]
        , cmOptimizations = ["Use stack-allocated array for small fixed-size containers"]
        , cmMemoryLayout = ContiguousLayout
        }
      else return CppMapping
        { cmType = "std::vector"
        , cmHeaders = ["<vector>"]
        , cmOptimizations = ["Use std::vector for dynamic arrays", "Reserve capacity if size is known"]
        , cmMemoryLayout = ContiguousLayout
        }
  
  | siAccessPattern shape == RandomAccess && isJust (siElementType shape) = do
    -- Map/dictionary type
    return CppMapping
      { cmType = "std::unordered_map"
      , cmHeaders = ["<unordered_map>"]
      , cmOptimizations = ["Use std::unordered_map for O(1) average access"]
      , cmMemoryLayout = HashBasedLayout
      }
  
  | otherwise = do
    -- Generic/unknown type
    return CppMapping
      { cmType = "std::variant"
      , cmHeaders = ["<variant>"]
      , cmOptimizations = ["Use std::variant for dynamic typing"]
      , cmMemoryLayout = CustomLayout "variant"
      }

-- | Optimize data structures based on shape analysis
optimizeDataStructures :: [CommonExpr] -> ShapeAnalysisM [(CommonExpr, CppMapping)]
optimizeDataStructures exprs = do
  shapes <- mapM analyzeShape exprs
  mappings <- mapM generateCppStructure shapes
  return $ zip exprs mappings

-- Helper functions

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
  }

stringShape :: ShapeInfo
stringShape = ShapeInfo
  { siDimensions = Vector.empty
  , siIsKnown = False
  , siElementType = Just TChar
  , siFieldTypes = HashMap.empty
  , siSize = Nothing
  , siAlignment = Nothing
  , siIsHomogeneous = True
  , siAccessPattern = SequentialAccess
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
  }

inferLiteralShape :: Literal -> ShapeInfo
inferLiteralShape (LInt _) = inferShape (TInt 32)
inferLiteralShape (LFloat _) = inferShape (TFloat 64)
inferLiteralShape (LBool _) = booleanShape
inferLiteralShape (LString _) = stringShape
inferLiteralShape (LBytes _) = bytesShape
inferLiteralShape (LChar _) = inferShape TChar
inferLiteralShape LNone = unknownShape
inferLiteralShape _ = unknownShape  -- Catch-all for other literal types

combineBinaryShapes :: BinaryOp -> ShapeInfo -> ShapeInfo -> ShapeAnalysisM ShapeInfo
combineBinaryShapes OpConcat leftShape rightShape
  | siIsHomogeneous leftShape && siIsHomogeneous rightShape && siElementType leftShape == siElementType rightShape = do
    return ShapeInfo
      { siDimensions = Vector.zipWith (+) (siDimensions leftShape) (siDimensions rightShape)
      , siIsKnown = siIsKnown leftShape && siIsKnown rightShape
      , siElementType = siElementType leftShape
      , siFieldTypes = HashMap.empty
      , siSize = liftA2 (+) (siSize leftShape) (siSize rightShape)
      , siAlignment = max <$> siAlignment leftShape <*> siAlignment rightShape
      , siIsHomogeneous = True
      , siAccessPattern = SequentialAccess
      }
combineBinaryShapes _ _left _right = return unknownShape

transformUnaryShape :: UnaryOp -> ShapeInfo -> ShapeInfo
transformUnaryShape OpNot _ = booleanShape
transformUnaryShape _ shape = shape

extractElementShape :: ShapeInfo -> ShapeInfo
extractElementShape shape = case siElementType shape of
  Just elemType -> inferShape elemType
  Nothing -> unknownShape

createSliceShape :: ShapeInfo -> ShapeInfo
createSliceShape shape = shape { siDimensions = Vector.empty, siIsKnown = False }

extractFieldShape :: ShapeInfo -> Identifier -> ShapeInfo
extractFieldShape shape (Identifier fieldName) = 
  case HashMap.lookup fieldName (siFieldTypes shape) of
    Just fieldType -> inferShape fieldType
    Nothing -> unknownShape

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (== x) xs

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\x acc -> case x of Just y -> y:acc; Nothing -> acc) []

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False
