{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Fluxus.Analysis.OwnershipInference
  ( OwnershipInferenceM
  , OwnershipInferenceState(..)
  , OwnershipContext(..)
  , OwnershipResult(..)
  , OwnershipStrategy(..)
  , runOwnershipInference
  , inferOwnership
  , analyzeOwnership
  , analyzeFunction
  , optimizeOwnership
  , getOwnershipInfo
  , setOwnershipInfo
  , inferOptimalStrategy
  , generateCppMemoryManagement
  ) where

import Fluxus.AST.Common
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad (void, when)
import Data.Text (Text)
-- import qualified Data.Text as T  -- unused
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)

type OwnershipInferenceM = ReaderT OwnershipContext (StateT OwnershipInferenceState (Except Text))

-- | Ownership strategies for C++ code generation
data OwnershipStrategy
  = StackOwned                    -- Stack allocation with automatic destruction
  | UniqueOwnership               -- std::unique_ptr for single ownership
  | SharedOwnership               -- std::shared_ptr for shared ownership
  | BorrowedReference             -- Reference or raw pointer (non-owning)
  | MoveSemantics                 -- std::move for transfer of ownership
  | CopySemantics                 -- Deep copy for value semantics
  | WeakReference                 -- std::weak_ptr to break cycles
  | CustomRAII                    -- Custom RAII wrapper
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

-- | Context for ownership inference
data OwnershipContext = OwnershipContext
  { ocCurrentFunction :: !(Maybe Identifier)     -- Current function being analyzed
  , ocFunctionDepth :: !Int                      -- Function nesting depth
  , ocInConstructor :: !Bool                     -- Whether in constructor context
  , ocInDestructor :: !Bool                      -- Whether in destructor context
  , ocInMoveContext :: !Bool                     -- Whether in move context
  , ocOptimizeForSpeed :: !Bool                  -- Optimize for performance vs memory
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | State for ownership inference
data OwnershipInferenceState = OwnershipInferenceState
  { oisOwnershipMap :: !(HashMap Identifier OwnershipInfo)        -- Variable ownership info
  , oisStrategyMap :: !(HashMap Identifier OwnershipStrategy)     -- Optimal strategies
  , oisDependencyGraph :: !(HashMap Identifier (Set Identifier)) -- Ownership dependencies
  , oisLifetimeConstraints :: ![(Identifier, Identifier)]        -- Lifetime relationships
  , oisCppMappings :: !(HashMap Type Text)                       -- Type to C++ mappings
  , oisOptimizationHints :: ![Text]                              -- Optimization suggestions
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- | Result of ownership analysis
data OwnershipResult = OwnershipResult
  { orExpression :: !CommonExpr
  , orOwnership :: !OwnershipInfo
  , orStrategy :: !OwnershipStrategy
  , orCppType :: !Text
  , orOptimizations :: ![Text]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Initial ownership context
initialContext :: OwnershipContext
initialContext = OwnershipContext
  { ocCurrentFunction = Nothing
  , ocFunctionDepth = 0
  , ocInConstructor = False
  , ocInDestructor = False
  , ocInMoveContext = False
  , ocOptimizeForSpeed = True
  }

-- | Initial ownership inference state
initialState :: OwnershipInferenceState
initialState = OwnershipInferenceState
  { oisOwnershipMap = HashMap.empty
  , oisStrategyMap = HashMap.empty
  , oisDependencyGraph = HashMap.empty
  , oisLifetimeConstraints = []
  , oisCppMappings = HashMap.empty
  , oisOptimizationHints = []
  }

-- | Run ownership inference
runOwnershipInference :: OwnershipInferenceM a -> Either Text (a, OwnershipInferenceState)
runOwnershipInference m = runExcept $ runStateT (runReaderT m initialContext) initialState

-- | Get ownership information for a variable
getOwnershipInfo :: Identifier -> OwnershipInferenceM OwnershipInfo
getOwnershipInfo var = do
  ownershipMap <- gets oisOwnershipMap
  return $ HashMap.lookupDefault defaultOwnership var ownershipMap
  where
    defaultOwnership = OwnershipInfo
      { ownsMemory = False
      , canMove = False
      , refCount = Nothing
      , escapes = NoEscape
      , memLocation = Unknown
      }

-- | Set ownership information for a variable
setOwnershipInfo :: Identifier -> OwnershipInfo -> OwnershipInferenceM ()
setOwnershipInfo var ownership = do
  modify $ \s -> s { oisOwnershipMap = HashMap.insert var ownership (oisOwnershipMap s) }
  -- Also infer and set optimal strategy
  strategy <- inferOptimalStrategy ownership
  modify $ \s -> s { oisStrategyMap = HashMap.insert var strategy (oisStrategyMap s) }

-- | Add ownership dependency between variables
_addOwnershipDependency :: Identifier -> Identifier -> OwnershipInferenceM ()
_addOwnershipDependency from to = do
  modify $ \s -> s { oisDependencyGraph = HashMap.insertWith Set.union from (Set.singleton to) (oisDependencyGraph s) }

-- | Add lifetime constraint (first identifier must outlive second)
_addLifetimeConstraint :: Identifier -> Identifier -> OwnershipInferenceM ()
_addLifetimeConstraint outlives depends = do
  modify $ \s -> s { oisLifetimeConstraints = (outlives, depends) : oisLifetimeConstraints s }

-- | Main ownership inference function
inferOwnership :: CommonExpr -> OwnershipInferenceM OwnershipResult
inferOwnership expr = do
  ownership <- analyzeExpression expr
  strategy <- inferOptimalStrategy ownership
  cppType <- generateCppType expr ownership strategy
  opts <- generateOptimizationHints expr ownership strategy
  return $ OwnershipResult expr ownership strategy cppType opts

-- | Analyze expression and infer ownership
analyzeExpression :: CommonExpr -> OwnershipInferenceM OwnershipInfo
analyzeExpression (CELiteral _) = do
  -- Literals are always stack-allocated and owned
  return OwnershipInfo
    { ownsMemory = True
    , canMove = True
    , refCount = Just 1
    , escapes = NoEscape
    , memLocation = Stack
    }

analyzeExpression (CEVar var) = getOwnershipInfo var

analyzeExpression (CEBinaryOp op left right) = do
  leftOwnership <- analyzeExpression (locatedValue left)
  rightOwnership <- analyzeExpression (locatedValue right)
  
  case op of
    -- Assignment-like operations transfer ownership
    OpConcat -> do
      -- String/list concatenation creates new owned value
      return OwnershipInfo
        { ownsMemory = True
        , canMove = True
        , refCount = Just 1
        , escapes = max (escapes leftOwnership) (escapes rightOwnership)
        , memLocation = determineResultLocation (memLocation leftOwnership) (memLocation rightOwnership)
        }
    -- Arithmetic operations typically create new values
    _ | op `elem` [OpAdd, OpSub, OpMul, OpDiv, OpMod, OpPow] -> do
      return OwnershipInfo
        { ownsMemory = True
        , canMove = True
        , refCount = Just 1
        , escapes = max (escapes leftOwnership) (escapes rightOwnership)
        , memLocation = Stack  -- Arithmetic results are typically stack values
        }
    -- Logical operations create temporary boolean values
    _ | op `elem` [OpAnd, OpOr] -> do
      return OwnershipInfo
        { ownsMemory = True
        , canMove = True
        , refCount = Just 1
        , escapes = NoEscape
        , memLocation = Stack
        }
    -- Default case
    _ -> return leftOwnership

analyzeExpression (CEUnaryOp op operand) = do
  operandOwnership <- analyzeExpression (locatedValue operand)
  case op of
    OpNot -> return OwnershipInfo
      { ownsMemory = True
      , canMove = True
      , refCount = Just 1
      , escapes = NoEscape
      , memLocation = Stack
      }
    OpNegate -> return operandOwnership { canMove = True }
    _ -> return operandOwnership

analyzeExpression (CEComparison _ left right) = do
  _ <- analyzeExpression (locatedValue left)
  _ <- analyzeExpression (locatedValue right)
  -- Comparisons always produce stack-allocated boolean values
  return OwnershipInfo
    { ownsMemory = True
    , canMove = True
    , refCount = Just 1
    , escapes = NoEscape
    , memLocation = Stack
    }

analyzeExpression (CECall func args) = do
  _funcOwnership <- analyzeExpression (locatedValue func)
  _argOwnerships <- mapM (analyzeExpression . locatedValue) args
  
  -- Function calls typically return new owned values
  -- The escape behavior depends on what the function does
  context <- ask
  let resultEscape = if ocInConstructor context then NoEscape else EscapeToHeap
  
  return OwnershipInfo
    { ownsMemory = True
    , canMove = True
    , refCount = Just 1
    , escapes = resultEscape
    , memLocation = if resultEscape == NoEscape then Stack else Heap
    }

analyzeExpression (CEIndex container index) = do
  containerOwnership <- analyzeExpression (locatedValue container)
  _ <- analyzeExpression (locatedValue index)
  
  -- Indexing typically returns a reference/borrow of the container element
  return OwnershipInfo
    { ownsMemory = False  -- References don't own memory
    , canMove = False     -- References can't be moved
    , refCount = Nothing  -- References don't have ref counts
    , escapes = escapes containerOwnership
    , memLocation = memLocation containerOwnership
    }

analyzeExpression (CESlice container start end) = do
  containerOwnership <- analyzeExpression (locatedValue container)
  -- Analyze slice bounds
  case start of
    Just startExpr -> void $ analyzeExpression (locatedValue startExpr)
    Nothing -> return ()
  case end of
    Just endExpr -> void $ analyzeExpression (locatedValue endExpr)
    Nothing -> return ()
  
  -- Slicing creates a new view/container that may share ownership
  return OwnershipInfo
    { ownsMemory = True   -- Slice owns its own structure
    , canMove = True
    , refCount = Nothing  -- May share references to original data
    , escapes = escapes containerOwnership
    , memLocation = memLocation containerOwnership
    }

analyzeExpression (CEAttribute obj _) = do
  objOwnership <- analyzeExpression (locatedValue obj)
  -- Attribute access typically returns reference to member
  return OwnershipInfo
    { ownsMemory = False  -- Attribute references don't own memory
    , canMove = False
    , refCount = Nothing
    , escapes = escapes objOwnership
    , memLocation = memLocation objOwnership
    }

-- | Analyze function and track ownership patterns
analyzeFunction :: Identifier -> [Identifier] -> [CommonExpr] -> CommonExpr -> OwnershipInferenceM ()
analyzeFunction funcName params body returnExpr = do
  local (\ctx -> ctx { ocCurrentFunction = Just funcName, ocFunctionDepth = ocFunctionDepth ctx + 1 }) $ do
    -- Set initial ownership for parameters (typically borrowed references)
    mapM_ (\param -> setOwnershipInfo param paramOwnership) params
    
    -- Analyze function body
    mapM_ analyzeExpression body
    
    -- Analyze return expression
    returnOwnership <- analyzeExpression returnExpr
    
    -- If function returns a value, it typically transfers ownership
    when (ownsMemory returnOwnership) $ do
      modify $ \s -> s { oisOptimizationHints = "Consider return value optimization (RVO)" : oisOptimizationHints s }
  where
    paramOwnership = OwnershipInfo
      { ownsMemory = False  -- Parameters are typically borrowed
      , canMove = False
      , refCount = Nothing
      , escapes = NoEscape
      , memLocation = Stack
      }

-- | Infer optimal ownership strategy based on ownership info
inferOptimalStrategy :: OwnershipInfo -> OwnershipInferenceM OwnershipStrategy
inferOptimalStrategy ownership = do
  context <- ask
  case (memLocation ownership, escapes ownership, ownsMemory ownership) of
    (Stack, NoEscape, True) -> return StackOwned
    (Stack, NoEscape, False) -> return BorrowedReference
    (Stack, EscapeToReturn, True) -> return $ if ocInMoveContext context then MoveSemantics else CopySemantics
    (Stack, EscapeToReturn, False) -> return BorrowedReference
    (Stack, EscapeToHeap, _) -> return $ if ocInMoveContext context then MoveSemantics else CopySemantics
    (Stack, EscapeToGlobal, _) -> return CopySemantics
    (Stack, EscapeUnknown, _) -> return CopySemantics  -- Conservative: copy when escape is unknown
    (Heap, _, True) -> do
      -- Determine if shared or unique ownership
      refCountVal <- case refCount ownership of
        Just 1 -> return UniqueOwnership
        Just n | n > 1 -> return SharedOwnership
        Just _ -> return UniqueOwnership  -- Other cases
        Nothing -> return UniqueOwnership  -- Default to unique
      return refCountVal
    (Heap, _, False) -> return BorrowedReference
    (Global, _, _) -> return BorrowedReference
    (Unknown, _, _) -> return UniqueOwnership  -- Conservative choice
    _ -> return BorrowedReference

-- | Generate C++ type based on ownership strategy
generateCppType :: CommonExpr -> OwnershipInfo -> OwnershipStrategy -> OwnershipInferenceM Text
generateCppType expr _ownership strategy = do
  let baseType = inferBaseType expr
  case strategy of
    StackOwned -> return baseType
    UniqueOwnership -> return $ "std::unique_ptr<" <> baseType <> ">"
    SharedOwnership -> return $ "std::shared_ptr<" <> baseType <> ">"
    BorrowedReference -> return $ baseType <> "&"
    MoveSemantics -> return $ baseType <> "&&"
    CopySemantics -> return baseType
    WeakReference -> return $ "std::weak_ptr<" <> baseType <> ">"
    CustomRAII -> return $ "RAII_" <> baseType
  where
    inferBaseType :: CommonExpr -> Text
    inferBaseType (CELiteral (LInt _)) = "int32_t"
    inferBaseType (CELiteral (LFloat _)) = "double"
    inferBaseType (CELiteral (LString _)) = "std::string"
    inferBaseType (CELiteral (LBool _)) = "bool"
    inferBaseType _ = "auto"  -- Let C++ deduce the type

-- | Generate optimization hints based on ownership analysis
generateOptimizationHints :: CommonExpr -> OwnershipInfo -> OwnershipStrategy -> OwnershipInferenceM [Text]
generateOptimizationHints _expr ownership strategy = do
  let hints = []
  
  -- Suggest RAII when appropriate
  hints1 <- if ownsMemory ownership && memLocation ownership == Stack
    then return ("Use RAII for automatic resource management" : hints)
    else return hints
  
  -- Suggest move semantics for owned heap objects
  hints2 <- if ownsMemory ownership && memLocation ownership == Heap && canMove ownership
    then return ("Consider std::move to avoid unnecessary copies" : hints1)
    else return hints1
  
  -- Suggest const references for borrowed objects
  hints3 <- if not (ownsMemory ownership) && not (canMove ownership)
    then return ("Use const reference to avoid copies" : hints2)
    else return hints2
  
  -- Strategy-specific hints
  hints4 <- case strategy of
    UniqueOwnership -> return ("Consider std::make_unique for exception safety" : hints3)
    SharedOwnership -> return ("Consider std::make_shared for better performance" : hints3)
    MoveSemantics -> return ("Use std::forward for perfect forwarding" : hints3)
    _ -> return hints3
  
  return hints4

-- | Generate C++ memory management code
generateCppMemoryManagement :: OwnershipStrategy -> Text -> Text -> Text
generateCppMemoryManagement strategy varName typeName = case strategy of
  StackOwned -> 
    typeName <> " " <> varName <> ";"
  UniqueOwnership -> 
    "auto " <> varName <> " = std::make_unique<" <> typeName <> ">();"
  SharedOwnership -> 
    "auto " <> varName <> " = std::make_shared<" <> typeName <> ">();"
  BorrowedReference -> 
    "const " <> typeName <> "& " <> varName <> ";"
  MoveSemantics -> 
    typeName <> " " <> varName <> " = std::move(source);"
  CopySemantics -> 
    typeName <> " " <> varName <> " = source;"
  WeakReference -> 
    "std::weak_ptr<" <> typeName <> "> " <> varName <> ";"
  CustomRAII -> 
    "RAII_" <> typeName <> " " <> varName <> ";"

-- | Optimize ownership based on analysis results
optimizeOwnership :: [CommonExpr] -> OwnershipInferenceM [(CommonExpr, OwnershipStrategy, Text)]
optimizeOwnership exprs = do
  results <- mapM inferOwnership exprs
  return [(orExpression r, orStrategy r, orCppType r) | r <- results]

-- | Analyze ownership patterns across multiple expressions
analyzeOwnership :: [CommonExpr] -> OwnershipInferenceM [OwnershipResult]
analyzeOwnership exprs = mapM inferOwnership exprs

-- | Helper function to determine result memory location
determineResultLocation :: MemoryLocation -> MemoryLocation -> MemoryLocation
determineResultLocation Stack Stack = Stack
determineResultLocation Heap _ = Heap
determineResultLocation _ Heap = Heap
determineResultLocation Global _ = Global
determineResultLocation _ Global = Global
determineResultLocation Unknown _ = Unknown
determineResultLocation _ Unknown = Unknown
