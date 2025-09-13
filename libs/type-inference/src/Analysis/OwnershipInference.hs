{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Fluxus.Analysis.OwnershipInference where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad (void, when, unless, foldM)
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)

-- AST Types (included for completeness)
type Identifier = Text
data Located a = Located { locatedSpan :: !SourceSpan, locatedValue :: !a }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

data SourceSpan = SourceSpan !Int !Int !Int !Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

data Type = TInt | TFloat | TString | TBool | TArray Type | TFunction [Type] Type | TCustom Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

data Literal = LInt !Int | LFloat !Double | LString !Text | LBool !Bool
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

data BinaryOp = OpAdd | OpSub | OpMul | OpDiv | OpMod | OpPow | OpConcat | OpAnd | OpOr | OpAssign
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

data UnaryOp = OpNot | OpNegate | OpDeref | OpRef
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

data ComparisonOp = CompEq | CompNeq | CompLt | CompGt | CompLte | CompGte
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

data CommonExpr
  = CELiteral !Literal
  | CEVar !Identifier
  | CEBinaryOp !BinaryOp !(Located CommonExpr) !(Located CommonExpr)
  | CEUnaryOp !UnaryOp !(Located CommonExpr)
  | CEComparison !ComparisonOp !(Located CommonExpr) !(Located CommonExpr)
  | CECall !(Located CommonExpr) ![Located CommonExpr]
  | CEIndex !(Located CommonExpr) !(Located CommonExpr)
  | CESlice !(Located CommonExpr) !(Maybe (Located CommonExpr)) !(Maybe (Located CommonExpr))
  | CEAttribute !(Located CommonExpr) !Identifier
  | CEAssign !Identifier !(Located CommonExpr)
  | CELet !Identifier !(Located CommonExpr) !(Located CommonExpr)
  | CEBlock ![Located CommonExpr]
  | CEIf !(Located CommonExpr) !(Located CommonExpr) !(Maybe (Located CommonExpr))
  | CEReturn !(Located CommonExpr)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- Ownership Types
data MemoryLocation = Stack | Heap | Global | Unknown
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

data EscapeAnalysis = NoEscape | EscapeToReturn | EscapeToHeap | EscapeToGlobal
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

data OwnershipInfo = OwnershipInfo
  { ownsMemory :: !Bool
  , canMove :: !Bool
  , refCount :: !(Maybe Int)
  , escapes :: !EscapeAnalysis
  , memLocation :: !MemoryLocation
  , isValid :: !Bool  -- Track if variable is still valid (not moved)
  , borrowedFrom :: !(Maybe Identifier)  -- Track borrowing relationships
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data OwnershipStrategy
  = StackOwned
  | UniqueOwnership
  | SharedOwnership
  | BorrowedReference
  | MoveSemantics
  | CopySemantics
  | WeakReference
  | CustomRAII
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

-- Function Signature for ownership analysis
data FunctionSummary = FunctionSummary
  { fsParameters :: ![OwnershipInfo]
  , fsReturnOwnership :: !OwnershipInfo
  , fsMovesParameters :: ![Bool]  -- Which parameters are moved
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- Context and State
data OwnershipContext = OwnershipContext
  { ocCurrentFunction :: !(Maybe Identifier)
  , ocFunctionDepth :: !Int
  , ocInConstructor :: !Bool
  , ocInDestructor :: !Bool
  , ocInMoveContext :: !Bool
  , ocOptimizeForSpeed :: !Bool
  , ocFunctionSummaries :: !(HashMap Identifier FunctionSummary)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data OwnershipInferenceState = OwnershipInferenceState
  { oisOwnershipMap :: !(HashMap Identifier OwnershipInfo)
  , oisStrategyMap :: !(HashMap Identifier OwnershipStrategy)
  , oisDependencyGraph :: !(HashMap Identifier (Set Identifier))
  , oisLifetimeConstraints :: ![(Identifier, Identifier)]
  , oisCppMappings :: !(HashMap Type Text)
  , oisOptimizationHints :: ![Text]
  , oisSharedReferences :: !(HashMap Identifier Int)  -- Track reference counts
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

data OwnershipResult = OwnershipResult
  { orExpression :: !CommonExpr
  , orOwnership :: !OwnershipInfo
  , orStrategy :: !OwnershipStrategy
  , orCppType :: !Text
  , orOptimizations :: ![Text]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

type OwnershipInferenceM = ReaderT OwnershipContext (StateT OwnershipInferenceState (Except Text))

-- Initial states
initialContext :: OwnershipContext
initialContext = OwnershipContext
  { ocCurrentFunction = Nothing
  , ocFunctionDepth = 0
  , ocInConstructor = False
  , ocInDestructor = False
  , ocInMoveContext = False
  , ocOptimizeForSpeed = True
  , ocFunctionSummaries = HashMap.fromList
      [ ("clone", FunctionSummary [borrowedRef] ownedHeapValue [False])
      , ("borrow", FunctionSummary [borrowedRef] borrowedRef [False])
      , ("move", FunctionSummary [ownedValue] ownedValue [True])
      , ("create_unique_object", FunctionSummary [] ownedHeapValue [])
      ]
  }
  where
    borrowedRef = OwnershipInfo False False Nothing NoEscape Stack True Nothing
    ownedValue = OwnershipInfo True True (Just 1) NoEscape Stack True Nothing
    ownedHeapValue = OwnershipInfo True True (Just 1) NoEscape Heap True Nothing

initialState :: OwnershipInferenceState
initialState = OwnershipInferenceState
  { oisOwnershipMap = HashMap.empty
  , oisStrategyMap = HashMap.empty
  , oisDependencyGraph = HashMap.empty
  , oisLifetimeConstraints = []
  , oisCppMappings = HashMap.empty
  , oisOptimizationHints = []
  , oisSharedReferences = HashMap.empty
  }

-- Core functions
runOwnershipInference :: OwnershipInferenceM a -> Either Text (a, OwnershipInferenceState)
runOwnershipInference m = runExcept $ runStateT (runReaderT m initialContext) initialState

-- Get ownership info with proper error handling
getOwnershipInfo :: Identifier -> OwnershipInferenceM OwnershipInfo
getOwnershipInfo var = do
  ownershipMap <- gets oisOwnershipMap
  case HashMap.lookup var ownershipMap of
    Just info -> return info
    Nothing -> throwError $ "Undefined variable: " <> var

-- Set ownership info and update reference counts
setOwnershipInfo :: Identifier -> OwnershipInfo -> OwnershipInferenceM ()
setOwnershipInfo var ownership = do
  -- Update ownership map
  modify $ \s -> s { oisOwnershipMap = HashMap.insert var ownership (oisOwnershipMap s) }
  
  -- Update shared references count
  when (isJust (refCount ownership)) $ do
    let count = fromMaybe 1 (refCount ownership)
    modify $ \s -> s { oisSharedReferences = HashMap.insert var count (oisSharedReferences s) }
  
  -- Infer and set strategy
  strategy <- inferOptimalStrategy ownership
  modify $ \s -> s { oisStrategyMap = HashMap.insert var strategy (oisStrategyMap s) }

-- Add ownership dependency
addOwnershipDependency :: Identifier -> Identifier -> OwnershipInferenceM ()
addOwnershipDependency from to = do
  modify $ \s -> s { 
    oisDependencyGraph = HashMap.insertWith Set.union from (Set.singleton to) (oisDependencyGraph s) 
  }

-- Add lifetime constraint
addLifetimeConstraint :: Identifier -> Identifier -> OwnershipInferenceM ()
addLifetimeConstraint outlives depends = do
  modify $ \s -> s { oisLifetimeConstraints = (outlives, depends) : oisLifetimeConstraints s }
  -- Also add to dependency graph
  addOwnershipDependency outlives depends

-- Invalidate variable after move
invalidateVariable :: Identifier -> OwnershipInferenceM ()
invalidateVariable var = do
  info <- getOwnershipInfo var
  setOwnershipInfo var (info { isValid = False, canMove = False })

-- Check if variable is valid
checkVariableValid :: Identifier -> OwnershipInferenceM ()
checkVariableValid var = do
  info <- getOwnershipInfo var
  unless (isValid info) $
    throwError $ "Use of moved value: " <> var

-- Increment reference count
incrementRefCount :: Identifier -> OwnershipInferenceM ()
incrementRefCount var = do
  sharedRefs <- gets oisSharedReferences
  let count = HashMap.lookupDefault 0 var sharedRefs + 1
  modify $ \s -> s { oisSharedReferences = HashMap.insert var count sharedRefs }
  
  -- Update ownership info if count > 1
  when (count > 1) $ do
    info <- getOwnershipInfo var
    setOwnershipInfo var (info { refCount = Just count })

-- Main analysis function with flow sensitivity
analyzeExpression :: CommonExpr -> OwnershipInferenceM OwnershipInfo
analyzeExpression (CELiteral lit) = return $ literalOwnership lit
  where
    literalOwnership (LInt _) = stackOwned
    literalOwnership (LFloat _) = stackOwned
    literalOwnership (LString _) = heapOwned  -- Strings are typically heap-allocated
    literalOwnership (LBool _) = stackOwned
    
    stackOwned = OwnershipInfo True True (Just 1) NoEscape Stack True Nothing
    heapOwned = OwnershipInfo True True (Just 1) NoEscape Heap True Nothing

analyzeExpression (CEVar var) = do
  checkVariableValid var
  getOwnershipInfo var

analyzeExpression (CEAssign var expr) = do
  -- Analyze right-hand side
  rhsOwnership <- analyzeExpression (locatedValue expr)
  
  -- Check if we're moving or copying
  if ownsMemory rhsOwnership && canMove rhsOwnership
    then do
      -- Move semantics
      setOwnershipInfo var rhsOwnership
      -- Invalidate source if it's a variable
      case locatedValue expr of
        CEVar sourceVar -> invalidateVariable sourceVar
        _ -> return ()
    else do
      -- Copy semantics or borrowing
      if ownsMemory rhsOwnership
        then do
          -- Deep copy
          setOwnershipInfo var (rhsOwnership { refCount = Just 1 })
        else do
          -- Borrowing
          case locatedValue expr of
            CEVar sourceVar -> do
              setOwnershipInfo var (rhsOwnership { borrowedFrom = Just sourceVar })
              addLifetimeConstraint sourceVar var
              incrementRefCount sourceVar
            _ -> setOwnershipInfo var rhsOwnership
  
  return rhsOwnership

analyzeExpression (CELet var bindExpr body) = do
  -- Analyze binding
  bindOwnership <- analyzeExpression (locatedValue bindExpr)
  setOwnershipInfo var bindOwnership
  
  -- Handle move semantics for let bindings
  case locatedValue bindExpr of
    CEVar sourceVar | ownsMemory bindOwnership && canMove bindOwnership -> 
      invalidateVariable sourceVar
    _ -> return ()
  
  -- Analyze body with new binding
  analyzeExpression (locatedValue body)

analyzeExpression (CEBlock exprs) = do
  -- Create a new scope
  originalState <- get
  
  -- Analyze each expression in sequence (flow-sensitive)
  results <- mapM (analyzeExpression . locatedValue) exprs
  
  -- Clean up local variables (simplified - in real implementation would track scope)
  -- Return ownership of last expression or void
  case results of
    [] -> return voidOwnership
    _ -> return (last results)
  where
    voidOwnership = OwnershipInfo False False Nothing NoEscape Stack True Nothing

analyzeExpression (CEBinaryOp op left right) = do
  leftOwnership <- analyzeExpression (locatedValue left)
  rightOwnership <- analyzeExpression (locatedValue right)
  
  case op of
    OpAssign -> throwError "Assignment should use CEAssign"
    OpConcat -> return $ OwnershipInfo 
      { ownsMemory = True
      , canMove = True
      , refCount = Just 1
      , escapes = max (escapes leftOwnership) (escapes rightOwnership)
      , memLocation = Heap  -- String concat typically heap allocates
      , isValid = True
      , borrowedFrom = Nothing
      }
    _ | op `elem` [OpAdd, OpSub, OpMul, OpDiv, OpMod, OpPow] ->
      return $ OwnershipInfo
        { ownsMemory = True
        , canMove = True
        , refCount = Just 1
        , escapes = NoEscape
        , memLocation = Stack
        , isValid = True
        , borrowedFrom = Nothing
        }
    _ -> return $ OwnershipInfo
      { ownsMemory = True
      , canMove = True
      , refCount = Just 1
      , escapes = NoEscape
      , memLocation = Stack
      , isValid = True
      , borrowedFrom = Nothing
      }

analyzeExpression (CEUnaryOp op operand) = do
  operandOwnership <- analyzeExpression (locatedValue operand)
  case op of
    OpRef -> do
      -- Taking a reference
      case locatedValue operand of
        CEVar var -> do
          incrementRefCount var
          return $ OwnershipInfo
            { ownsMemory = False
            , canMove = False
            , refCount = Nothing
            , escapes = escapes operandOwnership
            , memLocation = memLocation operandOwnership
            , isValid = True
            , borrowedFrom = Just var
            }
        _ -> return $ operandOwnership { ownsMemory = False, canMove = False }
    OpDeref -> do
      -- Dereferencing
      when (ownsMemory operandOwnership) $
        throwError "Cannot dereference owned value"
      return $ operandOwnership { borrowedFrom = Nothing }
    _ -> return operandOwnership

analyzeExpression (CECall func args) = do
  funcOwnership <- analyzeExpression (locatedValue func)
  argOwnerships <- mapM (analyzeExpression . locatedValue) args
  
  -- Look up function summary
  context <- ask
  case locatedValue func of
    CEVar funcName -> do
      case HashMap.lookup funcName (ocFunctionSummaries context) of
        Just summary -> do
          -- Apply function summary
          -- Check parameter moves
          sequence_ $ zipWith (handleParamMove funcName) [0..] (zip args (fsMovesParameters summary))
          return (fsReturnOwnership summary)
        Nothing -> do
          -- Unknown function - conservative defaults
          return $ OwnershipInfo True True (Just 1) EscapeToHeap Heap True Nothing
    _ -> return $ OwnershipInfo True True (Just 1) EscapeToHeap Heap True Nothing
  where
    handleParamMove funcName idx (arg, moves) = when moves $ do
      case locatedValue arg of
        CEVar var -> invalidateVariable var
        _ -> return ()

analyzeExpression (CEIndex container index) = do
  containerOwnership <- analyzeExpression (locatedValue container)
  _ <- analyzeExpression (locatedValue index)
  
  case locatedValue container of
    CEVar var -> do
      addLifetimeConstraint var ("index_result_" <> var)
      return $ OwnershipInfo
        { ownsMemory = False
        , canMove = False
        , refCount = Nothing
        , escapes = escapes containerOwnership
        , memLocation = memLocation containerOwnership
        , isValid = True
        , borrowedFrom = Just var
        }
    _ -> return $ containerOwnership { ownsMemory = False, canMove = False }

analyzeExpression (CESlice container start end) = do
  containerOwnership <- analyzeExpression (locatedValue container)
  
  -- Analyze bounds
  mapM_ (analyzeExpression . locatedValue) (maybeToList start)
  mapM_ (analyzeExpression . locatedValue) (maybeToList end)
  
  -- Slices are views (borrows) in this implementation
  case locatedValue container of
    CEVar var -> do
      incrementRefCount var
      addLifetimeConstraint var ("slice_result_" <> var)
      return $ OwnershipInfo
        { ownsMemory = False  -- View, not owner
        , canMove = False
        , refCount = Nothing
        , escapes = escapes containerOwnership
        , memLocation = memLocation containerOwnership
        , isValid = True
        , borrowedFrom = Just var
        }
    _ -> return $ containerOwnership { ownsMemory = False, canMove = False }
  where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

analyzeExpression (CEAttribute obj attr) = do
  objOwnership <- analyzeExpression (locatedValue obj)
  case locatedValue obj of
    CEVar var -> do
      addLifetimeConstraint var (var <> "." <> attr)
      return $ OwnershipInfo
        { ownsMemory = False
        , canMove = False
        , refCount = Nothing
        , escapes = escapes objOwnership
        , memLocation = memLocation objOwnership
        , isValid = True
        , borrowedFrom = Just var
        }
    _ -> return $ objOwnership { ownsMemory = False, canMove = False }

analyzeExpression (CEComparison _ left right) = do
  _ <- analyzeExpression (locatedValue left)
  _ <- analyzeExpression (locatedValue right)
  return $ OwnershipInfo True True (Just 1) NoEscape Stack True Nothing

analyzeExpression (CEIf cond thenBranch elseBranch) = do
  _ <- analyzeExpression (locatedValue cond)
  
  -- Save current state
  stateBefore <- get
  
  -- Analyze then branch
  thenOwnership <- analyzeExpression (locatedValue thenBranch)
  stateAfterThen <- get
  
  -- Restore state for else branch
  put stateBefore
  elseOwnership <- case elseBranch of
    Just e -> analyzeExpression (locatedValue e)
    Nothing -> return voidOwnership
  stateAfterElse <- get
  
  -- Merge states (simplified - real implementation would be more sophisticated)
  let mergedState = mergeStates stateAfterThen stateAfterElse
  put mergedState
  
  -- Return unified ownership
  return $ unifyOwnership thenOwnership elseOwnership
  where
    voidOwnership = OwnershipInfo False False Nothing NoEscape Stack True Nothing
    mergeStates s1 s2 = s1  -- Simplified
    unifyOwnership o1 o2 = o1  -- Simplified

analyzeExpression (CEReturn expr) = do
  ownership <- analyzeExpression (locatedValue expr)
  return $ ownership { escapes = EscapeToReturn }

-- Analyze function with flow sensitivity
analyzeFunction :: Identifier -> [Identifier] -> [Located CommonExpr] -> OwnershipInferenceM FunctionSummary
analyzeFunction funcName params body = do
  local (\ctx -> ctx { ocCurrentFunction = Just funcName, ocFunctionDepth = ocFunctionDepth ctx + 1 }) $ do
    -- Set parameter ownership
    let paramOwnership = OwnershipInfo False False Nothing NoEscape Stack True Nothing
    mapM_ (\p -> setOwnershipInfo p paramOwnership) params
    
    -- Analyze body with flow sensitivity
    bodyOwnerships <- mapM (analyzeExpression . locatedValue) body
    
    -- Determine return ownership
    let returnOwnership = case bodyOwnerships of
          [] -> OwnershipInfo False False Nothing NoEscape Stack True Nothing
          _ -> last bodyOwnerships
    
    -- Check which parameters were moved
    movedParams <- mapM checkParamMoved params
    
    return $ FunctionSummary 
      { fsParameters = replicate (length params) paramOwnership
      , fsReturnOwnership = returnOwnership
      , fsMovesParameters = movedParams
      }
  where
    checkParamMoved param = do
      info <- getOwnershipInfo param
      return (not (isValid info))

-- Infer optimal strategy
inferOptimalStrategy :: OwnershipInfo -> OwnershipInferenceM OwnershipStrategy
inferOptimalStrategy OwnershipInfo{..} = do
  context <- ask
  sharedRefs <- gets oisSharedReferences
  
  -- Check actual reference count from shared references map
  let actualRefCount = case borrowedFrom of
        Just var -> HashMap.lookupDefault 1 var sharedRefs
        Nothing -> fromMaybe 1 refCount
  
  case (memLocation, escapes, ownsMemory, actualRefCount > 1) of
    (Stack, NoEscape, True, False) -> return StackOwned
    (Stack, EscapeToReturn, True, False) -> 
      return $ if ocInMoveContext context then MoveSemantics else CopySemantics
    (Heap, _, True, True) -> return SharedOwnership  -- Multiple references
    (Heap, _, True, False) -> return UniqueOwnership  -- Single owner
    (_, _, False, _) -> return BorrowedReference
    _ -> return UniqueOwnership

-- Check lifetime constraints
checkLifetimeConstraints :: OwnershipInferenceM ()
checkLifetimeConstraints = do
  constraints <- gets oisLifetimeConstraints
  ownershipMap <- gets oisOwnershipMap
  
  forM_ constraints $ KATEX_INLINE_OPENoutlives, depends) -> do
    case (HashMap.lookup outlives ownershipMap, HashMap.lookup depends ownershipMap) of
      (Just outlivesInfo, Just dependsInfo) -> do
        when (not (isValid outlivesInfo) && isValid dependsInfo) $
          throwError $ "Lifetime error: " <> depends <> " outlives " <> outlives
      _ -> return ()

-- Generate C++ type
generateCppType :: CommonExpr -> OwnershipInfo -> OwnershipStrategy -> OwnershipInferenceM Text
generateCppType expr ownership strategy = do
  let baseType = inferBaseType expr
  case strategy of
    StackOwned -> return baseType
    UniqueOwnership -> return $ "std::unique_ptr<" <> baseType <> ">"
    SharedOwnership -> return $ "std::shared_ptr<" <> baseType <> ">"
    BorrowedReference -> 
      if canMove ownership 
        then return $ baseType <> "&"
        else return $ "const " <> baseType <> "&"
    MoveSemantics -> return $ baseType <> "&&"
    CopySemantics -> return baseType
    WeakReference -> return $ "std::weak_ptr<" <> baseType <> ">"
    CustomRAII -> return $ "RAII_" <> baseType
  where
    inferBaseType (CELiteral (LInt _)) = "int64_t"
    inferBaseType (CELiteral (LFloat _)) = "double"
    inferBaseType (CELiteral (LString _)) = "std::string"
    inferBaseType (CELiteral (LBool _)) = "bool"
    inferBaseType _ = "auto"

-- Generate optimization hints
generateOptimizationHints :: CommonExpr -> OwnershipInfo -> OwnershipStrategy -> OwnershipInferenceM [Text]
generateOptimizationHints expr ownership strategy = do
  hints <- case strategy of
    UniqueOwnership -> return ["Use std::make_unique for exception safety"]
    SharedOwnership -> return ["Use std::make_shared for single allocation"]
    MoveSemantics -> return ["Consider std::forward for perfect forwarding"]
    StackOwned | escapes ownership == NoEscape -> 
      return ["Stack allocation is optimal for non-escaping values"]
    _ -> return []
  
  extraHints <- if ownsMemory ownership && canMove ownership
    then return ["Consider std::move to transfer ownership"]
    else return []
  
  return (hints ++ extraHints)

-- Generate C++ memory management code
generateCppMemoryManagement :: OwnershipStrategy -> Text -> Text -> Text
generateCppMemoryManagement strategy varName typeName = case strategy of
  StackOwned -> typeName <> " " <> varName <> ";"
  UniqueOwnership -> "auto " <> varName <> " = std::make_unique<" <> typeName <> ">();"
  SharedOwnership -> "auto " <> varName <> " = std::make_shared<" <> typeName <> ">();"
  BorrowedReference -> "const " <> typeName <> "& " <> varName <> " = source;"
  MoveSemantics -> typeName <> " " <> varName <> " = std::move(source);"
  CopySemantics -> typeName <> " " <> varName <> " = source;"
  WeakReference -> "std::weak_ptr<" <> typeName <> "> " <> varName <> " = shared_source;"
  CustomRAII -> "RAII_" <> typeName <> " " <> varName <> ";"

-- Main entry points
inferOwnership :: CommonExpr -> OwnershipInferenceM OwnershipResult
inferOwnership expr = do
  ownership <- analyzeExpression expr
  strategy <- inferOptimalStrategy ownership
  cppType <- generateCppType expr ownership strategy
  opts <- generateOptimizationHints expr ownership strategy
  return $ OwnershipResult expr ownership strategy cppType opts

analyzeOwnership :: [CommonExpr] -> OwnershipInferenceM [OwnershipResult]
analyzeOwnership exprs = do
  results <- mapM inferOwnership exprs
  checkLifetimeConstraints  -- Check constraints after analysis
  return results

optimizeOwnership :: [CommonExpr] -> OwnershipInferenceM [(CommonExpr, OwnershipStrategy, Text)]
optimizeOwnership exprs = do
  results <- analyzeOwnership exprs
  return [(orExpression r, orStrategy r, orCppType r) | r <- results]