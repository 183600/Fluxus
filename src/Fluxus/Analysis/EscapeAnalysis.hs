{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Fluxus.Analysis.EscapeAnalysis
  ( EscapeAnalysisM
  , EscapeAnalysisState(..)
  , EscapeContext(..)
  , EscapeResult(..)
  , runEscapeAnalysis
  , analyzeEscape
  , analyzeExpression
  , analyzeFunction
  , getEscapeInfo
  , markEscape
  , isStackAllocatable
  , optimizeMemoryAllocation
  ) where

import Fluxus.AST.Common
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

type EscapeAnalysisM = ReaderT EscapeContext (State EscapeAnalysisState)

-- | Context for escape analysis
data EscapeContext = EscapeContext
  { ecFunctionDepth :: !Int              -- Current function nesting level
  , ecInReturn :: !Bool                  -- Whether we're analyzing a return statement
  , ecInGlobalAssignment :: !Bool        -- Whether we're analyzing global assignment
  , ecCurrentFunction :: !(Maybe Identifier)  -- Current function being analyzed
  , ecIsConstructor :: !Bool             -- Whether current function is a constructor
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | State for escape analysis
data EscapeAnalysisState = EscapeAnalysisState
  { easEscapeMap :: !(HashMap Identifier EscapeInfo)    -- Variable escape information
  , easAllocationSites :: !(HashMap Identifier MemoryLocation)  -- Where variables are allocated
  , easEscapeGraph :: !(HashMap Identifier (Set Identifier))    -- Escape dependency graph
  , easReturnEscapes :: !(Set Identifier)              -- Variables that escape via return
  , easGlobalEscapes :: !(Set Identifier)              -- Variables that escape to global scope
  , easHeapEscapes :: !(Set Identifier)                -- Variables that escape to heap
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- | Result of escape analysis
data EscapeResult = EscapeResult
  { erExpression :: !CommonExpr
  , erEscapeInfo :: !EscapeInfo
  , erMemoryLocation :: !MemoryLocation
  , erOptimizations :: ![Text]           -- Suggested optimizations
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Initial escape context
initialContext :: EscapeContext
initialContext = EscapeContext
  { ecFunctionDepth = 0
  , ecInReturn = False
  , ecInGlobalAssignment = False
  , ecCurrentFunction = Nothing
  , ecIsConstructor = False
  }

-- | Initial escape analysis state
initialState :: EscapeAnalysisState
initialState = EscapeAnalysisState
  { easEscapeMap = HashMap.empty
  , easAllocationSites = HashMap.empty
  , easEscapeGraph = HashMap.empty
  , easReturnEscapes = Set.empty
  , easGlobalEscapes = Set.empty
  , easHeapEscapes = Set.empty
  }

-- | Run escape analysis
runEscapeAnalysis :: EscapeAnalysisM a -> (a, EscapeAnalysisState)
runEscapeAnalysis m = runState (runReaderT m initialContext) initialState

-- | Mark a variable as escaping with specific escape information
markEscape :: Identifier -> EscapeInfo -> EscapeAnalysisM ()
markEscape var escapeInfo = do
  modify $ \s -> s { easEscapeMap = HashMap.insert var escapeInfo (easEscapeMap s) }
  case escapeInfo of
    EscapeToReturn -> modify $ \s -> s { easReturnEscapes = Set.insert var (easReturnEscapes s) }
    EscapeToGlobal -> modify $ \s -> s { easGlobalEscapes = Set.insert var (easGlobalEscapes s) }
    EscapeToHeap -> modify $ \s -> s { easHeapEscapes = Set.insert var (easHeapEscapes s) }
    _ -> return ()

-- | Get escape information for a variable
getEscapeInfo :: Identifier -> EscapeAnalysisM EscapeInfo
getEscapeInfo var = do
  escapeMap <- gets easEscapeMap
  return $ HashMap.lookupDefault NoEscape var escapeMap

-- | Add escape dependency between variables
_addEscapeDependency :: Identifier -> Identifier -> EscapeAnalysisM ()
_addEscapeDependency from to = do
  modify $ \s -> s { easEscapeGraph = HashMap.insertWith Set.union from (Set.singleton to) (easEscapeGraph s) }

-- | Propagate escape information through dependency graph
propagateEscapes :: EscapeAnalysisM ()
propagateEscapes = do
  graph <- gets easEscapeGraph
  escapeMap <- gets easEscapeMap
  let propagated = propagateEscapeInfo graph escapeMap
  modify $ \s -> s { easEscapeMap = propagated }
  where
    propagateEscapeInfo :: HashMap Identifier (Set Identifier) -> HashMap Identifier EscapeInfo -> HashMap Identifier EscapeInfo
    propagateEscapeInfo graph escapesMap = 
      let updated = HashMap.mapWithKey (propagateForVar graph escapesMap) escapesMap
      in if updated == escapesMap then escapesMap else propagateEscapeInfo graph updated
    
    propagateForVar :: HashMap Identifier (Set Identifier) -> HashMap Identifier EscapeInfo -> Identifier -> EscapeInfo -> EscapeInfo
    propagateForVar graph escapesMap var currentEscape =
      case HashMap.lookup var graph of
        Nothing -> currentEscape
        Just deps -> 
          let depEscapes = map (\dep -> HashMap.lookupDefault NoEscape dep escapesMap) (Set.toList deps)
              maxEscape = maximum (currentEscape : depEscapes)
          in maxEscape

-- | Main escape analysis function for expressions
analyzeEscape :: CommonExpr -> EscapeAnalysisM EscapeResult
analyzeEscape expr = do
  escapeInfo <- analyzeExpression expr
  memLoc <- determineMemoryLocation expr escapeInfo
  opts <- suggestOptimizations expr escapeInfo memLoc
  return $ EscapeResult expr escapeInfo memLoc opts

-- | Analyze expression and return escape information
analyzeExpression :: CommonExpr -> EscapeAnalysisM EscapeInfo
analyzeExpression (CELiteral _) = return NoEscape  -- Literals don't escape

analyzeExpression (CEVar var) = getEscapeInfo var

analyzeExpression (CEBinaryOp _op left right) = do
  leftEscape <- analyzeExpression (locatedValue left)
  rightEscape <- analyzeExpression (locatedValue right)
  return $ max leftEscape rightEscape

analyzeExpression (CEUnaryOp _ operand) = 
  analyzeExpression (locatedValue operand)

analyzeExpression (CEComparison _ left right) = do
  _ <- analyzeExpression (locatedValue left)
  _ <- analyzeExpression (locatedValue right)
  return NoEscape  -- Comparisons produce boolean values that don't escape

analyzeExpression (CECall func args) = do
  _funcEscape <- analyzeExpression (locatedValue func)
  _argEscapes <- mapM (analyzeExpression . locatedValue) args
  
  -- Function calls typically cause arguments to escape to the called function
  -- and potentially to the heap depending on what the function does
  context <- ask
  if ecInReturn context
    then return EscapeToReturn
    else if ecInGlobalAssignment context
      then return EscapeToGlobal
      else return EscapeToHeap  -- Conservative: assume function call causes heap escape

analyzeExpression (CEIndex container index) = do
  containerEscape <- analyzeExpression (locatedValue container)
  _ <- analyzeExpression (locatedValue index)
  -- Indexing inherits the escape behavior of the container
  return containerEscape

analyzeExpression (CESlice container start end) = do
  containerEscape <- analyzeExpression (locatedValue container)
  -- Analyze slice bounds
  case start of
    Just startExpr -> void $ analyzeExpression (locatedValue startExpr)
    Nothing -> return ()
  case end of
    Just endExpr -> void $ analyzeExpression (locatedValue endExpr)
    Nothing -> return ()
  -- Slicing typically creates a new object that inherits escape behavior
  return containerEscape

analyzeExpression (CEAttribute obj _) = do
  objEscape <- analyzeExpression (locatedValue obj)
  -- Attribute access inherits escape behavior of the object
  return objEscape

-- | Analyze function and track parameter escape behavior
analyzeFunction :: Identifier -> [Identifier] -> [CommonExpr] -> CommonExpr -> EscapeAnalysisM ()
analyzeFunction funcName params body returnExpr = do
  -- Enter function scope
  local (\ctx -> ctx { ecCurrentFunction = Just funcName, ecFunctionDepth = ecFunctionDepth ctx + 1 }) $ do
    -- Initially mark all parameters as no escape
    mapM_ (\param -> markEscape param NoEscape) params
    
    -- Analyze function body
    mapM_ analyzeExpression body
    
    -- Analyze return expression in return context
    local (\ctx -> ctx { ecInReturn = True }) $ do
      returnEscape <- analyzeExpression returnExpr
      -- Variables returned from function escape to caller
      case returnEscape of
        NoEscape -> return ()
        _ -> markReturnEscapes returnExpr
    
    -- Propagate escape information
    propagateEscapes
  where
    markReturnEscapes :: CommonExpr -> EscapeAnalysisM ()
    markReturnEscapes (CEVar var) = markEscape var EscapeToReturn
    markReturnEscapes (CEBinaryOp _ left right) = do
      markReturnEscapes (locatedValue left)
      markReturnEscapes (locatedValue right)
    markReturnEscapes (CEUnaryOp _ operand) = markReturnEscapes (locatedValue operand)
    markReturnEscapes (CECall _ args) = mapM_ (markReturnEscapes . locatedValue) args
    markReturnEscapes (CEIndex container _) = markReturnEscapes (locatedValue container)
    markReturnEscapes (CESlice container _ _) = markReturnEscapes (locatedValue container)
    markReturnEscapes (CEAttribute obj _) = markReturnEscapes (locatedValue obj)
    markReturnEscapes _ = return ()

-- | Determine optimal memory location based on escape analysis
determineMemoryLocation :: CommonExpr -> EscapeInfo -> EscapeAnalysisM MemoryLocation
determineMemoryLocation expr escapeInfo = do
  case escapeInfo of
    NoEscape -> return Stack           -- Can be stack allocated
    EscapeToReturn -> 
      case expr of
        CELiteral _ -> return Stack    -- Literals can be stack allocated even if returned
        _ -> return Stack              -- Return values can often be stack allocated with RVO
    EscapeToHeap -> return Heap        -- Must be heap allocated
    EscapeToGlobal -> return Global    -- Global allocation
    EscapeUnknown -> return Heap       -- Conservative: heap allocate if unknown

-- | Check if an expression can be stack allocated
isStackAllocatable :: CommonExpr -> EscapeAnalysisM Bool
isStackAllocatable expr = do
  escapeInfo <- analyzeExpression expr
  memLoc <- determineMemoryLocation expr escapeInfo
  return $ memLoc == Stack

-- | Suggest memory allocation optimizations
suggestOptimizations :: CommonExpr -> EscapeInfo -> MemoryLocation -> EscapeAnalysisM [Text]
suggestOptimizations _expr escapeInfo memLoc = do
  let opts = []
  
  -- Suggest stack allocation when possible
  opts1 <- if memLoc == Stack
    then return (T.pack "Use stack allocation (automatic storage duration)" : opts)
    else return opts
  
  -- Suggest RAII optimizations
  opts2 <- case escapeInfo of
    NoEscape -> return (T.pack "Use RAII with automatic destruction" : opts1)
    EscapeToReturn -> return (T.pack "Consider return value optimization (RVO)" : opts1)
    _ -> return opts1
  
  -- Suggest move semantics for heap-allocated objects
  opts3 <- if memLoc == Heap
    then return (T.pack "Use move semantics to avoid unnecessary copies" : opts2)
    else return opts2
  
  -- Suggest shared_ptr vs unique_ptr based on escape pattern
  opts4 <- case (memLoc, escapeInfo) of
    (Heap, EscapeToReturn) -> return (T.pack "Consider std::unique_ptr for single ownership" : opts3)
    (Heap, EscapeToHeap) -> return (T.pack "Consider std::shared_ptr for shared ownership" : opts3)
    _ -> return opts3
  
  return opts4

-- | Optimize memory allocation based on escape analysis results
optimizeMemoryAllocation :: CommonExpr -> EscapeAnalysisM (CommonExpr, [Text])
optimizeMemoryAllocation expr = do
  result <- analyzeEscape expr
  let optimizedExpr = applyOptimizations expr (erMemoryLocation result)
  return (optimizedExpr, erOptimizations result)
  where
    applyOptimizations :: CommonExpr -> MemoryLocation -> CommonExpr
    applyOptimizations e Stack = e  -- Keep as-is for stack allocation
    applyOptimizations e Heap = e   -- Could be transformed to use smart pointers
    applyOptimizations e Global = e -- Keep as-is for global allocation
    applyOptimizations e Unknown = e -- No optimization when unknown
