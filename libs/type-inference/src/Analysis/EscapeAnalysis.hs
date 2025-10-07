{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Fluxus.Analysis.EscapeAnalysis
  ( EscapeAnalysisM
  , EscapeAnalysisState(..)
  , EscapeContext(..)
  , EscapeResult(..)
  , ProgramAnalysis(..)
  , runEscapeAnalysis
  , analyzeProgramEscape
  , optimizeProgram
  ) where

import Fluxus.AST.Common
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad (foldM, when, forM, unless)
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, mapMaybe, isJust)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.List (foldl')

-- | Escape analysis monad
type EscapeAnalysisM = ReaderT EscapeContext (State EscapeAnalysisState)

-- | Function summary for interprocedural analysis
data FunctionSummary = FunctionSummary
  { fsParameters :: ![Identifier]                      -- Function parameters
  , fsParameterEscapes :: !(HashMap Int EscapeInfo)   -- Which parameters escape and how
  , fsReturnEscapes :: ![Int]                          -- Which parameters escape through return
  , fsCreatesEscaping :: !Bool                         -- Whether function creates escaping objects
  , fsCapturedVars :: !(HashSet Identifier)           -- Variables captured by closures
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Enhanced context for escape analysis
data EscapeContext = EscapeContext
  { ecFunctionDepth :: !Int                            -- Current function nesting level
  , ecCurrentFunction :: !(Maybe Identifier)           -- Current function being analyzed
  , ecCurrentBlock :: !Int                             -- Current block ID for flow-sensitive analysis
  , ecVariableScopes :: !(HashMap Identifier Int)     -- Variable to scope mapping
  , ecScopeStack :: ![Int]                            -- Stack of active scopes
  , ecControlFlow :: !ControlFlowContext              -- Current control flow context
  , ecAliasContext :: !AliasContext                   -- Alias tracking
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Control flow context
data ControlFlowContext = ControlFlowContext
  { cfcInReturn :: !Bool                               -- In return statement
  , cfcInLoop :: !Bool                                 -- Inside loop body
  , cfcInConditional :: !Bool                          -- Inside conditional branch
  , cfcLoopDepth :: !Int                              -- Nested loop depth
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Alias tracking context
data AliasContext = AliasContext
  { acAliases :: !(HashMap Identifier (HashSet Identifier))  -- Variable aliasing relationships
  , acPointsTo :: !(HashMap Identifier (HashSet Identifier)) -- Pointer analysis: what each var points to
  , acFieldAccess :: !(HashMap Identifier [(Identifier, Identifier)]) -- Object field access tracking
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Enhanced state for escape analysis
data EscapeAnalysisState = EscapeAnalysisState
  { easEscapeMap :: !(HashMap Identifier EscapeInfo)         -- Variable escape information
  , easEscapeGraph :: !(HashMap Identifier (Set Identifier)) -- Escape dependency graph  
  , easFunctionSummaries :: !(HashMap Identifier FunctionSummary) -- Function analysis summaries
  , easAllocationSites :: !(HashMap Identifier AllocationSite)    -- Allocation site information
  , easClosureCaptured :: !(HashMap Identifier (HashSet Identifier)) -- Closure captured variables
  , easFlowState :: !(HashMap (Int, Identifier) EscapeInfo)  -- Flow-sensitive escape info
  , easNextBlockId :: !Int                                   -- Next block ID for flow analysis
  , easOptimizationLog :: ![OptimizationEntry]              -- Log of applied optimizations
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- | Allocation site information
data AllocationSite = AllocationSite
  { asLocation :: !SourceLocation
  , asType :: !AllocationType
  , asSize :: !(Maybe Int)          -- Size if known statically
  , asEscapes :: !Bool              -- Whether allocation escapes
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Type of allocation
data AllocationType
  = NewObject                       -- Object construction
  | ArrayAlloc                      -- Array allocation
  | ClosureAlloc                    -- Closure allocation
  | StringAlloc                     -- String allocation
  deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Optimization log entry
data OptimizationEntry = OptimizationEntry
  { oeLocation :: !SourceLocation
  , oeOriginal :: !CommonExpr
  , oeOptimized :: !CommonExpr
  , oeReason :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Result of escape analysis
data EscapeResult = EscapeResult
  { erExpression :: !CommonExpr
  , erEscapeInfo :: !EscapeInfo
  , erMemoryLocation :: !MemoryLocation
  , erOptimizations :: ![Text]
  , erAliases :: !(HashSet Identifier)    -- Variables that alias this expression
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Program-level analysis result
data ProgramAnalysis = ProgramAnalysis
  { paFunctions :: !(HashMap Identifier FunctionSummary)
  , paGlobalEscapes :: !(HashSet Identifier)
  , paOptimizationOpportunities :: ![OptimizationOpportunity]
  , paStatistics :: !AnalysisStatistics
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- | Optimization opportunity
data OptimizationOpportunity = OptimizationOpportunity
  { ooLocation :: !SourceLocation
  , ooType :: !OptimizationType
  , ooDescription :: !Text
  , ooEstimatedBenefit :: !Int    -- Estimated performance improvement (0-100)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Type of optimization
data OptimizationType
  = StackAllocation               -- Convert heap to stack allocation
  | ElideCopy                     -- Eliminate unnecessary copy
  | InlineAllocation              -- Inline small allocations
  | ScalarReplacement             -- Replace aggregate with scalars
  | EscapeElision                 -- Remove allocation entirely
  deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Analysis statistics
data AnalysisStatistics = AnalysisStatistics
  { asAnalyzedFunctions :: !Int
  , asEscapingAllocations :: !Int
  , asStackAllocations :: !Int
  , asOptimizedAllocations :: !Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Statement type for analysis
data Statement
  = StmtExpr !(Located CommonExpr)
  | StmtAssign !Identifier !(Located CommonExpr)
  | StmtDeclare !Identifier !(Maybe (Located CommonExpr))
  | StmtReturn !(Maybe (Located CommonExpr))
  | StmtIf !(Located CommonExpr) ![Statement] !(Maybe [Statement])
  | StmtWhile !(Located CommonExpr) ![Statement]
  | StmtFor !Identifier !(Located CommonExpr) ![Statement]
  | StmtBlock ![Statement]
  deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Initial escape context
initialContext :: EscapeContext
initialContext = EscapeContext
  { ecFunctionDepth = 0
  , ecCurrentFunction = Nothing
  , ecCurrentBlock = 0
  , ecVariableScopes = HashMap.empty
  , ecScopeStack = [0]
  , ecControlFlow = ControlFlowContext False False False 0
  , ecAliasContext = AliasContext HashMap.empty HashMap.empty HashMap.empty
  }

-- | Initial escape analysis state
initialState :: EscapeAnalysisState
initialState = EscapeAnalysisState
  { easEscapeMap = HashMap.empty
  , easEscapeGraph = HashMap.empty
  , easFunctionSummaries = HashMap.empty
  , easAllocationSites = HashMap.empty
  , easClosureCaptured = HashMap.empty
  , easFlowState = HashMap.empty
  , easNextBlockId = 1
  , easOptimizationLog = []
  }

-- | Run escape analysis
runEscapeAnalysis :: EscapeAnalysisM a -> (a, EscapeAnalysisState)
runEscapeAnalysis m = runState (runReaderT m initialContext) initialState

-- | Analyze entire program
analyzeProgramEscape :: [Statement] -> ProgramAnalysis
analyzeProgramEscape stmts = 
  let (_, finalState) = runEscapeAnalysis $ do
        -- Phase 1: Build function summaries
        buildFunctionSummaries stmts
        
        -- Phase 2: Analyze with interprocedural information
        analyzeStatements stmts
        
        -- Phase 3: Propagate escape information
        propagateEscapes
        
        -- Phase 4: Identify optimization opportunities
        opportunities <- identifyOptimizations
        
        return opportunities
        
      stats = gatherStatistics finalState
  in ProgramAnalysis
     { paFunctions = easFunctionSummaries finalState
     , paGlobalEscapes = gatherGlobalEscapes finalState
     , paOptimizationOpportunities = []  -- Filled by identifyOptimizations
     , paStatistics = stats
     }

-- | Build function summaries for interprocedural analysis
buildFunctionSummaries :: [Statement] -> EscapeAnalysisM ()
buildFunctionSummaries stmts = mapM_ buildSummaryForStmt stmts
  where
    buildSummaryForStmt :: Statement -> EscapeAnalysisM ()
    buildSummaryForStmt (StmtDeclare fname (Just (Located _ (CELambda params body)))) = do
      -- Analyze function in isolation
      summary <- analyzeFunctionForSummary fname params body
      modify $ \s -> s { easFunctionSummaries = HashMap.insert fname summary (easFunctionSummaries s) }
    buildSummaryForStmt (StmtBlock stmts') = mapM_ buildSummaryForStmt stmts'
    buildSummaryForStmt _ = return ()

-- | Analyze function to build summary
analyzeFunctionForSummary :: Identifier -> [Identifier] -> CommonExpr -> EscapeAnalysisM FunctionSummary
analyzeFunctionForSummary fname params body = do
  -- Save current state
  savedState <- get
  savedContext <- ask
  
  -- Reset for function analysis
  modify $ \s -> s { easEscapeMap = HashMap.empty, easEscapeGraph = HashMap.empty }
  
  -- Analyze function body
  local (\ctx -> ctx { ecCurrentFunction = Just fname }) $ do
    -- Mark parameters
    forM_ (zip [0..] params) $ KATEX_INLINE_OPENidx, param) ->
      modify $ \s -> s { easEscapeMap = HashMap.insert param NoEscape (easEscapeMap s) }
    
    -- Analyze body
    _ <- analyzeExpression body
    
    -- Determine which parameters escape
    escapeMap <- gets easEscapeMap
    let paramEscapes = HashMap.fromList 
          [(idx, fromMaybe NoEscape (HashMap.lookup param escapeMap)) | (idx, param) <- zip [0..] params]
    
    -- Identify captured variables
    let captured = identifyCapturedVars params body
    
    -- Restore state
    put savedState
    
    return $ FunctionSummary
      { fsParameters = params
      , fsParameterEscapes = paramEscapes
      , fsReturnEscapes = [idx | (idx, esc) <- HashMap.toList paramEscapes, esc == EscapeToReturn]
      , fsCreatesEscaping = any (== EscapeToHeap) (HashMap.elems paramEscapes)
      , fsCapturedVars = captured
      }

-- | Identify variables captured by closures
identifyCapturedVars :: [Identifier] -> CommonExpr -> HashSet Identifier
identifyCapturedVars params expr = 
  let freeVars = collectFreeVars expr
      paramSet = HashSet.fromList params
  in HashSet.difference freeVars paramSet

-- | Collect free variables in expression
collectFreeVars :: CommonExpr -> HashSet Identifier
collectFreeVars = \case
  CEVar var -> HashSet.singleton var
  CELiteral _ -> HashSet.empty
  CEBinaryOp _ l r -> HashSet.union (collectFreeVars (locatedValue l)) (collectFreeVars (locatedValue r))
  CEUnaryOp _ e -> collectFreeVars (locatedValue e)
  CEComparison _ l r -> HashSet.union (collectFreeVars (locatedValue l)) (collectFreeVars (locatedValue r))
  CECall f args -> HashSet.unions $ collectFreeVars (locatedValue f) : map (collectFreeVars . locatedValue) args
  CEIndex e idx -> HashSet.union (collectFreeVars (locatedValue e)) (collectFreeVars (locatedValue idx))
  CESlice e s end -> HashSet.unions $ collectFreeVars (locatedValue e) : mapMaybe (fmap (collectFreeVars . locatedValue)) [s, end]
  CEAttribute e _ -> collectFreeVars (locatedValue e)
  CELambda params body -> HashSet.difference (collectFreeVars body) (HashSet.fromList params)

-- | Analyze statements with flow sensitivity
analyzeStatements :: [Statement] -> EscapeAnalysisM ()
analyzeStatements = mapM_ analyzeStatement

-- | Analyze a single statement
analyzeStatement :: Statement -> EscapeAnalysisM ()
analyzeStatement = \case
  StmtExpr expr -> void $ analyzeExpression (locatedValue expr)
  
  StmtAssign var expr -> do
    -- Create new scope for assignment if needed
    enterNewBlock
    escapeInfo <- analyzeExpression (locatedValue expr)
    markEscape var escapeInfo
    
    -- Track assignment for alias analysis
    case locatedValue expr of
      CEVar src -> addAlias var src
      _ -> return ()
  
  StmtDeclare var Nothing -> markEscape var NoEscape
  
  StmtDeclare var (Just expr) -> do
    enterNewBlock
    escapeInfo <- analyzeExpression (locatedValue expr)
    markEscape var escapeInfo
    
    -- Track allocation site
    case locatedValue expr of
      CECall (Located _ (CEVar "new")) _ -> 
        trackAllocationSite var (sourceLocation expr) NewObject
      _ -> return ()
  
  StmtReturn Nothing -> return ()
  
  StmtReturn (Just expr) -> do
    local (\ctx -> ctx { ecControlFlow = (ecControlFlow ctx) { cfcInReturn = True } }) $ do
      escapeInfo <- analyzeExpression (locatedValue expr)
      -- Mark variables in return expression as escaping
      markReturnEscapes (locatedValue expr)
  
  StmtIf cond thenStmts elseStmts -> do
    -- Analyze condition
    _ <- analyzeExpression (locatedValue cond)
    
    -- Analyze branches with separate flow states
    savedBlock <- gets easNextBlockId
    
    -- Then branch
    enterNewBlock
    local (\ctx -> ctx { ecControlFlow = (ecControlFlow ctx) { cfcInConditional = True } }) $
      analyzeStatements thenStmts
    
    -- Else branch
    case elseStmts of
      Just stmts -> do
        modify $ \s -> s { easNextBlockId = savedBlock + 1 }
        enterNewBlock
        local (\ctx -> ctx { ecControlFlow = (ecControlFlow ctx) { cfcInConditional = True } }) $
          analyzeStatements stmts
      Nothing -> return ()
    
    -- Merge flow states
    mergeFlowStates
  
  StmtWhile cond body -> do
    -- Analyze in loop context
    local (\ctx -> ctx { ecControlFlow = (ecControlFlow ctx) 
                        { cfcInLoop = True
                        , cfcLoopDepth = cfcLoopDepth (ecControlFlow ctx) + 1 
                        }}) $ do
      -- Conservative: assume loop condition can cause escape
      _ <- analyzeExpression (locatedValue cond)
      
      -- Analyze body (may execute multiple times)
      enterNewBlock
      analyzeStatements body
      
      -- In loops, any assignment might escape through multiple iterations
      promoteLoopEscapes
  
  StmtFor var iter body -> do
    -- Iterator might escape
    iterEscape <- analyzeExpression (locatedValue iter)
    markEscape var iterEscape
    
    -- Analyze loop body
    local (\ctx -> ctx { ecControlFlow = (ecControlFlow ctx) 
                        { cfcInLoop = True
                        , cfcLoopDepth = cfcLoopDepth (ecControlFlow ctx) + 1 
                        }}) $ do
      enterNewBlock
      analyzeStatements body
      promoteLoopEscapes
  
  StmtBlock stmts -> do
    -- Create new scope
    enterScope
    analyzeStatements stmts
    exitScope

-- | Enter new block for flow-sensitive analysis
enterNewBlock :: EscapeAnalysisM ()
enterNewBlock = modify $ \s -> s { easNextBlockId = easNextBlockId s + 1 }

-- | Enter new variable scope
enterScope :: EscapeAnalysisM ()
enterScope = do
  blockId <- gets easNextBlockId
  local (\ctx -> ctx { ecScopeStack = blockId : ecScopeStack ctx }) (return ())

-- | Exit variable scope
exitScope :: EscapeAnalysisM ()
exitScope = local (\ctx -> ctx { ecScopeStack = tail (ecScopeStack ctx) }) (return ())

-- | Add alias relationship
addAlias :: Identifier -> Identifier -> EscapeAnalysisM ()
addAlias var1 var2 = do
  local (\ctx -> 
    let aliases = acAliases (ecAliasContext ctx)
        updated = HashMap.insertWith HashSet.union var1 (HashSet.singleton var2) aliases
    in ctx { ecAliasContext = (ecAliasContext ctx) { acAliases = updated } }
  ) (return ())

-- | Track allocation site
trackAllocationSite :: Identifier -> SourceLocation -> AllocationType -> EscapeAnalysisM ()
trackAllocationSite var loc allocType = do
  let site = AllocationSite loc allocType Nothing False
  modify $ \s -> s { easAllocationSites = HashMap.insert var site (easAllocationSites s) }

-- | Mark variables in expression as escaping through return
markReturnEscapes :: CommonExpr -> EscapeAnalysisM ()
markReturnEscapes = \case
  CEVar var -> markEscape var EscapeToReturn
  CEBinaryOp _ l r -> do
    markReturnEscapes (locatedValue l)
    markReturnEscapes (locatedValue r)
  CEUnaryOp _ e -> markReturnEscapes (locatedValue e)
  CECall _ args -> mapM_ (markReturnEscapes . locatedValue) args
  CEIndex e _ -> markReturnEscapes (locatedValue e)
  CESlice e _ _ -> markReturnEscapes (locatedValue e)
  CEAttribute e _ -> markReturnEscapes (locatedValue e)
  _ -> return ()

-- | Promote escapes in loop context
promoteLoopEscapes :: EscapeAnalysisM ()
promoteLoopEscapes = do
  -- In loops, stack allocations might not be safe
  escapeMap <- gets easEscapeMap
  let promoted = HashMap.map promoteEscape escapeMap
  modify $ \s -> s { easEscapeMap = promoted }
  where
    promoteEscape NoEscape = EscapeToHeap  -- Conservative in loops
    promoteEscape other = other

-- | Merge flow states from different control flow paths
mergeFlowStates :: EscapeAnalysisM ()
mergeFlowStates = do
  -- Conservative merge: take the maximum escape level
  flowStates <- gets easFlowState
  escapeMap <- gets easEscapeMap
  
  let merged = HashMap.mapWithKey (mergeVarEscapes flowStates) escapeMap
  modify $ \s -> s { easEscapeMap = merged }
  where
    mergeVarEscapes flowStates var currentEscape =
      let blockEscapes = [escape | ((_, v), escape) <- HashMap.toList flowStates, v == var]
      in maximum (currentEscape : blockEscapes)

-- | Enhanced expression analysis with interprocedural support
analyzeExpression :: CommonExpr -> EscapeAnalysisM EscapeInfo
analyzeExpression = \case
  CELiteral _ -> return NoEscape
  
  CEVar var -> do
    -- Check aliases
    ctx <- ask
    let aliases = fromMaybe HashSet.empty $ HashMap.lookup var (acAliases (ecAliasContext ctx))
    if HashSet.null aliases
      then getEscapeInfo var
      else do
        -- If variable has aliases, take maximum escape of all aliases
        selfEscape <- getEscapeInfo var
        aliasEscapes <- mapM getEscapeInfo (HashSet.toList aliases)
        return $ maximum (selfEscape : aliasEscapes)
  
  CEBinaryOp _ l r -> do
    leftEscape <- analyzeExpression (locatedValue l)
    rightEscape <- analyzeExpression (locatedValue r)
    return $ max leftEscape rightEscape
  
  CEUnaryOp UnaryRef e -> do
    -- Taking address of variable causes it to escape
    case locatedValue e of
      CEVar var -> markEscape var EscapeToHeap
      _ -> return ()
    return EscapeToHeap
  
  CEUnaryOp _ e -> analyzeExpression (locatedValue e)
  
  CEComparison _ l r -> do
    _ <- analyzeExpression (locatedValue l)
    _ <- analyzeExpression (locatedValue r)
    return NoEscape
  
  CECall func args -> analyzeCallWithSummary func args
  
  CEIndex container idx -> do
    containerEscape <- analyzeExpression (locatedValue container)
    _ <- analyzeExpression (locatedValue idx)
    return containerEscape
  
  CESlice container start end -> do
    containerEscape <- analyzeExpression (locatedValue container)
    mapM_ (mapM (analyzeExpression . locatedValue)) [start, end]
    -- Slicing usually creates new object
    return $ if containerEscape == NoEscape then NoEscape else EscapeToHeap
  
  CEAttribute obj _ -> analyzeExpression (locatedValue obj)
  
  CELambda params body -> do
    -- Analyze closure
    let captured = collectFreeVars body HashSet.\\ HashSet.fromList params
    
    -- Mark captured variables as escaping
    forM_ (HashSet.toList captured) $ \var -> do
      modify $ \s -> s { easClosureCaptured = 
        HashMap.insertWith HashSet.union var (HashSet.singleton var) (easClosureCaptured s) }
      markEscape var EscapeToHeap
    
    -- Lambda itself might escape
    ctx <- ask
    if cfcInReturn (ecControlFlow ctx)
      then return EscapeToReturn
      else return EscapeToHeap

-- | Analyze function call with interprocedural information
analyzeCallWithSummary :: Located CommonExpr -> [Located CommonExpr] -> EscapeAnalysisM EscapeInfo
analyzeCallWithSummary func args = do
  case locatedValue func of
    CEVar fname -> do
      summaries <- gets easFunctionSummaries
      case HashMap.lookup fname summaries of
        Just summary -> do
          -- Use function summary for precise analysis
          argEscapes <- forM (zip [0..] args) $ KATEX_INLINE_OPENidx, arg) -> do
            argEscape <- analyzeExpression (locatedValue arg)
            
            -- Check if this parameter escapes in the function
            case HashMap.lookup idx (fsParameterEscapes summary) of
              Just paramEscape -> do
                -- If argument is a variable and parameter escapes, mark it
                case locatedValue arg of
                  CEVar var -> when (paramEscape /= NoEscape) $ markEscape var paramEscape
                  _ -> return ()
                return paramEscape
              Nothing -> return argEscape
          
          -- Determine overall escape behavior
          ctx <- ask
          if cfcInReturn (ecControlFlow ctx) && not (null (fsReturnEscapes summary))
            then return EscapeToReturn
            else if fsCreatesEscaping summary
              then return EscapeToHeap
              else return NoEscape
        
        Nothing -> do
          -- No summary available, fall back to conservative analysis
          mapM_ (analyzeExpression . locatedValue) args
          ctx <- ask
          if cfcInReturn (ecControlFlow ctx)
            then return EscapeToReturn
            else return EscapeToHeap
    
    _ -> do
      -- Indirect call, be conservative
      _ <- analyzeExpression (locatedValue func)
      mapM_ (analyzeExpression . locatedValue) args
      return EscapeToHeap

-- | Mark variable as escaping
markEscape :: Identifier -> EscapeInfo -> EscapeAnalysisM ()
markEscape var escapeInfo = do
  ctx <- ask
  blockId <- gets ((fromMaybe 0 . listToMaybe . ecScopeStack) <$>)
  
  -- Update escape map
  modify $ \s -> s { easEscapeMap = HashMap.insert var escapeInfo (easEscapeMap s) }
  
  -- Update flow-sensitive state
  modify $ \s -> s { easFlowState = HashMap.insert (blockId, var) escapeInfo (easFlowState s) }
  
  -- Update allocation site if it exists
  when (escapeInfo /= NoEscape) $
    modify $ \s -> s { easAllocationSites = 
      HashMap.adjust (\site -> site { asEscapes = True }) var (easAllocationSites s) }

-- | Get escape information for variable
getEscapeInfo :: Identifier -> EscapeAnalysisM EscapeInfo
getEscapeInfo var = do
  escapeMap <- gets easEscapeMap
  return $ HashMap.lookupDefault NoEscape var escapeMap

-- | Add escape dependency
addEscapeDependency :: Identifier -> Identifier -> EscapeAnalysisM ()
addEscapeDependency from to =
  modify $ \s -> s { easEscapeGraph = 
    HashMap.insertWith Set.union from (Set.singleton to) (easEscapeGraph s) }

-- | Propagate escape information through dependency graph
propagateEscapes :: EscapeAnalysisM ()
propagateEscapes = do
  graph <- gets easEscapeGraph
  escapeMap <- gets easEscapeMap
  closureCaptured <- gets easClosureCaptured
  
  -- Add closure dependencies
  let graphWithClosures = HashMap.unionWith Set.union graph 
        (HashMap.map (Set.fromList . HashSet.toList) closureCaptured)
  
  -- Fixed-point iteration
  let propagated = fixpoint (propagateStep graphWithClosures) escapeMap
  modify $ \s -> s { easEscapeMap = propagated }
  where
    fixpoint f x = let x' = f x in if x' == x then x else fixpoint f x'
    
    propagateStep graph escapes = 
      HashMap.mapWithKey (propagateVar graph escapes) escapes
    
    propagateVar graph escapes var currentEscape =
      case HashMap.lookup var graph of
        Nothing -> currentEscape
        Just deps -> 
          let depEscapes = [HashMap.lookupDefault NoEscape dep escapes | dep <- Set.toList deps]
          in maximum (currentEscape : depEscapes)

-- | Identify optimization opportunities
identifyOptimizations :: EscapeAnalysisM [OptimizationOpportunity]
identifyOptimizations = do
  escapeMap <- gets easEscapeMap
  allocSites <- gets easAllocationSites
  
  let opportunities = []
  
  -- Check each allocation site
  forM (HashMap.toList allocSites) $ KATEX_INLINE_OPENvar, site) -> do
    escape <- getEscapeInfo var
    case (escape, asEscapes site) of
      (NoEscape, True) -> 
        return [OptimizationOpportunity 
          (asLocation site) 
          StackAllocation 
          "Variable does not escape - can use stack allocation" 
          80]
      (EscapeToReturn, True) ->
        return [OptimizationOpportunity 
          (asLocation site) 
          ElideCopy 
          "Return value optimization possible" 
          60]
      _ -> return []
  
  return $ concat opportunities

-- | Gather statistics
gatherStatistics :: EscapeAnalysisState -> AnalysisStatistics
gatherStatistics state = AnalysisStatistics
  { asAnalyzedFunctions = HashMap.size (easFunctionSummaries state)
  , asEscapingAllocations = length [s | s <- HashMap.elems (easAllocationSites state), asEscapes s]
  , asStackAllocations = length [s | s <- HashMap.elems (easAllocationSites state), not (asEscapes s)]
  , asOptimizedAllocations = length (easOptimizationLog state)
  }

-- | Gather global escapes
gatherGlobalEscapes :: EscapeAnalysisState -> HashSet Identifier
gatherGlobalEscapes state = 
  HashSet.fromList [var | (var, esc) <- HashMap.toList (easEscapeMap state), esc == EscapeToGlobal]

-- | Optimize program based on escape analysis
optimizeProgram :: [Statement] -> ([Statement], ProgramAnalysis)
optimizeProgram stmts = 
  let analysis = analyzeProgramEscape stmts
      optimized = applyOptimizations stmts analysis
  in (optimized, analysis)

-- | Apply optimizations to program
applyOptimizations :: [Statement] -> ProgramAnalysis -> [Statement]
applyOptimizations stmts analysis = map (optimizeStatement analysis) stmts

-- | Optimize individual statement
optimizeStatement :: ProgramAnalysis -> Statement -> Statement
optimizeStatement analysis = \case
  StmtDeclare var (Just expr) ->
    let optimizedExpr = optimizeAllocation analysis (locatedValue expr)
    in StmtDeclare var (Just (expr { locatedValue = optimizedExpr }))
  
  StmtAssign var expr ->
    let optimizedExpr = optimizeAllocation analysis (locatedValue expr)
    in StmtAssign var (expr { locatedValue = optimizedExpr })
  
  StmtIf cond thenStmts elseStmts ->
    StmtIf cond 
      (map (optimizeStatement analysis) thenStmts)
      (fmap (map (optimizeStatement analysis)) elseStmts)
  
  StmtWhile cond body ->
    StmtWhile cond (map (optimizeStatement analysis) body)
  
  StmtFor var iter body ->
    StmtFor var iter (map (optimizeStatement analysis) body)
  
  StmtBlock stmts ->
    StmtBlock (map (optimizeStatement analysis) stmts)
  
  other -> other

-- | Optimize allocation based on escape analysis
optimizeAllocation :: ProgramAnalysis -> CommonExpr -> CommonExpr
optimizeAllocation analysis = \case
  CECall (Located loc (CEVar "new")) args 
    | canStackAllocate analysis args -> 
        CECall (Located loc (CEVar "stack_alloc")) args
  
  CECall (Located loc (CEVar "make_unique")) args
    | canElideAllocation analysis args ->
        CECall (Located loc (CEVar "make_inplace")) args
  
  other -> other

-- | Check if allocation can be on stack
canStackAllocate :: ProgramAnalysis -> [Located CommonExpr] -> Bool
canStackAllocate _ _ = False  -- Simplified for now

-- | Check if allocation can be elided
canElideAllocation :: ProgramAnalysis -> [Located CommonExpr] -> Bool  
canElideAllocation _ _ = False  -- Simplified for now