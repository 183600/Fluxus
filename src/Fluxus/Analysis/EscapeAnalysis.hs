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
import Fluxus.AST.Python

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad (forM_, void, when)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)



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
  { asLocation :: !SourceSpan
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
  { oeLocation :: !SourceSpan
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
  { ooLocation :: !SourceSpan
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
analyzeProgramEscape :: [Located PythonStmt] -> ProgramAnalysis
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
buildFunctionSummaries :: [Located PythonStmt] -> EscapeAnalysisM ()
buildFunctionSummaries stmts = mapM_ buildSummaryForStmt stmts
  where
    buildSummaryForStmt :: Located PythonStmt -> EscapeAnalysisM ()
    buildSummaryForStmt (Located _ (PyFuncDef funcDef)) = do
      let fname = pyFuncName funcDef
          params = pyFuncParams funcDef
          _body = pyFuncBody funcDef
      -- Analyze function in isolation
      let dummyBody = Located (SourceSpan "<dummy>" (SourcePos 0 0) (SourcePos 0 0)) (PyLiteral PyNone)
      summary <- analyzeFunctionForSummary fname (extractParamNames params) (PyLambda params dummyBody)
      modify $ \s -> s { easFunctionSummaries = HashMap.insert fname summary (easFunctionSummaries s) }
    buildSummaryForStmt (Located _ (PyFor _ _ _ body _)) = mapM_ buildSummaryForStmt body
    buildSummaryForStmt (Located _ (PyIf _ thenStmts elseStmts)) = do
      mapM_ buildSummaryForStmt thenStmts
      mapM_ buildSummaryForStmt elseStmts
    buildSummaryForStmt (Located _ (PyWhile _ body _)) = mapM_ buildSummaryForStmt body
    buildSummaryForStmt (Located _ (PyWith _ _ body)) = mapM_ buildSummaryForStmt body
    buildSummaryForStmt (Located _ (PyTry body _ _ _)) = mapM_ buildSummaryForStmt body
    buildSummaryForStmt _ = return ()

-- | Analyze function to build summary
analyzeFunctionForSummary :: Identifier -> [Identifier] -> PythonExpr -> EscapeAnalysisM FunctionSummary
analyzeFunctionForSummary fname params body = do
  -- Save current state
  savedState <- get
  
  -- Reset for function analysis
  modify $ \s -> s { easEscapeMap = HashMap.empty, easEscapeGraph = HashMap.empty }
  
  -- Analyze function body
  local (\ctx -> ctx { ecCurrentFunction = Just fname }) $ do
    -- Mark parameters
    forM_ (zip [0::Int ..] params) $ \(_idx, param) ->
      modify $ \s -> s { easEscapeMap = HashMap.insert param NoEscape (easEscapeMap s) }
    
    -- Analyze body
    _ <- analyzePythonExpression body
    
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

-- | Identify captured variables in a function
identifyCapturedVars :: [Identifier] -> PythonExpr -> HashSet Identifier
identifyCapturedVars _params expr =
  case expr of
    PyLambda params' body -> HashSet.difference (collectFreeVarsPython (locValue body)) (HashSet.fromList (extractParamNames params'))
    _ -> collectFreeVarsPython expr



-- | Helper functions for free variable collection
extractArgExpr :: Located PythonArgument -> Located PythonExpr
extractArgExpr (Located _ (ArgPositional expr)) = expr
extractArgExpr (Located _ (ArgKeyword _ expr)) = expr
extractArgExpr (Located _ (ArgStarred expr)) = expr
extractArgExpr (Located _ (ArgKwStarred expr)) = expr

extractParamNames :: [Located PythonParameter] -> [Identifier]
extractParamNames = mapMaybe extractParamName
  where
    extractParamName (Located _ (ParamNormal name _ _)) = Just name
    extractParamName (Located _ (ParamVarArgs name _)) = Just name
    extractParamName (Located _ (ParamKwArgs name _)) = Just name
    extractParamName (Located _ (ParamKwOnly name _ _)) = Just name
    extractParamName (Located _ (ParamPosOnly name _ _)) = Just name

collectFreeVarsPattern :: PythonPattern -> HashSet Identifier
collectFreeVarsPattern (PatVar var) = HashSet.singleton var
collectFreeVarsPattern PatWildcard = HashSet.empty
collectFreeVarsPattern (PatLiteral _) = HashSet.empty
collectFreeVarsPattern (PatTuple patterns) = HashSet.unions $ map (collectFreeVarsPattern . locValue) patterns
collectFreeVarsPattern (PatList patterns) = HashSet.unions $ map (collectFreeVarsPattern . locValue) patterns
collectFreeVarsPattern (PatStarred _) = HashSet.empty
collectFreeVarsPattern (PatOr patterns) = HashSet.unions $ map (collectFreeVarsPattern . locValue) patterns
collectFreeVarsPattern (PatCapture _) = HashSet.empty
collectFreeVarsPattern (PatAs _ _) = HashSet.empty
collectFreeVarsPattern (PatValue _) = HashSet.empty
collectFreeVarsPattern (PatSequence _) = HashSet.empty
collectFreeVarsPattern (PatMapping patterns mrest) =
  HashSet.unions $
    map (collectFreeVarsPython . locValue . fst) patterns ++
    map (collectFreeVarsPattern . locValue . snd) patterns ++
    maybe [] (return . HashSet.singleton) mrest
collectFreeVarsPattern (PatClass _ patterns kwPatterns) =
  HashSet.unions $
    map (collectFreeVarsPattern . locValue) patterns ++
    map (collectFreeVarsPattern . locValue . snd) kwPatterns

collectFreeVarsFString :: FStringPart -> HashSet Identifier
collectFreeVarsFString (FStringLiteral _) = HashSet.empty
collectFreeVarsFString (FStringExpr expr _ _) = collectFreeVarsPython (locValue expr)

collectFreeVarsSlice :: PythonSlice -> HashSet Identifier
collectFreeVarsSlice (SliceIndex idx) = collectFreeVarsPython (locValue idx)
collectFreeVarsSlice (SliceSlice s end _) = HashSet.unions $ mapMaybe (fmap (collectFreeVarsPython . locValue)) [s, end]
collectFreeVarsSlice (SliceExtSlice slices) = HashSet.unions $ map (collectFreeVarsSlice . locValue) slices

collectFreeVarsComp :: PythonComprehension -> HashSet Identifier
collectFreeVarsComp (PythonComprehension target iter filters _) =
  HashSet.union (collectFreeVarsPattern (locValue target)) (HashSet.unions $ collectFreeVarsPython (locValue iter) : map (collectFreeVarsPython . locValue) filters)

-- | Collect free variables in Python expression
collectFreeVarsPython :: PythonExpr -> HashSet Identifier
collectFreeVarsPython = \case
  PyVar var -> HashSet.singleton var
  PyLiteral _ -> HashSet.empty
  PyBinaryOp _ l r -> HashSet.union (collectFreeVarsPython (locValue l)) (collectFreeVarsPython (locValue r))
  PyUnaryOp _ e -> collectFreeVarsPython (locValue e)
  PyComparison _ exprs -> HashSet.unions $ map (collectFreeVarsPython . locValue) exprs
  PyBoolOp _ exprs -> HashSet.unions $ map (collectFreeVarsPython . locValue) exprs
  PyCall f args -> HashSet.unions $ collectFreeVarsPython (locValue f) : map (collectFreeVarsPython . locValue . extractArgExpr) args
  PySubscript e idx -> HashSet.union (collectFreeVarsPython (locValue e)) (collectFreeVarsSlice (locValue idx))
  PySlice s end step -> HashSet.unions $ mapMaybe (fmap (collectFreeVarsPython . locValue)) [s, end, step]
  PyAttribute e _ -> collectFreeVarsPython (locValue e)
  PyList exprs -> HashSet.unions $ map (collectFreeVarsPython . locValue) exprs
  PyTuple exprs -> HashSet.unions $ map (collectFreeVarsPython . locValue) exprs
  PySet exprs -> HashSet.unions $ map (collectFreeVarsPython . locValue) exprs
  PyDict pairs -> HashSet.unions $ map (\(k, v) -> HashSet.union (collectFreeVarsPython (locValue k)) (collectFreeVarsPython (locValue v))) pairs
  PyLambda params body -> HashSet.difference (collectFreeVarsPython (locValue body)) (HashSet.fromList (extractParamNames params))
  PyIfExp cond thenExpr elseExpr -> HashSet.unions [collectFreeVarsPython (locValue cond), collectFreeVarsPython (locValue thenExpr), collectFreeVarsPython (locValue elseExpr)]
  PyListComp expr comps -> HashSet.union (collectFreeVarsPython (locValue expr)) (HashSet.unions $ map collectFreeVarsComp comps)
  PySetComp expr comps -> HashSet.union (collectFreeVarsPython (locValue expr)) (HashSet.unions $ map collectFreeVarsComp comps)
  PyDictComp kexpr vexpr comps -> HashSet.unions [collectFreeVarsPython (locValue kexpr), collectFreeVarsPython (locValue vexpr), HashSet.unions $ map collectFreeVarsComp comps]
  PyGenComp expr comps -> HashSet.union (collectFreeVarsPython (locValue expr)) (HashSet.unions $ map collectFreeVarsComp comps)
  PyAwait expr -> collectFreeVarsPython (locValue expr)
  PyFString parts -> HashSet.unions $ map collectFreeVarsFString parts
  _ -> HashSet.empty
    
    
-- | Analyze Python slice expressions
analyzePythonSlice :: PythonSlice -> EscapeAnalysisM EscapeInfo
analyzePythonSlice (SliceIndex idx) = analyzePythonExpression (locValue idx)
analyzePythonSlice (SliceSlice start end _) = do
  startEscape <- maybe (return NoEscape) (analyzePythonExpression . locValue) start
  endEscape <- maybe (return NoEscape) (analyzePythonExpression . locValue) end
  return $ max startEscape endEscape
analyzePythonSlice (SliceExtSlice slices) = do
  sliceEscapes <- mapM (analyzePythonSlice . locValue) slices
  return $ maximum sliceEscapes

-- | Analyze Python expressions
analyzePythonExpression :: PythonExpr -> EscapeAnalysisM EscapeInfo
analyzePythonExpression expr = case expr of
  PyVar var -> getEscapeInfo var
  PyLiteral _ -> return NoEscape
  PyBinaryOp _ l r -> do
    leftEscape <- analyzePythonExpression (locValue l)
    rightEscape <- analyzePythonExpression (locValue r)
    return $ max leftEscape rightEscape
  PyUnaryOp _ e -> analyzePythonExpression (locValue e)
  PyComparison _ exprs -> do
    escapeList <- mapM (analyzePythonExpression . locValue) exprs
    return $ maximum escapeList
  PyBoolOp _ exprs -> do
    escapeList <- mapM (analyzePythonExpression . locValue) exprs
    return $ maximum escapeList
  PyCall func args -> do
    funcEscape <- analyzePythonExpression (locValue func)
    argEscapes <- mapM (analyzePythonExpression . locValue . extractArgExpr') args
    return $ maximum (funcEscape : argEscapes)
  PySubscript container idx -> do
    containerEscape <- analyzePythonExpression (locValue container)
    idxEscape <- analyzePythonSlice (locValue idx)
    return $ max containerEscape idxEscape
  PySlice container start end -> do
    containerEscape <- case container of
      Just c -> analyzePythonExpression (locValue c)
      Nothing -> return NoEscape
    startEscape <- maybe (return NoEscape) (analyzePythonExpression . locValue) start
    endEscape <- maybe (return NoEscape) (analyzePythonExpression . locValue) end
    return $ maximum [containerEscape, startEscape, endEscape]
  PyAttribute obj _ -> analyzePythonExpression (locValue obj)
  PyList exprs -> do
    escapeList <- mapM (analyzePythonExpression . locValue) exprs
    return $ maximum escapeList
  PyTuple exprs -> do
    escapeList <- mapM (analyzePythonExpression . locValue) exprs
    return $ maximum escapeList
  PySet exprs -> do
    escapeList <- mapM (analyzePythonExpression . locValue) exprs
    return $ maximum escapeList
  PyDict pairs -> do
    escapeList <- mapM (\(k, v) -> do
      kEscape <- analyzePythonExpression (locValue k)
      vEscape <- analyzePythonExpression (locValue v)
      return $ max kEscape vEscape) pairs
    return $ maximum escapeList
  PyLambda params body -> do
    -- Analyze closure
    let captured = collectFreeVarsPython (locValue body) `HashSet.difference` HashSet.fromList (extractParamNames params)
    
    -- Mark captured variables as escaping
    mapM_ (\var -> markEscape var EscapeToHeap) (HashSet.toList captured)
    return EscapeToHeap
  PyIfExp cond thenExpr elseExpr -> do
    condEscape <- analyzePythonExpression (locValue cond)
    thenEscape <- analyzePythonExpression (locValue thenExpr)
    elseEscape <- analyzePythonExpression (locValue elseExpr)
    return $ maximum [condEscape, thenEscape, elseEscape]
  PyListComp expr' comps -> do
    exprEscape <- analyzePythonExpression (locValue expr')
    compEscapes <- mapM analyzeComprehension comps
    return $ maximum (exprEscape : compEscapes)
  PySetComp expr' comps -> do
    exprEscape <- analyzePythonExpression (locValue expr')
    compEscapes <- mapM analyzeComprehension comps
    return $ maximum (exprEscape : compEscapes)
  PyDictComp kexpr vexpr comps -> do
    kEscape <- analyzePythonExpression (locValue kexpr)
    vEscape <- analyzePythonExpression (locValue vexpr)
    compEscapes <- mapM analyzeComprehension comps
    return $ maximum (kEscape : vEscape : compEscapes)
  PyGenComp expr' comps -> do
    exprEscape <- analyzePythonExpression (locValue expr')
    compEscapes <- mapM analyzeComprehension comps
    return $ maximum (exprEscape : compEscapes)
  PyAwait expr' -> analyzePythonExpression (locValue expr')
  PyFString parts -> do
    escapeList <- mapM analyzeFStringPart parts
    return $ maximum escapeList
  _ -> return NoEscape
  where
    extractArgExpr' (Located _ (ArgPositional expr')) = expr'
    extractArgExpr' (Located _ (ArgKeyword _ expr')) = expr'
    extractArgExpr' (Located _ (ArgStarred expr')) = expr'
    extractArgExpr' (Located _ (ArgKwStarred expr')) = expr'
    
    analyzeComprehension (PythonComprehension _target iter filters _) = do
      -- Pattern doesn't escape, just track variables
      iterEscape <- analyzePythonExpression (locValue iter)
      filterEscapes <- mapM (analyzePythonExpression . locValue) filters
      return $ maximum (iterEscape : filterEscapes)
    
    analyzeFStringPart (FStringLiteral _) = return NoEscape
    analyzeFStringPart (FStringExpr expr' _ _) = analyzePythonExpression (locValue expr')

-- | Analyze Common expressions
analyzeExpression :: CommonExpr -> EscapeAnalysisM EscapeInfo
analyzeExpression expr = case expr of
  CEVar var -> getEscapeInfo var
  CELiteral _ -> return NoEscape
  CEBinaryOp _ l r -> do
    leftEscape <- analyzeExpression (locValue l)
    rightEscape <- analyzeExpression (locValue r)
    return $ max leftEscape rightEscape
  CEUnaryOp _ e -> analyzeExpression (locValue e)
  CEComparison _ l r -> do
    leftEscape <- analyzeExpression (locValue l)
    rightEscape <- analyzeExpression (locValue r)
    return $ max leftEscape rightEscape
  CECall func args -> do
    funcEscape <- analyzeExpression (locValue func)
    argEscapes <- mapM (analyzeExpression . locValue) args
    return $ maximum (funcEscape : argEscapes)
  CEIndex container idx -> do
    containerEscape <- analyzeExpression (locValue container)
    idxEscape <- analyzeExpression (locValue idx)
    return $ max containerEscape idxEscape
  CESlice container start end -> do
    containerEscape <- analyzeExpression (locValue container)
    startEscape <- maybe (return NoEscape) (analyzeExpression . locValue) start
    endEscape <- maybe (return NoEscape) (analyzeExpression . locValue) end
    return $ maximum [containerEscape, startEscape, endEscape]
  CEAttribute obj _ -> analyzeExpression (locValue obj)
  _ -> do
    escapeInfo <- local (\ctx -> ctx { ecControlFlow = (ecControlFlow ctx) { cfcInReturn = True } }) $ do
      escapeInfo <- analyzeExpression expr
      return escapeInfo
    return escapeInfo
    
    -- Then branch
    -- | Analyze statements with flow sensitivity
analyzeStatements :: [Located PythonStmt] -> EscapeAnalysisM ()
analyzeStatements = mapM_ analyzeStatement

-- | Analyze a single statement
analyzeStatement :: Located PythonStmt -> EscapeAnalysisM ()
analyzeStatement (Located _ stmt) = case stmt of
  PyExprStmt expr -> void $ analyzePythonExpression (locValue expr)
  
  PyAssign [Located _ (PatVar var)] expr -> do
    -- Create new scope for assignment if needed
    enterNewBlock
    escapeInfo <- analyzePythonExpression (locValue expr)
    markEscape var escapeInfo
    
    -- Track assignment for alias analysis
    case locValue expr of
      PyVar src -> addAlias var src
      _ -> return ()
  
  PyAnnAssign (Located _ (PatVar var)) _ Nothing -> markEscape var NoEscape
  
  PyAnnAssign (Located _ (PatVar var)) _ (Just expr) -> do
    enterNewBlock
    escapeInfo <- analyzePythonExpression (locValue expr)
    markEscape var escapeInfo
    
    -- Track allocation site
    case locValue expr of
      PyCall (Located _ (PyVar (Identifier "new"))) _ -> 
        trackAllocationSite var (locSpan expr) NewObject
      _ -> return ()
  
  PyReturn Nothing -> return ()
  
  PyReturn (Just _expr) -> do
    exitScope
  
  PyAugAssign _ _ _ -> return ()
  
  PyYield _ -> return ()
  
  PyYieldFrom _ -> return ()
  
  PyBreak -> return ()
  
  PyContinue -> return ()
  
  PyGlobal _ -> return ()
  
  PyNonlocal _ -> return ()
  
  PyAssert _ _ -> return ()
  
  PyDel _ -> return ()
  
  PyPass -> return ()
  
  PyImport _ -> return ()
  
  _ -> return ()
  
  PyClassDef _ -> return ()
  
  PyMatch _ -> return ()

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
exitScope = local (\ctx -> ctx { ecScopeStack = drop 1 (ecScopeStack ctx) }) (return ())

-- | Add alias relationship
addAlias :: Identifier -> Identifier -> EscapeAnalysisM ()
addAlias var1 var2 = do
  local (\ctx ->
    let aliases = acAliases (ecAliasContext ctx)
        updated = HashMap.insertWith HashSet.union var1 (HashSet.singleton var2) aliases
        newAliasCtx = (ecAliasContext ctx) { acAliases = updated }
        newCtx = ctx { ecAliasContext = newAliasCtx }
    in newCtx) (return ())

-- | Track allocation site for escape analysis
trackAllocationSite :: Identifier -> SourceSpan -> AllocationType -> EscapeAnalysisM ()
trackAllocationSite var loc allocType = do
  let site = AllocationSite loc allocType Nothing False
  modify $ \s -> s { easAllocationSites = HashMap.insert var site (easAllocationSites s) }







-- | Analyze function call with interprocedural information
analyzeCallWithSummary :: Located CommonExpr -> [Located CommonExpr] -> EscapeAnalysisM EscapeInfo
analyzeCallWithSummary func args = do
  case locValue func of
    CEVar fname -> do
      summaries <- gets easFunctionSummaries
      case HashMap.lookup fname summaries of
        Just summary -> do
          -- Use function summary for precise analysis
          _argEscapes <- mapM (\(idx, arg) -> do
            argEscape <- analyzeExpression (locValue arg)
            
            -- Check if this parameter escapes in the function
            case HashMap.lookup idx (fsParameterEscapes summary) of
              Just paramEscape -> do
                -- If argument is a variable and parameter escapes, mark it
                case locValue arg of
                  CEVar var -> when (paramEscape /= NoEscape) $ markEscape var paramEscape
                  _ -> return ()
                return paramEscape
              Nothing -> return argEscape) (zip [0..] args)
          
          -- Determine overall escape behavior
          ctx <- ask
          if cfcInReturn (ecControlFlow ctx) && not (null (fsReturnEscapes summary))
            then return EscapeToReturn
            else if fsCreatesEscaping summary
              then return EscapeToHeap
              else return NoEscape
        
        Nothing -> do
          -- No summary available, fall back to conservative analysis
          mapM_ (analyzeExpression . locValue) args
          ctx <- ask
          if cfcInReturn (ecControlFlow ctx)
            then return EscapeToReturn
            else return EscapeToHeap
    
    _ -> do
      -- Indirect call, be conservative
      _ <- analyzeExpression (locValue func)
      mapM_ (analyzeExpression . locValue) args
      return EscapeToHeap

-- | Mark variable as escaping
markEscape :: Identifier -> EscapeInfo -> EscapeAnalysisM ()
markEscape var escapeInfo = do
  ctx <- ask
  let blockId = fromMaybe 0 $ listToMaybe $ ecScopeStack ctx
  
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
    
    propagateStep graph escapeMap =
      HashMap.mapWithKey (propagateVar graph escapeMap) escapeMap
    
    propagateVar graph escapeMap var currentEscape =
      case HashMap.lookup var graph of
        Nothing -> currentEscape
        Just deps -> 
          let depEscapes = [HashMap.lookupDefault NoEscape dep escapeMap | dep <- Set.toList deps]
          in maximum (currentEscape : depEscapes)

-- | Identify optimization opportunities
identifyOptimizations :: EscapeAnalysisM [OptimizationOpportunity]
identifyOptimizations = do
  _escapeMap <- gets easEscapeMap
  allocSites <- gets easAllocationSites
  
  let opportunities = []
  
  -- Check each allocation site
  mapM_ (\(var, site) -> do
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
      _ -> return []) (HashMap.toList allocSites)

  return $ concat opportunities

-- | Gather statistics
gatherStatistics :: EscapeAnalysisState -> AnalysisStatistics
gatherStatistics state' = AnalysisStatistics
  { asAnalyzedFunctions = HashMap.size (easFunctionSummaries state')
  , asEscapingAllocations = length [s | s <- HashMap.elems (easAllocationSites state'), asEscapes s]
  , asStackAllocations = length [s | s <- HashMap.elems (easAllocationSites state'), not (asEscapes s)]
  , asOptimizedAllocations = length (easOptimizationLog state')
  }

-- | Gather global escapes
gatherGlobalEscapes :: EscapeAnalysisState -> HashSet Identifier
gatherGlobalEscapes state' = 
  HashSet.fromList [var | (var, esc) <- HashMap.toList (easEscapeMap state'), esc == EscapeToGlobal]

-- | Optimize program based on escape analysis
optimizeProgram :: [Located PythonStmt] -> ([Located PythonStmt], ProgramAnalysis)
optimizeProgram stmts = 
  let analysis = analyzeProgramEscape stmts
      optimized = applyOptimizations stmts analysis
  in (optimized, analysis)

-- | Apply optimizations to program
applyOptimizations :: [Located PythonStmt] -> ProgramAnalysis -> [Located PythonStmt]
applyOptimizations stmts analysis = map (optimizeStatement analysis) stmts

-- | Optimize individual statement
optimizeStatement :: ProgramAnalysis -> Located PythonStmt -> Located PythonStmt
optimizeStatement analysis (Located span stmt) = Located span $ case stmt of
  PyExprStmt expr ->
    PyExprStmt expr  -- TODO: Add Python-specific allocation optimization

  PyAssign pats expr ->
    PyAssign pats expr  -- TODO: Add Python-specific allocation optimization
  
  PyIf cond thenStmts elseStmts ->
    PyIf cond 
      (map (optimizeStatement analysis) thenStmts)
      (map (optimizeStatement analysis) elseStmts)
  
  PyWhile cond thenStmts elseStmts ->
    PyWhile cond
      (map (optimizeStatement analysis) thenStmts)
      (map (optimizeStatement analysis) elseStmts)
  
  PyFor { pyForAsync = async, pyForTarget = target, pyForIter = iter, pyForBody = body, pyForElse = elseStmts } ->
    PyFor async target iter (map (optimizeStatement analysis) body) (map (optimizeStatement analysis) elseStmts)
  
  PyWith { pyWithAsync = async, pyWithItems = items, pyWithBody = body } ->
    PyWith async (map (optimizeWithItem analysis) items) (map (optimizeStatement analysis) body)
  
  other -> other
  where
    optimizeWithItem analysis (Located span item) = Located span $ case item of
      PythonWithItem contextExpr varPat ->
        PythonWithItem contextExpr varPat  -- TODO: Add Python-specific allocation optimization

-- | Check if a location escapes (simplified version)
locationEscapes :: ProgramAnalysis -> SourceSpan -> Bool
locationEscapes analysis loc = 
  -- For now, use a simple heuristic: check if the location is in global escapes
  -- This is a simplified version; a full implementation would track individual allocations
  Set.member (Identifier "temp") (Set.fromList $ HashSet.toList (paGlobalEscapes analysis))  -- Placeholder implementation

-- | Optimize allocation based on escape analysis
optimizeAllocation :: ProgramAnalysis -> CommonExpr -> CommonExpr
optimizeAllocation analysis = \case
  CECall (Located loc (CEVar (Identifier "new"))) args
    | not (locationEscapes analysis loc) -> CECall (Located loc (CEVar (Identifier "stack_alloc"))) args
    | otherwise -> CECall (Located loc (CEVar (Identifier "new"))) args

  CECall (Located loc (CEVar (Identifier "make_unique"))) args
    | not (locationEscapes analysis loc) -> CECall (Located loc (CEVar (Identifier "stack_alloc"))) args
    | otherwise -> CECall (Located loc (CEVar (Identifier "make_unique"))) args

  other -> other

