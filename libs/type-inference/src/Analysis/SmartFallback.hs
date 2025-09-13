{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Fluxus.Analysis.SmartFallback
  ( SmartFallbackM
  , FallbackDecision(..)
  , FallbackReason(..)
  , DynamismLevel(..)
  , AnalysisResult(..)
  , FunctionInfo(..)
  , runSmartFallback
  , analyzeProgram
  , optimizeProgram
  ) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Writer
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (foldl')
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- ============================================================================
-- Core Types
-- ============================================================================

-- | AST types (simplified for single file)
data SourceSpan = SourceSpan 
  { ssFile :: !FilePath
  , ssStartLine :: !Int
  , ssStartCol :: !Int
  , ssEndLine :: !Int
  , ssEndCol :: !Int
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

data Located a = Located
  { locSpan :: !SourceSpan
  , locatedValue :: !a
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

newtype Identifier = Identifier Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data Literal
  = LInt !Integer
  | LFloat !Double
  | LString !Text
  | LBool !Bool
  | LNull
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data BinaryOp = Add | Sub | Mul | Div | Mod | And | Or
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data UnaryOp = Negate | Not
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data CompareOp = Eq_ | Neq | Lt | Lte | Gt | Gte
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Common expression type
data CommonExpr
  = CELiteral !Literal
  | CEVar !Identifier
  | CEBinaryOp !BinaryOp !(Located CommonExpr) !(Located CommonExpr)
  | CEUnaryOp !UnaryOp !(Located CommonExpr)
  | CEComparison !CompareOp !(Located CommonExpr) !(Located CommonExpr)
  | CECall !(Located CommonExpr) ![Located CommonExpr]
  | CEIndex !(Located CommonExpr) !(Located CommonExpr)
  | CESlice !(Located CommonExpr) !(Maybe (Located CommonExpr)) !(Maybe (Located CommonExpr))
  | CEAttribute !(Located CommonExpr) !Identifier
  | CELambda ![Identifier] !(Located CommonExpr)
  | CEList ![Located CommonExpr]
  | CERecord ![(Identifier, Located CommonExpr)]
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Statement type for complete analysis
data Statement
  = SAssign !Identifier !(Located CommonExpr)
  | SReturn !(Maybe (Located CommonExpr))
  | SExpr !(Located CommonExpr)
  | SIf !(Located CommonExpr) ![Statement] ![Statement]
  | SWhile !(Located CommonExpr) ![Statement]
  | SFor !Identifier !(Located CommonExpr) ![Statement]
  | SFunction !Identifier ![Identifier] ![Statement]
  | SBlock ![Statement]
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- ============================================================================
-- Analysis Types
-- ============================================================================

-- | Monad stack for smart fallback analysis
type SmartFallbackM = ExceptT AnalysisError (WriterT [AnalysisWarning] (ReaderT FallbackContext (State FallbackState)))

-- | Analysis errors
data AnalysisError
  = UnsupportedConstruct !Text !SourceSpan
  | TypeAnalysisFailure !Text !CommonExpr
  | ScopeError !Identifier !SourceSpan
  | RecursionDepthExceeded !Int
  | InternalError !Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Analysis warnings
data AnalysisWarning
  = PotentialPerformanceIssue !Text !SourceSpan
  | UncertainDynamism !CommonExpr !DynamismLevel
  | MissingFunctionInfo !Identifier
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Context for fallback analysis
data FallbackContext = FallbackContext
  { fcOptimizationLevel :: !Int
  , fcFallbackThreshold :: !DynamismLevel
  , fcPreferPerformance :: !Bool
  , fcMaxAnalysisDepth :: !Int
  , fcFunctionDatabase :: !(HashMap Identifier FunctionInfo)
  , fcCurrentScope :: !ScopeInfo
  } deriving stock (Generic)
    deriving anyclass (NFData)

-- | Function information for analysis
data FunctionInfo = FunctionInfo
  { fiPurity :: !Purity
  , fiDynamism :: !DynamismLevel
  , fiSideEffects :: !Bool
  , fiInlinable :: !Bool
  , fiSpecializations :: ![TypeSignature]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data Purity = Pure | Impure | Unknown
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Simple type signature representation
data TypeSignature = TypeSignature
  { tsParams :: ![Text]
  , tsReturn :: !Text
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Scope information
data ScopeInfo = ScopeInfo
  { siVariables :: !(HashMap Identifier VariableInfo)
  , siParent :: !(Maybe ScopeInfo)
  , siDepth :: !Int
  } deriving stock (Generic)
    deriving anyclass (NFData)

-- | Variable information in scope
data VariableInfo = VariableInfo
  { viDynamism :: !DynamismLevel
  , viMutable :: !Bool
  , viDefinedAt :: !SourceSpan
  , viUsageCount :: !Int
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | State for fallback analysis
data FallbackState = FallbackState
  { fsGlobalDynamismMap :: !(HashMap Identifier DynamismLevel)
  , fsFallbackDecisions :: ![FallbackDecision]
  , fsAnalysisCache :: !(HashMap CommonExpr AnalysisResult)
  , fsCurrentDepth :: !Int
  , fsStatistics :: !AnalysisStatistics
  } deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Analysis statistics
data AnalysisStatistics = AnalysisStatistics
  { asExpressionsAnalyzed :: !Int
  , asStatementsAnalyzed :: !Int
  , asCacheHits :: !Int
  , asCacheMisses :: !Int
  , asFallbackCount :: !Int
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Level of dynamism in code
data DynamismLevel
  = FullyStatic
  | MostlyStatic
  | SemiDynamic
  | HighlyDynamic
  | FullyDynamic
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- | Detailed reasons for fallback
data FallbackReason
  = TypesUnknown ![Identifier]
  | ComplexControlFlow !Text
  | DynamicDispatch !Identifier
  | RuntimeReflection
  | ExternalDependencies ![Text]
  | PerformanceThreshold !Int
  | MemoryConstraints !Int
  | UnsafeOperation !Text
  | UnresolvedReference !Identifier
  | MutableStateAccess !Identifier
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Analysis result for an expression
data AnalysisResult = AnalysisResult
  { arExpression :: !CommonExpr
  , arDynamism :: !DynamismLevel
  , arReasons :: ![FallbackReason]
  , arDataDependencies :: !(Set Identifier)
  , arCanOptimize :: !Bool
  , arSuggestedOptimizations :: ![OptimizationStrategy]
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Optimization strategies
data OptimizationStrategy
  = ConstantFolding
  | DeadCodeElimination
  | CommonSubexpressionElimination
  | LoopUnrolling !Int
  | Inlining !Identifier
  | Specialization !TypeSignature
  | PartialEvaluation
  | StrengthReduction
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Fallback decision
data FallbackDecision = FallbackDecision
  { fdExpression :: !CommonExpr
  , fdShouldFallback :: !Bool
  , fdReasons :: ![FallbackReason]
  , fdDynamismLevel :: !DynamismLevel
  , fdAlternatives :: ![OptimizationStrategy]
  , fdLocation :: !SourceSpan
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- ============================================================================
-- Initialization
-- ============================================================================

-- | Create initial context with built-in functions
initialContext :: FallbackContext
initialContext = FallbackContext
  { fcOptimizationLevel = 2
  , fcFallbackThreshold = SemiDynamic
  , fcPreferPerformance = True
  , fcMaxAnalysisDepth = 100
  , fcFunctionDatabase = builtinFunctions
  , fcCurrentScope = initialScope
  }

-- | Built-in function database
builtinFunctions :: HashMap Identifier FunctionInfo
builtinFunctions = HashMap.fromList
  [ (Identifier "add", FunctionInfo Pure FullyStatic False True [])
  , (Identifier "sub", FunctionInfo Pure FullyStatic False True [])
  , (Identifier "mul", FunctionInfo Pure FullyStatic False True [])
  , (Identifier "div", FunctionInfo Pure MostlyStatic False True [])
  , (Identifier "print", FunctionInfo Impure FullyDynamic True False [])
  , (Identifier "read", FunctionInfo Impure FullyDynamic True False [])
  , (Identifier "eval", FunctionInfo Impure FullyDynamic True False [])
  , (Identifier "len", FunctionInfo Pure MostlyStatic False True [])
  , (Identifier "map", FunctionInfo Pure SemiDynamic False False [])
  , (Identifier "filter", FunctionInfo Pure SemiDynamic False False [])
  ]

-- | Initial scope
initialScope :: ScopeInfo
initialScope = ScopeInfo
  { siVariables = HashMap.empty
  , siParent = Nothing
  , siDepth = 0
  }

-- | Initial state
initialState :: FallbackState
initialState = FallbackState
  { fsGlobalDynamismMap = HashMap.empty
  , fsFallbackDecisions = []
  , fsAnalysisCache = HashMap.empty
  , fsCurrentDepth = 0
  , fsStatistics = AnalysisStatistics 0 0 0 0 0
  }

-- ============================================================================
-- Main Entry Points
-- ============================================================================

-- | Run smart fallback analysis
runSmartFallback :: SmartFallbackM a -> Either AnalysisError (a, FallbackState, [AnalysisWarning])
runSmartFallback m = 
  let ((result, warnings), state) = runState (runReaderT (runWriterT (runExceptT m)) initialContext) initialState
  in case result of
    Left err -> Left err
    Right val -> Right (val, state, warnings)

-- | Analyze a complete program
analyzeProgram :: [Statement] -> SmartFallbackM [AnalysisResult]
analyzeProgram stmts = do
  -- First pass: build variable dynamism map
  mapM_ analyzeStatement stmts
  
  -- Second pass: analyze expressions with complete context
  results <- concat <$> mapM collectExpressions stmts
  
  -- Update statistics
  modify $ \s -> s { fsStatistics = (fsStatistics s) { asStatementsAnalyzed = length stmts } }
  
  return results

-- | Optimize a program based on analysis
optimizeProgram :: [Statement] -> SmartFallbackM [Statement]
optimizeProgram stmts = do
  -- Analyze first
  results <- analyzeProgram stmts
  
  -- Create optimization plan
  let decisions = map makeOptimizationDecision results
  
  -- Apply optimizations
  optimizedStmts <- mapM (applyOptimizations decisions) stmts
  
  return optimizedStmts

-- ============================================================================
-- Statement Analysis
-- ============================================================================

-- | Analyze a statement and update context
analyzeStatement :: Statement -> SmartFallbackM ()
analyzeStatement = \case
  SAssign var expr -> do
    result <- analyzeExpression (locatedValue expr)
    updateVariableDynamism var (arDynamism result)
    
  SReturn mexpr -> 
    case mexpr of
      Just expr -> void $ analyzeExpression (locatedValue expr)
      Nothing -> return ()
    
  SExpr expr -> 
    void $ analyzeExpression (locatedValue expr)
    
  SIf cond thenStmts elseStmts -> do
    condResult <- analyzeExpression (locatedValue cond)
    withNewScope $ do
      mapM_ analyzeStatement thenStmts
      mapM_ analyzeStatement elseStmts
    
  SWhile cond body -> do
    condResult <- analyzeExpression (locatedValue cond)
    withNewScope $ mapM_ analyzeStatement body
    
  SFor var iterExpr body -> do
    iterResult <- analyzeExpression (locatedValue iterExpr)
    withNewScope $ do
      updateVariableDynamism var (arDynamism iterResult)
      mapM_ analyzeStatement body
    
  SFunction name params body -> do
    -- Analyze function body in new scope
    withNewScope $ do
      -- Mark parameters as potentially dynamic
      mapM_ (`updateVariableDynamism` SemiDynamic) params
      mapM_ analyzeStatement body
    
  SBlock stmts ->
    withNewScope $ mapM_ analyzeStatement stmts

-- | Collect all expressions from statements for analysis
collectExpressions :: Statement -> SmartFallbackM [AnalysisResult]
collectExpressions = \case
  SAssign _ expr -> (:[]) <$> analyzeExpression (locatedValue expr)
  SReturn (Just expr) -> (:[]) <$> analyzeExpression (locatedValue expr)
  SReturn Nothing -> return []
  SExpr expr -> (:[]) <$> analyzeExpression (locatedValue expr)
  SIf cond thenStmts elseStmts -> do
    condResult <- analyzeExpression (locatedValue cond)
    thenResults <- concat <$> mapM collectExpressions thenStmts
    elseResults <- concat <$> mapM collectExpressions elseStmts
    return $ condResult : (thenResults ++ elseResults)
  SWhile cond body -> do
    condResult <- analyzeExpression (locatedValue cond)
    bodyResults <- concat <$> mapM collectExpressions body
    return $ condResult : bodyResults
  SFor _ iterExpr body -> do
    iterResult <- analyzeExpression (locatedValue iterExpr)
    bodyResults <- concat <$> mapM collectExpressions body
    return $ iterResult : bodyResults
  SFunction _ _ body ->
    concat <$> mapM collectExpressions body
  SBlock stmts ->
    concat <$> mapM collectExpressions stmts

-- ============================================================================
-- Expression Analysis
-- ============================================================================

-- | Analyze an expression and return detailed result
analyzeExpression :: CommonExpr -> SmartFallbackM AnalysisResult
analyzeExpression expr = do
  -- Check cache first
  cache <- gets fsAnalysisCache
  case HashMap.lookup expr cache of
    Just result -> do
      updateStatistics True
      return result
    Nothing -> do
      updateStatistics False
      
      -- Check recursion depth
      depth <- gets fsCurrentDepth
      maxDepth <- asks fcMaxAnalysisDepth
      when (depth >= maxDepth) $
        throwError $ RecursionDepthExceeded depth
      
      -- Increment depth
      modify $ \s -> s { fsCurrentDepth = depth + 1 }
      
      -- Perform analysis
      result <- analyzeExpressionImpl expr
      
      -- Cache result
      modify $ \s -> s 
        { fsAnalysisCache = HashMap.insert expr result (fsAnalysisCache s)
        , fsCurrentDepth = depth
        }
      
      return result

-- | Implementation of expression analysis
analyzeExpressionImpl :: CommonExpr -> SmartFallbackM AnalysisResult
analyzeExpressionImpl expr = case expr of
  CELiteral lit -> return $ AnalysisResult
    { arExpression = expr
    , arDynamism = FullyStatic
    , arReasons = []
    , arDataDependencies = Set.empty
    , arCanOptimize = True
    , arSuggestedOptimizations = [ConstantFolding]
    }
  
  CEVar var -> do
    dynamism <- lookupVariableDynamism var
    let reasons = if dynamism > MostlyStatic 
                  then [UnresolvedReference var]
                  else []
    return $ AnalysisResult
      { arExpression = expr
      , arDynamism = dynamism
      , arReasons = reasons
      , arDataDependencies = Set.singleton var
      , arCanOptimize = dynamism <= MostlyStatic
      , arSuggestedOptimizations = []
      }
  
  CEBinaryOp op left right -> do
    leftResult <- analyzeExpression (locatedValue left)
    rightResult <- analyzeExpression (locatedValue right)
    let dynamism = max (arDynamism leftResult) (arDynamism rightResult)
    let reasons = arReasons leftResult ++ arReasons rightResult
    let deps = Set.union (arDataDependencies leftResult) (arDataDependencies rightResult)
    return $ AnalysisResult
      { arExpression = expr
      , arDynamism = dynamism
      , arReasons = reasons
      , arDataDependencies = deps
      , arCanOptimize = dynamism <= MostlyStatic && null reasons
      , arSuggestedOptimizations = 
          if dynamism == FullyStatic 
          then [ConstantFolding, StrengthReduction]
          else if dynamism <= MostlyStatic
          then [CommonSubexpressionElimination]
          else []
      }
  
  CEUnaryOp op operand -> do
    operandResult <- analyzeExpression (locatedValue operand)
    return $ operandResult
      { arExpression = expr
      , arSuggestedOptimizations = 
          if arDynamism operandResult == FullyStatic
          then ConstantFolding : arSuggestedOptimizations operandResult
          else arSuggestedOptimizations operandResult
      }
  
  CEComparison op left right -> do
    leftResult <- analyzeExpression (locatedValue left)
    rightResult <- analyzeExpression (locatedValue right)
    let dynamism = max (arDynamism leftResult) (arDynamism rightResult)
    let reasons = arReasons leftResult ++ arReasons rightResult
    let deps = Set.union (arDataDependencies leftResult) (arDataDependencies rightResult)
    return $ AnalysisResult
      { arExpression = expr
      , arDynamism = dynamism
      , arReasons = reasons
      , arDataDependencies = deps
      , arCanOptimize = dynamism <= SemiDynamic
      , arSuggestedOptimizations = 
          if dynamism == FullyStatic then [ConstantFolding] else []
      }
  
  CECall func args -> analyzeCallExpression expr func args
  
  CEIndex container index -> do
    containerResult <- analyzeExpression (locatedValue container)
    indexResult <- analyzeExpression (locatedValue index)
    let dynamism = max SemiDynamic $ max (arDynamism containerResult) (arDynamism indexResult)
    let reasons = if arDynamism indexResult > MostlyStatic
                  then ComplexControlFlow "Dynamic index" : arReasons containerResult ++ arReasons indexResult
                  else arReasons containerResult ++ arReasons indexResult
    return $ AnalysisResult
      { arExpression = expr
      , arDynamism = dynamism
      , arReasons = reasons
      , arDataDependencies = Set.union (arDataDependencies containerResult) (arDataDependencies indexResult)
      , arCanOptimize = dynamism <= SemiDynamic
      , arSuggestedOptimizations = []
      }
  
  CESlice container start end -> do
    containerResult <- analyzeExpression (locatedValue container)
    startResult <- maybe (return $ staticResult expr) (analyzeExpression . locatedValue) start
    endResult <- maybe (return $ staticResult expr) (analyzeExpression . locatedValue) end
    let dynamism = maximum [arDynamism containerResult, arDynamism startResult, arDynamism endResult]
    return $ AnalysisResult
      { arExpression = expr
      , arDynamism = max MostlyStatic dynamism
      , arReasons = concat [arReasons containerResult, arReasons startResult, arReasons endResult]
      , arDataDependencies = Set.unions [arDataDependencies containerResult, arDataDependencies startResult, arDataDependencies endResult]
      , arCanOptimize = dynamism <= SemiDynamic
      , arSuggestedOptimizations = []
      }
  
  CEAttribute obj attr -> do
    objResult <- analyzeExpression (locatedValue obj)
    let dynamism = max SemiDynamic (arDynamism objResult)
    let reasons = DynamicDispatch attr : arReasons objResult
    return $ AnalysisResult
      { arExpression = expr
      , arDynamism = dynamism
      , arReasons = reasons
      , arDataDependencies = arDataDependencies objResult
      , arCanOptimize = False
      , arSuggestedOptimizations = [Specialization (TypeSignature [] "Unknown")]
      }
  
  CELambda params body -> do
    withNewScope $ do
      mapM_ (`updateVariableDynamism` SemiDynamic) params
      bodyResult <- analyzeExpression (locatedValue body)
      return $ AnalysisResult
        { arExpression = expr
        , arDynamism = max SemiDynamic (arDynamism bodyResult)
        , arReasons = arReasons bodyResult
        , arDataDependencies = Set.difference (arDataDependencies bodyResult) (Set.fromList params)
        , arCanOptimize = arDynamism bodyResult <= SemiDynamic
        , arSuggestedOptimizations = [PartialEvaluation]
        }
  
  CEList elements -> do
    elementResults <- mapM (analyzeExpression . locatedValue) elements
    let dynamism = maximum (FullyStatic : map arDynamism elementResults)
    return $ AnalysisResult
      { arExpression = expr
      , arDynamism = dynamism
      , arReasons = concatMap arReasons elementResults
      , arDataDependencies = Set.unions (map arDataDependencies elementResults)
      , arCanOptimize = dynamism <= MostlyStatic
      , arSuggestedOptimizations = if dynamism == FullyStatic then [ConstantFolding] else []
      }
  
  CERecord fields -> do
    fieldResults <- mapM (analyzeExpression . locatedValue . snd) fields
    let dynamism = maximum (FullyStatic : map arDynamism fieldResults)
    return $ AnalysisResult
      { arExpression = expr
      , arDynamism = dynamism
      , arReasons = concatMap arReasons fieldResults
      , arDataDependencies = Set.unions (map arDataDependencies fieldResults)
      , arCanOptimize = dynamism <= MostlyStatic
      , arSuggestedOptimizations = []
      }

-- | Analyze a function call expression
analyzeCallExpression :: CommonExpr -> Located CommonExpr -> [Located CommonExpr] -> SmartFallbackM AnalysisResult
analyzeCallExpression expr func args = do
  funcResult <- analyzeExpression (locatedValue func)
  argResults <- mapM (analyzeExpression . locatedValue) args
  
  -- Try to get function info
  funcInfo <- case locatedValue func of
    CEVar name -> asks (HashMap.lookup name . fcFunctionDatabase)
    _ -> return Nothing
  
  let baseDynamism = maximum (arDynamism funcResult : map arDynamism argResults)
  
  case funcInfo of
    Just info -> do
      let adjustedDynamism = max (fiDynamism info) baseDynamism
      let reasons = if fiSideEffects info
                    then UnsafeOperation "Side effects" : concatMap arReasons argResults
                    else concatMap arReasons argResults
      let optimizations = if fiInlinable info && adjustedDynamism <= MostlyStatic
                          then case locatedValue func of
                                 CEVar name -> [Inlining name]
                                 _ -> []
                          else []
      return $ AnalysisResult
        { arExpression = expr
        , arDynamism = adjustedDynamism
        , arReasons = reasons
        , arDataDependencies = Set.unions (arDataDependencies funcResult : map arDataDependencies argResults)
        , arCanOptimize = fiPurity info == Pure && adjustedDynamism <= SemiDynamic
        , arSuggestedOptimizations = optimizations
        }
    
    Nothing -> do
      -- Unknown function - be conservative
      case locatedValue func of
        CEVar name -> tell [MissingFunctionInfo name]
        _ -> return ()
      
      return $ AnalysisResult
        { arExpression = expr
        , arDynamism = max SemiDynamic baseDynamism
        , arReasons = case locatedValue func of
                        CEVar name -> UnresolvedReference name : concatMap arReasons argResults
                        _ -> RuntimeReflection : concatMap arReasons argResults
        , arDataDependencies = Set.unions (arDataDependencies funcResult : map arDataDependencies argResults)
        , arCanOptimize = False
        , arSuggestedOptimizations = []
        }

-- ============================================================================
-- Optimization Application
-- ============================================================================

-- | Make optimization decision based on analysis result
makeOptimizationDecision :: AnalysisResult -> FallbackDecision
makeOptimizationDecision result@AnalysisResult{..} = FallbackDecision
  { fdExpression = arExpression
  , fdShouldFallback = not arCanOptimize || arDynamism >= HighlyDynamic
  , fdReasons = arReasons
  , fdDynamismLevel = arDynamism
  , fdAlternatives = arSuggestedOptimizations
  , fdLocation = dummySpan -- Would need actual location
  }

-- | Apply optimizations to a statement
applyOptimizations :: [FallbackDecision] -> Statement -> SmartFallbackM Statement
applyOptimizations decisions stmt = case stmt of
  SAssign var expr -> do
    optExpr <- optimizeLocatedExpr decisions expr
    return $ SAssign var optExpr
  
  SReturn (Just expr) -> do
    optExpr <- optimizeLocatedExpr decisions expr
    return $ SReturn (Just optExpr)
  
  SExpr expr -> do
    optExpr <- optimizeLocatedExpr decisions expr
    return $ SExpr optExpr
  
  SIf cond thenStmts elseStmts -> do
    optCond <- optimizeLocatedExpr decisions cond
    optThen <- mapM (applyOptimizations decisions) thenStmts
    optElse <- mapM (applyOptimizations decisions) elseStmts
    return $ SIf optCond optThen optElse
  
  SWhile cond body -> do
    optCond <- optimizeLocatedExpr decisions cond
    optBody <- mapM (applyOptimizations decisions) body
    return $ SWhile optCond optBody
  
  SFor var iterExpr body -> do
    optIter <- optimizeLocatedExpr decisions iterExpr
    optBody <- mapM (applyOptimizations decisions) body
    return $ SFor var optIter optBody
  
  SFunction name params body -> do
    optBody <- mapM (applyOptimizations decisions) body
    return $ SFunction name params optBody
  
  SBlock stmts -> do
    optStmts <- mapM (applyOptimizations decisions) stmts
    return $ SBlock optStmts
  
  other -> return other

-- | Optimize a located expression
optimizeLocatedExpr :: [FallbackDecision] -> Located CommonExpr -> SmartFallbackM (Located CommonExpr)
optimizeLocatedExpr decisions (Located span expr) = do
  optExpr <- optimizeExpression decisions expr
  return $ Located span optExpr

-- | Optimize an expression based on decisions
optimizeExpression :: [FallbackDecision] -> CommonExpr -> SmartFallbackM CommonExpr
optimizeExpression decisions expr = do
  -- Find decision for this expression
  let decision = lookup expr [(fdExpression d, d) | d <- decisions]
  
  case decision of
    Just d | fdShouldFallback d -> do
      -- Create runtime wrapper
      modify $ \s -> s 
        { fsFallbackDecisions = d : fsFallbackDecisions s
        , fsStatistics = (fsStatistics s) { asFallbackCount = asFallbackCount (fsStatistics s) + 1 }
        }
      return $ createRuntimeWrapper expr
    
    _ -> do
      -- Apply static optimization
      applyStaticOptimization expr

-- | Apply static optimization to expression
applyStaticOptimization :: CommonExpr -> SmartFallbackM CommonExpr
applyStaticOptimization = \case
  CEBinaryOp op left right -> do
    optLeft <- optimizeExpression [] (locatedValue left)
    optRight <- optimizeExpression [] (locatedValue right)
    -- Try constant folding
    case (optLeft, optRight) of
      (CELiteral (LInt a), CELiteral (LInt b)) -> 
        case op of
          Add -> return $ CELiteral (LInt (a + b))
          Sub -> return $ CELiteral (LInt (a - b))
          Mul -> return $ CELiteral (LInt (a * b))
          _ -> return $ CEBinaryOp op (Located (locSpan left) optLeft) (Located (locSpan right) optRight)
      _ -> return $ CEBinaryOp op (Located (locSpan left) optLeft) (Located (locSpan right) optRight)
  
  CEUnaryOp op operand -> do
    optOperand <- optimizeExpression [] (locatedValue operand)
    case (op, optOperand) of
      (Negate, CELiteral (LInt n)) -> return $ CELiteral (LInt (-n))
      (Not, CELiteral (LBool b)) -> return $ CELiteral (LBool (not b))
      _ -> return $ CEUnaryOp op (Located (locSpan operand) optOperand)
  
  CECall func args -> do
    optFunc <- optimizeExpression [] (locatedValue func)
    optArgs <- mapM (\arg -> do
      opt <- optimizeExpression [] (locatedValue arg)
      return $ Located (locSpan arg) opt) args
    return $ CECall (Located (locSpan func) optFunc) optArgs
  
  other -> return other

-- | Create runtime wrapper for dynamic execution
createRuntimeWrapper :: CommonExpr -> CommonExpr
createRuntimeWrapper expr = 
  CECall (Located dummySpan $ CEVar (Identifier "__runtime_execute")) 
         [Located dummySpan expr]

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Execute in a new scope
withNewScope :: SmartFallbackM a -> SmartFallbackM a
withNewScope action = do
  currentScope <- asks fcCurrentScope
  let newScope = ScopeInfo
        { siVariables = HashMap.empty
        , siParent = Just currentScope
        , siDepth = siDepth currentScope + 1
        }
  local (\ctx -> ctx { fcCurrentScope = newScope }) action

-- | Update variable dynamism in current scope
updateVariableDynamism :: Identifier -> DynamismLevel -> SmartFallbackM ()
updateVariableDynamism var level = do
  modify $ \s -> s 
    { fsGlobalDynamismMap = HashMap.insert var level (fsGlobalDynamismMap s) }

-- | Lookup variable dynamism
lookupVariableDynamism :: Identifier -> SmartFallbackM DynamismLevel
lookupVariableDynamism var = do
  globalMap <- gets fsGlobalDynamismMap
  funcDb <- asks fcFunctionDatabase
  
  -- Check if it's a known function
  case HashMap.lookup var funcDb of
    Just info -> return (fiDynamism info)
    Nothing -> return $ fromMaybe SemiDynamic (HashMap.lookup var globalMap)

-- | Update statistics
updateStatistics :: Bool -> SmartFallbackM ()
updateStatistics isHit = modify $ \s -> 
  let stats = fsStatistics s
  in s { fsStatistics = if isHit
           then stats { asCacheHits = asCacheHits stats + 1 }
           else stats { asCacheMisses = asCacheMisses stats + 1
                      , asExpressionsAnalyzed = asExpressionsAnalyzed stats + 1 }
       }

-- | Create a static result
staticResult :: CommonExpr -> AnalysisResult
staticResult expr = AnalysisResult
  { arExpression = expr
  , arDynamism = FullyStatic
  , arReasons = []
  , arDataDependencies = Set.empty
  , arCanOptimize = True
  , arSuggestedOptimizations = []
  }

-- | Dummy source span for generated code
dummySpan :: SourceSpan
dummySpan = SourceSpan "<generated>" 0 0 0 0

-- | Helper to create located value
noLoc :: a -> Located a
noLoc = Located dummySpan

-- ============================================================================
-- Public API Functions
-- ============================================================================

-- | Get fallback statistics
getFallbackStats :: SmartFallbackM (Int, Int, Double)
getFallbackStats = do
  stats <- gets fsStatistics
  decisions <- gets fsFallbackDecisions
  let total = length decisions
  let fallbacks = length $ filter fdShouldFallback decisions
  let percentage = if total > 0 
                   then fromIntegral fallbacks / fromIntegral total * 100 
                   else 0
  return (total, fallbacks, percentage)

-- | Generate analysis report
generateReport :: SmartFallbackM Text
generateReport = do
  (total, fallbacks, percentage) <- getFallbackStats
  stats <- gets fsStatistics
  warnings <- snd <$> listen (return ())
  
  return $ T.unlines
    [ "=== Smart Fallback Analysis Report ==="
    , ""
    , "Statistics:"
    , "  Expressions analyzed: " <> T.pack (show $ asExpressionsAnalyzed stats)
    , "  Statements analyzed: " <> T.pack (show $ asStatementsAnalyzed stats)
    , "  Cache hits: " <> T.pack (show $ asCacheHits stats)
    , "  Cache misses: " <> T.pack (show $ asCacheMisses stats)
    , ""
    , "Fallback Decisions:"
    , "  Total decisions: " <> T.pack (show total)
    , "  Fallbacks needed: " <> T.pack (show fallbacks)
    , "  Fallback rate: " <> T.pack (show percentage) <> "%"
    , ""
    , "Warnings: " <> T.pack (show $ length warnings)
    ]

-- | Example usage
example :: IO ()
example = do
  let program = 
        [ SFunction (Identifier "factorial") [Identifier "n"]
            [ SIf (Located dummySpan $ CEComparison Lt 
                    (Located dummySpan $ CEVar (Identifier "n"))
                    (Located dummySpan $ CELiteral (LInt 2)))
                [SReturn (Just $ Located dummySpan $ CELiteral (LInt 1))]
                [SReturn (Just $ Located dummySpan $ 
                   CEBinaryOp Mul 
                     (Located dummySpan $ CEVar (Identifier "n"))
                     (Located dummySpan $ CECall 
                       (Located dummySpan $ CEVar (Identifier "factorial"))
                       [Located dummySpan $ CEBinaryOp Sub 
                         (Located dummySpan $ CEVar (Identifier "n"))
                         (Located dummySpan $ CELiteral (LInt 1))]))]
            ]
        , SExpr (Located dummySpan $ CECall 
            (Located dummySpan $ CEVar (Identifier "print"))
            [Located dummySpan $ CECall 
              (Located dummySpan $ CEVar (Identifier "factorial"))
              [Located dummySpan $ CELiteral (LInt 5)]])
        ]
  
  case runSmartFallback (analyzeProgram program) of
    Left err -> putStrLn $ "Analysis error: " ++ show err
    Right (results, state, warnings) -> do
      putStrLn "Analysis completed successfully"
      putStrLn $ "Results: " ++ show (length results)
      putStrLn $ "Warnings: " ++ show (length warnings)
      case runSmartFallback (optimizeProgram program) of
        Left err -> putStrLn $ "Optimization error: " ++ show err
        Right (optimized, _, _) -> putStrLn $ "Optimized statements: " ++ show (length optimized)