{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Fluxus.Analysis.SmartFallback
  ( SmartFallbackM
  , FallbackDecision(..)
  , FallbackReason(..)
  , DynamismLevel(..)
  , runSmartFallback
  , analyzeDynamism
  , shouldFallbackToRuntime
  , optimizeWithFallback
  ) where

import Fluxus.AST.Common
import Control.Monad.State
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

type SmartFallbackM = ReaderT FallbackContext (State FallbackState)

-- | Context for fallback analysis
data FallbackContext = FallbackContext
  { fcOptimizationLevel :: !Int              -- Current optimization level (0-3)
  , fcFallbackThreshold :: !DynamismLevel    -- Threshold for fallback decisions
  , fcPreferPerformance :: !Bool             -- Prefer performance over compatibility
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | State for fallback analysis
data FallbackState = FallbackState
  { fsDynamismMap :: !(HashMap Identifier DynamismLevel)  -- Variable dynamism levels
  , fsFallbackDecisions :: ![FallbackDecision]            -- Made fallback decisions
  , fsStaticAnalysisLimits :: !(Set CommonExpr)           -- Expressions that exceed static analysis
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- | Level of dynamism in code
data DynamismLevel
  = FullyStatic      -- Can be fully analyzed at compile time
  | MostlyStatic     -- Mostly static with minor dynamic aspects
  | SemiDynamic      -- Mix of static and dynamic behavior
  | HighlyDynamic    -- Heavily dynamic, hard to analyze
  | FullyDynamic     -- Cannot be statically analyzed at all
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- | Reason for fallback decision
data FallbackReason
  = TypesUnknown              -- Cannot determine types at compile time
  | ComplexControlFlow        -- Complex control flow patterns
  | DynamicDispatch          -- Virtual method calls that can't be resolved
  | RuntimeReflection        -- Use of reflection or dynamic features
  | ExternalDependencies     -- Dependencies on external runtime libraries
  | PerformanceThreshold     -- Analysis would be too slow/complex
  | MemoryConstraints        -- Analysis would use too much memory
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Decision about whether to fallback to runtime
data FallbackDecision = FallbackDecision
  { fdExpression :: !CommonExpr            -- Expression being analyzed
  , fdShouldFallback :: !Bool              -- Whether to fallback
  , fdReason :: !FallbackReason            -- Reason for the decision
  , fdDynamismLevel :: !DynamismLevel      -- Assessed dynamism level
  , fdAlternatives :: ![Text]              -- Alternative optimization strategies
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Initial context
initialContext :: FallbackContext
initialContext = FallbackContext
  { fcOptimizationLevel = 2
  , fcFallbackThreshold = SemiDynamic
  , fcPreferPerformance = True
  }

-- | Initial state
initialState :: FallbackState
initialState = FallbackState
  { fsDynamismMap = HashMap.empty
  , fsFallbackDecisions = []
  , fsStaticAnalysisLimits = Set.empty
  }

-- | Run smart fallback analysis
runSmartFallback :: SmartFallbackM a -> (a, FallbackState)
runSmartFallback m = runState (runReaderT m initialContext) initialState

-- | Analyze the dynamism level of an expression
analyzeDynamism :: CommonExpr -> SmartFallbackM DynamismLevel
analyzeDynamism (CELiteral _) = return FullyStatic

analyzeDynamism (CEVar var) = do
  dynamismMap <- gets fsDynamismMap
  return $ HashMap.lookupDefault MostlyStatic var dynamismMap

analyzeDynamism (CEBinaryOp _ left right) = do
  leftDynamism <- analyzeDynamism (locatedValue left)
  rightDynamism <- analyzeDynamism (locatedValue right)
  return $ max leftDynamism rightDynamism

analyzeDynamism (CEUnaryOp _ operand) = do
  analyzeDynamism (locatedValue operand)

analyzeDynamism (CEComparison _ left right) = do
  leftDynamism <- analyzeDynamism (locatedValue left)
  rightDynamism <- analyzeDynamism (locatedValue right)
  return $ max leftDynamism rightDynamism

analyzeDynamism (CECall func args) = do
  funcDynamism <- analyzeDynamism (locatedValue func)
  argDynamisms <- mapM (analyzeDynamism . locatedValue) args
  let maxArgDynamism = maximum (FullyStatic : argDynamisms)
  -- Function calls are generally more dynamic
  return $ max SemiDynamic (max funcDynamism maxArgDynamism)

analyzeDynamism (CEIndex container index) = do
  containerDynamism <- analyzeDynamism (locatedValue container)
  indexDynamism <- analyzeDynamism (locatedValue index)
  -- Indexing can be dynamic if index is computed
  return $ max MostlyStatic (max containerDynamism indexDynamism)

analyzeDynamism (CESlice container start end) = do
  containerDynamism <- analyzeDynamism (locatedValue container)
  startDynamism <- case start of
    Just s -> analyzeDynamism (locatedValue s)
    Nothing -> return FullyStatic
  endDynamism <- case end of
    Just e -> analyzeDynamism (locatedValue e)
    Nothing -> return FullyStatic
  return $ maximum [containerDynamism, startDynamism, endDynamism]

analyzeDynamism (CEAttribute obj _) = do
  objDynamism <- analyzeDynamism (locatedValue obj)
  -- Attribute access is potentially dynamic
  return $ max SemiDynamic objDynamism

-- | Determine if code should fallback to runtime execution
shouldFallbackToRuntime :: CommonExpr -> SmartFallbackM Bool
shouldFallbackToRuntime expr = do
  dynamismLevel <- analyzeDynamism expr
  threshold <- asks fcFallbackThreshold
  
  let shouldFallback = dynamismLevel >= threshold
  
  -- Determine the reason for fallback
  reason <- if shouldFallback
    then determineReason expr dynamismLevel
    else return TypesUnknown  -- Not used if no fallback
  
  -- Record the decision
  let decision = FallbackDecision
        { fdExpression = expr
        , fdShouldFallback = shouldFallback
        , fdReason = reason
        , fdDynamismLevel = dynamismLevel
        , fdAlternatives = generateAlternatives expr
        }
  
  modify $ \s -> s { fsFallbackDecisions = decision : fsFallbackDecisions s }
  
  return shouldFallback

-- | Determine the specific reason for fallback
determineReason :: CommonExpr -> DynamismLevel -> SmartFallbackM FallbackReason
determineReason (CECall _ _) HighlyDynamic = return DynamicDispatch
determineReason (CECall _ _) FullyDynamic = return RuntimeReflection
determineReason (CEAttribute _ _) _ = return DynamicDispatch
determineReason _ HighlyDynamic = return ComplexControlFlow
determineReason _ FullyDynamic = return TypesUnknown
determineReason _ _ = return PerformanceThreshold

-- | Generate alternative optimization strategies
generateAlternatives :: CommonExpr -> [Text]
generateAlternatives (CECall _ _) = 
  [ "Use static dispatch where possible"
  , "Cache method lookups"
  , "Generate specialized versions for common cases"
  ]
generateAlternatives (CEAttribute _ _) = 
  [ "Use struct field access instead of dynamic lookup"
  , "Cache attribute offsets"
  ]
generateAlternatives _ = 
  [ "Use partial evaluation"
  , "Apply constant propagation"
  , "Generate hybrid static/dynamic code"
  ]

-- | Optimize expression with intelligent fallback
optimizeWithFallback :: CommonExpr -> SmartFallbackM CommonExpr
optimizeWithFallback expr = do
  shouldFallback <- shouldFallbackToRuntime expr
  
  if shouldFallback
    then do
      -- Wrap expression for runtime execution
      return $ createRuntimeWrapper expr
    else do
      -- Apply static optimizations
      optimizeStatically expr

-- | Create a runtime wrapper for dynamic execution
createRuntimeWrapper :: CommonExpr -> CommonExpr
createRuntimeWrapper expr = 
  -- This would create a wrapper that delegates to the original runtime
  CECall (noLoc $ CEVar (Identifier "runtime_execute")) [noLoc expr]

-- | Apply static optimizations to expression
optimizeStatically :: CommonExpr -> SmartFallbackM CommonExpr
optimizeStatically (CEBinaryOp op left right) = do
  optimizedLeft <- optimizeWithFallback (locatedValue left)
  optimizedRight <- optimizeWithFallback (locatedValue right)
  return $ CEBinaryOp op (Located (locSpan left) optimizedLeft) (Located (locSpan right) optimizedRight)

optimizeStatically (CEUnaryOp op operand) = do
  optimizedOperand <- optimizeWithFallback (locatedValue operand)
  return $ CEUnaryOp op (Located (locSpan operand) optimizedOperand)

optimizeStatically (CECall func args) = do
  optimizedFunc <- optimizeWithFallback (locatedValue func)
  optimizedArgs <- mapM (\arg -> do
    opt <- optimizeWithFallback (locatedValue arg)
    return $ Located (locSpan arg) opt) args
  return $ CECall (Located (locSpan func) optimizedFunc) optimizedArgs

optimizeStatically expr = return expr  -- Keep other expressions as-is

-- | Set dynamism level for a variable
setVariableDynamism :: Identifier -> DynamismLevel -> SmartFallbackM ()
setVariableDynamism var level = do
  modify $ \s -> s { fsDynamismMap = HashMap.insert var level (fsDynamismMap s) }

-- | Mark expression as exceeding static analysis limits
markStaticAnalysisLimit :: CommonExpr -> SmartFallbackM ()
markStaticAnalysisLimit expr = do
  modify $ \s -> s { fsStaticAnalysisLimits = Set.insert expr (fsStaticAnalysisLimits s) }

-- | Get all fallback decisions made
getFallbackDecisions :: SmartFallbackM [FallbackDecision]
getFallbackDecisions = gets fsFallbackDecisions

-- | Get statistics about fallback usage
getFallbackStats :: SmartFallbackM (Int, Int, Double)
getFallbackStats = do
  decisions <- gets fsFallbackDecisions
  let total = length decisions
  let fallbacks = length $ filter fdShouldFallback decisions
  let percentage = if total > 0 then fromIntegral fallbacks / fromIntegral total * 100 else 0
  return (total, fallbacks, percentage)