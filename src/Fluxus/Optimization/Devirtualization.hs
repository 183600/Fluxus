{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Fluxus.Optimization.Devirtualization
  ( DevirtualizationM
  , DevirtualizationState(..)
  , DevirtualizationResult(..)
  , VirtualCallInfo(..)
  , runDevirtualization
  , devirtualize
  , resolveVirtualCall
  , analyzeCallSites
  , optimizeDispatch
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

type DevirtualizationM = ReaderT DevirtualizationContext (State DevirtualizationState)

-- | Context for devirtualization
data DevirtualizationContext = DevirtualizationContext
  { dcCurrentFunction :: !(Maybe QualifiedName)
  , dcTypeEnvironment :: !(HashMap Identifier Type)    -- Known variable types
  , dcClassHierarchy :: !(HashMap QualifiedName [QualifiedName])  -- Inheritance relationships
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | State for devirtualization analysis
data DevirtualizationState = DevirtualizationState
  { dsVirtualCalls :: !(HashMap QualifiedName [VirtualCallInfo])  -- Virtual call sites
  , dsResolvedCalls :: !(HashMap QualifiedName QualifiedName)     -- Resolved virtual calls
  , dsCallFrequency :: !(HashMap QualifiedName Int)              -- Call frequency analysis
  , dsTypeConstraints :: !(HashMap Identifier (Set Type))        -- Type constraints for variables
  , dsOptimizations :: ![Text]                                   -- Applied optimizations
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- | Information about a virtual call site
data VirtualCallInfo = VirtualCallInfo
  { vciCallSite :: !QualifiedName          -- Location of the call
  , vciReceiver :: !Identifier             -- Receiver variable
  , vciMethod :: !Identifier               -- Method being called
  , vciPossibleTypes :: !(Set Type)        -- Possible receiver types
  , vciIsResolvable :: !Bool               -- Whether call can be resolved statically
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Result of devirtualization
data DevirtualizationResult = DevirtualizationResult
  { drExpression :: !CommonExpr
  , drOptimizations :: ![Text]             -- Applied optimizations
  , drResolvedCalls :: !Int                -- Number of resolved virtual calls
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Initial context
initialContext :: DevirtualizationContext
initialContext = DevirtualizationContext
  { dcCurrentFunction = Nothing
  , dcTypeEnvironment = HashMap.empty
  , dcClassHierarchy = HashMap.empty
  }

-- | Initial state
initialState :: DevirtualizationState
initialState = DevirtualizationState
  { dsVirtualCalls = HashMap.empty
  , dsResolvedCalls = HashMap.empty
  , dsCallFrequency = HashMap.empty
  , dsTypeConstraints = HashMap.empty
  , dsOptimizations = []
  }

-- | Run devirtualization
runDevirtualization :: DevirtualizationM a -> (a, DevirtualizationState)
runDevirtualization m = runState (runReaderT m initialContext) initialState

-- | Devirtualize an expression by resolving virtual calls
devirtualize :: CommonExpr -> DevirtualizationM DevirtualizationResult
devirtualize expr = do
  -- First pass: analyze call sites and collect type information
  analyzeCallSites expr
  
  -- Second pass: resolve virtual calls where possible
  resolvedExpr <- optimizeDispatch expr
  
  -- Collect optimization statistics
  resolvedCount <- length <$> gets dsResolvedCalls
  optimizations <- gets dsOptimizations
  
  return $ DevirtualizationResult resolvedExpr optimizations resolvedCount

-- | Analyze call sites and collect information for devirtualization
analyzeCallSites :: CommonExpr -> DevirtualizationM ()
analyzeCallSites (CECall func args) = do
  case locatedValue func of
    CEAttribute obj methodName -> do
      -- This is a method call - potential virtual call
      case locatedValue obj of
        CEVar receiverVar -> do
          -- Record this as a potential virtual call
          let callInfo = VirtualCallInfo
                { vciCallSite = QualifiedName [] methodName
                , vciReceiver = receiverVar
                , vciMethod = methodName
                , vciPossibleTypes = Set.empty  -- Will be filled by type analysis
                , vciIsResolvable = False
                }
          recordVirtualCall methodName callInfo
        _ -> return ()
    _ -> return ()
  
  -- Recursively analyze arguments
  mapM_ (analyzeCallSites . locatedValue) args

analyzeCallSites (CEBinaryOp _ left right) = do
  analyzeCallSites (locatedValue left)
  analyzeCallSites (locatedValue right)

analyzeCallSites (CEUnaryOp _ operand) = do
  analyzeCallSites (locatedValue operand)

analyzeCallSites (CEComparison _ left right) = do
  analyzeCallSites (locatedValue left)
  analyzeCallSites (locatedValue right)

analyzeCallSites (CEIndex container index) = do
  analyzeCallSites (locatedValue container)
  analyzeCallSites (locatedValue index)

analyzeCallSites (CESlice container start end) = do
  analyzeCallSites (locatedValue container)
  maybe (return ()) (analyzeCallSites . locatedValue) start
  maybe (return ()) (analyzeCallSites . locatedValue) end

analyzeCallSites (CEAttribute obj _) = do
  analyzeCallSites (locatedValue obj)

analyzeCallSites _ = return ()

-- | Record a virtual call for analysis
recordVirtualCall :: Identifier -> VirtualCallInfo -> DevirtualizationM ()
recordVirtualCall methodName callInfo = do
  let qualifiedName = QualifiedName [] methodName  -- Convert to QualifiedName
  modify $ \s -> s { dsVirtualCalls = HashMap.insertWith (++) qualifiedName [callInfo] (dsVirtualCalls s) }

-- | Optimize dispatch by resolving virtual calls where possible
optimizeDispatch :: CommonExpr -> DevirtualizationM CommonExpr
optimizeDispatch (CECall func args) = do
  case locatedValue func of
    CEAttribute obj methodName -> do
      -- Check if this virtual call can be resolved
      case locatedValue obj of
        CEVar receiverVar -> do
          resolvedMethod <- resolveVirtualCall methodName receiverVar
          case resolvedMethod of
            Just actualMethod -> do
              -- Replace with direct call
              let directCall = CEVar (case actualMethod of QualifiedName _ name -> name)
              optimizedArgs <- mapM (\arg -> do
                optimizedExpr <- optimizeDispatch (locatedValue arg)
                return $ Located (locSpan arg) optimizedExpr) args
              return $ CECall (Located (locSpan func) directCall) optimizedArgs
            Nothing -> do
              -- Cannot resolve - keep as virtual call
              optimizedObj <- fmap (Located (locSpan obj)) $ optimizeDispatch (locatedValue obj)
              optimizedArgs <- mapM (\arg -> do
                optimizedExpr <- optimizeDispatch (locatedValue arg)
                return $ Located (locSpan arg) optimizedExpr) args
              return $ CECall (Located (locSpan func) (CEAttribute optimizedObj methodName)) optimizedArgs
        _ -> do
          -- Complex receiver expression
          optimizedFunc <- fmap (Located (locSpan func)) $ optimizeDispatch (locatedValue func)
          optimizedArgs <- mapM (\arg -> do
            optimizedExpr <- optimizeDispatch (locatedValue arg)
            return $ Located (locSpan arg) optimizedExpr) args
          return $ CECall optimizedFunc optimizedArgs
    _ -> do
      -- Regular function call
      optimizedFunc <- fmap (Located (locSpan func)) $ optimizeDispatch (locatedValue func)
      optimizedArgs <- mapM (\arg -> do
        optimizedExpr <- optimizeDispatch (locatedValue arg)
        return $ Located (locSpan arg) optimizedExpr) args
      return $ CECall optimizedFunc optimizedArgs

optimizeDispatch (CEBinaryOp op left right) = do
  optimizedLeft <- fmap (Located (locSpan left)) $ optimizeDispatch (locatedValue left)
  optimizedRight <- fmap (Located (locSpan right)) $ optimizeDispatch (locatedValue right)
  return $ CEBinaryOp op optimizedLeft optimizedRight

optimizeDispatch (CEUnaryOp op operand) = do
  optimizedOperand <- fmap (Located (locSpan operand)) $ optimizeDispatch (locatedValue operand)
  return $ CEUnaryOp op optimizedOperand

optimizeDispatch (CEComparison op left right) = do
  optimizedLeft <- fmap (Located (locSpan left)) $ optimizeDispatch (locatedValue left)
  optimizedRight <- fmap (Located (locSpan right)) $ optimizeDispatch (locatedValue right)
  return $ CEComparison op optimizedLeft optimizedRight

optimizeDispatch (CEIndex container index) = do
  optimizedContainer <- fmap (Located (locSpan container)) $ optimizeDispatch (locatedValue container)
  optimizedIndex <- fmap (Located (locSpan index)) $ optimizeDispatch (locatedValue index)
  return $ CEIndex optimizedContainer optimizedIndex

optimizeDispatch (CESlice container start end) = do
  optimizedContainer <- fmap (Located (locSpan container)) $ optimizeDispatch (locatedValue container)
  optimizedStart <- case start of
    Just s -> fmap (Just . Located (locSpan s)) $ optimizeDispatch (locatedValue s)
    Nothing -> return Nothing
  optimizedEnd <- case end of
    Just e -> fmap (Just . Located (locSpan e)) $ optimizeDispatch (locatedValue e)
    Nothing -> return Nothing
  return $ CESlice optimizedContainer optimizedStart optimizedEnd

optimizeDispatch (CEAttribute obj attr) = do
  optimizedObj <- fmap (Located (locSpan obj)) $ optimizeDispatch (locatedValue obj)
  return $ CEAttribute optimizedObj attr

optimizeDispatch expr = return expr  -- Literals and variables unchanged

-- | Resolve a virtual call if possible
resolveVirtualCall :: Identifier -> Identifier -> DevirtualizationM (Maybe QualifiedName)
resolveVirtualCall methodName receiverVar = do
  -- Look up the type of the receiver variable
  typeEnv <- asks dcTypeEnvironment
  case HashMap.lookup receiverVar typeEnv of
    Just receiverType -> do
      -- Check if we can resolve the method for this type
      resolvedMethod <- resolveMethodForType methodName receiverType
      case resolvedMethod of
        Just method -> do
          -- Record the successful resolution
          let qualifiedName = QualifiedName [] methodName  -- Convert to QualifiedName
          modify $ \s -> s { dsResolvedCalls = HashMap.insert qualifiedName method (dsResolvedCalls s) }
          return $ Just method
        Nothing -> return Nothing
    Nothing -> return Nothing

-- | Resolve a method for a specific type
resolveMethodForType :: Identifier -> Type -> DevirtualizationM (Maybe QualifiedName)
resolveMethodForType methodName receiverType = do
  case receiverType of
    TStruct className _ -> do
      -- For struct types, we can often resolve methods directly
      let methodQualName = QualifiedName [ModuleName $ T.pack $ show className] methodName
      return $ Just methodQualName
    TInterface _ _ -> do
      -- Interface types require more complex analysis
      return Nothing
    _ -> return Nothing

