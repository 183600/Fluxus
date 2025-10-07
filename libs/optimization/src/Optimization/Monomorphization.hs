{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Fluxus.Optimization.Monomorphization
  ( MonomorphizationM
  , MonomorphizationState(..)
  , MonomorphizationResult(..)
  , runMonomorphization
  , monomorphize
  , specializeFunction
  , generateSpecializations
  , collectTypeInstantiations
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

type MonomorphizationM = ReaderT MonomorphizationContext (State MonomorphizationState)

-- | Context for monomorphization
data MonomorphizationContext = MonomorphizationContext
  { mcCurrentFunction :: !(Maybe QualifiedName)
  , mcTypeStack :: ![Type]                    -- Stack of type contexts
  , mcOptimizeForSize :: !Bool                -- Optimize for code size vs speed
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | State for monomorphization
data MonomorphizationState = MonomorphizationState
  { msSpecializations :: !(HashMap QualifiedName (Set [Type]))  -- Function to type instantiations
  , msGeneratedFunctions :: !(HashMap (QualifiedName, [Type]) QualifiedName)  -- Generated specialized functions
  , msTypeInstantiations :: !(HashMap QualifiedName [Type])     -- Current type instantiations
  , msCallGraph :: !(HashMap QualifiedName (Set QualifiedName)) -- Function call dependencies
  , msSpecializationQueue :: ![(QualifiedName, [Type])]         -- Functions to specialize
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- | Result of monomorphization
data MonomorphizationResult = MonomorphizationResult
  { mrExpression :: !CommonExpr
  , mrSpecializations :: !(HashMap QualifiedName [CommonExpr])  -- Generated specialized functions
  , mrOptimizations :: ![Text]                                 -- Applied optimizations
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Initial context
initialContext :: MonomorphizationContext
initialContext = MonomorphizationContext
  { mcCurrentFunction = Nothing
  , mcTypeStack = []
  , mcOptimizeForSize = False
  }

-- | Initial state
initialState :: MonomorphizationState
initialState = MonomorphizationState
  { msSpecializations = HashMap.empty
  , msGeneratedFunctions = HashMap.empty
  , msTypeInstantiations = HashMap.empty
  , msCallGraph = HashMap.empty
  , msSpecializationQueue = []
  }

-- | Run monomorphization
runMonomorphization :: MonomorphizationM a -> (a, MonomorphizationState)
runMonomorphization m = runState (runReaderT m initialContext) initialState

-- | Monomorphize an expression by generating type-specialized versions
monomorphize :: CommonExpr -> MonomorphizationM MonomorphizationResult
monomorphize expr = do
  -- First pass: collect type instantiations
  collectTypeInstantiations expr
  
  -- Second pass: generate specializations
  specializations <- generateSpecializations
  
  -- Third pass: transform expression to use specialized functions
  transformedExpr <- transformExpression expr
  
  let optimizations = ["Generated monomorphic versions for polymorphic functions"]
  
  return $ MonomorphizationResult transformedExpr specializations optimizations

-- | Collect type instantiations from expressions
collectTypeInstantiations :: CommonExpr -> MonomorphizationM ()
collectTypeInstantiations (CECall func args) = do
  -- For function calls, try to infer the type instantiation
  case locatedValue func of
    CEVar (Identifier funcName) -> do
      -- Collect argument types (simplified - would need full type inference)
      argTypes <- mapM inferExpressionType args
      let qualName = QualifiedName [] (Identifier funcName)
      recordTypeInstantiation qualName argTypes
    _ -> return ()
  
  -- Recursively collect from arguments
  mapM_ (collectTypeInstantiations . locatedValue) args

collectTypeInstantiations (CEBinaryOp _ left right) = do
  collectTypeInstantiations (locatedValue left)
  collectTypeInstantiations (locatedValue right)

collectTypeInstantiations (CEUnaryOp _ operand) = do
  collectTypeInstantiations (locatedValue operand)

collectTypeInstantiations (CEComparison _ left right) = do
  collectTypeInstantiations (locatedValue left)
  collectTypeInstantiations (locatedValue right)

collectTypeInstantiations (CEIndex container index) = do
  collectTypeInstantiations (locatedValue container)
  collectTypeInstantiations (locatedValue index)

collectTypeInstantiations (CESlice container start end) = do
  collectTypeInstantiations (locatedValue container)
  maybe (return ()) (collectTypeInstantiations . locatedValue) start
  maybe (return ()) (collectTypeInstantiations . locatedValue) end

collectTypeInstantiations (CEAttribute obj _) = do
  collectTypeInstantiations (locatedValue obj)

collectTypeInstantiations _ = return ()

-- | Record a type instantiation for a function
recordTypeInstantiation :: QualifiedName -> [Type] -> MonomorphizationM ()
recordTypeInstantiation funcName types = do
  modify $ \s -> s 
    { msSpecializations = HashMap.insertWith Set.union funcName (Set.singleton types) (msSpecializations s)
    , msSpecializationQueue = (funcName, types) : msSpecializationQueue s
    }

-- | Generate all required specializations
generateSpecializations :: MonomorphizationM (HashMap QualifiedName [CommonExpr])
generateSpecializations = do
  queue <- gets msSpecializationQueue
  specializations <- mapM (uncurry generateSpecialization) queue
  return $ HashMap.fromList specializations

-- | Generate a single specialization
generateSpecialization :: QualifiedName -> [Type] -> MonomorphizationM (QualifiedName, [CommonExpr])
generateSpecialization funcName types = do
  -- Generate specialized function name
  let specializedName = createSpecializedName funcName types
  
  -- Record the mapping
  modify $ \s -> s { msGeneratedFunctions = HashMap.insert (funcName, types) specializedName (msGeneratedFunctions s) }
  
  -- Generate specialized function body (placeholder)
  let specializedBody = [CELiteral (LString "specialized function")]
  
  return (specializedName, specializedBody)

-- | Create a specialized function name
createSpecializedName :: QualifiedName -> [Type] -> QualifiedName
createSpecializedName (QualifiedName modules (Identifier name)) types =
  let typeStr = T.intercalate "_" $ map typeToString types
      specializedName = name <> "_" <> typeStr
  in QualifiedName modules (Identifier specializedName)

-- | Convert type to string for naming
typeToString :: Type -> Text
typeToString (TInt size) = "i" <> T.pack (show size)
typeToString (TFloat size) = "f" <> T.pack (show size)
typeToString TBool = "bool"
typeToString TString = "str"
typeToString (TList t) = "list_" <> typeToString t
typeToString (TDict k v) = "dict_" <> typeToString k <> "_" <> typeToString v
typeToString _ = "any"

-- | Transform expression to use specialized functions
transformExpression :: CommonExpr -> MonomorphizationM CommonExpr
transformExpression expr@(CECall func args) = do
  case locatedValue func of
    CEVar (Identifier funcName) -> do
      -- Look up specialized version
      argTypes <- mapM inferExpressionType args
      let qualName = QualifiedName [] (Identifier funcName)
      specializedName <- lookupSpecialization qualName argTypes
      
      -- Transform arguments recursively
      transformedArgs <- mapM (\arg -> fmap (Located (locSpan arg)) $ transformExpression (locatedValue arg)) args
      
      -- Use specialized function if available
      case specializedName of
        Just specName -> do
          let specVar = CEVar (case specName of QualifiedName _ (Identifier n) -> Identifier n)
          return $ CECall (Located (locSpan func) specVar) transformedArgs
        Nothing -> do
          let transformedFunc = Located (locSpan func) (CEVar (Identifier funcName))
          return $ CECall transformedFunc transformedArgs
    _ -> do
      -- Transform function expression and arguments
      transformedFunc <- fmap (Located (locSpan func)) $ transformExpression (locatedValue func)
      transformedArgs <- mapM (\arg -> fmap (Located (locSpan arg)) $ transformExpression (locatedValue arg)) args
      return $ CECall transformedFunc transformedArgs

transformExpression (CEBinaryOp op left right) = do
  transformedLeft <- fmap (Located (locSpan left)) $ transformExpression (locatedValue left)
  transformedRight <- fmap (Located (locSpan right)) $ transformExpression (locatedValue right)
  return $ CEBinaryOp op transformedLeft transformedRight

transformExpression (CEUnaryOp op operand) = do
  transformedOperand <- fmap (Located (locSpan operand)) $ transformExpression (locatedValue operand)
  return $ CEUnaryOp op transformedOperand

transformExpression (CEComparison op left right) = do
  transformedLeft <- fmap (Located (locSpan left)) $ transformExpression (locatedValue left)
  transformedRight <- fmap (Located (locSpan right)) $ transformExpression (locatedValue right)
  return $ CEComparison op transformedLeft transformedRight

transformExpression (CEIndex container index) = do
  transformedContainer <- fmap (Located (locSpan container)) $ transformExpression (locatedValue container)
  transformedIndex <- fmap (Located (locSpan index)) $ transformExpression (locatedValue index)
  return $ CEIndex transformedContainer transformedIndex

transformExpression (CESlice container start end) = do
  transformedContainer <- fmap (Located (locSpan container)) $ transformExpression (locatedValue container)
  transformedStart <- case start of
    Just s -> fmap (Just . Located (locSpan s)) $ transformExpression (locatedValue s)
    Nothing -> return Nothing
  transformedEnd <- case end of
    Just e -> fmap (Just . Located (locSpan e)) $ transformExpression (locatedValue e)
    Nothing -> return Nothing
  return $ CESlice transformedContainer transformedStart transformedEnd

transformExpression (CEAttribute obj attr) = do
  transformedObj <- fmap (Located (locSpan obj)) $ transformExpression (locatedValue obj)
  return $ CEAttribute transformedObj attr

transformExpression expr = return expr  -- Literals and variables unchanged

-- | Look up specialized version of a function
lookupSpecialization :: QualifiedName -> [Type] -> MonomorphizationM (Maybe QualifiedName)
lookupSpecialization funcName types = do
  generated <- gets msGeneratedFunctions
  return $ HashMap.lookup (funcName, types) generated

-- | Specialize a function for given types
specializeFunction :: QualifiedName -> [Type] -> MonomorphizationM QualifiedName
specializeFunction funcName types = do
  existing <- lookupSpecialization funcName types
  case existing of
    Just name -> return name
    Nothing -> do
      let specializedName = createSpecializedName funcName types
      modify $ \s -> s { msGeneratedFunctions = HashMap.insert (funcName, types) specializedName (msGeneratedFunctions s) }
      return specializedName

-- | Infer type of expression (simplified)
inferExpressionType :: Located CommonExpr -> MonomorphizationM Type
inferExpressionType (Located _ (CELiteral (LInt _))) = return $ TInt 32
inferExpressionType (Located _ (CELiteral (LFloat _))) = return $ TFloat 64
inferExpressionType (Located _ (CELiteral (LString _))) = return TString
inferExpressionType (Located _ (CELiteral (LBool _))) = return TBool
inferExpressionType _ = return TAny  -- Fallback
