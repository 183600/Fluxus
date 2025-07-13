{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Fluxus.Analysis.TypeInference
  ( TypeInferenceM
  , TypeConstraints
  , InferenceResult(..)
  , TypeInferenceState(..)
  , TypeEnvironment
  , runTypeInference
  , inferType
  , inferExpr
  , unifyTypes
  , instantiate
  , generalize
  , freshTypeVar
  , applySubstitution
  , solveConstraints
  , lookupVarType
  , bindVarType
  ) where

import Fluxus.AST.Common
import Control.Monad.State
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

type TypeInferenceM = StateT TypeInferenceState (Except Text)
type TypeConstraints = [(Type, Type)]
type TypeEnvironment = HashMap Identifier Type
type Substitution = HashMap TypeVar Type

data TypeInferenceState = TypeInferenceState
  { nextTyVar :: !Int
  , constraints :: !TypeConstraints
  , substitutions :: !Substitution
  , typeEnv :: !TypeEnvironment
  , currentScope :: ![TypeEnvironment]  -- Stack of scopes for nested contexts
  } deriving (Show, Generic)

data InferenceResult = InferenceResult
  { resultType :: !Type
  , resultConstraints :: !TypeConstraints
  , resultSubstitutions :: !Substitution
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Run type inference with initial environment
runTypeInference :: TypeEnvironment -> TypeInferenceM a -> Either Text a
runTypeInference env m = runExcept $ evalStateT m initialState
  where
    initialState = TypeInferenceState
      { nextTyVar = 0
      , constraints = []
      , substitutions = HashMap.empty
      , typeEnv = env
      , currentScope = []
      }

-- | Generate a fresh type variable
freshTypeVar :: TypeInferenceM Type
freshTypeVar = do
  s <- get
  let n = nextTyVar s
  put s { nextTyVar = n + 1 }
  return $ TVar (TypeVar $ "t" <> T.pack (show n))

-- | Add a constraint
addConstraint :: Type -> Type -> TypeInferenceM ()
addConstraint t1 t2 = modify $ \s -> s { constraints = (t1, t2) : constraints s }

-- | Look up variable type in current environment
lookupVarType :: Identifier -> TypeInferenceM Type
lookupVarType var = do
  env <- gets typeEnv
  case HashMap.lookup var env of
    Just t -> return t
    Nothing -> throwError $ "Undefined variable: " <> T.pack (show var)

-- | Bind variable to type in current environment
bindVarType :: Identifier -> Type -> TypeInferenceM ()
bindVarType var t = modify $ \s -> s { typeEnv = HashMap.insert var t (typeEnv s) }

-- | Push new scope for nested contexts (functions, blocks, etc.)
pushScope :: TypeInferenceM ()
pushScope = do
  env <- gets typeEnv
  modify $ \s -> s { currentScope = env : currentScope s }

-- | Pop scope and restore previous environment
popScope :: TypeInferenceM ()
popScope = do
  scopes <- gets currentScope
  case scopes of
    [] -> throwError "Cannot pop scope: no scopes on stack"
    (prev:rest) -> modify $ \s -> s { typeEnv = prev, currentScope = rest }

-- | Main type inference function for expressions
inferType :: CommonExpr -> TypeInferenceM InferenceResult
inferType expr = do
  t <- inferExpr expr
  constraints <- gets constraints
  substitutions <- gets substitutions
  return $ InferenceResult t constraints substitutions

-- | Infer type of expressions
inferExpr :: CommonExpr -> TypeInferenceM Type
inferExpr (CELiteral lit) = inferLiteral lit
inferExpr (CEVar var) = lookupVarType var
inferExpr (CEBinaryOp op left right) = do
  leftType <- inferExpr (locatedValue left)
  rightType <- inferExpr (locatedValue right)
  inferBinaryOp op leftType rightType
inferExpr (CEUnaryOp op operand) = do
  operandType <- inferExpr (locatedValue operand)
  inferUnaryOp op operandType
inferExpr (CEComparison op left right) = do
  leftType <- inferExpr (locatedValue left)
  rightType <- inferExpr (locatedValue right)
  addConstraint leftType rightType  -- Both operands should have same type
  return TBool
inferExpr (CECall func args) = do
  funcType <- inferExpr (locatedValue func)
  argTypes <- mapM (inferExpr . locatedValue) args
  resultType <- freshTypeVar
  let expectedFuncType = TFunction argTypes resultType
  addConstraint funcType expectedFuncType
  return resultType
inferExpr (CEIndex container index) = do
  containerType <- inferExpr (locatedValue container)
  indexType <- inferExpr (locatedValue index)
  elementType <- freshTypeVar
  -- Handle different container types
  case containerType of
    TList elemT -> do
      addConstraint indexType (TInt 32)  -- Index should be int
      addConstraint elementType elemT
    TDict keyT valT -> do
      addConstraint indexType keyT
      addConstraint elementType valT
    TString -> do
      addConstraint indexType (TInt 32)
      addConstraint elementType TChar
    _ -> do
      -- Generic indexable type
      addConstraint containerType (TList elementType)
      addConstraint indexType (TInt 32)
  return elementType
inferExpr (CESlice container start end) = do
  containerType <- inferExpr (locatedValue container)
  -- Infer slice bounds if present
  case start of
    Just startExpr -> do
      startType <- inferExpr (locatedValue startExpr)
      addConstraint startType (TInt 32)
    Nothing -> return ()
  case end of
    Just endExpr -> do
      endType <- inferExpr (locatedValue endExpr)
      addConstraint endType (TInt 32)
    Nothing -> return ()
  -- Slice result has same type as container
  return containerType
inferExpr (CEAttribute obj attr) = do
  objType <- inferExpr (locatedValue obj)
  -- For now, return Any for attribute access (could be improved with struct/class analysis)
  return TAny

-- | Infer type of literals
inferLiteral :: Literal -> TypeInferenceM Type
inferLiteral (LInt _) = return $ TInt 32  -- Default to 32-bit int
inferLiteral (LUInt _) = return $ TUInt 32
inferLiteral (LFloat _) = return $ TFloat 64  -- Default to double
inferLiteral (LBool _) = return TBool
inferLiteral (LString _) = return TString
inferLiteral (LBytes _) = return TBytes
inferLiteral (LChar _) = return TChar
inferLiteral LNone = return $ TOptional TAny  -- None can be any optional type

-- | Infer type of binary operations
inferBinaryOp :: BinaryOp -> Type -> Type -> TypeInferenceM Type
inferBinaryOp op leftType rightType = case op of
  -- Arithmetic operations
  OpAdd -> inferArithmeticOp leftType rightType
  OpSub -> inferArithmeticOp leftType rightType
  OpMul -> inferArithmeticOp leftType rightType
  OpDiv -> inferArithmeticOp leftType rightType
  OpMod -> inferArithmeticOp leftType rightType
  OpPow -> inferArithmeticOp leftType rightType
  OpFloorDiv -> inferArithmeticOp leftType rightType
  
  -- Bitwise operations (require integer types)
  OpBitAnd -> inferBitwiseOp leftType rightType
  OpBitOr -> inferBitwiseOp leftType rightType
  OpBitXor -> inferBitwiseOp leftType rightType
  OpShiftL -> inferBitwiseOp leftType rightType
  OpShiftR -> inferBitwiseOp leftType rightType
  
  -- Logical operations (require bool types)
  OpAnd -> do
    addConstraint leftType TBool
    addConstraint rightType TBool
    return TBool
  OpOr -> do
    addConstraint leftType TBool
    addConstraint rightType TBool
    return TBool
  
  -- String/list concatenation
  OpConcat -> do
    -- Could be string + string or list + list
    addConstraint leftType rightType
    return leftType
  
  -- Membership testing
  OpIn -> do
    -- right should be a container type containing elements of leftType
    case rightType of
      TList elemType -> addConstraint leftType elemType
      TSet elemType -> addConstraint leftType elemType
      TDict keyType _ -> addConstraint leftType keyType
      TString -> addConstraint leftType TChar
      _ -> return ()  -- Generic container
    return TBool
  OpNotIn -> do
    case rightType of
      TList elemType -> addConstraint leftType elemType
      TSet elemType -> addConstraint leftType elemType
      TDict keyType _ -> addConstraint leftType keyType
      TString -> addConstraint leftType TChar
      _ -> return ()
    return TBool

-- | Helper for arithmetic operations
inferArithmeticOp :: Type -> Type -> TypeInferenceM Type
inferArithmeticOp leftType rightType = do
  addConstraint leftType rightType  -- Both operands should have same type
  -- Return the same type (with some numeric promotion rules)
  case (leftType, rightType) of
    (TFloat _, _) -> return leftType
    (_, TFloat _) -> return rightType
    (TInt _, TInt _) -> return leftType
    (TUInt _, TUInt _) -> return leftType
    _ -> return leftType  -- Fallback

-- | Helper for bitwise operations
inferBitwiseOp :: Type -> Type -> TypeInferenceM Type
inferBitwiseOp leftType rightType = do
  -- Ensure both operands are integer types
  case leftType of
    TInt _ -> return ()
    TUInt _ -> return ()
    _ -> throwError "Bitwise operations require integer types"
  case rightType of
    TInt _ -> return ()
    TUInt _ -> return ()
    _ -> throwError "Bitwise operations require integer types"
  addConstraint leftType rightType
  return leftType

-- | Infer type of unary operations
inferUnaryOp :: UnaryOp -> Type -> TypeInferenceM Type
inferUnaryOp OpNot operandType = do
  addConstraint operandType TBool
  return TBool
inferUnaryOp OpNegate operandType = do
  -- Negate requires numeric type
  case operandType of
    TInt _ -> return operandType
    TFloat _ -> return operandType
    _ -> throwError "Negate operation requires numeric type"
inferUnaryOp OpBitNot operandType = do
  -- Bitwise not requires integer type
  case operandType of
    TInt _ -> return operandType
    TUInt _ -> return operandType
    _ -> throwError "Bitwise not operation requires integer type"
inferUnaryOp OpPositive operandType = do
  -- Unary plus requires numeric type
  case operandType of
    TInt _ -> return operandType
    TFloat _ -> return operandType
    _ -> throwError "Unary plus operation requires numeric type"

-- | Unify two types and generate constraints
unifyTypes :: Type -> Type -> TypeInferenceM (Maybe TypeConstraints)
unifyTypes t1 t2 = do
  result <- unify t1 t2
  case result of
    Left _ -> return Nothing
    Right constraints -> return (Just constraints)

-- | Core unification algorithm
unify :: Type -> Type -> TypeInferenceM (Either Text TypeConstraints)
unify t1 t2 | t1 == t2 = return $ Right []
unify (TVar v1) t2 = do
  if occurs v1 t2
    then return $ Left "Occurs check failed"
    else return $ Right [(TVar v1, t2)]
unify t1 (TVar v2) = unify (TVar v2) t1
unify (TList t1) (TList t2) = unify t1 t2
unify (TTuple ts1) (TTuple ts2)
  | length ts1 == length ts2 = do
      results <- mapM (uncurry unify) (zip ts1 ts2)
      let constraints = concat [cs | Right cs <- results]
      if any isLeft results
        then return $ Left "Tuple unification failed"
        else return $ Right constraints
  | otherwise = return $ Left "Tuple arity mismatch"
unify (TDict k1 v1) (TDict k2 v2) = do
  keyResult <- unify k1 k2
  valueResult <- unify v1 v2
  case (keyResult, valueResult) of
    (Right kcs, Right vcs) -> return $ Right (kcs ++ vcs)
    _ -> return $ Left "Dictionary unification failed"
unify (TFunction args1 ret1) (TFunction args2 ret2)
  | length args1 == length args2 = do
      argResults <- mapM (uncurry unify) (zip args1 args2)
      retResult <- unify ret1 ret2
      let argConstraints = concat [cs | Right cs <- argResults]
      case retResult of
        Right retConstraints -> return $ Right (argConstraints ++ retConstraints)
        Left err -> return $ Left err
  | otherwise = return $ Left "Function arity mismatch"
unify (TOptional t1) (TOptional t2) = unify t1 t2
unify t1 t2 = return $ Left $ "Cannot unify " <> T.pack (show t1) <> " with " <> T.pack (show t2)

-- | Check if type variable occurs in type (prevents infinite types)
occurs :: TypeVar -> Type -> Bool
occurs var = go
  where
    go (TVar v) = var == v
    go (TList t) = go t
    go (TTuple ts) = any go ts
    go (TDict k v) = go k || go v
    go (TSet t) = go t
    go (TOptional t) = go t
    go (TFunction args ret) = any go args || go ret
    go (TMethod rec args ret) = go rec || any go args || go ret
    go (TStruct _ args) = any go args
    go (TEnum _ args) = any go args
    go (TInterface _ args) = any go args
    go (TUnion ts) = any go ts
    go (TGeneric _ args) = any go args
    go (TForall _ _ t) = go t
    go (TOwned t) = go t
    go (TShared t) = go t
    go (TBorrowed t) = go t
    go (TMutable t) = go t
    go _ = False

-- | Apply substitution to a type
applySubstitution :: Substitution -> Type -> Type
applySubstitution subst = go
  where
    go t@(TVar v) = HashMap.lookupDefault t v subst
    go (TList t) = TList (go t)
    go (TTuple ts) = TTuple (map go ts)
    go (TDict k v) = TDict (go k) (go v)
    go (TSet t) = TSet (go t)
    go (TOptional t) = TOptional (go t)
    go (TFunction args ret) = TFunction (map go args) (go ret)
    go (TMethod rec args ret) = TMethod (go rec) (map go args) (go ret)
    go (TStruct name args) = TStruct name (map go args)
    go (TEnum name args) = TEnum name (map go args)
    go (TInterface name args) = TInterface name (map go args)
    go (TUnion ts) = TUnion (map go ts)
    go (TGeneric name args) = TGeneric name (map go args)
    go (TForall vars constraints t) = TForall vars constraints (go t)
    go (TOwned t) = TOwned (go t)
    go (TShared t) = TShared (go t)
    go (TBorrowed t) = TBorrowed (go t)
    go (TMutable t) = TMutable (go t)
    go t = t

-- | Solve constraints and update substitutions
solveConstraints :: TypeInferenceM ()
solveConstraints = do
  constraints <- gets constraints
  mapM_ solveConstraint constraints
  where
    solveConstraint (t1, t2) = do
      result <- unify t1 t2
      case result of
        Right newConstraints -> do
          modify $ \s -> s { constraints = newConstraints ++ constraints s }
        Left err -> throwError $ "Failed to solve constraint: " <> err

-- | Instantiate a polymorphic type with fresh type variables
instantiate :: Type -> TypeInferenceM Type
instantiate (TForall vars constraints t) = do
  freshVars <- mapM (const freshTypeVar) vars
  let varMap = HashMap.fromList (zip vars (map (\(TVar v) -> v) freshVars))
  let substitution = HashMap.mapKeys (\(TypeVar name) -> TypeVar name) 
                   $ HashMap.map TVar varMap
  return $ applySubstitution substitution t
instantiate t = return t

-- | Generalize a type by quantifying over free type variables
generalize :: TypeEnvironment -> Type -> Type
generalize env t = 
  let freeVars = Set.toList $ freeVarsInType t `Set.difference` freeVarsInEnv env
  in if null freeVars
     then t
     else TForall freeVars [] t
  where
    freeVarsInType :: Type -> Set TypeVar
    freeVarsInType (TVar v) = Set.singleton v
    freeVarsInType (TList t) = freeVarsInType t
    freeVarsInType (TTuple ts) = Set.unions (map freeVarsInType ts)
    freeVarsInType (TDict k v) = freeVarsInType k `Set.union` freeVarsInType v
    freeVarsInType (TSet t) = freeVarsInType t
    freeVarsInType (TOptional t) = freeVarsInType t
    freeVarsInType (TFunction args ret) = Set.unions (map freeVarsInType (ret:args))
    freeVarsInType (TMethod rec args ret) = Set.unions (map freeVarsInType (rec:ret:args))
    freeVarsInType (TStruct _ args) = Set.unions (map freeVarsInType args)
    freeVarsInType (TEnum _ args) = Set.unions (map freeVarsInType args)
    freeVarsInType (TInterface _ args) = Set.unions (map freeVarsInType args)
    freeVarsInType (TUnion ts) = Set.unions (map freeVarsInType ts)
    freeVarsInType (TGeneric _ args) = Set.unions (map freeVarsInType args)
    freeVarsInType (TForall vars _ t) = freeVarsInType t `Set.difference` Set.fromList vars
    freeVarsInType (TOwned t) = freeVarsInType t
    freeVarsInType (TShared t) = freeVarsInType t
    freeVarsInType (TBorrowed t) = freeVarsInType t
    freeVarsInType (TMutable t) = freeVarsInType t
    freeVarsInType _ = Set.empty
    
    freeVarsInEnv :: TypeEnvironment -> Set TypeVar
    freeVarsInEnv = Set.unions . map freeVarsInType . HashMap.elems

-- Helper function for checking if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
