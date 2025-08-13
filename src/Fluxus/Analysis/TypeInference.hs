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
  , inferStatement
  , inferDeclaration
  , inferProgram
  , unifyTypes
  , instantiate
  , generalize
  , freshTypeVar
  , applySubstitution
  , solveConstraints
  , lookupVarType
  , bindVarType
  , withNewScope
  , inferFunctionType
  , inferClassType
  , checkTypes
  , inferCommonExpr
  , inferASTType
  ) where

import Fluxus.AST.Common
import qualified Fluxus.AST.Common as Common
import qualified Fluxus.AST.Python as Python
import Fluxus.AST.Python (PythonAST(..), PythonModule(..), pyModule, PythonStmt(..), PythonExpr(..), PythonPattern(..), PythonLiteral(..), PythonFuncDef(..), PythonClassDef(..), PythonParameter(..), PythonImport(..), PythonArgument(..))
import Fluxus.AST.Go (GoAST(..), GoPackage(..), goPackage, GoStmt(..), GoExpr(..), GoType(..), GoLiteral(..), GoDecl(..), GoFunction(..), GoReceiver(..), GoImport(..), GoField(..), GoFile(..), GoForClause(..), GoRangeClause(..), GoConstraint(..), GoBuiltin(..))
import Control.Monad.State
import Control.Monad.Except
import Control.Monad (foldM, forM, forM_, when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, catMaybes)
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
  return $ TVar (Common.TypeVar $ "t" <> T.pack (show n))

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

-- | Infer type of common expressions
inferExpr :: CommonExpr -> TypeInferenceM Type
inferExpr = inferCommonExpr

-- | Infer types for CommonExpr (simplified core implementation)
inferCommonExpr :: CommonExpr -> TypeInferenceM Type
inferCommonExpr (CELiteral lit) = inferLiteral lit
inferCommonExpr (CEVar var) = lookupVarType var
inferCommonExpr (CEBinaryOp op left right) = do
  leftType <- inferCommonExpr (locatedValue left)
  rightType <- inferCommonExpr (locatedValue right)
  inferBinaryOp op leftType rightType
inferCommonExpr (CEUnaryOp op operand) = do
  operandType <- inferCommonExpr (locatedValue operand)
  inferUnaryOp op operandType
inferCommonExpr (CEComparison op left right) = do
  leftType <- inferCommonExpr (locatedValue left)
  rightType <- inferCommonExpr (locatedValue right)
  addConstraint leftType rightType  -- Both operands should have same type
  return TBool
inferCommonExpr (CECall func args) = do
  funcType <- inferCommonExpr (locatedValue func)
  argTypes <- mapM (inferCommonExpr . locatedValue) args
  resultType <- freshTypeVar
  let expectedFuncType = TFunction argTypes resultType
  addConstraint funcType expectedFuncType
  return resultType
inferCommonExpr (CEIndex container index) = do
  containerType <- inferCommonExpr (locatedValue container)
  indexType <- inferCommonExpr (locatedValue index)
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
inferCommonExpr (CESlice container start end) = do
  containerType <- inferCommonExpr (locatedValue container)
  -- Infer slice bounds if present
  case start of
    Just startExpr -> do
      startType <- inferCommonExpr (locatedValue startExpr)
      addConstraint startType (TInt 32)
    Nothing -> return ()
  case end of
    Just endExpr -> do
      endType <- inferCommonExpr (locatedValue endExpr)
      addConstraint endType (TInt 32)
    Nothing -> return ()
  -- Slice result has same type as container
  return containerType
inferCommonExpr (CEAttribute obj attr) = do
  objType <- inferCommonExpr (locatedValue obj)
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
  OpDiv -> do
    -- Division always returns float
    addConstraint leftType (TFloat 64)
    addConstraint rightType (TFloat 64)
    return $ TFloat 64
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
  let substitution = HashMap.mapKeys (\(Common.TypeVar name) -> Common.TypeVar name) 
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

-- | Infer types for Python statements
inferPythonStatement :: PythonStmt -> TypeInferenceM ()
inferPythonStatement stmt = case stmt of
  PyExprStmt expr -> do
    _ <- inferPythonExpr (locatedValue expr)
    return ()
  
  PyAssign targets value -> do
    valueType <- inferPythonExpr (locatedValue value)
    mapM_ (\target -> inferPythonPattern (locatedValue target) valueType) targets
    return ()
  
  PyAugAssign target op value -> do
    targetType <- freshTypeVar
    inferPythonPattern (locatedValue target) targetType
    valueType <- inferPythonExpr (locatedValue value)
    resultType <- inferBinaryOp op targetType valueType
    inferPythonPattern (locatedValue target) resultType
    return ()
  
  PyAnnAssign target typeExpr maybeValue -> do
    annotatedType <- inferPythonTypeExpr (locatedValue typeExpr)
    case maybeValue of
      Just value -> do
        valueType <- inferPythonExpr (locatedValue value)
        addConstraint valueType annotatedType
      Nothing -> return ()
    inferPythonPattern (locatedValue target) annotatedType
    return ()
  
  PyReturn maybeExpr -> case maybeExpr of
    Just expr -> do
      _ <- inferPythonExpr (locatedValue expr)
      return ()
    Nothing -> return ()
  
  PyIf condition thenStmts elseStmts -> do
    condType <- inferPythonExpr (locatedValue condition)
    addConstraint condType TBool
    mapM_ (inferPythonStatement . locatedValue) thenStmts
    mapM_ (inferPythonStatement . locatedValue) elseStmts
    return ()
  
  PyWhile condition body elseClause -> do
    condType <- inferPythonExpr (locatedValue condition)
    addConstraint condType TBool
    mapM_ (inferPythonStatement . locatedValue) body
    mapM_ (inferPythonStatement . locatedValue) elseClause
    return ()
  
  PyFor target iter body elseClause -> do
    iterType <- inferPythonExpr (locatedValue iter)
    elemType <- freshTypeVar
    addConstraint iterType (TList elemType)  -- Assume iterable is a list for now
    inferPythonPattern (locatedValue target) elemType
    mapM_ (inferPythonStatement . locatedValue) body
    mapM_ (inferPythonStatement . locatedValue) elseClause
    return ()
  
  PyFuncDef funcDef -> do
    inferPythonFuncDef funcDef
    return ()
  
  PyClassDef classDef -> do
    inferPythonClassDef classDef
    return ()
  
  _ -> return ()  -- Simplified: skip other statement types for now

-- | Infer types for Go statements
inferGoStatement :: GoStmt -> TypeInferenceM ()
inferGoStatement stmt = case stmt of
  GoExprStmt expr -> do
    _ <- inferGoExpr (locatedValue expr)
    return ()
  
  GoAssign lvalues rvalues -> do
    lvalueTypes <- mapM (inferGoExpr . locatedValue) lvalues
    rvalueTypes <- mapM (inferGoExpr . locatedValue) rvalues
    -- Add constraints that corresponding lvalues and rvalues have same types
    mapM_ (uncurry addConstraint) (zip lvalueTypes rvalueTypes)
    return ()
  
  GoDefine identifiers rvalues -> do
    rvalueTypes <- mapM (inferGoExpr . locatedValue) rvalues
    -- Bind identifiers to their inferred types
    mapM_ (uncurry bindVarType) (zip identifiers rvalueTypes)
    return ()
  
  GoVarStmt varDecls -> do
    mapM_ inferGoVarDecl varDecls
    return ()
  
  GoReturn exprs -> do
    _ <- mapM (inferGoExpr . locatedValue) exprs
    return ()
  
  GoIf maybeInit condition thenStmt maybeElseStmt -> do
    condType <- inferGoExpr (locatedValue condition)
    addConstraint condType TBool
    _ <- inferGoStatement (locatedValue thenStmt)
    case maybeElseStmt of
      Just elseStmt -> do
        _ <- inferGoStatement (locatedValue elseStmt)
        return ()
      Nothing -> return ()
    return ()
  
  GoFor maybeClause body -> do
    case maybeClause of
      Just clause -> do
        case goForCond clause of
          Just cond -> do
            condType <- inferGoExpr (locatedValue cond)
            addConstraint condType TBool
          Nothing -> return ()
      Nothing -> return ()
    _ <- inferGoStatement (locatedValue body)
    return ()
  
  _ -> return ()  -- Simplified: skip other statement types for now

-- | Generic statement inference dispatcher
inferStatement :: Either PythonStmt GoStmt -> TypeInferenceM ()
inferStatement (Left pyStmt) = inferPythonStatement pyStmt
inferStatement (Right goStmt) = inferGoStatement goStmt

-- | Infer types for Python function definitions
inferPythonFuncDef :: PythonFuncDef -> TypeInferenceM ()
inferPythonFuncDef funcDef = do
  -- Infer parameter types
  paramTypes <- mapM (inferPythonParameter . locatedValue) (pyFuncParams funcDef)
  
  -- Create function type with parameter types and a fresh return type
  returnType <- freshTypeVar
  let funcType = TFunction paramTypes returnType
  
  -- Bind the function name to its type in the current scope
  -- This allows recursive calls to find the function
  bindVarType (pyFuncName funcDef) funcType
  
  -- Create new scope for function body
  withNewScope $ do
    -- Infer types from function body
    mapM_ (inferPythonStatement . locatedValue) (pyFuncBody funcDef)
    
    -- Note: In a more complete implementation, we would update the return type 
    -- based on the actual return statements in the function body

-- | Infer types for Python class definitions
inferPythonClassDef :: PythonClassDef -> TypeInferenceM ()
inferPythonClassDef classDef = do
  -- Create class type
  let className = QualifiedName [] (pyClassName classDef)
  let classType = TStruct className []  -- Simplified: no type parameters
  
  -- Bind class name
  bindVarType (pyClassName classDef) classType
  
  -- Create new scope for class methods
  withNewScope $ do
    -- Bind 'self' parameter for methods
    bindVarType (Identifier "self") classType
    
    -- Infer method types
    mapM_ (inferPythonStatement . locatedValue) (pyClassBody classDef)

-- | Infer types for Go declarations
inferGoDecl :: GoDecl -> TypeInferenceM ()
inferGoDecl decl = case decl of
  GoConstDecl constDecls -> do
    mapM_ inferGoConstDecl constDecls
    return ()
  
  GoTypeDecl name goType -> do
    typeVal <- inferGoType (locatedValue goType)
    bindVarType name typeVal
    return ()
  
  GoVarDecl varDecls -> do
    mapM_ inferGoVarDecl varDecls
    return ()
  
  GoFuncDecl func -> do
    inferGoFunction func
    return ()
  
  GoMethodDecl receiver func -> do
    -- Create new scope with receiver (if it has a name)
    withNewScope $ do
      receiverType <- inferGoReceiver receiver
      -- Skip binding receiver name for now since it's Maybe Identifier
      inferGoFunction func
    return ()
  
  _ -> return ()  -- Skip import declarations

-- | Generic declaration inference dispatcher
inferDeclaration :: Either PythonStmt GoDecl -> TypeInferenceM ()
inferDeclaration (Left pyStmt) = case pyStmt of
  PyFuncDef funcDef -> inferPythonFuncDef funcDef
  PyClassDef classDef -> inferPythonClassDef classDef
  _ -> return ()
inferDeclaration (Right goDecl) = inferGoDecl goDecl

-- | Infer types for entire program/module
inferProgram :: Either PythonAST GoAST -> TypeInferenceM ()
inferProgram (Left pyAST) = do
  let pythonModule = pyModule pyAST
  
  -- Infer types for module imports
  mapM_ (inferPythonImport . locatedValue) (pyModuleImports pythonModule)
  
  -- Infer types for module body statements
  mapM_ (inferPythonStatement . locatedValue) (pyModuleBody pythonModule)
  
inferProgram (Right goAST) = do
  let goPackage' = goPackage goAST
  
  -- Infer types for all files in package
  mapM_ inferGoFile (goPackageFiles goPackage')
  where
    inferGoFile :: GoFile -> TypeInferenceM ()
    inferGoFile file = do
      -- Infer types for imports
      mapM_ (inferGoImport . locatedValue) (goFileImports file)
      
      -- Infer types for declarations
      mapM_ (inferGoDecl . locatedValue) (goFileDecls file)

-- | Top-level AST type inference
inferASTType :: Either PythonAST GoAST -> TypeInferenceM ()
inferASTType = inferProgram

-- | Helper to run type inference with new scope
withNewScope :: TypeInferenceM a -> TypeInferenceM a
withNewScope action = do
  pushScope
  result <- action
  popScope
  return result

-- | Infer function type signature
inferFunctionType :: [Type] -> Type -> Type
inferFunctionType paramTypes returnType = TFunction paramTypes returnType

-- | Infer class/struct type
inferClassType :: Identifier -> [Type] -> Type
inferClassType className fieldTypes = TStruct (QualifiedName [] className) fieldTypes

-- | Type checking (validation after inference)
checkTypes :: TypeInferenceM Bool
checkTypes = do
  constraints <- gets constraints
  result <- mapM (uncurry checkTypeCompatibility) constraints
  return $ and result
  where
    checkTypeCompatibility :: Type -> Type -> TypeInferenceM Bool
    checkTypeCompatibility t1 t2 = do
      unifyResult <- unify t1 t2
      case unifyResult of
        Right _ -> return True
        Left _ -> return False

-- Helper function for checking if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- | Helper functions for Python type inference

-- | Infer type from Python expression
inferPythonExpr :: PythonExpr -> TypeInferenceM Type
inferPythonExpr expr = case expr of
  PyLiteral lit -> inferPythonLiteral lit
  PyVar var -> lookupVarType var
  PyBinaryOp op left right -> do
    leftType <- inferPythonExpr (locatedValue left)
    rightType <- inferPythonExpr (locatedValue right)
    inferBinaryOp op leftType rightType
  PyUnaryOp op operand -> do
    operandType <- inferPythonExpr (locatedValue operand)
    inferUnaryOp op operandType
  PyCall func args -> do
    -- Infer argument types first
    argTypes <- mapM (inferPythonArgument . locatedValue) args
    
    funcType <- case locatedValue func of
      PyVar var -> do
        -- First try to look up the function in the current environment
        envLookup <- lookupVarType var `catchError` (\_ -> freshTypeVar)
        return envLookup
      _ -> inferPythonExpr (locatedValue func)
    
    resultType <- freshTypeVar
    let expectedFuncType = TFunction argTypes resultType
    addConstraint funcType expectedFuncType
    return resultType
  PyAttribute obj attr -> do
    _ <- inferPythonExpr (locatedValue obj)
    -- For now, return Any for attribute access
    return TAny
  PySubscript obj slice -> do
    objType <- inferPythonExpr (locatedValue obj)
    -- Simplified: assume list indexing
    elemType <- freshTypeVar
    addConstraint objType (TList elemType)
    return elemType
  _ -> return TAny  -- Simplified: return Any for other expressions

-- | Infer type from Python argument
inferPythonArgument :: PythonArgument -> TypeInferenceM Type
inferPythonArgument arg = case arg of
  ArgPositional expr -> inferPythonExpr (locatedValue expr)
  ArgKeyword _ expr -> inferPythonExpr (locatedValue expr)
  ArgStarred expr -> inferPythonExpr (locatedValue expr)
  ArgKwStarred expr -> inferPythonExpr (locatedValue expr)

-- | Infer type from Python literal
inferPythonLiteral :: PythonLiteral -> TypeInferenceM Type
inferPythonLiteral lit = case lit of
  PyInt _ -> return $ TInt 32
  PyFloat _ -> return $ TFloat 64
  PyComplex _ _ -> return $ TComplex (TFloat 64)
  PyString _ -> return TString
  PyFString _ _ -> return TString
  PyBytes _ -> return TBytes
  PyBool _ -> return TBool
  PyNone -> return $ TOptional TAny
  PyEllipsis -> return TAny

-- | Infer type from Python pattern and bind variables
inferPythonPattern :: PythonPattern -> Type -> TypeInferenceM ()
inferPythonPattern pattern expectedType = case pattern of
  PatVar var -> bindVarType var expectedType
  PatTuple patterns -> do
    elemTypes <- mapM (const freshTypeVar) patterns
    addConstraint expectedType (TTuple elemTypes)
    mapM_ (uncurry inferPythonPattern) (zip (map locatedValue patterns) elemTypes)
  PatList patterns -> do
    elemType <- freshTypeVar
    addConstraint expectedType (TList elemType)
    mapM_ (\p -> inferPythonPattern (locatedValue p) elemType) patterns
  _ -> return ()  -- Simplified: skip other pattern types

-- | Infer type from Python parameter
inferPythonParameter :: PythonParameter -> TypeInferenceM Type
inferPythonParameter param = case param of
  ParamNormal name maybeType maybeDefault -> do
    paramType <- case maybeType of
      Just typeExpr -> inferPythonTypeExpr (locatedValue typeExpr)
      Nothing -> freshTypeVar
    bindVarType name paramType
    return paramType
  ParamVarArgs name maybeType -> do
    elemType <- case maybeType of
      Just typeExpr -> inferPythonTypeExpr (locatedValue typeExpr)
      Nothing -> freshTypeVar
    let varArgsType = TList elemType
    bindVarType name varArgsType
    return varArgsType
  ParamKwArgs name maybeType -> do
    valueType <- case maybeType of
      Just typeExpr -> inferPythonTypeExpr (locatedValue typeExpr)
      Nothing -> freshTypeVar
    let kwArgsType = TDict TString valueType
    bindVarType name kwArgsType
    return kwArgsType

-- | Infer type from Python type expression
inferPythonTypeExpr :: Python.PythonTypeExpr -> TypeInferenceM Type
inferPythonTypeExpr typeExpr = case typeExpr of
  Python.TypeVar var -> return $ TVar (Common.TypeVar var)
  Python.TypeName name -> case name of
    QualifiedName [] (Identifier "int") -> return $ TInt 32
    QualifiedName [] (Identifier "float") -> return $ TFloat 64
    QualifiedName [] (Identifier "bool") -> return TBool
    QualifiedName [] (Identifier "str") -> return TString
    QualifiedName [] (Identifier "bytes") -> return TBytes
    QualifiedName [] (Identifier "list") -> do
      elemType <- freshTypeVar
      return $ TList elemType
    QualifiedName [] (Identifier "dict") -> do
      keyType <- freshTypeVar
      valueType <- freshTypeVar
      return $ TDict keyType valueType
    _ -> return TAny
  Python.TypeSubscript baseType args -> case (baseType, args) of
    (Located _ (Python.TypeName (QualifiedName [] (Identifier "list"))), [elemTypeExpr]) -> do
      elemT <- inferPythonTypeExpr (locatedValue elemTypeExpr)
      return $ TList elemT
    (Located _ (Python.TypeName (QualifiedName [] (Identifier "dict"))), [keyTypeExpr, valueTypeExpr]) -> do
      keyT <- inferPythonTypeExpr (locatedValue keyTypeExpr)
      valueT <- inferPythonTypeExpr (locatedValue valueTypeExpr)
      return $ TDict keyT valueT
    _ -> return TAny
  Python.TypeTuple elemExprs -> do
    elemTypes <- mapM (inferPythonTypeExpr . locatedValue) elemExprs
    return $ TTuple elemTypes
  Python.TypeOptional typeExpr -> do
    innerType <- inferPythonTypeExpr (locatedValue typeExpr)
    return $ TOptional innerType
  _ -> return TAny

-- | Infer Python import (placeholder)
inferPythonImport :: PythonImport -> TypeInferenceM ()
inferPythonImport _ = return ()

-- | Helper functions for Go type inference

-- | Infer type from Go expression
inferGoExpr :: GoExpr -> TypeInferenceM Type
inferGoExpr expr = case expr of
  GoLiteral lit -> inferGoLiteral lit
  GoIdent var -> lookupVarType var
  GoBinaryOp op left right -> do
    leftType <- inferGoExpr (locatedValue left)
    rightType <- inferGoExpr (locatedValue right)
    inferBinaryOp op leftType rightType
  GoUnaryOp op operand -> do
    operandType <- inferGoExpr (locatedValue operand)
    inferUnaryOp op operandType
  GoCall func args -> do
    funcType <- inferGoExpr (locatedValue func)
    argTypes <- mapM (inferGoExpr . locatedValue) args
    resultType <- freshTypeVar
    let expectedFuncType = TFunction argTypes resultType
    addConstraint funcType expectedFuncType
    return resultType
  GoSelector obj field -> do
    _ <- inferGoExpr (locatedValue obj)
    -- For now, return Any for field access
    return TAny
  GoIndex obj index -> do
    objType <- inferGoExpr (locatedValue obj)
    indexType <- inferGoExpr (locatedValue index)
    elemType <- freshTypeVar
    addConstraint objType (TList elemType)
    addConstraint indexType (TInt 32)
    return elemType
  _ -> return TAny  -- Simplified: return Any for other expressions

-- | Infer type from Go literal
inferGoLiteral :: GoLiteral -> TypeInferenceM Type
inferGoLiteral lit = case lit of
  GoInt _ -> return $ TInt 32
  GoFloat _ -> return $ TFloat 64
  GoBool _ -> return TBool
  GoString _ -> return TString
  GoRune _ -> return TChar
  GoNil -> return $ TOptional TAny

-- | Infer type from Go type specification with enhanced constraint support
inferGoType :: GoType -> TypeInferenceM Type
inferGoType goType = case goType of
  GoBasicType name -> case name of
    Identifier "int" -> return $ TInt 32
    Identifier "int8" -> return $ TInt 8
    Identifier "int16" -> return $ TInt 16
    Identifier "int32" -> return $ TInt 32
    Identifier "int64" -> return $ TInt 64
    Identifier "uint" -> return $ TUInt 32
    Identifier "uint8" -> return $ TUInt 8
    Identifier "uint16" -> return $ TUInt 16
    Identifier "uint32" -> return $ TUInt 32
    Identifier "uint64" -> return $ TUInt 64
    Identifier "float32" -> return $ TFloat 32
    Identifier "float64" -> return $ TFloat 64
    Identifier "bool" -> return TBool
    Identifier "string" -> return TString
    Identifier "byte" -> return $ TUInt 8
    Identifier "rune" -> return $ TInt 32
    Identifier "comparable" -> return TAny  -- Special interface for comparison
    Identifier "any" -> return TAny  -- Go 1.18+ any type
    _ -> return TAny
  GoSliceType elemType -> do
    elemT <- inferGoType (locatedValue elemType)
    return $ TList elemT
  GoMapType keyType valueType -> do
    keyT <- inferGoType (locatedValue keyType)
    valueT <- inferGoType (locatedValue valueType)
    return $ TDict keyT valueT
  GoPointerType baseType -> do
    baseT <- inferGoType (locatedValue baseType)
    return $ TOwned baseT  -- Use owned type for pointers
  GoInterfaceType methods -> do
    -- For interface types, create a generic interface type
    return $ TInterface (QualifiedName [] (Identifier "Interface")) []
  GoTypeParam identifier constraint -> do
    -- Handle type parameters with constraints
    case constraint of
      Just constr -> do
        constrType <- inferGoConstraint (locatedValue constr)
        return $ TVar (Common.TypeVar (unIdentifier identifier))
      Nothing -> return $ TVar (Common.TypeVar (unIdentifier identifier))
  GoGenericType name args -> do
    -- Handle generic types like List[int]
    argTypes <- mapM (inferGoType . locatedValue) args
    return $ TGeneric name argTypes
  _ -> return TAny
  where
    unIdentifier (Identifier name) = name

-- | Infer type from Go constraints with approximation support
inferGoConstraint :: GoConstraint -> TypeInferenceM Type
inferGoConstraint constraint = case constraint of
  GoBasicConstraint typeExpr -> do
    inferGoType (locatedValue typeExpr)
  GoApproximationConstraint typeExpr -> do
    -- For approximation constraints, create a flexible type variable
    baseType <- inferGoType (locatedValue typeExpr)
    return $ TVar (Common.TypeVar ("~" <> T.pack (show baseType)))
  GoUnionConstraint constraints -> do
    -- For union constraints, create a union type
    constraintTypes <- mapM (inferGoConstraint . locatedValue) constraints
    return $ TUnion constraintTypes
  GoInterfaceConstraint methods -> do
    -- For interface constraints, create an interface type
    return $ TInterface (QualifiedName [] (Identifier "Interface")) []
  GoMethodSetConstraint typeExprs -> do
    -- For method set constraints, create an interface type
    constraintTypes <- mapM (inferGoType . locatedValue) typeExprs
    return $ TInterface (QualifiedName [] (Identifier "MethodSet")) constraintTypes
  GoComparableConstraint -> do
    -- For comparable constraint, create a comparable interface type
    return $ TInterface (QualifiedName [] (Identifier "comparable")) []
  GoOrderedConstraint -> do
    -- For ordered constraint, create an ordered interface type
    return $ TInterface (QualifiedName [] (Identifier "ordered")) []

-- | Enhanced type inference for Go built-in functions with Go 1.21+ support
inferGoBuiltin :: GoBuiltin -> [Type] -> TypeInferenceM Type
inferGoBuiltin GoMake [ty] = return ty
inferGoBuiltin GoMake [ty, sizeTy] = do
  addConstraint sizeTy (TInt 32)
  return ty
inferGoBuiltin GoNew [ty] = return $ TOwned ty
inferGoBuiltin GoLen [containerTy] = do
  -- Len works on arrays, slices, strings, maps, channels
  case containerTy of
    TList _ -> return ()
    TString -> return ()
    TDict _ _ -> return ()
    _ -> addConstraint containerTy (TList TAny)  -- Default assumption
  return $ TInt 32
inferGoBuiltin GoCap [containerTy] = do
  -- Cap works on slices, arrays, channels
  case containerTy of
    TList _ -> return ()
    _ -> addConstraint containerTy (TList TAny)
  return $ TInt 32
inferGoBuiltin GoAppend [sliceTy, elemTy] = do
  addConstraint sliceTy (TList elemTy)
  return sliceTy
inferGoBuiltin GoCopy [dstTy, srcTy] = do
  addConstraint dstTy (TList TAny)
  addConstraint srcTy (TList TAny)
  return $ TInt 32
inferGoBuiltin GoDelete [mapTy, keyTy] = do
  case mapTy of
    TDict k _ -> addConstraint keyTy k
    _ -> addConstraint mapTy (TDict keyTy TAny)
  return TVoid
inferGoBuiltin GoClose [chanTy] = do
  -- Close works on channels
  return TVoid
inferGoBuiltin GoPanic [msgTy] = do
  addConstraint msgTy TString
  return TVoid  -- Panic never returns
inferGoBuiltin GoRecover [] = return $ TOptional TString
inferGoBuiltin GoReal [complexTy] = do
  -- Real part of complex number
  case complexTy of
    TComplex _ -> return $ TFloat 64
    _ -> do
      addConstraint complexTy (TComplex (TFloat 64))
      return $ TFloat 64
inferGoBuiltin GoImagBuiltin [complexTy] = do
  -- Imaginary part of complex number
  case complexTy of
    TComplex _ -> return $ TFloat 64
    _ -> do
      addConstraint complexTy (TComplex (TFloat 64))
      return $ TFloat 64
inferGoBuiltin GoComplex [realTy, imagTy] = do
  addConstraint realTy (TFloat 64)
  addConstraint imagTy (TFloat 64)
  return $ TComplex (TFloat 64)
-- Go 1.21+ built-ins
inferGoBuiltin GoMin [t1, t2] = do
  -- Min requires both arguments to be the same ordered type
  addConstraint t1 t2
  addConstraint t1 (TInterface (QualifiedName [] (Identifier "Ordered")) [])
  return t1
inferGoBuiltin GoMax [t1, t2] = do
  -- Max requires both arguments to be the same ordered type
  addConstraint t1 t2
  addConstraint t1 (TInterface (QualifiedName [] (Identifier "Ordered")) [])
  return t1
inferGoBuiltin GoClear [containerTy] = do
  -- Clear works on maps and slices
  case containerTy of
    TDict _ _ -> return ()
    TList _ -> return ()
    _ -> throwError "clear() requires map or slice type"
  return TVoid
-- Go 1.20+ unsafe operations
inferGoBuiltin GoUnsafeString [ptrTy, lenTy] = do
  addConstraint ptrTy (TOwned TAny)  -- Should be *T
  addConstraint lenTy (TInt 64)      -- Should be int
  return TString
inferGoBuiltin GoUnsafeSlice [ptrTy, lenTy] = do
  addConstraint ptrTy (TOwned TAny)  -- Should be *T
  addConstraint lenTy (TInt 64)      -- Should be int
  return $ TList TAny
-- Go 1.20+ errors.Join
inferGoBuiltin GoErrorsJoin argTypes = do
  -- All arguments should be error types (string in simplified model)
  mapM_ (\ty -> addConstraint ty TString) argTypes
  return TString
inferGoBuiltin _ _ = return TAny  -- Fallback for unsupported built-ins

-- | Enhanced type inference for new standard library packages (Go 1.21+)
inferSlicesFunction :: Identifier -> [Type] -> TypeInferenceM Type
inferSlicesFunction (Identifier "Clone") [ty] = return $ TList ty
inferSlicesFunction (Identifier "Compact") [ty] = return $ TList ty
inferSlicesFunction (Identifier "Sort") [ty] = do
  -- Check if element type is ordered
  addConstraint ty (TInterface (QualifiedName [] (Identifier "Ordered")) [])
  return $ TList ty
inferSlicesFunction (Identifier "Insert") [ty, elemTy] = do
  addConstraint ty (TList elemTy)
  return $ TList elemTy
inferSlicesFunction (Identifier "Delete") [ty, indexTy] = do
  addConstraint ty (TList TAny)
  addConstraint indexTy (TInt 32)
  return ty
inferSlicesFunction _ _ = return TAny

inferMapsFunction :: Identifier -> [Type] -> TypeInferenceM Type
inferMapsFunction (Identifier "Keys") [TDict k _] = return $ TList k
inferMapsFunction (Identifier "Values") [TDict _ v] = return $ TList v
inferMapsFunction (Identifier "Clone") [TDict k v] = return $ TDict k v
inferMapsFunction (Identifier "Delete") [TDict k _, keyTy] = do
  addConstraint keyTy k
  return TVoid
inferMapsFunction _ _ = return TAny

inferUniqueFunction :: Identifier -> [Type] -> TypeInferenceM Type
inferUniqueFunction (Identifier "Make") [ty] = do
  -- Unique package provides unique handles
  return $ TOwned ty
inferUniqueFunction _ _ = return TAny

inferIterFunction :: Identifier -> [Type] -> TypeInferenceM Type
inferIterFunction (Identifier "Pull") [seqTy] = do
  -- Iter package provides pull-based iteration
  elemTy <- freshTypeVar
  addConstraint seqTy (TList elemTy)
  return $ TFunction [] (TTuple [elemTy, TBool])
inferIterFunction _ _ = return TAny

-- | Enhanced constraint satisfaction checking for Go 1.18+ generics
checkConstraintSatisfaction :: Type -> GoConstraint -> TypeInferenceM Bool
checkConstraintSatisfaction ty constraint = case constraint of
  GoBasicConstraint baseType -> do
    baseTy <- inferGoType (locatedValue baseType)
    unifyResult <- unifyTypes ty baseTy
    return $ unifyResult /= Nothing
  GoApproximationConstraint baseType -> do
    baseTy <- inferGoType (locatedValue baseType)
    -- Check if ty is approximately baseTy (underlying type)
    checkApproximation ty baseTy
  GoUnionConstraint constraints -> do
    -- Check if ty satisfies any of the union constraints
    results <- mapM (checkConstraintSatisfaction ty . locatedValue) constraints
    return $ or results
  GoInterfaceConstraint methods -> do
    -- Check if ty implements the interface (simplified)
    return True  -- For now, assume satisfaction
  GoMethodSetConstraint typeExprs -> do
    -- Check if ty has the required method set
    constraintTypes <- mapM (inferGoType . locatedValue) typeExprs
    results <- mapM (checkTypeCompatibility ty) constraintTypes
    return $ and results
  GoComparableConstraint -> checkIsComparable ty
  GoOrderedConstraint -> checkIsOrdered ty
  where
    checkApproximation :: Type -> Type -> TypeInferenceM Bool
    checkApproximation ty baseTy = do
      -- Simplified approximation check - in real implementation would check underlying types
      unifyResult <- unifyTypes ty baseTy
      return $ unifyResult /= Nothing
    
    checkTypeCompatibility :: Type -> Type -> TypeInferenceM Bool
    checkTypeCompatibility ty1 ty2 = do
      unifyResult <- unifyTypes ty1 ty2
      return $ unifyResult /= Nothing
    
    checkIsComparable :: Type -> TypeInferenceM Bool
    checkIsComparable ty = case ty of
      TInt _ -> return True
      TUInt _ -> return True
      TFloat _ -> return True
      TString -> return True
      TBool -> return True
      TList _ -> return True  -- Lists are comparable if elements are
      TDict _ _ -> return True  -- Maps are comparable if keys are comparable
      _ -> return False
    
    checkIsOrdered :: Type -> TypeInferenceM Bool
    checkIsOrdered ty = case ty of
      TInt _ -> return True
      TUInt _ -> return True
      TFloat _ -> return True
      TString -> return True
      _ -> return False

-- | Enhanced generic type inference for Go 1.18+ 
inferGenericFunctionCall :: Type -> [Type] -> TypeInferenceM Type
inferGenericFunctionCall funcType argTypes = case funcType of
  TForall typeVars constraints returnType -> do
    -- Create substitution from type variables to argument types
    substitution <- inferTypeSubstitution typeVars argTypes
    -- Apply substitution and check constraints
    let appliedType = applySubstitution substitution returnType
    -- Verify constraints are satisfied
    constraintsSatisfied <- checkConstraints constraints substitution
    if constraintsSatisfied
      then return appliedType
      else throwError "Generic constraints not satisfied"
  _ -> throwError "Expected generic function type"
  where
    inferTypeSubstitution :: [TypeVar] -> [Type] -> TypeInferenceM Substitution
    inferTypeSubstitution vars args = do
      let varMap = HashMap.fromList (zip vars args)
      return $ HashMap.map (\t -> t) varMap  -- Simplified substitution
    
    checkConstraints :: [TypeConstraint] -> Substitution -> TypeInferenceM Bool
    checkConstraints constraints substitution = do
      results <- mapM (checkAppliedConstraint substitution) constraints
      return $ and results
    
    checkAppliedConstraint :: Substitution -> TypeConstraint -> TypeInferenceM Bool
    checkAppliedConstraint substitution constraint = do
      -- Apply substitution to constraint types and check satisfaction
      return True  -- Simplified for now

-- | Infer Go variable declaration
inferGoVarDecl :: (Identifier, Maybe (Located GoType), Maybe (Located GoExpr)) -> TypeInferenceM ()
inferGoVarDecl (name, maybeType, maybeExpr) = do
  declaredType <- case maybeType of
    Just goType -> inferGoType (locatedValue goType)
    Nothing -> freshTypeVar
  
  case maybeExpr of
    Just expr -> do
      exprType <- inferGoExpr (locatedValue expr)
      addConstraint exprType declaredType
    Nothing -> return ()
  
  bindVarType name declaredType

-- | Infer Go constant declaration
inferGoConstDecl :: (Identifier, Maybe (Located GoType), Located GoExpr) -> TypeInferenceM ()
inferGoConstDecl (name, maybeType, expr) = do
  exprType <- inferGoExpr (locatedValue expr)
  
  declaredType <- case maybeType of
    Just goType -> do
      t <- inferGoType (locatedValue goType)
      addConstraint exprType t
      return t
    Nothing -> return exprType
  
  bindVarType name declaredType

-- | Infer Go function (simplified)
inferGoFunction :: GoFunction -> TypeInferenceM ()
inferGoFunction func = do
  -- Simplified implementation - would need to access GoFunction fields properly
  return ()

-- | Infer Go parameter
inferGoParameter :: (Identifier, Located GoType) -> TypeInferenceM Type
inferGoParameter (name, goType) = do
  paramType <- inferGoType (locatedValue goType)
  bindVarType name paramType
  return paramType

-- | Infer Go receiver
inferGoReceiver :: GoReceiver -> TypeInferenceM Type
inferGoReceiver receiver = do
  receiverType <- inferGoType (locatedValue $ goReceiverType receiver)
  return receiverType

-- | Infer Go import (placeholder)
inferGoImport :: GoImport -> TypeInferenceM ()
inferGoImport _ = return ()