{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Fluxus.Analysis.TypeInference
  ( TypeInferenceM
  , TypeConstraints
  , InferenceResult(..)
  , TypeInferenceState(..)
  , TypeEnvironment
  , InferenceError(..)
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
import Fluxus.AST.Go (GoAST(..), GoPackage(..), goPackage, GoStmt(..), GoExpr(..), GoType(..), GoLiteral(..), GoDecl(..), GoTypeDecl(..), GoFunction(..), GoReceiver(..), GoImport(..), GoFile(..), GoForClause(..), GoBinding(..), BindingLHS(..), BindKind(..))
import qualified Fluxus.AST.Go as Go
import Control.Monad.State
import Control.Monad.Except
import Control.Monad (foldM, forM, forM_, zipWithM_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | Enhanced error type for better error reporting
data InferenceError
  = UnificationError Type Type SourceSpan
  | OccursCheckFailed TypeVar Type SourceSpan  
  | UndefinedVariable Identifier SourceSpan
  | TypeMismatch Type Type Text SourceSpan
  | AttributeNotFound Identifier Type SourceSpan
  | ImportError Text SourceSpan
  | GenericError Text
  deriving (Show, Eq, Generic)

instance NFData InferenceError

-- | Convert error to text for compatibility
errorToText :: InferenceError -> Text
errorToText = \case
  UnificationError t1 t2 loc -> 
    "Cannot unify " <> T.pack (show t1) <> " with " <> T.pack (show t2) <> " at " <> T.pack (show loc)
  OccursCheckFailed var ty loc ->
    "Occurs check failed: " <> T.pack (show var) <> " in " <> T.pack (show ty) <> " at " <> T.pack (show loc)
  UndefinedVariable var loc ->
    "Undefined variable: " <> T.pack (show var) <> " at " <> T.pack (show loc)
  TypeMismatch expected actual msg loc ->
    msg <> ": expected " <> T.pack (show expected) <> ", got " <> T.pack (show actual) <> " at " <> T.pack (show loc)
  AttributeNotFound attr ty loc ->
    "Attribute " <> T.pack (show attr) <> " not found in type " <> T.pack (show ty) <> " at " <> T.pack (show loc)
  ImportError msg loc ->
    "Import error: " <> msg <> " at " <> T.pack (show loc)
  GenericError msg -> msg

-- | Helper function to extract value from Go's Located type
goLocatedValue :: Go.Located a -> a
goLocatedValue = Go.locValue

type TypeInferenceM = StateT TypeInferenceState (Except Text)
type TypeConstraints = [(Type, Type)]
type TypeEnvironment = HashMap Identifier Type
type Substitution = HashMap TypeVar Type

-- | Type definitions for structs, classes, etc.
data TypeDefinition = TypeDefinition
  { typeDefName :: !QualifiedName
  , typeDefFields :: !(HashMap Identifier Type)
  , typeDefMethods :: !(HashMap Identifier Type)
  , typeDefParent :: !(Maybe Type)
  } deriving (Show, Generic)

-- | Improved state with scope stack and type definitions
data TypeInferenceState = TypeInferenceState
  { nextTyVar :: !Int
  , constraints :: !TypeConstraints
  , substitution :: !Substitution  -- Single substitution instead of substitutions
  , scopeStack :: ![TypeEnvironment]  -- Stack of scopes (top is current)
  , typeDefinitions :: !(HashMap QualifiedName TypeDefinition)  -- Type definitions
  , currentModule :: !Text  -- Current module name for imports
  , importedModules :: !(HashMap Text TypeEnvironment)  -- Imported module environments
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
      , substitution = HashMap.empty
      , scopeStack = [env]  -- Initial environment as base scope
      , typeDefinitions = HashMap.empty
      , currentModule = "main"
      , importedModules = HashMap.empty
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

-- | Add a warning message for debugging
addWarning :: Text -> TypeInferenceM ()
addWarning _msg = return ()

-- | Look up variable type in scope stack
lookupVarType :: Identifier -> TypeInferenceM Type
lookupVarType var = do
  scopes <- gets scopeStack
  case lookupInScopes var scopes of
    Just t -> do
      -- Apply current substitution to the type
      subst <- gets substitution
      let result = applySubstitution subst t
      addWarning $ "Variable lookup: " <> T.pack (show var) <> ", type=" <> T.pack (show result)
      return result
    Nothing -> throwError $ errorToText $ UndefinedVariable var (SourceSpan (T.pack "<no-file>") (SourcePos 0 0) (SourcePos 0 0))
  where
    lookupInScopes :: Identifier -> [TypeEnvironment] -> Maybe Type
    lookupInScopes _ [] = Nothing
    lookupInScopes v (scope:rest) = 
      case HashMap.lookup v scope of
        Just ty -> Just ty
        Nothing -> lookupInScopes v rest

-- | Bind variable to type in current scope
bindVarType :: Identifier -> Type -> TypeInferenceM ()
bindVarType var t = do
  scopes <- gets scopeStack
  case scopes of
    [] -> throwError "No scope available"
    (current:rest) -> do
      let updated = HashMap.insert var t current
      modify $ \s -> s { scopeStack = updated : rest }

-- | Push new empty scope
pushScope :: TypeInferenceM ()
pushScope = modify $ \s -> s { scopeStack = HashMap.empty : scopeStack s }

-- | Pop scope
popScope :: TypeInferenceM ()
popScope = do
  scopes <- gets scopeStack
  case scopes of
    [] -> throwError "Cannot pop scope: no scopes on stack"
    (_:rest) -> modify $ \s -> s { scopeStack = rest }

-- | Register a type definition
registerTypeDefinition :: QualifiedName -> TypeDefinition -> TypeInferenceM ()
registerTypeDefinition name def = 
  modify $ \s -> s { typeDefinitions = HashMap.insert name def (typeDefinitions s) }

-- | Look up type definition
lookupTypeDefinition :: QualifiedName -> TypeInferenceM (Maybe TypeDefinition)
lookupTypeDefinition name = gets (HashMap.lookup name . typeDefinitions)

-- | Main type inference function for expressions
inferType :: CommonExpr -> TypeInferenceM InferenceResult
inferType expr = do
  t <- inferExpr expr
  currentConstraints <- gets constraints
  currentSubstitution <- gets substitution
  return $ InferenceResult t currentConstraints currentSubstitution

-- | Infer type of common expressions
inferExpr :: CommonExpr -> TypeInferenceM Type
inferExpr = inferCommonExpr

-- | Enhanced common expression inference with better attribute handling
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
inferCommonExpr (CEComparison _op left right) = do
  leftType <- inferCommonExpr (locatedValue left)
  rightType <- inferCommonExpr (locatedValue right)
  addConstraint leftType rightType
  return TBool
inferCommonExpr (CECall func args) = do
  funcType <- inferCommonExpr (locatedValue func)
  argTypes <- mapM (inferCommonExpr . locatedValue) args
  freshResultType <- freshTypeVar
  let expectedFuncType = TFunction argTypes freshResultType
  addConstraint funcType expectedFuncType
  return freshResultType
inferCommonExpr (CEIndex container index) = do
  containerType <- inferCommonExpr (locatedValue container)
  indexType <- inferCommonExpr (locatedValue index)
  elementType <- freshTypeVar
  case containerType of
    TList elemT -> do
      addConstraint indexType (TInt 32)
      addConstraint elementType elemT
    TDict keyT valT -> do
      addConstraint indexType keyT
      addConstraint elementType valT
    TString -> do
      addConstraint indexType (TInt 32)
      addConstraint elementType TChar
    _ -> do
      addConstraint containerType (TList elementType)
      addConstraint indexType (TInt 32)
  return elementType
inferCommonExpr (CESlice container start end) = do
  containerType <- inferCommonExpr (locatedValue container)
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
  return containerType
inferCommonExpr (CEAttribute obj attr) = do
  objType <- inferCommonExpr (locatedValue obj)
  inferAttributeAccess objType attr

-- | Enhanced attribute access inference
inferAttributeAccess :: Type -> Identifier -> TypeInferenceM Type
inferAttributeAccess objType attr = case objType of
  TStruct qualName _ -> do
    maybeTypeDef <- lookupTypeDefinition qualName
    case maybeTypeDef of
      Just typeDef -> 
        case HashMap.lookup attr (typeDefFields typeDef) of
          Just fieldType -> return fieldType
          Nothing -> 
            case HashMap.lookup attr (typeDefMethods typeDef) of
              Just methodType -> return methodType
              Nothing -> throwError $ errorToText $ AttributeNotFound attr objType (SourceSpan (T.pack "<no-file>") (SourcePos 0 0) (SourcePos 0 0))
      Nothing -> freshTypeVar  -- Type definition not found, return fresh var
  _ -> freshTypeVar  -- For other types, return fresh type variable

-- | Infer type of literals
inferLiteral :: Literal -> TypeInferenceM Type
inferLiteral (LInt _) = return $ TInt 32
inferLiteral (LUInt _) = return $ TUInt 32
inferLiteral (LFloat _) = return $ TFloat 64
inferLiteral (LBool _) = return TBool
inferLiteral (LString _) = return TString
inferLiteral (LBytes _) = return TBytes
inferLiteral (LChar _) = return TChar
inferLiteral LNone = return $ TOptional TAny

-- | Infer type of binary operations
inferBinaryOp :: BinaryOp -> Type -> Type -> TypeInferenceM Type
inferBinaryOp op leftType rightType = do
  addWarning $ "Binary op: " <> T.pack (show op) <> ", left=" <> T.pack (show leftType) <> ", right=" <> T.pack (show rightType)
  case op of
    OpAdd -> inferArithmeticOp leftType rightType
    OpSub -> inferArithmeticOp leftType rightType
    OpMul -> inferArithmeticOp leftType rightType
    OpDiv -> do
      -- For division, we should allow proper numeric type coercion
      -- and default to float division to maintain precision
      -- Allow int to float conversion by not requiring exact type equality
      addWarning $ "Division type inference: left=" <> T.pack (show leftType) <> ", right=" <> T.pack (show rightType)
      case (leftType, rightType) of
        (TInt _, TInt _) -> do
          -- Both are ints, result should be float
          -- Don't add constraint for division since both ints should result in float
          addWarning "Division: both ints, returning float"
          return (TFloat 64)
        (TFloat _, TInt _) -> do
          -- Left is float, right is int - convert int to float
          addConstraint rightType (TFloat 64)
          addWarning "Division: left float, right int, returning float"
          return (TFloat 64)
        (TInt _, TFloat _) -> do
          -- Left is int, right is float - convert int to float
          addConstraint leftType (TFloat 64)
          addWarning "Division: left int, right float, returning float"
          return (TFloat 64)
        (TFloat _, TFloat _) -> do
          -- Both floats, return float
          addConstraint leftType rightType
          addWarning "Division: both floats, returning float"
          return (TFloat 64)
        _ -> do
          -- For other numeric types, allow conversion to float
          addConstraint leftType rightType
          addWarning "Division: other case, returning result of unification"
          return (TFloat 64)
    OpMod -> inferArithmeticOp leftType rightType
    OpPow -> inferArithmeticOp leftType rightType
    OpFloorDiv -> inferArithmeticOp leftType rightType
    OpBitAnd -> inferBitwiseOp leftType rightType
    OpBitOr -> inferBitwiseOp leftType rightType
    OpBitXor -> inferBitwiseOp leftType rightType
    OpShiftL -> inferBitwiseOp leftType rightType
    OpShiftR -> inferBitwiseOp leftType rightType
    OpAnd -> do
      addConstraint leftType TBool
      addConstraint rightType TBool
      return TBool
    OpOr -> do
      addConstraint leftType TBool
      addConstraint rightType TBool
      return TBool
    OpXor -> do
      addConstraint leftType TBool
      addConstraint rightType TBool
      return TBool
    OpConcat -> do
      addConstraint leftType rightType
      return leftType
    OpIn -> do
      case rightType of
        TList elemType -> addConstraint leftType elemType
        TSet elemType -> addConstraint leftType elemType
        TDict keyType _ -> addConstraint leftType keyType
        TString -> addConstraint leftType TChar
        _ -> return ()
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
  case (leftType, rightType) of
    (TFloat _, TInt _) -> do
      -- Allow int to float conversion, return float
      addWarning $ "Arithmetic: float <- int conversion: " <> T.pack (show leftType) <> ", " <> T.pack (show rightType)
      return leftType
    (TInt _, TFloat _) -> do
      -- Allow int to float conversion, return float
      addWarning $ "Arithmetic: int -> float conversion: " <> T.pack (show leftType) <> ", " <> T.pack (show rightType)
      return rightType
    (TFloat _, TFloat _) -> do
      -- Both floats, return float
      addConstraint leftType rightType
      return leftType
    (TInt _, TInt _) -> do
      -- Both ints, return int
      addConstraint leftType rightType
      return leftType
    (TUInt _, TUInt _) -> do
      -- Both uints, return uint
      addConstraint leftType rightType
      return leftType
    _ -> do
      -- For other cases, try to unify and return left type
      addConstraint leftType rightType
      return leftType

-- | Helper for bitwise operations
inferBitwiseOp :: Type -> Type -> TypeInferenceM Type
inferBitwiseOp leftType rightType = do
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
  case operandType of
    TInt _ -> return operandType
    TFloat _ -> return operandType
    _ -> throwError "Negate operation requires numeric type"
inferUnaryOp OpBitNot operandType = do
  case operandType of
    TInt _ -> return operandType
    TUInt _ -> return operandType
    _ -> throwError "Bitwise not operation requires integer type"
inferUnaryOp OpPositive operandType = do
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
    Right subst -> do
      -- Apply substitution immediately
      applySubstitutionToState subst
      return (Just [])

-- | Core unification algorithm that returns a substitution
unify :: Type -> Type -> TypeInferenceM (Either Text Substitution)
unify t1 t2 | t1 == t2 = return $ Right HashMap.empty
unify (TVar v1) t2 = do
  if occurs v1 t2
    then return $ Left "Occurs check failed"
    else return $ Right (HashMap.singleton v1 t2)
unify t1 (TVar v2) = unify (TVar v2) t1
unify (TList t1) (TList t2) = unify t1 t2
unify (TTuple ts1) (TTuple ts2)
  | length ts1 == length ts2 = unifyList (zip ts1 ts2)
  | otherwise = return $ Left "Tuple arity mismatch"
unify (TDict k1 v1) (TDict k2 v2) = do
  s1 <- unify k1 k2
  case s1 of
    Left err -> return $ Left err
    Right subst1 -> do
      s2 <- unify (applySubstitution subst1 v1) (applySubstitution subst1 v2)
      case s2 of
        Left err -> return $ Left err
        Right subst2 -> return $ Right (composeSubst subst2 subst1)
unify (TFunction args1 ret1) (TFunction args2 ret2)
  | length args1 == length args2 = do
      s1 <- unifyList (zip args1 args2)
      case s1 of
        Left err -> return $ Left err
        Right subst1 -> do
          s2 <- unify (applySubstitution subst1 ret1) (applySubstitution subst1 ret2)
          case s2 of
            Left err -> return $ Left err
            Right subst2 -> return $ Right (composeSubst subst2 subst1)
  | otherwise = return $ Left "Function arity mismatch"
unify (TOptional t1) (TOptional t2) = unify t1 t2
unify t1 t2 = do
  -- Allow numeric type conversion (int to float)
  case (t1, t2) of
    (TFloat _, TInt _) -> do
      -- When unifying float and int, convert int to float
      return $ Right HashMap.empty  -- Allow float <- int conversion
    (TInt _, TFloat _) -> do
      -- When unifying int and float, convert int to float
      return $ Right HashMap.empty  -- Allow int -> float conversion
    (TInt bw1, TInt bw2)
      | bw1 == bw2 -> return $ Right HashMap.empty
      | otherwise -> return $ Right HashMap.empty  -- Allow different bitwidth int unification
    (TFloat bw1, TFloat bw2)
      | bw1 == bw2 -> return $ Right HashMap.empty
      | otherwise -> return $ Right HashMap.empty  -- Allow different bitwidth float unification
    -- Handle TAny (any type can unify with TAny)
    (TAny, _) -> return $ Right HashMap.empty
    (_, TAny) -> return $ Right HashMap.empty
    -- Catch-all for any remaining cases, especially involving numeric types or bit widths
    _ | isNumericType t1 && isNumericType t2 -> return $ Right HashMap.empty
    _ -> return $ Left $ "Cannot unify " <> T.pack (show t1) <> " with " <> T.pack (show t2)
  where
    -- Helper function to check if a type is numeric
    isNumericType (TInt _) = True
    isNumericType (TFloat _) = True
    isNumericType (TUInt _) = True
    isNumericType _ = False

-- | Unify a list of type pairs
unifyList :: [(Type, Type)] -> TypeInferenceM (Either Text Substitution)
unifyList [] = return $ Right HashMap.empty
unifyList ((t1, t2):rest) = do
  s1 <- unify t1 t2
  case s1 of
    Left err -> return $ Left err
    Right subst1 -> do
      let restWithSubst = map (\(a, b) -> (applySubstitution subst1 a, applySubstitution subst1 b)) rest
      s2 <- unifyList restWithSubst
      case s2 of
        Left err -> return $ Left err
        Right subst2 -> return $ Right (composeSubst subst2 subst1)

-- | Compose two substitutions
composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s2 s1 = HashMap.map (applySubstitution s2) s1 `HashMap.union` s2

-- | Apply substitution to current state
applySubstitutionToState :: Substitution -> TypeInferenceM ()
applySubstitutionToState newSubst = do
  currentState <- get
  let composedSubst = composeSubst newSubst (substitution currentState)
  let updatedConstraints = map (\(t1, t2) -> 
        (applySubstitution newSubst t1, applySubstitution newSubst t2)) 
        (constraints currentState)
  let updatedScopes = map (HashMap.map (applySubstitution newSubst)) (scopeStack currentState)
  put currentState 
    { substitution = composedSubst
    , constraints = updatedConstraints
    , scopeStack = updatedScopes
    }

-- | Check if type variable occurs in type
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
    go (TForall vars typeConstraints t) = TForall vars typeConstraints (go t)
    go (TOwned t) = TOwned (go t)
    go (TShared t) = TShared (go t)
    go (TBorrowed t) = TBorrowed (go t)
    go (TMutable t) = TMutable (go t)
    go t = t

-- | Improved incremental constraint solving
solveConstraints :: TypeInferenceM ()
solveConstraints = do
  constrs <- gets constraints
  modify $ \s -> s { constraints = [] }  -- Clear constraints
  solveLoop constrs
  where
    solveLoop :: TypeConstraints -> TypeInferenceM ()
    solveLoop [] = return ()
    solveLoop ((t1, t2):rest) = do
      -- Apply current substitution to the types
      subst <- gets substitution
      let t1' = applySubstitution subst t1
      let t2' = applySubstitution subst t2

      -- Try to unify the constraint being processed
      addWarning $ "Constraint: " <> T.pack (show t1') <> " ~ " <> T.pack (show t2')

      -- Try to unify
      result <- unify t1' t2'
      case result of
        Left err -> do
          addWarning $ "Unification failed: " <> err
          throwError err
        Right newSubst -> do
          -- Apply new substitution to remaining constraints
          let rest' = map (\(a, b) -> 
                (applySubstitution newSubst a, applySubstitution newSubst b)) rest
          -- Update state with new substitution
          applySubstitutionToState newSubst
          -- Continue with updated constraints
          solveLoop rest'

-- | Instantiate a polymorphic type with fresh type variables
instantiate :: Type -> TypeInferenceM Type
instantiate (TForall vars _constraints t) = do
  freshVars <- mapM (const freshTypeVar) vars
  let varMap = HashMap.fromList (zip vars freshVars)
  let newSubstitution = varMap
  return $ applySubstitution newSubstitution t
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
    freeVarsInType (TList elemType) = freeVarsInType elemType
    freeVarsInType (TTuple ts) = Set.unions (map freeVarsInType ts)
    freeVarsInType (TDict k v) = freeVarsInType k `Set.union` freeVarsInType v
    freeVarsInType (TSet elemType) = freeVarsInType elemType
    freeVarsInType (TOptional elemType) = freeVarsInType elemType
    freeVarsInType (TFunction args ret) = Set.unions (map freeVarsInType (ret:args))
    freeVarsInType (TMethod rec args ret) = Set.unions (map freeVarsInType (rec:ret:args))
    freeVarsInType (TStruct _ args) = Set.unions (map freeVarsInType args)
    freeVarsInType (TEnum _ args) = Set.unions (map freeVarsInType args)
    freeVarsInType (TInterface _ args) = Set.unions (map freeVarsInType args)
    freeVarsInType (TUnion ts) = Set.unions (map freeVarsInType ts)
    freeVarsInType (TGeneric _ args) = Set.unions (map freeVarsInType args)
    freeVarsInType (TForall vars _ elemType) = freeVarsInType elemType `Set.difference` Set.fromList vars
    freeVarsInType (TOwned elemType) = freeVarsInType elemType
    freeVarsInType (TShared elemType) = freeVarsInType elemType
    freeVarsInType (TBorrowed elemType) = freeVarsInType elemType
    freeVarsInType (TMutable elemType) = freeVarsInType elemType
    freeVarsInType _ = Set.empty
    
    freeVarsInEnv :: TypeEnvironment -> Set TypeVar
    freeVarsInEnv = Set.unions . map freeVarsInType . HashMap.elems

-- | Infer types for Python statements
inferPythonStatement :: PythonStmt -> TypeInferenceM ()
inferPythonStatement stmt = case stmt of
  PyExprStmt expr -> do
    _ <- inferPythonExpr (locatedValue expr)
    return ()
  
  PyAssign patterns expr -> do
    rvalueType <- inferPythonExpr (locatedValue expr)
    lvalueTypes <- forM patterns $ \pattern -> do
      patternType <- freshTypeVar
      inferPythonPattern (locatedValue pattern) patternType
      return patternType
    addWarning $ "Assignment: lvalueTypes=" <> T.pack (show lvalueTypes) <> ", rvalueType=" <> T.pack (show rvalueType)
    mapM_ (\lvalueType -> addConstraint lvalueType rvalueType) lvalueTypes
    return ()
  
  PyAugAssign target op value -> do
    targetType <- freshTypeVar
    inferPythonPattern (locatedValue target) targetType
    valueType <- inferPythonExpr (locatedValue value)
    operationResultType <- inferBinaryOp op targetType valueType
    inferPythonPattern (locatedValue target) operationResultType
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
  
  PyFor { pyForTarget = target, pyForIter = iter, pyForBody = body, pyForElse = elseClause } -> do
    iterType <- inferPythonExpr (locatedValue iter)
    elemType <- freshTypeVar
    addConstraint iterType (TList elemType)
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
  
  _ -> return ()

-- | Helper function to extract value from Go.Located
-- | Infer types for Go statements
inferGoStatement :: GoStmt -> TypeInferenceM ()
inferGoStatement stmt = case stmt of
  GoExprStmt expr -> do
    _ <- inferGoExpr (goLocatedValue expr)
    return ()
  
  GoBind binding -> do
    case bindKind binding of
      BindAssign -> do
        case bindLHS binding of
          LHSExprs lvalues -> do
            lvalueTypes <- mapM (inferGoExpr . goLocatedValue) lvalues
            rvalueTypes <- mapM (inferGoExpr . goLocatedValue) (bindRHS binding)
            mapM_ (uncurry addConstraint) (zip lvalueTypes rvalueTypes)
          _ -> return ()
      BindDefine -> do
        case bindLHS binding of
          LHSIdents identifiers -> do
            rvalueTypes <- mapM (inferGoExpr . goLocatedValue) (bindRHS binding)
            mapM_ (uncurry bindVarType) (zip identifiers rvalueTypes)
          _ -> return ()
      BindVar -> do
        case bindLHS binding of
          LHSIdents identifiers -> do
            mapM_ (\ident -> inferGoVarDecl (ident, Nothing, Nothing)) identifiers
          _ -> return ()
    return ()
  
  GoReturn exprs -> do
    _ <- mapM (inferGoExpr . goLocatedValue) exprs
    return ()
  
  GoIf _maybeInit condition thenStmt maybeElseStmt -> do
    condType <- inferGoExpr (goLocatedValue condition)
    addConstraint condType TBool
    _ <- inferGoStatement (goLocatedValue thenStmt)
    case maybeElseStmt of
      Just elseStmt -> do
        _ <- inferGoStatement (goLocatedValue elseStmt)
        return ()
      Nothing -> return ()
    return ()
  
  GoFor maybeClause body -> do
    case maybeClause of
      Just clause -> do
        case goForCond clause of
          Just cond -> do
            condType <- inferGoExpr (goLocatedValue cond)
            addConstraint condType TBool
          Nothing -> return ()
      Nothing -> return ()
    _ <- inferGoStatement (goLocatedValue body)
    return ()
  
  _ -> return ()

-- | Generic statement inference dispatcher
inferStatement :: Either PythonStmt GoStmt -> TypeInferenceM ()
inferStatement (Left pyStmt) = inferPythonStatement pyStmt
inferStatement (Right goStmt) = inferGoStatement goStmt

-- | Enhanced Python function definition inference
inferPythonFuncDef :: PythonFuncDef -> TypeInferenceM ()
inferPythonFuncDef funcDef = do
  paramTypes <- mapM (inferPythonParameter . locatedValue) (pyFuncParams funcDef)
  returnType <- freshTypeVar
  let funcType = TFunction paramTypes returnType
  
  bindVarType (pyFuncName funcDef) funcType
  
  withNewScope $ do
    -- Bind parameters in function scope
    zipWithM_ bindParamType (pyFuncParams funcDef) paramTypes
    -- Infer function body
    mapM_ (inferPythonStatement . locatedValue) (pyFuncBody funcDef)
  where
    bindParamType param ty = case locatedValue param of
      ParamNormal name _ _ -> bindVarType name ty
      ParamVarArgs name _ -> bindVarType name ty
      ParamKwArgs name _ -> bindVarType name ty
      ParamKwOnly name _ _ -> bindVarType name ty
      ParamPosOnly name _ _ -> bindVarType name ty

-- | Enhanced Python class definition inference
inferPythonClassDef :: PythonClassDef -> TypeInferenceM ()
inferPythonClassDef classDef = do
  let className = QualifiedName [] (pyClassName classDef)
  let classType = TStruct className []
  
  bindVarType (pyClassName classDef) classType
  
  -- Collect class fields and methods
  fields <- collectClassFields (pyClassBody classDef)
  methods <- collectClassMethods classType (pyClassBody classDef)
  
  -- Register type definition
  let typeDef = TypeDefinition
        { typeDefName = className
        , typeDefFields = fields
        , typeDefMethods = methods
        , typeDefParent = Nothing
        }
  registerTypeDefinition className typeDef

  let selfType = TStruct className []
  
  withNewScope $ do
    bindVarType (Identifier "self") selfType
    mapM_ (inferPythonStatement . locatedValue) (pyClassBody classDef)
  where
    collectClassFields :: [Common.Located Python.PythonStmt] -> TypeInferenceM (HashMap Common.Identifier Type)
    collectClassFields _ = do
      -- Simplified: collect assignments in __init__ method
      return HashMap.empty
    
    collectClassMethods :: Type -> [Common.Located Python.PythonStmt] -> TypeInferenceM (HashMap Common.Identifier Type)
    collectClassMethods ctype stmts = do
      let methods = mapMaybe extractMethod stmts
      foldM (inferMethod ctype) HashMap.empty methods
    
    extractMethod :: Common.Located Python.PythonStmt -> Maybe Python.PythonFuncDef
    extractMethod (Common.Located _ (Python.PyFuncDef def)) = Just def
    extractMethod _ = Nothing
    
    inferMethod :: Type -> HashMap Identifier Type -> PythonFuncDef -> TypeInferenceM (HashMap Identifier Type)
    inferMethod ctype acc funcDef = do
      paramTypes <- mapM (inferPythonParameter . locatedValue) (pyFuncParams funcDef)
      returnType <- freshTypeVar
      let methodType = TMethod ctype (drop 1 paramTypes) returnType  -- Drop self parameter
      return $ HashMap.insert (pyFuncName funcDef) methodType acc

-- | Infer types for Go declarations
inferGoDecl :: GoDecl -> TypeInferenceM ()
inferGoDecl decl = case decl of
  GoConstDecl constDecls -> do
    mapM_ processGoConstSpec constDecls
    return ()
  
  GoTypeDeclStmt goTypeDecl -> do
    let name = goTypeDeclName goTypeDecl
        typeLoc = goTypeDeclType goTypeDecl
    typeVal <- inferGoType (goLocatedValue typeLoc)
    bindVarType name typeVal
    return ()
  
  GoBindDecl bindings -> do
    mapM_ inferGoBinding bindings
    return ()
  
  GoFuncDecl func -> do
    inferGoFunction func
    return ()
  
  GoMethodDecl receiver func -> do
    withNewScope $ do
      _ <- inferGoReceiver receiver
      inferGoFunction func
    return ()
  
  _ -> return ()
  where
    processGoConstSpec spec = do
      let names = Go.constNames spec
          maybeType = Go.constType spec
          maybeValues = Go.constValues spec
      case maybeValues of
        Just values -> case length names == length values of
          True -> mapM_ (\(name, value) -> inferGoConstDecl (name, maybeType, value)) (zip names values)
          False -> return ()  -- Error case: mismatched names and values
        Nothing -> return ()  -- Error case: no values provided

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
  
  mapM_ (inferPythonImport . locatedValue) (pyModuleImports pythonModule)
  mapM_ (inferPythonStatement . locatedValue) (pyModuleBody pythonModule)
  
inferProgram (Right goAST) = do
  let goPackage' = goPackage goAST
  
  mapM_ inferGoFile (goPackageFiles goPackage')
  where
    inferGoFile :: GoFile -> TypeInferenceM ()
    inferGoFile file = do
      mapM_ (inferGoImport . goLocatedValue) (goFileImports file)
      mapM_ (inferGoDecl . goLocatedValue) (goFileDecls file)

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
  solveConstraints
  remainingConstraints <- gets constraints
  return $ null remainingConstraints  -- All constraints should be solved

-- isLeft is unused in the current implementation
-- isLeft (Left _) = True
-- isLeft (Right _) = False

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
    argTypes <- mapM (inferPythonArgument . locatedValue) args

    funcType <- case locatedValue func of
      PyVar var -> do
        -- Check if it's a builtin function
        case getPythonBuiltinType var of
          Just builtinType -> 
            -- Handle variadic functions like print
            if isVariadicBuiltin var
            then return builtinType  -- Don't add constraints for variadic functions
            else return builtinType
          Nothing -> do
            envLookup <- lookupVarType var `catchError` (\_ -> freshTypeVar)
            return envLookup
      _ -> inferPythonExpr (locatedValue func)

    returnType <- freshTypeVar
    let expectedFuncType = TFunction argTypes returnType
    
    -- Skip constraint checking for variadic builtin functions
    case locatedValue func of
      PyVar var -> 
        if isVariadicBuiltin var
        then return returnType
        else do
          addConstraint funcType expectedFuncType
          return returnType
      _ -> do
        addConstraint funcType expectedFuncType
        return returnType
  PyAttribute obj attr -> do
    objType <- inferPythonExpr (locatedValue obj)
    inferAttributeAccess objType attr
  PySubscript obj slice -> do
    objType <- inferPythonExpr (locatedValue obj)
    _ <- inferPythonSlice (locatedValue slice)
    elemType <- freshTypeVar
    addConstraint objType (TList elemType)
    return elemType
  PyList elems -> do
    elemType <- if null elems
      then freshTypeVar
      else do
        types <- mapM (inferPythonExpr . locatedValue) elems
        case types of
          [] -> do
            elemType <- freshTypeVar
            return elemType
          (firstType:restTypes) -> do
            mapM_ (addConstraint firstType) restTypes
            return firstType
    return $ TList elemType
  PyDict entries -> do
    keyType <- freshTypeVar
    valueType <- freshTypeVar
    forM_ entries $ \(k, v) -> do
      kType <- inferPythonExpr (locatedValue k)
      vType <- inferPythonExpr (locatedValue v)
      addConstraint kType keyType
      addConstraint vType valueType
    return $ TDict keyType valueType
  _ -> freshTypeVar  -- For other expressions, return fresh type variable

-- | Infer type from Python slice
inferPythonSlice :: Python.PythonSlice -> TypeInferenceM Type
inferPythonSlice slice = case slice of
  Python.SliceIndex expr -> inferPythonExpr (locatedValue expr)
  Python.SliceSlice maybeLower maybeUpper maybeStep -> do
    _ <- mapM (inferPythonExpr . locatedValue) maybeLower
    _ <- mapM (inferPythonExpr . locatedValue) maybeUpper
    _ <- mapM (inferPythonExpr . locatedValue) maybeStep
    return $ TInt 32
  Python.SliceExtSlice slices -> do
    _ <- mapM (inferPythonSlice . locatedValue) slices
    return $ TInt 32

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
  PyInt i -> do
    addWarning $ "Literal int: " <> T.pack (show i) <> ", returning TInt 32"
    return $ TInt 32
  PyFloat _ -> do
    addWarning "Literal float: returning TFloat 64"
    return $ TFloat 64
  PyComplex _ _ -> return $ TComplex (TFloat 64)
  PyString _ -> return TString
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
  _ -> return ()

-- | Infer type from Python parameter
inferPythonParameter :: PythonParameter -> TypeInferenceM Type
inferPythonParameter param = case param of
  ParamNormal name maybeType _maybeDefault -> do
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
  ParamKwOnly name maybeType _maybeDefault -> do
    valueType <- case maybeType of
      Just typeExpr -> inferPythonTypeExpr (locatedValue typeExpr)
      Nothing -> freshTypeVar
    let kwOnlyType = TDict TString valueType
    bindVarType name kwOnlyType
    return kwOnlyType
  ParamPosOnly name maybeType _maybeDefault -> do
    paramType <- case maybeType of
      Just typeExpr -> inferPythonTypeExpr (locatedValue typeExpr)
      Nothing -> freshTypeVar
    bindVarType name paramType
    return paramType

-- | Infer type from Python type expression
inferPythonTypeExpr :: Python.PythonTypeExpr -> TypeInferenceM Type
inferPythonTypeExpr typeExpr = case typeExpr of
  Python.TypeVar var -> return $ TVar (Common.TypeVar var)
  Python.TypeName name -> inferTypeFromName name
  Python.TypeSubscript baseType args -> inferParameterizedType baseType args
  Python.TypeTuple elemExprs -> do
    elemTypes <- mapM (inferPythonTypeExpr . locatedValue) elemExprs
    return $ TTuple elemTypes
  Python.TypeOptional innerTypeExpr -> do
    innerType <- inferPythonTypeExpr (locatedValue innerTypeExpr)
    return $ TOptional innerType
  _ -> freshTypeVar
  where
    inferTypeFromName :: QualifiedName -> TypeInferenceM Type
    inferTypeFromName (QualifiedName [] (Identifier "int")) = return $ TInt 32
    inferTypeFromName (QualifiedName [] (Identifier "float")) = return $ TFloat 64
    inferTypeFromName (QualifiedName [] (Identifier "bool")) = return TBool
    inferTypeFromName (QualifiedName [] (Identifier "str")) = return TString
    inferTypeFromName (QualifiedName [] (Identifier "bytes")) = return TBytes
    inferTypeFromName _ = freshTypeVar
    
    inferParameterizedType :: Common.Located Python.PythonTypeExpr -> [Common.Located Python.PythonTypeExpr] -> TypeInferenceM Type
    inferParameterizedType baseType args = case (baseType, args) of
      (Common.Located _ (Python.TypeName (QualifiedName [] (Identifier "list"))), [elemTypeExpr]) -> do
        elemT <- inferPythonTypeExpr (locatedValue elemTypeExpr)
        return $ TList elemT
      (Common.Located _ (Python.TypeName (QualifiedName [] (Identifier "dict"))), [keyTypeExpr, valueTypeExpr]) -> do
        keyT <- inferPythonTypeExpr (locatedValue keyTypeExpr)
        valueT <- inferPythonTypeExpr (locatedValue valueTypeExpr)
        return $ TDict keyT valueT
      _ -> freshTypeVar

-- | Enhanced Python import handling
inferPythonImport :: PythonImport -> TypeInferenceM ()
inferPythonImport imp = case imp of
  ImportModule moduleName maybeAlias -> do
    -- Load module types (simplified: add standard library types)
    let modIdentifier = case moduleName of
            ModuleName name -> Identifier name
    moduleEnv <- loadPythonModule modIdentifier
    let aliasText = case moduleName of
            ModuleName name -> name
        alias = fromMaybe (Identifier aliasText) maybeAlias
    -- Store module environment
    modify $ \s -> s { importedModules = HashMap.insert (case alias of Identifier name -> name) moduleEnv (importedModules s) }
  ImportFrom moduleName items _level -> do
    let modIdentifier = case moduleName of
            ModuleName name -> Identifier name
    moduleEnv <- loadPythonModule modIdentifier
    forM_ items $ \name -> do
      case HashMap.lookup name moduleEnv of
        Just ty -> bindVarType name ty
        Nothing -> return ()
  ImportFromStar moduleName -> do
    let modIdentifier = case moduleName of
            ModuleName name -> Identifier name
    _moduleEnv <- loadPythonModule modIdentifier
    -- For star imports, we can't determine what's being imported ahead of time
    -- In a real implementation, we'd need to analyze the module and extract all public names
    return ()
  where
    loadPythonModule :: Identifier -> TypeInferenceM TypeEnvironment
    loadPythonModule (Identifier modName) = 
      case modName of
        "typing" -> return typingModule
        "math" -> return mathModule
        "array" -> return arrayModule
        "sys" -> return sysModule
        _ -> return HashMap.empty
    
    typingModule = HashMap.fromList
      [ (Identifier "List", TList TAny)
      , (Identifier "Dict", TDict TAny TAny)
      , (Identifier "Optional", TOptional TAny)
      ]
    
    mathModule = HashMap.fromList
      [ (Identifier "sqrt", TFunction [TFloat 64] (TFloat 64))
      , (Identifier "sin", TFunction [TFloat 64] (TFloat 64))
      , (Identifier "cos", TFunction [TFloat 64] (TFloat 64))
      ]
    
    arrayModule = HashMap.fromList
      [ (Identifier "array", TFunction [TString, TList (TInt (BitWidth 32))] (TList (TInt (BitWidth 32))))
      ]
    
    sysModule = HashMap.fromList
      [ (Identifier "exit", TFunction [TInt 32] TVoid)
      ]

-- | Infer type from Go expression
inferGoExpr :: GoExpr -> TypeInferenceM Type
inferGoExpr expr = case expr of
  GoLiteral lit -> inferGoLiteral lit
  GoIdent var -> case var of
        Go.Identifier name -> lookupVarType (Identifier name)
  GoBinaryOp op left right -> do
    leftType <- inferGoExpr (goLocatedValue left)
    rightType <- inferGoExpr (goLocatedValue right)
    let commonOp = case op of
          Go.OpAdd -> Common.OpAdd
          Go.OpSub -> Common.OpSub
          Go.OpMul -> Common.OpMul
          Go.OpQuo -> Common.OpDiv
          Go.OpRem -> Common.OpMod
          Go.OpAnd -> Common.OpBitAnd
          Go.OpOr -> Common.OpBitOr
          Go.OpXor -> Common.OpBitXor
          Go.OpShiftL -> Common.OpShiftL
          Go.OpShiftR -> Common.OpShiftR
          Go.OpAndAnd -> Common.OpAnd
          Go.OpOrOr -> Common.OpOr
          Go.OpAndNot -> Common.OpBitXor  -- Map &^ to bitwise XOR as fallback
    inferBinaryOp commonOp leftType rightType
  GoUnaryOp op operand -> do
    operandType <- inferGoExpr (goLocatedValue operand)
    let commonOp = case op of
          Go.OpPos -> Common.OpPositive
          Go.OpNeg -> Common.OpNegate
          Go.OpNot -> Common.OpNot
          Go.OpBitNot -> Common.OpBitNot
    inferUnaryOp commonOp operandType
  GoCall func args -> do
    funcType <- inferGoExpr (goLocatedValue func)
    argTypes <- mapM (inferGoExpr . goLocatedValue) args
    callResultType <- freshTypeVar
    let expectedFuncType = TFunction argTypes callResultType
    addConstraint funcType expectedFuncType
    return callResultType
  GoSelector obj field -> do
    objType <- inferGoExpr (goLocatedValue obj)
    inferAttributeAccess objType field
  GoIndex obj index -> do
    objType <- inferGoExpr (goLocatedValue obj)
    indexType <- inferGoExpr (goLocatedValue index)
    elemType <- freshTypeVar
    addConstraint objType (TList elemType)
    addConstraint indexType (TInt 32)
    return elemType
  _ -> freshTypeVar

-- | Infer type from Go literal
inferGoLiteral :: GoLiteral -> TypeInferenceM Type
inferGoLiteral lit = case lit of
  GoInt _ -> return $ TInt 32
  GoFloat _ -> return $ TFloat 64
  GoImag _ -> return $ TFloat 64  -- Imaginary numbers treated as floats
  GoBool _ -> return TBool
  GoString _ -> return TString
  GoRawString _ -> return TString  -- Raw strings treated as regular strings
  GoRune _ -> return TChar
  GoNil -> return $ TOptional TAny

-- | Infer type from Go type specification
inferGoType :: GoType -> TypeInferenceM Type
inferGoType goType = case goType of
  GoBasicType name -> inferGoBasicType name
  GoSliceType elemType -> do
    elemT <- inferGoType (goLocatedValue elemType)
    return $ TList elemT
  GoMapType keyType valueType -> do
    keyT <- inferGoType (goLocatedValue keyType)
    valueT <- inferGoType (goLocatedValue valueType)
    return $ TDict keyT valueT
  GoPointerType baseType -> do
    baseT <- inferGoType (goLocatedValue baseType)
    return $ TOwned baseT
  GoInterfaceType _methods -> 
    return $ TInterface (QualifiedName [] (Identifier "Interface")) []
  GoTypeParam identifier _constraint -> 
    return $ TVar (Common.TypeVar (unIdentifier identifier))
  GoGenericType name args -> do
    argTypes <- mapM (inferGoType . goLocatedValue) args
    return $ TGeneric name argTypes
  _ -> freshTypeVar
  where
    unIdentifier (Identifier name) = name
    
    inferGoBasicType :: Identifier -> TypeInferenceM Type
    inferGoBasicType (Identifier name) = case name of
      "int" -> return $ TInt 32
      "int8" -> return $ TInt 8
      "int16" -> return $ TInt 16
      "int32" -> return $ TInt 32
      "int64" -> return $ TInt 64
      "uint" -> return $ TUInt 32
      "uint8" -> return $ TUInt 8
      "uint16" -> return $ TUInt 16
      "uint32" -> return $ TUInt 32
      "uint64" -> return $ TUInt 64
      "float32" -> return $ TFloat 32
      "float64" -> return $ TFloat 64
      "bool" -> return TBool
      "string" -> return TString
      "byte" -> return $ TUInt 8
      "rune" -> return $ TInt 32
      _ -> freshTypeVar

-- | Infer Go variable declaration
inferGoVarDecl :: (Identifier, Maybe (Go.Located GoType), Maybe (Go.Located GoExpr)) -> TypeInferenceM ()
inferGoVarDecl (name, maybeType, maybeExpr) = do
  declaredType <- case maybeType of
    Just goType -> inferGoType (goLocatedValue goType)
    Nothing -> freshTypeVar
  
  case maybeExpr of
    Just expr -> do
      exprType <- inferGoExpr (goLocatedValue expr)
      addConstraint exprType declaredType
    Nothing -> return ()
  
  bindVarType name declaredType

-- | Infer Go constant declaration
inferGoConstDecl :: (Identifier, Maybe (Go.Located GoType), Go.Located GoExpr) -> TypeInferenceM ()
inferGoConstDecl (name, maybeType, expr) = do
  exprType <- inferGoExpr (goLocatedValue expr)
  
  declaredType <- case maybeType of
    Just goType -> do
      t <- inferGoType (goLocatedValue goType)
      addConstraint exprType t
      return t
    Nothing -> return exprType
  
  bindVarType name declaredType

-- | Infer Go function
inferGoFunction :: GoFunction -> TypeInferenceM ()
inferGoFunction _func = do
  -- Simplified implementation
  return ()

-- | Infer Go receiver
inferGoReceiver :: GoReceiver -> TypeInferenceM Type
inferGoReceiver receiver = do
  receiverType <- inferGoType (goLocatedValue $ goReceiverType receiver)
  return receiverType

-- | Enhanced Go import handling
inferGoImport :: GoImport -> TypeInferenceM ()
inferGoImport imp = case imp of
  GoImportNormal maybeAlias path -> do
    moduleEnv <- loadGoPackage path
    let aliasText = case fromMaybe (extractPackageName path) maybeAlias of
          Identifier text -> text
    modify $ \s -> s { importedModules = HashMap.insert aliasText moduleEnv (importedModules s) }
  GoImportDot path -> do
    moduleEnv <- loadGoPackage path
    let Identifier aliasText = extractPackageName path
    modify $ \s -> s { importedModules = HashMap.insert aliasText moduleEnv (importedModules s) }
  GoImportBlank path -> do
    moduleEnv <- loadGoPackage path
    let Identifier aliasText = extractPackageName path
    modify $ \s -> s { importedModules = HashMap.insert aliasText moduleEnv (importedModules s) }
  where
    extractPackageName :: Text -> Identifier
    extractPackageName path = 
      let parts = T.splitOn "/" path
      in Identifier $ last parts
    
    loadGoPackage :: Text -> TypeInferenceM TypeEnvironment
    loadGoPackage path = case path of
      "fmt" -> return fmtPackage
      "math" -> return mathPackage
      _ -> return HashMap.empty
    
    fmtPackage = HashMap.fromList
      [ (Identifier "Printf", TFunction [TString] TVoid)
      , (Identifier "Println", TFunction [TAny] TVoid)
      ]
    
    mathPackage = HashMap.fromList
      [ (Identifier "Sqrt", TFunction [TFloat 64] (TFloat 64))
      , (Identifier "Sin", TFunction [TFloat 64] (TFloat 64))
      ]

-- | Get type for Python builtin function
getPythonBuiltinType :: Identifier -> Maybe Type
getPythonBuiltinType (Identifier name) = case name of
  "print" -> Just $ TFunction [TAny] TVoid  -- print can take any type and returns void (variadic)
  "len" -> Just $ TFunction [TAny] (TInt 32)  -- len returns int
  "range" -> Just $ TFunction [TInt 32, TInt 32, TInt 32] (TList (TInt 32))  -- range returns list of int
  "str" -> Just $ TFunction [TAny] TString  -- str converts any type to string
  "int" -> Just $ TFunction [TAny] (TInt 32)  -- int converts any type to int
  "float" -> Just $ TFunction [TAny] (TFloat 64)  -- float converts any type to float
  "bool" -> Just $ TFunction [TAny] TBool  -- bool converts any type to bool
  "abs" -> Just $ TFunction [TAny] TAny  -- abs returns same type as input
  "min" -> Just $ TFunction [TAny, TAny] TAny  -- min returns same type as inputs
  "max" -> Just $ TFunction [TAny, TAny] TAny  -- max returns same type as inputs
  _ -> Nothing

-- | Check if a builtin function is variadic (can take variable number of arguments)
isVariadicBuiltin :: Identifier -> Bool
isVariadicBuiltin (Identifier name) = name `elem` ["print"]

-- | Infer types for Go bindings
inferGoBinding :: GoBinding -> TypeInferenceM ()
inferGoBinding binding = case bindKind binding of
  BindVar -> do
    case bindLHS binding of
      LHSIdents identifiers -> do
        case bindType binding of
          Just typeLoc -> do
            typeVal <- inferGoType (goLocatedValue typeLoc)
            mapM_ (`bindVarType` typeVal) identifiers
          Nothing -> do
            -- No type annotation, infer from RHS if available
            case bindRHS binding of
              (expr:_) -> do
                typeVal <- inferGoExpr (goLocatedValue expr)
                mapM_ (`bindVarType` typeVal) identifiers
              [] -> mapM_ (`bindVarType` TAny) identifiers
      _ -> return ()
  _ -> return ()