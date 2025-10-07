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
  , freshTypeVar
  , applySubstitution
  , lookupVarType
  , bindVarType
  , inferCommonExpr
  , checkTypes
  , solveConstraints
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

-- Added missing helper definitions after refactor --
inferArithmeticOp :: Type -> Type -> TypeInferenceM Type
inferArithmeticOp l r = do addConstraint l r; return l

inferBitwiseOp :: Type -> Type -> TypeInferenceM Type
inferBitwiseOp l r = do addConstraint l r; return l

inferUnaryOp :: UnaryOp -> Type -> TypeInferenceM Type
inferUnaryOp _ t = return t

applySubstitution :: Substitution -> Type -> Type
applySubstitution subst t = case t of
  TVar (TypeVar name) -> HashMap.lookupDefault t (TypeVar name) subst
  TList elemT -> TList (applySubstitution subst elemT)
  TTuple ts -> TTuple (map (applySubstitution subst) ts)
  TDict k v -> TDict (applySubstitution subst k) (applySubstitution subst v)
  TSet e -> TSet (applySubstitution subst e)
  TOptional e -> TOptional (applySubstitution subst e)
  TFunction args ret -> TFunction (map (applySubstitution subst) args) (applySubstitution subst ret)
  TMethod recv args ret -> TMethod (applySubstitution subst recv) (map (applySubstitution subst) args) (applySubstitution subst ret)
  TStruct q ts -> TStruct q (map (applySubstitution subst) ts)
  TEnum q ts -> TEnum q (map (applySubstitution subst) ts)
  TInterface q ts -> TInterface q (map (applySubstitution subst) ts)
  TUnion ts -> TUnion (map (applySubstitution subst) ts)
  TOwned x -> TOwned (applySubstitution subst x)
  TShared x -> TShared (applySubstitution subst x)
  TBorrowed x -> TBorrowed (applySubstitution subst x)
  TMutable x -> TMutable (applySubstitution subst x)
  TComplex x -> TComplex (applySubstitution subst x)
  _ -> t

-- Added stubs for driver compatibility after parser refactor
inferASTType :: Either PythonAST GoAST -> TypeInferenceM ()
inferASTType _ = return ()
solveConstraints :: TypeInferenceM ()
solveConstraints = return ()
checkTypes :: TypeInferenceM Bool
checkTypes = return True
