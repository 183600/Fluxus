{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Fluxus.Optimization.BasicPasses
  ( OptimizationM
  , OptimizationState(..)
  , OptimizationResult(..)
  , BasicPassConfig(..)
  , runBasicOptimizations
  , constantFolding
  , deadCodeElimination
  , constantPropagation
  , algebraicSimplification
  , peepholeOptimization
  , commonSubexpressionElimination
  , strengthReduction
  ) where

import Fluxus.AST.Common
import Fluxus.AST.Python
import Fluxus.AST.Go
import Control.Monad.State
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust, catMaybes)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable(..))

type OptimizationM = ReaderT BasicPassConfig (State OptimizationState)

-- | Scope for variable tracking
data Scope = Scope
  { scopeConstants :: !(HashMap Identifier Literal)
  , scopeLiveVars :: !(Set Identifier)
  , scopeDefinedVars :: !(Set Identifier)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Common expression type for CSE
data CommonExpr = CommonExpr
  { ceOp :: !Text
  , ceOperands :: ![Text]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

instance Hashable CommonExpr where
  hashWithSalt salt (CommonExpr op operands) = 
    hashWithSalt salt (op, operands)

-- | Configuration for basic optimization passes
data BasicPassConfig = BasicPassConfig
  { bpcEnableConstantFolding :: !Bool
  , bpcEnableDeadCodeElimination :: !Bool  
  , bpcEnableConstantPropagation :: !Bool
  , bpcEnableAlgebraicSimplification :: !Bool
  , bpcEnablePeepholeOptimization :: !Bool
  , bpcEnableCSE :: !Bool
  , bpcEnableStrengthReduction :: !Bool
  , bpcMaxIterations :: !Int
  , bpcAggressiveOptimization :: !Bool
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | State for optimization passes
data OptimizationState = OptimizationState
  { osScopeStack :: ![Scope]                             -- Stack of scopes
  , osGlobalConstants :: !(HashMap Identifier Literal)   -- Global constants
  , osDeadCode :: ![Text]                                -- Dead code identified
  , osSubexpressions :: !(HashMap CommonExpr Identifier) -- Common subexpressions
  , osOptimizations :: ![Text]                           -- Applied optimizations
  , osIterationCount :: !Int                             -- Current iteration
  , osChanged :: !Bool                                   -- Whether changes were made
  , osConstantsFoldedCount :: !Int                       -- Count of constants folded
  , osDeadCodeRemovedCount :: !Int                       -- Count of dead code removed
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- | Result of optimization
data OptimizationResult = OptimizationResult  
  { orPythonAST :: !(Maybe PythonAST)
  , orGoAST :: !(Maybe GoAST)
  , orOptimizations :: ![Text]
  , orIterations :: !Int
  , orConstantsFolded :: !Int
  , orDeadCodeRemoved :: !Int
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- | Default configuration
defaultConfig :: BasicPassConfig
defaultConfig = BasicPassConfig
  { bpcEnableConstantFolding = True
  , bpcEnableDeadCodeElimination = True
  , bpcEnableConstantPropagation = True
  , bpcEnableAlgebraicSimplification = True
  , bpcEnablePeepholeOptimization = True
  , bpcEnableCSE = True
  , bpcEnableStrengthReduction = True
  , bpcMaxIterations = 10
  , bpcAggressiveOptimization = False
  }

-- | Initial state
initialState :: OptimizationState
initialState = OptimizationState
  { osScopeStack = [emptyScope]
  , osGlobalConstants = HashMap.empty
  , osDeadCode = []
  , osSubexpressions = HashMap.empty
  , osOptimizations = []
  , osIterationCount = 0
  , osChanged = False
  , osConstantsFoldedCount = 0
  , osDeadCodeRemovedCount = 0
  }

emptyScope :: Scope
emptyScope = Scope HashMap.empty Set.empty Set.empty

-- Scope management functions
pushScope :: OptimizationM ()
pushScope = modify $ \s -> s { osScopeStack = emptyScope : osScopeStack s }

popScope :: OptimizationM ()
popScope = modify $ \s -> s { osScopeStack = drop 1 (osScopeStack s) }

withNewScope :: OptimizationM a -> OptimizationM a
withNewScope action = do
  pushScope
  result <- action
  popScope
  return result

lookupConstant :: Identifier -> OptimizationM (Maybe Literal)
lookupConstant ident = do
  scopes <- gets osScopeStack
  let searchScopes [] = Nothing
      searchScopes (s:ss) = case HashMap.lookup ident (scopeConstants s) of
        Just val -> Just val
        Nothing -> searchScopes ss
  return $ searchScopes scopes

addConstant :: Identifier -> Literal -> OptimizationM ()
addConstant ident lit = modify $ \s ->
  case osScopeStack s of
    [] -> s
    (scope:rest) -> s { osScopeStack = scope { scopeConstants = HashMap.insert ident lit (scopeConstants scope) } : rest }

markLive :: Identifier -> OptimizationM ()
markLive ident = modify $ \s ->
  case osScopeStack s of
    [] -> s
    (scope:rest) -> s { osScopeStack = scope { scopeLiveVars = Set.insert ident (scopeLiveVars scope) } : rest }

isLive :: Identifier -> OptimizationM Bool
isLive ident = do
  scopes <- gets osScopeStack
  return $ any (Set.member ident . scopeLiveVars) scopes

markDefined :: Identifier -> OptimizationM ()
markDefined ident = modify $ \s ->
  case osScopeStack s of
    [] -> s
    (scope:rest) -> s { osScopeStack = scope { scopeDefinedVars = Set.insert ident (scopeDefinedVars scope) } : rest }

isDefined :: Identifier -> OptimizationM Bool
isDefined ident = do
  scopes <- gets osScopeStack
  return $ any (Set.member ident . scopeDefinedVars) scopes

-- | Run basic optimizations on either Python or Go AST
runBasicOptimizations :: BasicPassConfig -> Either PythonAST GoAST -> OptimizationResult
runBasicOptimizations config ast = 
  let (result, finalState) = runState (runReaderT (optimizeAST ast) config) initialState
  in result { orOptimizations = reverse $ osOptimizations finalState
           , orIterations = osIterationCount finalState
           , orConstantsFolded = osConstantsFoldedCount finalState
           , orDeadCodeRemoved = osDeadCodeRemovedCount finalState
           }

-- | Main optimization function that runs all passes iteratively
optimizeAST :: Either PythonAST GoAST -> OptimizationM OptimizationResult
optimizeAST originalAST = do
  config <- ask
  
  let runPasses ast = do
        modify $ \s -> s { osChanged = False }
        
        -- Run all enabled passes in order
        ast1 <- if bpcEnableConstantFolding config then constantFoldingPass ast else return ast
        ast2 <- if bpcEnableConstantPropagation config then constantPropagationPass ast1 else return ast1
        ast3 <- if bpcEnableAlgebraicSimplification config then algebraicSimplificationPass ast2 else return ast2
        ast4 <- if bpcEnableStrengthReduction config then strengthReductionPass ast3 else return ast3
        ast5 <- if bpcEnableCSE config then commonSubexpressionEliminationPass ast4 else return ast4
        ast6 <- if bpcEnableDeadCodeElimination config then deadCodeEliminationPass ast5 else return ast5
        ast7 <- if bpcEnablePeepholeOptimization config then peepholeOptimizationPass ast6 else return ast6
        
        return ast7
  
  -- Fixed-point iteration
  let iterate ast iterCount = do
        modify $ \s -> s { osIterationCount = iterCount }
        
        if iterCount >= bpcMaxIterations config
          then return ast
          else do
            newAST <- runPasses ast
            changed <- gets osChanged
            if changed
              then iterate newAST (iterCount + 1)
              else return newAST
  
  finalAST <- iterate originalAST 0
  
  state <- get
  return $ OptimizationResult
    { orPythonAST = case finalAST of
        Left pyAST -> Just pyAST
        Right _ -> Nothing
    , orGoAST = case finalAST of
        Right goAST -> Just goAST
        Left _ -> Nothing
    , orOptimizations = []
    , orIterations = 0
    , orConstantsFolded = 0
    , orDeadCodeRemoved = 0
    }

-- ============================================================================
-- CONSTANT FOLDING PASS
-- ============================================================================

constantFoldingPass :: Either PythonAST GoAST -> OptimizationM (Either PythonAST GoAST)
constantFoldingPass (Left pyAST) = Left <$> constantFoldingPython pyAST
constantFoldingPass (Right goAST) = Right <$> constantFoldingGo goAST

constantFoldingPython :: PythonAST -> OptimizationM PythonAST
constantFoldingPython (PythonModule stmts) = do
  newStmts <- mapM constantFoldPythonStmt stmts
  return $ PythonModule newStmts

constantFoldPythonStmt :: Located PythonStmt -> OptimizationM (Located PythonStmt)
constantFoldPythonStmt (Located span stmt) = do
  newStmt <- case stmt of
    PyExprStmt expr -> PyExprStmt <$> constantFoldPythonExpr expr
    
    PyAssign targets value -> do
      newValue <- constantFoldPythonExpr value
      return $ PyAssign targets newValue
    
    PyIf condition thenStmts elseStmts -> do
      newCondition <- constantFoldPythonExpr condition
      case locatedValue newCondition of
        PyLiteral (LBool True) -> do
          recordOptimization "Eliminated always-true if statement"
          modify $ \s -> s { osChanged = True }
          PyBlock <$> mapM constantFoldPythonStmt thenStmts
        PyLiteral (LBool False) -> do
          recordOptimization "Eliminated always-false if statement"
          modify $ \s -> s { osChanged = True }
          PyBlock <$> mapM constantFoldPythonStmt elseStmts
        _ -> do
          newThenStmts <- mapM constantFoldPythonStmt thenStmts
          newElseStmts <- mapM constantFoldPythonStmt elseStmts
          return $ PyIf newCondition newThenStmts newElseStmts
    
    PyFor target iter body -> do
      newIter <- constantFoldPythonExpr iter
      newBody <- withNewScope $ mapM constantFoldPythonStmt body
      return $ PyFor target newIter newBody
    
    PyFunctionDef name params body returnType -> do
      newBody <- withNewScope $ mapM constantFoldPythonStmt body
      return $ PyFunctionDef name params newBody returnType
    
    PyReturn mexpr -> PyReturn <$> traverse constantFoldPythonExpr mexpr
    
    PyBlock stmts -> PyBlock <$> mapM constantFoldPythonStmt stmts
    
    _ -> return stmt
    
  return $ Located span newStmt

constantFoldPythonExpr :: Located PythonExpr -> OptimizationM (Located PythonExpr)
constantFoldPythonExpr (Located span expr) = do
  newExpr <- case expr of
    PyBinaryOp op left right -> do
      newLeft <- constantFoldPythonExpr left
      newRight <- constantFoldPythonExpr right
      
      case (locatedValue newLeft, locatedValue newRight) of
        (PyLiteral leftLit, PyLiteral rightLit) -> do
          case constantFoldBinaryOp op leftLit rightLit of
            Just result -> do
              recordOptimization $ "Folded constant expression: " <> T.pack (show op)
              modify $ \s -> s { osChanged = True, osConstantsFoldedCount = osConstantsFoldedCount s + 1 }
              return $ PyLiteral result
            Nothing -> return $ PyBinaryOp op newLeft newRight
        _ -> return $ PyBinaryOp op newLeft newRight
    
    PyUnaryOp op operand -> do
      newOperand <- constantFoldPythonExpr operand
      case locatedValue newOperand of
        PyLiteral lit -> do
          case constantFoldUnaryOp op lit of
            Just result -> do
              recordOptimization $ "Folded constant unary operation: " <> T.pack (show op)
              modify $ \s -> s { osChanged = True, osConstantsFoldedCount = osConstantsFoldedCount s + 1 }
              return $ PyLiteral result
            Nothing -> return $ PyUnaryOp op newOperand
        _ -> return $ PyUnaryOp op newOperand
    
    PyCall func args -> do
      newFunc <- constantFoldPythonExpr func
      newArgs <- mapM constantFoldPythonArg args
      return $ PyCall newFunc newArgs
    
    PyIndex container index -> do
      newContainer <- constantFoldPythonExpr container
      newIndex <- constantFoldPythonExpr index
      -- Try to fold list/string indexing with constants
      case (locatedValue newContainer, locatedValue newIndex) of
        (PyList elems, PyLiteral (LInt idx)) | idx >= 0 && fromIntegral idx < length elems ->
          return $ locatedValue (elems !! fromIntegral idx)
        (PyLiteral (LString str), PyLiteral (LInt idx)) | idx >= 0 && fromIntegral idx < T.length str ->
          return $ PyLiteral $ LString $ T.singleton $ T.index str (fromIntegral idx)
        _ -> return $ PyIndex newContainer newIndex
    
    PyList elems -> PyList <$> mapM constantFoldPythonExpr elems
    PyTuple elems -> PyTuple <$> mapM constantFoldPythonExpr elems
    PyDict pairs -> PyDict <$> mapM (KATEX_INLINE_OPENk, v) -> (,) <$> constantFoldPythonExpr k <*> constantFoldPythonExpr v) pairs
    
    _ -> return expr
    
  return $ Located span newExpr

constantFoldPythonArg :: PythonArgument -> OptimizationM PythonArgument
constantFoldPythonArg (ArgPositional expr) = ArgPositional <$> constantFoldPythonExpr expr
constantFoldPythonArg (ArgKeyword kw expr) = ArgKeyword kw <$> constantFoldPythonExpr expr
constantFoldPythonArg (ArgStarred expr) = ArgStarred <$> constantFoldPythonExpr expr

constantFoldingGo :: GoAST -> OptimizationM GoAST
constantFoldingGo (GoPackage packageName imports decls) = do
  newDecls <- mapM constantFoldGoDecl decls
  return $ GoPackage packageName imports newDecls

constantFoldGoDecl :: Located GoDecl -> OptimizationM (Located GoDecl)
constantFoldGoDecl (Located span decl) = do
  newDecl <- case decl of
    GoFunctionDecl name params results body -> do
      newBody <- withNewScope $ constantFoldGoStmt body
      return $ GoFunctionDecl name params results newBody
    _ -> return decl
  return $ Located span newDecl

constantFoldGoStmt :: Located GoStmt -> OptimizationM (Located GoStmt)
constantFoldGoStmt (Located span stmt) = do
  newStmt <- case stmt of
    GoExprStmt expr -> GoExprStmt <$> constantFoldGoExpr expr
    GoAssignment lhs rhs -> GoAssignment lhs <$> constantFoldGoExpr rhs
    GoIf condition thenStmt elseStmt -> do
      newCondition <- constantFoldGoExpr condition
      newThenStmt <- constantFoldGoStmt thenStmt
      newElseStmt <- traverse constantFoldGoStmt elseStmt
      return $ GoIf newCondition newThenStmt newElseStmt
    GoFor forClause body -> do
      newBody <- withNewScope $ constantFoldGoStmt body
      return $ GoFor forClause newBody
    GoReturn mexpr -> GoReturn <$> traverse constantFoldGoExpr mexpr
    GoBlock stmts -> GoBlock <$> mapM constantFoldGoStmt stmts
    _ -> return stmt
  return $ Located span newStmt

constantFoldGoExpr :: Located GoExpr -> OptimizationM (Located GoExpr)
constantFoldGoExpr (Located span expr) = do
  newExpr <- case expr of
    GoBinaryOp op left right -> do
      newLeft <- constantFoldGoExpr left
      newRight <- constantFoldGoExpr right
      
      case (locatedValue newLeft, locatedValue newRight) of
        (GoLiteral leftLit, GoLiteral rightLit) -> do
          case constantFoldBinaryOpGo op leftLit rightLit of
            Just result -> do
              recordOptimization $ "Folded Go constant expression: " <> T.pack (show op)
              modify $ \s -> s { osChanged = True, osConstantsFoldedCount = osConstantsFoldedCount s + 1 }
              return $ GoLiteral result
            Nothing -> return $ GoBinaryOp op newLeft newRight
        _ -> return $ GoBinaryOp op newLeft newRight
    
    GoUnaryOp op operand -> do
      newOperand <- constantFoldGoExpr operand
      case locatedValue newOperand of
        GoLiteral lit -> do
          case constantFoldUnaryOpGo op lit of
            Just result -> do
              recordOptimization $ "Folded Go constant unary operation: " <> T.pack (show op)
              modify $ \s -> s { osChanged = True, osConstantsFoldedCount = osConstantsFoldedCount s + 1 }
              return $ GoLiteral result
            Nothing -> return $ GoUnaryOp op newOperand
        _ -> return $ GoUnaryOp op newOperand
    
    GoCall func args -> do
      newFunc <- constantFoldGoExpr func
      newArgs <- mapM constantFoldGoExpr args
      return $ GoCall newFunc newArgs
    
    _ -> return expr
    
  return $ Located span newExpr

-- Extended constant folding for binary operations
constantFoldBinaryOp :: BinaryOp -> Literal -> Literal -> Maybe Literal
constantFoldBinaryOp OpAdd (LInt a) (LInt b) = Just $ LInt (a + b)
constantFoldBinaryOp OpAdd (LFloat a) (LFloat b) = Just $ LFloat (a + b)
constantFoldBinaryOp OpAdd (LString a) (LString b) = Just $ LString (a <> b)
constantFoldBinaryOp OpSub (LInt a) (LInt b) = Just $ LInt (a - b)
constantFoldBinaryOp OpSub (LFloat a) (LFloat b) = Just $ LFloat (a - b)
constantFoldBinaryOp OpMul (LInt a) (LInt b) = Just $ LInt (a * b)
constantFoldBinaryOp OpMul (LFloat a) (LFloat b) = Just $ LFloat (a * b)
constantFoldBinaryOp OpDiv (LFloat a) (LFloat b) | b /= 0 = Just $ LFloat (a / b)
constantFoldBinaryOp OpFloorDiv (LInt a) (LInt b) | b /= 0 = Just $ LInt (a `div` b)
constantFoldBinaryOp OpMod (LInt a) (LInt b) | b /= 0 = Just $ LInt (a `mod` b)
constantFoldBinaryOp OpPow (LInt a) (LInt b) | b >= 0 = Just $ LInt (a ^ b)
constantFoldBinaryOp OpAnd (LBool a) (LBool b) = Just $ LBool (a && b)
constantFoldBinaryOp OpOr (LBool a) (LBool b) = Just $ LBool (a || b)
constantFoldBinaryOp OpEq a b = Just $ LBool (a == b)
constantFoldBinaryOp OpNe a b = Just $ LBool (a /= b)
constantFoldBinaryOp OpLt (LInt a) (LInt b) = Just $ LBool (a < b)
constantFoldBinaryOp OpLt (LFloat a) (LFloat b) = Just $ LBool (a < b)
constantFoldBinaryOp OpLe (LInt a) (LInt b) = Just $ LBool (a <= b)
constantFoldBinaryOp OpLe (LFloat a) (LFloat b) = Just $ LBool (a <= b)
constantFoldBinaryOp OpGt (LInt a) (LInt b) = Just $ LBool (a > b)
constantFoldBinaryOp OpGt (LFloat a) (LFloat b) = Just $ LBool (a > b)
constantFoldBinaryOp OpGe (LInt a) (LInt b) = Just $ LBool (a >= b)
constantFoldBinaryOp OpGe (LFloat a) (LFloat b) = Just $ LBool (a >= b)
constantFoldBinaryOp _ _ _ = Nothing

constantFoldUnaryOp :: UnaryOp -> Literal -> Maybe Literal
constantFoldUnaryOp OpNot (LBool b) = Just $ LBool (not b)
constantFoldUnaryOp OpNegate (LInt i) = Just $ LInt (-i)
constantFoldUnaryOp OpNegate (LFloat f) = Just $ LFloat (-f)
constantFoldUnaryOp _ _ = Nothing

constantFoldBinaryOpGo :: GoBinaryOp -> GoLiteral -> GoLiteral -> Maybe GoLiteral
constantFoldBinaryOpGo GoOpAdd (GoInt a) (GoInt b) = Just $ GoInt (a + b)
constantFoldBinaryOpGo GoOpAdd (GoFloat a) (GoFloat b) = Just $ GoFloat (a + b)
constantFoldBinaryOpGo GoOpAdd (GoString a) (GoString b) = Just $ GoString (a <> b)
constantFoldBinaryOpGo GoOpSub (GoInt a) (GoInt b) = Just $ GoInt (a - b)
constantFoldBinaryOpGo GoOpSub (GoFloat a) (GoFloat b) = Just $ GoFloat (a - b)
constantFoldBinaryOpGo GoOpMul (GoInt a) (GoInt b) = Just $ GoInt (a * b)
constantFoldBinaryOpGo GoOpMul (GoFloat a) (GoFloat b) = Just $ GoFloat (a * b)
constantFoldBinaryOpGo GoOpDiv (GoInt a) (GoInt b) | b /= 0 = Just $ GoInt (a `div` b)
constantFoldBinaryOpGo GoOpDiv (GoFloat a) (GoFloat b) | b /= 0 = Just $ GoFloat (a / b)
constantFoldBinaryOpGo GoOpMod (GoInt a) (GoInt b) | b /= 0 = Just $ GoInt (a `mod` b)
constantFoldBinaryOpGo GoOpEq a b = Just $ GoBool (a == b)
constantFoldBinaryOpGo GoOpNe a b = Just $ GoBool (a /= b)
constantFoldBinaryOpGo GoOpLt (GoInt a) (GoInt b) = Just $ GoBool (a < b)
constantFoldBinaryOpGo GoOpLt (GoFloat a) (GoFloat b) = Just $ GoBool (a < b)
constantFoldBinaryOpGo GoOpLe (GoInt a) (GoInt b) = Just $ GoBool (a <= b)
constantFoldBinaryOpGo GoOpLe (GoFloat a) (GoFloat b) = Just $ GoBool (a <= b)
constantFoldBinaryOpGo GoOpGt (GoInt a) (GoInt b) = Just $ GoBool (a > b)
constantFoldBinaryOpGo GoOpGt (GoFloat a) (GoFloat b) = Just $ GoBool (a > b)
constantFoldBinaryOpGo GoOpGe (GoInt a) (GoInt b) = Just $ GoBool (a >= b)
constantFoldBinaryOpGo GoOpGe (GoFloat a) (GoFloat b) = Just $ GoBool (a >= b)
constantFoldBinaryOpGo GoOpAnd (GoBool a) (GoBool b) = Just $ GoBool (a && b)
constantFoldBinaryOpGo GoOpOr (GoBool a) (GoBool b) = Just $ GoBool (a || b)
constantFoldBinaryOpGo _ _ _ = Nothing

constantFoldUnaryOpGo :: GoUnaryOp -> GoLiteral -> Maybe GoLiteral
constantFoldUnaryOpGo GoOpNegate (GoInt i) = Just $ GoInt (-i)
constantFoldUnaryOpGo GoOpNegate (GoFloat f) = Just $ GoFloat (-f)
constantFoldUnaryOpGo GoOpNot (GoBool b) = Just $ GoBool (not b)
constantFoldUnaryOpGo _ _ = Nothing

-- ============================================================================
-- DEAD CODE ELIMINATION PASS
-- ============================================================================

deadCodeEliminationPass :: Either PythonAST GoAST -> OptimizationM (Either PythonAST GoAST)
deadCodeEliminationPass (Left pyAST) = do
  -- First pass: collect live variables
  collectLiveVariablesPython pyAST
  -- Second pass: eliminate dead code
  Left <$> eliminateDeadCodePython pyAST
deadCodeEliminationPass (Right goAST) = do
  collectLiveVariablesGo goAST
  Right <$> eliminateDeadCodeGo goAST

collectLiveVariablesPython :: PythonAST -> OptimizationM ()
collectLiveVariablesPython (PythonModule stmts) = mapM_ collectLiveVarsPyStmt (reverse stmts)

collectLiveVarsPyStmt :: Located PythonStmt -> OptimizationM ()
collectLiveVarsPyStmt (Located _ stmt) = case stmt of
  PyExprStmt expr -> collectLiveVarsPyExpr expr
  PyAssign targets value -> do
    collectLiveVarsPyExpr value
    mapM_ markDefined targets
  PyReturn (Just expr) -> collectLiveVarsPyExpr expr
  PyIf cond thenStmts elseStmts -> do
    collectLiveVarsPyExpr cond
    mapM_ collectLiveVarsPyStmt thenStmts
    mapM_ collectLiveVarsPyStmt elseStmts
  PyFor target iter body -> do
    collectLiveVarsPyExpr iter
    markDefined target
    mapM_ collectLiveVarsPyStmt body
  PyFunctionDef name _ body _ -> do
    markDefined name
    withNewScope $ mapM_ collectLiveVarsPyStmt body
  _ -> return ()

collectLiveVarsPyExpr :: Located PythonExpr -> OptimizationM ()
collectLiveVarsPyExpr (Located _ expr) = case expr of
  PyIdentifier ident -> markLive ident
  PyBinaryOp _ left right -> do
    collectLiveVarsPyExpr left
    collectLiveVarsPyExpr right
  PyUnaryOp _ operand -> collectLiveVarsPyExpr operand
  PyCall func args -> do
    collectLiveVarsPyExpr func
    mapM_ collectLiveVarsPyArg args
  PyIndex container idx -> do
    collectLiveVarsPyExpr container
    collectLiveVarsPyExpr idx
  PyList elems -> mapM_ collectLiveVarsPyExpr elems
  PyTuple elems -> mapM_ collectLiveVarsPyExpr elems
  PyDict pairs -> mapM_ (KATEX_INLINE_OPENk, v) -> collectLiveVarsPyExpr k >> collectLiveVarsPyExpr v) pairs
  _ -> return ()

collectLiveVarsPyArg :: PythonArgument -> OptimizationM ()
collectLiveVarsPyArg (ArgPositional expr) = collectLiveVarsPyExpr expr
collectLiveVarsPyArg (ArgKeyword _ expr) = collectLiveVarsPyExpr expr
collectLiveVarsPyArg (ArgStarred expr) = collectLiveVarsPyExpr expr

eliminateDeadCodePython :: PythonAST -> OptimizationM PythonAST
eliminateDeadCodePython (PythonModule stmts) = do
  newStmts <- catMaybes <$> mapM eliminateDeadPyStmt stmts
  return $ PythonModule newStmts

eliminateDeadPyStmt :: Located PythonStmt -> OptimizationM (Maybe (Located PythonStmt))
eliminateDeadPyStmt stmt@(Located span s) = case s of
  PyAssign [target] _ -> do
    live <- isLive target
    if not live
      then do
        recordOptimization $ "Eliminated dead assignment to " <> target
        modify $ \st -> st { osChanged = True, osDeadCodeRemovedCount = osDeadCodeRemovedCount st + 1 }
        return Nothing
      else return $ Just stmt
  PyBlock stmts -> do
    newStmts <- catMaybes <$> mapM eliminateDeadPyStmt stmts
    return $ Just $ Located span $ PyBlock newStmts
  PyIf cond thenStmts elseStmts -> do
    newThenStmts <- catMaybes <$> mapM eliminateDeadPyStmt thenStmts
    newElseStmts <- catMaybes <$> mapM eliminateDeadPyStmt elseStmts
    return $ Just $ Located span $ PyIf cond newThenStmts newElseStmts
  _ -> return $ Just stmt

collectLiveVariablesGo :: GoAST -> OptimizationM ()
collectLiveVariablesGo (GoPackage _ _ decls) = mapM_ collectLiveVarsGoDecl decls

collectLiveVarsGoDecl :: Located GoDecl -> OptimizationM ()
collectLiveVarsGoDecl (Located _ decl) = case decl of
  GoFunctionDecl name _ _ body -> do
    markDefined name
    withNewScope $ collectLiveVarsGoStmt body
  _ -> return ()

collectLiveVarsGoStmt :: Located GoStmt -> OptimizationM ()
collectLiveVarsGoStmt (Located _ stmt) = case stmt of
  GoExprStmt expr -> collectLiveVarsGoExpr expr
  GoAssignment lhs rhs -> do
    collectLiveVarsGoExpr rhs
    markDefined lhs
  GoReturn (Just expr) -> collectLiveVarsGoExpr expr
  GoIf cond thenStmt elseStmt -> do
    collectLiveVarsGoExpr cond
    collectLiveVarsGoStmt thenStmt
    traverse_ collectLiveVarsGoStmt elseStmt
  GoFor _ body -> collectLiveVarsGoStmt body
  GoBlock stmts -> mapM_ collectLiveVarsGoStmt stmts
  _ -> return ()

collectLiveVarsGoExpr :: Located GoExpr -> OptimizationM ()
collectLiveVarsGoExpr (Located _ expr) = case expr of
  GoIdentifier ident -> markLive ident
  GoBinaryOp _ left right -> do
    collectLiveVarsGoExpr left
    collectLiveVarsGoExpr right
  GoUnaryOp _ operand -> collectLiveVarsGoExpr operand
  GoCall func args -> do
    collectLiveVarsGoExpr func
    mapM_ collectLiveVarsGoExpr args
  _ -> return ()

eliminateDeadCodeGo :: GoAST -> OptimizationM GoAST
eliminateDeadCodeGo (GoPackage packageName imports decls) = do
  newDecls <- mapM eliminateDeadGoDecl decls
  return $ GoPackage packageName imports newDecls

eliminateDeadGoDecl :: Located GoDecl -> OptimizationM (Located GoDecl)
eliminateDeadGoDecl (Located span decl) = case decl of
  GoFunctionDecl name params results body -> do
    newBody <- withNewScope $ eliminateDeadGoStmt body
    return $ Located span $ GoFunctionDecl name params results newBody
  _ -> return $ Located span decl

eliminateDeadGoStmt :: Located GoStmt -> OptimizationM (Located GoStmt)
eliminateDeadGoStmt stmt@(Located span s) = case s of
  GoAssignment target _ -> do
    live <- isLive target
    if not live
      then do
        recordOptimization $ "Eliminated dead Go assignment to " <> target
        modify $ \st -> st { osChanged = True, osDeadCodeRemovedCount = osDeadCodeRemovedCount st + 1 }
        return $ Located span GoNop
      else return stmt
  GoBlock stmts -> do
    newStmts <- mapM eliminateDeadGoStmt stmts
    return $ Located span $ GoBlock (filter (not . isNop) newStmts)
  _ -> return stmt
  where
    isNop (Located _ GoNop) = True
    isNop _ = False

-- ============================================================================
-- CONSTANT PROPAGATION PASS
-- ============================================================================

constantPropagationPass :: Either PythonAST GoAST -> OptimizationM (Either PythonAST GoAST)
constantPropagationPass (Left pyAST) = Left <$> constantPropagationPython pyAST
constantPropagationPass (Right goAST) = Right <$> constantPropagationGo goAST

constantPropagationPython :: PythonAST -> OptimizationM PythonAST
constantPropagationPython (PythonModule stmts) = do
  newStmts <- mapM propagateConstantsPyStmt stmts
  return $ PythonModule newStmts

propagateConstantsPyStmt :: Located PythonStmt -> OptimizationM (Located PythonStmt)
propagateConstantsPyStmt (Located span stmt) = do
  newStmt <- case stmt of
    PyAssign [target] value -> do
      newValue <- propagateConstantsPyExpr value
      -- If the value is a constant, record it
      case locatedValue newValue of
        PyLiteral lit -> addConstant target lit
        _ -> return ()
      return $ PyAssign [target] newValue
    
    PyExprStmt expr -> PyExprStmt <$> propagateConstantsPyExpr expr
    
    PyIf cond thenStmts elseStmts -> do
      newCond <- propagateConstantsPyExpr cond
      newThenStmts <- withNewScope $ mapM propagateConstantsPyStmt thenStmts
      newElseStmts <- withNewScope $ mapM propagateConstantsPyStmt elseStmts
      return $ PyIf newCond newThenStmts newElseStmts
    
    PyFor target iter body -> do
      newIter <- propagateConstantsPyExpr iter
      newBody <- withNewScope $ mapM propagateConstantsPyStmt body
      return $ PyFor target newIter newBody
    
    PyFunctionDef name params body returnType -> do
      newBody <- withNewScope $ mapM propagateConstantsPyStmt body
      return $ PyFunctionDef name params newBody returnType
    
    PyReturn mexpr -> PyReturn <$> traverse propagateConstantsPyExpr mexpr
    
    PyBlock stmts -> PyBlock <$> mapM propagateConstantsPyStmt stmts
    
    _ -> return stmt
    
  return $ Located span newStmt

propagateConstantsPyExpr :: Located PythonExpr -> OptimizationM (Located PythonExpr)
propagateConstantsPyExpr (Located span expr) = do
  newExpr <- case expr of
    PyIdentifier ident -> do
      mval <- lookupConstant ident
      case mval of
        Just lit -> do
          recordOptimization $ "Propagated constant " <> ident
          modify $ \s -> s { osChanged = True }
          return $ PyLiteral lit
        Nothing -> return expr
    
    PyBinaryOp op left right -> do
      newLeft <- propagateConstantsPyExpr left
      newRight <- propagateConstantsPyExpr right
      -- Try constant folding after propagation
      case (locatedValue newLeft, locatedValue newRight) of
        (PyLiteral leftLit, PyLiteral rightLit) ->
          case constantFoldBinaryOp op leftLit rightLit of
            Just result -> return $ PyLiteral result
            Nothing -> return $ PyBinaryOp op newLeft newRight
        _ -> return $ PyBinaryOp op newLeft newRight
    
    PyUnaryOp op operand -> do
      newOperand <- propagateConstantsPyExpr operand
      case locatedValue newOperand of
        PyLiteral lit ->
          case constantFoldUnaryOp op lit of
            Just result -> return $ PyLiteral result
            Nothing -> return $ PyUnaryOp op newOperand
        _ -> return $ PyUnaryOp op newOperand
    
    PyCall func args -> do
      newFunc <- propagateConstantsPyExpr func
      newArgs <- mapM propagateConstantsPyArg args
      return $ PyCall newFunc newArgs
    
    PyIndex container idx -> do
      newContainer <- propagateConstantsPyExpr container
      newIdx <- propagateConstantsPyExpr idx
      return $ PyIndex newContainer newIdx
    
    PyList elems -> PyList <$> mapM propagateConstantsPyExpr elems
    PyTuple elems -> PyTuple <$> mapM propagateConstantsPyExpr elems
    PyDict pairs -> PyDict <$> mapM (KATEX_INLINE_OPENk, v) -> (,) <$> propagateConstantsPyExpr k <*> propagateConstantsPyExpr v) pairs
    
    _ -> return expr
    
  return $ Located span newExpr

propagateConstantsPyArg :: PythonArgument -> OptimizationM PythonArgument
propagateConstantsPyArg (ArgPositional expr) = ArgPositional <$> propagateConstantsPyExpr expr
propagateConstantsPyArg (ArgKeyword kw expr) = ArgKeyword kw <$> propagateConstantsPyExpr expr
propagateConstantsPyArg (ArgStarred expr) = ArgStarred <$> propagateConstantsPyExpr expr

constantPropagationGo :: GoAST -> OptimizationM GoAST
constantPropagationGo (GoPackage packageName imports decls) = do
  newDecls <- mapM propagateConstantsGoDecl decls
  return $ GoPackage packageName imports newDecls

propagateConstantsGoDecl :: Located GoDecl -> OptimizationM (Located GoDecl)
propagateConstantsGoDecl (Located span decl) = case decl of
  GoFunctionDecl name params results body -> do
    newBody <- withNewScope $ propagateConstantsGoStmt body
    return $ Located span $ GoFunctionDecl name params results newBody
  _ -> return $ Located span decl

propagateConstantsGoStmt :: Located GoStmt -> OptimizationM (Located GoStmt)
propagateConstantsGoStmt (Located span stmt) = do
  newStmt <- case stmt of
    GoAssignment target value -> do
      newValue <- propagateConstantsGoExpr value
      -- Record constant if applicable
      case locatedValue newValue of
        GoLiteral lit -> addConstant target (goLiteralToLiteral lit)
        _ -> return ()
      return $ GoAssignment target newValue
    
    GoExprStmt expr -> GoExprStmt <$> propagateConstantsGoExpr expr
    GoIf cond thenStmt elseStmt -> do
      newCond <- propagateConstantsGoExpr cond
      newThenStmt <- propagateConstantsGoStmt thenStmt
      newElseStmt <- traverse propagateConstantsGoStmt elseStmt
      return $ GoIf newCond newThenStmt newElseStmt
    GoFor forClause body -> do
      newBody <- withNewScope $ propagateConstantsGoStmt body
      return $ GoFor forClause newBody
    GoReturn mexpr -> GoReturn <$> traverse propagateConstantsGoExpr mexpr
    GoBlock stmts -> GoBlock <$> mapM propagateConstantsGoStmt stmts
    _ -> return stmt
  return $ Located span newStmt

propagateConstantsGoExpr :: Located GoExpr -> OptimizationM (Located GoExpr)
propagateConstantsGoExpr (Located span expr) = do
  newExpr <- case expr of
    GoIdentifier ident -> do
      mval <- lookupConstant ident
      case mval of
        Just lit -> do
          recordOptimization $ "Propagated Go constant " <> ident
          modify $ \s -> s { osChanged = True }
          return $ GoLiteral (literalToGoLiteral lit)
        Nothing -> return expr
    
    GoBinaryOp op left right -> do
      newLeft <- propagateConstantsGoExpr left
      newRight <- propagateConstantsGoExpr right
      return $ GoBinaryOp op newLeft newRight
    
    GoUnaryOp op operand -> do
      newOperand <- propagateConstantsGoExpr operand
      return $ GoUnaryOp op newOperand
    
    GoCall func args -> do
      newFunc <- propagateConstantsGoExpr func
      newArgs <- mapM propagateConstantsGoExpr args
      return $ GoCall newFunc newArgs
    
    _ -> return expr
  return $ Located span newExpr

-- Helper conversions
goLiteralToLiteral :: GoLiteral -> Literal
goLiteralToLiteral (GoInt i) = LInt i
goLiteralToLiteral (GoFloat f) = LFloat f
goLiteralToLiteral (GoString s) = LString s
goLiteralToLiteral (GoBool b) = LBool b
goLiteralToLiteral GoNil = LNone

literalToGoLiteral :: Literal -> GoLiteral
literalToGoLiteral (LInt i) = GoInt i
literalToGoLiteral (LFloat f) = GoFloat f
literalToGoLiteral (LString s) = GoString s
literalToGoLiteral (LBool b) = GoBool b
literalToGoLiteral LNone = GoNil

-- ============================================================================
-- ALGEBRAIC SIMPLIFICATION PASS
-- ============================================================================

algebraicSimplificationPass :: Either PythonAST GoAST -> OptimizationM (Either PythonAST GoAST)
algebraicSimplificationPass (Left pyAST) = Left <$> algebraicSimplificationPython pyAST
algebraicSimplificationPass (Right goAST) = Right <$> algebraicSimplificationGo goAST

algebraicSimplificationPython :: PythonAST -> OptimizationM PythonAST
algebraicSimplificationPython (PythonModule stmts) = do
  newStmts <- mapM simplifyAlgebraicPyStmt stmts
  return $ PythonModule newStmts

simplifyAlgebraicPyStmt :: Located PythonStmt -> OptimizationM (Located PythonStmt)
simplifyAlgebraicPyStmt (Located span stmt) = do
  newStmt <- case stmt of
    PyExprStmt expr -> PyExprStmt <$> simplifyAlgebraicPyExpr expr
    PyAssign targets value -> PyAssign targets <$> simplifyAlgebraicPyExpr value
    PyIf cond thenStmts elseStmts -> do
      newCond <- simplifyAlgebraicPyExpr cond
      newThenStmts <- mapM simplifyAlgebraicPyStmt thenStmts
      newElseStmts <- mapM simplifyAlgebraicPyStmt elseStmts
      return $ PyIf newCond newThenStmts newElseStmts
    PyFor target iter body -> do
      newIter <- simplifyAlgebraicPyExpr iter
      newBody <- mapM simplifyAlgebraicPyStmt body
      return $ PyFor target newIter newBody
    PyFunctionDef name params body returnType -> do
      newBody <- mapM simplifyAlgebraicPyStmt body
      return $ PyFunctionDef name params newBody returnType
    PyReturn mexpr -> PyReturn <$> traverse simplifyAlgebraicPyExpr mexpr
    PyBlock stmts -> PyBlock <$> mapM simplifyAlgebraicPyStmt stmts
    _ -> return stmt
  return $ Located span newStmt

simplifyAlgebraicPyExpr :: Located PythonExpr -> OptimizationM (Located PythonExpr)
simplifyAlgebraicPyExpr (Located span expr) = do
  newExpr <- case expr of
    PyBinaryOp op left right -> do
      newLeft <- simplifyAlgebraicPyExpr left
      newRight <- simplifyAlgebraicPyExpr right
      
      -- Algebraic simplifications
      case op of
        -- x + 0 = x, 0 + x = x
        OpAdd | isZero (locatedValue newRight) -> do
          recordOptimization "Simplified x + 0 to x"
          modify $ \s -> s { osChanged = True }
          return $ locatedValue newLeft
        OpAdd | isZero (locatedValue newLeft) -> do
          recordOptimization "Simplified 0 + x to x"
          modify $ \s -> s { osChanged = True }
          return $ locatedValue newRight
        
        -- x - 0 = x
        OpSub | isZero (locatedValue newRight) -> do
          recordOptimization "Simplified x - 0 to x"
          modify $ \s -> s { osChanged = True }
          return $ locatedValue newLeft
        
        -- x * 1 = x, 1 * x = x
        OpMul | isOne (locatedValue newRight) -> do
          recordOptimization "Simplified x * 1 to x"
          modify $ \s -> s { osChanged = True }
          return $ locatedValue newLeft
        OpMul | isOne (locatedValue newLeft) -> do
          recordOptimization "Simplified 1 * x to x"
          modify $ \s -> s { osChanged = True }
          return $ locatedValue newRight
        
        -- x * 0 = 0, 0 * x = 0
        OpMul | isZero (locatedValue newRight) || isZero (locatedValue newLeft) -> do
          recordOptimization "Simplified x * 0 to 0"
          modify $ \s -> s { osChanged = True }
          return $ PyLiteral (LInt 0)
        
        -- x / 1 = x
        OpDiv | isOne (locatedValue newRight) -> do
          recordOptimization "Simplified x / 1 to x"
          modify $ \s -> s { osChanged = True }
          return $ locatedValue newLeft
        
        -- x ** 1 = x
        OpPow | isOne (locatedValue newRight) -> do
          recordOptimization "Simplified x ** 1 to x"
          modify $ \s -> s { osChanged = True }
          return $ locatedValue newLeft
        
        -- x ** 0 = 1
        OpPow | isZero (locatedValue newRight) -> do
          recordOptimization "Simplified x ** 0 to 1"
          modify $ \s -> s { osChanged = True }
          return $ PyLiteral (LInt 1)
        
        _ -> return $ PyBinaryOp op newLeft newRight
    
    PyUnaryOp op operand -> do
      newOperand <- simplifyAlgebraicPyExpr operand
      
      -- Double negation elimination
      case (op, locatedValue newOperand) of
        (OpNegate, PyUnaryOp OpNegate inner) -> do
          recordOptimization "Eliminated double negation"
          modify $ \s -> s { osChanged = True }
          return $ locatedValue inner
        (OpNot, PyUnaryOp OpNot inner) -> do
          recordOptimization "Eliminated double logical negation"
          modify $ \s -> s { osChanged = True }
          return $ locatedValue inner
        _ -> return $ PyUnaryOp op newOperand
    
    PyCall func args -> do
      newFunc <- simplifyAlgebraicPyExpr func
      newArgs <- mapM simplifyAlgebraicPyArg args
      return $ PyCall newFunc newArgs
    
    PyIndex container idx -> do
      newContainer <- simplifyAlgebraicPyExpr container
      newIdx <- simplifyAlgebraicPyExpr idx
      return $ PyIndex newContainer newIdx
    
    PyList elems -> PyList <$> mapM simplifyAlgebraicPyExpr elems
    PyTuple elems -> PyTuple <$> mapM simplifyAlgebraicPyExpr elems
    PyDict pairs -> PyDict <$> mapM (KATEX_INLINE_OPENk, v) -> (,) <$> simplifyAlgebraicPyExpr k <*> simplifyAlgebraicPyExpr v) pairs
    
    _ -> return expr
  return $ Located span newExpr

simplifyAlgebraicPyArg :: PythonArgument -> OptimizationM PythonArgument
simplifyAlgebraicPyArg (ArgPositional expr) = ArgPositional <$> simplifyAlgebraicPyExpr expr
simplifyAlgebraicPyArg (ArgKeyword kw expr) = ArgKeyword kw <$> simplifyAlgebraicPyExpr expr
simplifyAlgebraicPyArg (ArgStarred expr) = ArgStarred <$> simplifyAlgebraicPyExpr expr

isZero :: PythonExpr -> Bool
isZero (PyLiteral (LInt 0)) = True
isZero (PyLiteral (LFloat 0.0)) = True
isZero _ = False

isOne :: PythonExpr -> Bool
isOne (PyLiteral (LInt 1)) = True
isOne (PyLiteral (LFloat 1.0)) = True
isOne _ = False

algebraicSimplificationGo :: GoAST -> OptimizationM GoAST
algebraicSimplificationGo (GoPackage packageName imports decls) = do
  newDecls <- mapM simplifyAlgebraicGoDecl decls
  return $ GoPackage packageName imports newDecls

simplifyAlgebraicGoDecl :: Located GoDecl -> OptimizationM (Located GoDecl)
simplifyAlgebraicGoDecl (Located span decl) = case decl of
  GoFunctionDecl name params results body -> do
    newBody <- simplifyAlgebraicGoStmt body
    return $ Located span $ GoFunctionDecl name params results newBody
  _ -> return $ Located span decl

simplifyAlgebraicGoStmt :: Located GoStmt -> OptimizationM (Located GoStmt)
simplifyAlgebraicGoStmt (Located span stmt) = do
  newStmt <- case stmt of
    GoExprStmt expr -> GoExprStmt <$> simplifyAlgebraicGoExpr expr
    GoAssignment lhs rhs -> GoAssignment lhs <$> simplifyAlgebraicGoExpr rhs
    GoIf cond thenStmt elseStmt -> do
      newCond <- simplifyAlgebraicGoExpr cond
      newThenStmt <- simplifyAlgebraicGoStmt thenStmt
      newElseStmt <- traverse simplifyAlgebraicGoStmt elseStmt
      return $ GoIf newCond newThenStmt newElseStmt
    GoFor forClause body -> do
      newBody <- simplifyAlgebraicGoStmt body
      return $ GoFor forClause newBody
    GoReturn mexpr -> GoReturn <$> traverse simplifyAlgebraicGoExpr mexpr
    GoBlock stmts -> GoBlock <$> mapM simplifyAlgebraicGoStmt stmts
    _ -> return stmt
  return $ Located span newStmt

simplifyAlgebraicGoExpr :: Located GoExpr -> OptimizationM (Located GoExpr)
simplifyAlgebraicGoExpr (Located span expr) = do
  newExpr <- case expr of
    GoBinaryOp op left right -> do
      newLeft <- simplifyAlgebraicGoExpr left
      newRight <- simplifyAlgebraicGoExpr right
      
      case op of
        GoOpAdd | isGoZero (locatedValue newRight) -> do
          recordOptimization "Simplified Go x + 0 to x"
          modify $ \s -> s { osChanged = True }
          return $ locatedValue newLeft
        GoOpSub | isGoZero (locatedValue newRight) -> do
          recordOptimization "Simplified Go x - 0 to x"
          modify $ \s -> s { osChanged = True }
          return $ locatedValue newLeft
        GoOpMul | isGoOne (locatedValue newRight) -> do
          recordOptimization "Simplified Go x * 1 to x"
          modify $ \s -> s { osChanged = True }
          return $ locatedValue newLeft
        GoOpMul | isGoZero (locatedValue newRight) || isGoZero (locatedValue newLeft) -> do
          recordOptimization "Simplified Go x * 0 to 0"
          modify $ \s -> s { osChanged = True }
          return $ GoLiteral (GoInt 0)
        GoOpDiv | isGoOne (locatedValue newRight) -> do
          recordOptimization "Simplified Go x / 1 to x"
          modify $ \s -> s { osChanged = True }
          return $ locatedValue newLeft
        _ -> return $ GoBinaryOp op newLeft newRight
    
    GoUnaryOp op operand -> do
      newOperand <- simplifyAlgebraicGoExpr operand
      case (op, locatedValue newOperand) of
        (GoOpNegate, GoUnaryOp GoOpNegate inner) -> do
          recordOptimization "Eliminated Go double negation"
          modify $ \s -> s { osChanged = True }
          return $ locatedValue inner
        (GoOpNot, GoUnaryOp GoOpNot inner) -> do
          recordOptimization "Eliminated Go double logical negation"
          modify $ \s -> s { osChanged = True }
          return $ locatedValue inner
        _ -> return $ GoUnaryOp op newOperand
    
    GoCall func args -> do
      newFunc <- simplifyAlgebraicGoExpr func
      newArgs <- mapM simplifyAlgebraicGoExpr args
      return $ GoCall newFunc newArgs
    
    _ -> return expr
  return $ Located span newExpr

isGoZero :: GoExpr -> Bool
isGoZero (GoLiteral (GoInt 0)) = True
isGoZero (GoLiteral (GoFloat 0.0)) = True
isGoZero _ = False

isGoOne :: GoExpr -> Bool
isGoOne (GoLiteral (GoInt 1)) = True
isGoOne (GoLiteral (GoFloat 1.0)) = True
isGoOne _ = False

-- ============================================================================
-- STRENGTH REDUCTION PASS
-- ============================================================================

strengthReductionPass :: Either PythonAST GoAST -> OptimizationM (Either PythonAST GoAST)
strengthReductionPass (Left pyAST) = Left <$> strengthReductionPython pyAST
strengthReductionPass (Right goAST) = Right <$> strengthReductionGo goAST

strengthReductionPython :: PythonAST -> OptimizationM PythonAST
strengthReductionPython (PythonModule stmts) = do
  newStmts <- mapM strengthReducePyStmt stmts
  return $ PythonModule newStmts

strengthReducePyStmt :: Located PythonStmt -> OptimizationM (Located PythonStmt)
strengthReducePyStmt (Located span stmt) = do
  newStmt <- case stmt of
    PyExprStmt expr -> PyExprStmt <$> strengthReducePyExpr expr
    PyAssign targets value -> PyAssign targets <$> strengthReducePyExpr value
    PyIf cond thenStmts elseStmts -> do
      newCond <- strengthReducePyExpr cond
      newThenStmts <- mapM strengthReducePyStmt thenStmts
      newElseStmts <- mapM strengthReducePyStmt elseStmts
      return $ PyIf newCond newThenStmts newElseStmts
    PyFor target iter body -> do
      newIter <- strengthReducePyExpr iter
      newBody <- mapM strengthReducePyStmt body
      return $ PyFor target newIter newBody
    PyFunctionDef name params body returnType -> do
      newBody <- mapM strengthReducePyStmt body
      return $ PyFunctionDef name params newBody returnType
    PyReturn mexpr -> PyReturn <$> traverse strengthReducePyExpr mexpr
    PyBlock stmts -> PyBlock <$> mapM strengthReducePyStmt stmts
    _ -> return stmt
  return $ Located span newStmt

strengthReducePyExpr :: Located PythonExpr -> OptimizationM (Located PythonExpr)
strengthReducePyExpr (Located span expr) = do
  newExpr <- case expr of
    PyBinaryOp op left right -> do
      newLeft <- strengthReducePyExpr left
      newRight <- strengthReducePyExpr right
      
      -- Strength reduction patterns
      case op of
        -- x * 2 => x + x (multiplication by 2 to addition)
        OpMul | isPowerOfTwo (locatedValue newRight) -> do
          let power = getPowerOfTwo (locatedValue newRight)
          if power == 1
            then do
              recordOptimization "Reduced x * 2 to x + x"
              modify $ \s -> s { osChanged = True }
              return $ PyBinaryOp OpAdd newLeft newLeft
            else if power > 1 && power <= 4
              then do
                recordOptimization $ "Reduced x * " <> T.pack (show (2^power)) <> " to left shift"
                modify $ \s -> s { osChanged = True }
                return $ PyBinaryOp OpLShift newLeft (Located span $ PyLiteral $ LInt power)
              else return $ PyBinaryOp op newLeft newRight
        
        -- x / 2^n => x >> n (division by power of 2 to right shift)
        OpFloorDiv | isPowerOfTwo (locatedValue newRight) -> do
          let power = getPowerOfTwo (locatedValue newRight)
          if power > 0
            then do
              recordOptimization $ "Reduced x // " <> T.pack (show (2^power)) <> " to right shift"
              modify $ \s -> s { osChanged = True }
              return $ PyBinaryOp OpRShift newLeft (Located span $ PyLiteral $ LInt power)
            else return $ PyBinaryOp op newLeft newRight
        
        -- x % 2^n => x & (2^n - 1) (modulo by power of 2 to bitwise and)
        OpMod | isPowerOfTwo (locatedValue newRight) -> do
          let power = getPowerOfTwo (locatedValue newRight)
          if power > 0
            then do
              recordOptimization $ "Reduced x % " <> T.pack (show (2^power)) <> " to bitwise and"
              modify $ \s -> s { osChanged = True }
              let mask = (2^power) - 1
              return $ PyBinaryOp OpBitAnd newLeft (Located span $ PyLiteral $ LInt mask)
            else return $ PyBinaryOp op newLeft newRight
        
        _ -> return $ PyBinaryOp op newLeft newRight
    
    PyUnaryOp op operand -> do
      newOperand <- strengthReducePyExpr operand
      return $ PyUnaryOp op newOperand
    
    PyCall func args -> do
      newFunc <- strengthReducePyExpr func
      newArgs <- mapM strengthReducePyArg args
      return $ PyCall newFunc newArgs
    
    PyIndex container idx -> do
      newContainer <- strengthReducePyExpr container
      newIdx <- strengthReducePyExpr idx
      return $ PyIndex newContainer newIdx
    
    PyList elems -> PyList <$> mapM strengthReducePyExpr elems
    PyTuple elems -> PyTuple <$> mapM strengthReducePyExpr elems
    PyDict pairs -> PyDict <$> mapM (KATEX_INLINE_OPENk, v) -> (,) <$> strengthReducePyExpr k <*> strengthReducePyExpr v) pairs
    
    _ -> return expr
  return $ Located span newExpr

strengthReducePyArg :: PythonArgument -> OptimizationM PythonArgument
strengthReducePyArg (ArgPositional expr) = ArgPositional <$> strengthReducePyExpr expr
strengthReducePyArg (ArgKeyword kw expr) = ArgKeyword kw <$> strengthReducePyExpr expr
strengthReducePyArg (ArgStarred expr) = ArgStarred <$> strengthReducePyExpr expr

isPowerOfTwo :: PythonExpr -> Bool
isPowerOfTwo (PyLiteral (LInt n)) = n > 0 && (n .&. (n - 1)) == 0
isPowerOfTwo _ = False

getPowerOfTwo :: PythonExpr -> Integer
getPowerOfTwo (PyLiteral (LInt n)) = 
  let countBits x p = if x == 1 then p else countBits (x `div` 2) (p + 1)
  in if n > 0 && (n .&. (n - 1)) == 0 then countBits n 0 else 0
getPowerOfTwo _ = 0

strengthReductionGo :: GoAST -> OptimizationM GoAST
strengthReductionGo (GoPackage packageName imports decls) = do
  newDecls <- mapM strengthReduceGoDecl decls
  return $ GoPackage packageName imports newDecls

strengthReduceGoDecl :: Located GoDecl -> OptimizationM (Located GoDecl)
strengthReduceGoDecl (Located span decl) = case decl of
  GoFunctionDecl name params results body -> do
    newBody <- strengthReduceGoStmt body
    return $ Located span $ GoFunctionDecl name params results newBody
  _ -> return $ Located span decl

strengthReduceGoStmt :: Located GoStmt -> OptimizationM (Located GoStmt)
strengthReduceGoStmt (Located span stmt) = do
  newStmt <- case stmt of
    GoExprStmt expr -> GoExprStmt <$> strengthReduceGoExpr expr
    GoAssignment lhs rhs -> GoAssignment lhs <$> strengthReduceGoExpr rhs
    GoIf cond thenStmt elseStmt -> do
      newCond <- strengthReduceGoExpr cond
      newThenStmt <- strengthReduceGoStmt thenStmt
      newElseStmt <- traverse strengthReduceGoStmt elseStmt
      return $ GoIf newCond newThenStmt newElseStmt
    GoFor forClause body -> do
      newBody <- strengthReduceGoStmt body
      return $ GoFor forClause newBody
    GoReturn mexpr -> GoReturn <$> traverse strengthReduceGoExpr mexpr
    GoBlock stmts -> GoBlock <$> mapM strengthReduceGoStmt stmts
    _ -> return stmt
  return $ Located span newStmt

strengthReduceGoExpr :: Located GoExpr -> OptimizationM (Located GoExpr)
strengthReduceGoExpr (Located span expr) = do
  newExpr <- case expr of
    GoBinaryOp op left right -> do
      newLeft <- strengthReduceGoExpr left
      newRight <- strengthReduceGoExpr right
      
      case op of
        GoOpMul | isGoPowerOfTwo (locatedValue newRight) -> do
          let power = getGoPowerOfTwo (locatedValue newRight)
          if power == 1
            then do
              recordOptimization "Reduced Go x * 2 to x + x"
              modify $ \s -> s { osChanged = True }
              return $ GoBinaryOp GoOpAdd newLeft newLeft
            else if power > 1 && power <= 4
              then do
                recordOptimization $ "Reduced Go x * " <> T.pack (show (2^power)) <> " to left shift"
                modify $ \s -> s { osChanged = True }
                return $ GoBinaryOp GoOpLShift newLeft (Located span $ GoLiteral $ GoInt power)
              else return $ GoBinaryOp op newLeft newRight
        _ -> return $ GoBinaryOp op newLeft newRight
    
    GoUnaryOp op operand -> do
      newOperand <- strengthReduceGoExpr operand
      return $ GoUnaryOp op newOperand
    
    GoCall func args -> do
      newFunc <- strengthReduceGoExpr func
      newArgs <- mapM strengthReduceGoExpr args
      return $ GoCall newFunc newArgs
    
    _ -> return expr
  return $ Located span newExpr

isGoPowerOfTwo :: GoExpr -> Bool
isGoPowerOfTwo (GoLiteral (GoInt n)) = n > 0 && (n .&. (n - 1)) == 0
isGoPowerOfTwo _ = False

getGoPowerOfTwo :: GoExpr -> Integer
getGoPowerOfTwo (GoLiteral (GoInt n)) = 
  let countBits x p = if x == 1 then p else countBits (x `div` 2) (p + 1)
  in if n > 0 && (n .&. (n - 1)) == 0 then countBits n 0 else 0
getGoPowerOfTwo _ = 0

-- ============================================================================
-- COMMON SUBEXPRESSION ELIMINATION PASS
-- ============================================================================

commonSubexpressionEliminationPass :: Either PythonAST GoAST -> OptimizationM (Either PythonAST GoAST)
commonSubexpressionEliminationPass (Left pyAST) = Left <$> csePython pyAST
commonSubexpressionEliminationPass (Right goAST) = Right <$> cseGo goAST

csePython :: PythonAST -> OptimizationM PythonAST
csePython (PythonModule stmts) = do
  newStmts <- mapM csePyStmt stmts
  return $ PythonModule newStmts

csePyStmt :: Located PythonStmt -> OptimizationM (Located PythonStmt)
csePyStmt (Located span stmt) = do
  newStmt <- case stmt of
    PyExprStmt expr -> PyExprStmt <$> csePyExpr expr
    PyAssign targets value -> PyAssign targets <$> csePyExpr value
    PyIf cond thenStmts elseStmts -> do
      newCond <- csePyExpr cond
      newThenStmts <- mapM csePyStmt thenStmts
      newElseStmts <- mapM csePyStmt elseStmts
      return $ PyIf newCond newThenStmts newElseStmts
    PyFor target iter body -> do
      newIter <- csePyExpr iter
      -- Clear CSE cache for loop body as it may execute multiple times
      modify $ \s -> s { osSubexpressions = HashMap.empty }
      newBody <- mapM csePyStmt body
      return $ PyFor target newIter newBody
    PyFunctionDef name params body returnType -> do
      -- Clear CSE cache for function body
      modify $ \s -> s { osSubexpressions = HashMap.empty }
      newBody <- mapM csePyStmt body
      return $ PyFunctionDef name params newBody returnType
    PyReturn mexpr -> PyReturn <$> traverse csePyExpr mexpr
    PyBlock stmts -> PyBlock <$> mapM csePyStmt stmts
    _ -> return stmt
  return $ Located span newStmt

csePyExpr :: Located PythonExpr -> OptimizationM (Located PythonExpr)
csePyExpr (Located span expr) = do
  newExpr <- case expr of
    PyBinaryOp op left right -> do
      newLeft <- csePyExpr left
      newRight <- csePyExpr right
      
      -- Check if this expression has been computed before
      let exprKey = CommonExpr (T.pack $ show op) [exprToText $ locatedValue newLeft, exprToText $ locatedValue newRight]
      cache <- gets osSubexpressions
      
      case HashMap.lookup exprKey cache of
        Just varName -> do
          recordOptimization $ "Reused common subexpression: " <> T.pack (show op)
          modify $ \s -> s { osChanged = True }
          return $ PyIdentifier varName
        Nothing -> do
          -- Generate new variable name and cache it
          let newVarName = "_cse_" <> T.pack (show $ HashMap.size cache)
          modify $ \s -> s { osSubexpressions = HashMap.insert exprKey newVarName (osSubexpressions s) }
          return $ PyBinaryOp op newLeft newRight
    
    PyUnaryOp op operand -> do
      newOperand <- csePyExpr operand
      return $ PyUnaryOp op newOperand
    
    PyCall func args -> do
      newFunc <- csePyExpr func
      newArgs <- mapM csePyArg args
      return $ PyCall newFunc newArgs
    
    PyIndex container idx -> do
      newContainer <- csePyExpr container
      newIdx <- csePyExpr idx
      return $ PyIndex newContainer newIdx
    
    PyList elems -> PyList <$> mapM csePyExpr elems
    PyTuple elems -> PyTuple <$> mapM csePyExpr elems
    PyDict pairs -> PyDict <$> mapM (KATEX_INLINE_OPENk, v) -> (,) <$> csePyExpr k <*> csePyExpr v) pairs
    
    _ -> return expr
  return $ Located span newExpr

csePyArg :: PythonArgument -> OptimizationM PythonArgument
csePyArg (ArgPositional expr) = ArgPositional <$> csePyExpr expr
csePyArg (ArgKeyword kw expr) = ArgKeyword kw <$> csePyExpr expr
csePyArg (ArgStarred expr) = ArgStarred <$> csePyExpr expr

exprToText :: PythonExpr -> Text
exprToText (PyIdentifier name) = name
exprToText (PyLiteral lit) = T.pack $ show lit
exprToText _ = "_complex_"

cseGo :: GoAST -> OptimizationM GoAST
cseGo (GoPackage packageName imports decls) = do
  newDecls <- mapM cseGoDecl decls
  return $ GoPackage packageName imports newDecls

cseGoDecl :: Located GoDecl -> OptimizationM (Located GoDecl)
cseGoDecl (Located span decl) = case decl of
  GoFunctionDecl name params results body -> do
    modify $ \s -> s { osSubexpressions = HashMap.empty }
    newBody <- cseGoStmt body
    return $ Located span $ GoFunctionDecl name params results newBody
  _ -> return $ Located span decl

cseGoStmt :: Located GoStmt -> OptimizationM (Located GoStmt)
cseGoStmt (Located span stmt) = do
  newStmt <- case stmt of
    GoExprStmt expr -> GoExprStmt <$> cseGoExpr expr
    GoAssignment lhs rhs -> GoAssignment lhs <$> cseGoExpr rhs
    GoIf cond thenStmt elseStmt -> do
      newCond <- cseGoExpr cond
      newThenStmt <- cseGoStmt thenStmt
      newElseStmt <- traverse cseGoStmt elseStmt
      return $ GoIf newCond newThenStmt newElseStmt
    GoFor forClause body -> do
      modify $ \s -> s { osSubexpressions = HashMap.empty }
      newBody <- cseGoStmt body
      return $ GoFor forClause newBody
    GoReturn mexpr -> GoReturn <$> traverse cseGoExpr mexpr
    GoBlock stmts -> GoBlock <$> mapM cseGoStmt stmts
    _ -> return stmt
  return $ Located span newStmt

cseGoExpr :: Located GoExpr -> OptimizationM (Located GoExpr)
cseGoExpr (Located span expr) = do
  newExpr <- case expr of
    GoBinaryOp op left right -> do
      newLeft <- cseGoExpr left
      newRight <- cseGoExpr right
      
      let exprKey = CommonExpr (T.pack $ show op) [goExprToText $ locatedValue newLeft, goExprToText $ locatedValue newRight]
      cache <- gets osSubexpressions
      
      case HashMap.lookup exprKey cache of
        Just varName -> do
          recordOptimization $ "Reused Go common subexpression: " <> T.pack (show op)
          modify $ \s -> s { osChanged = True }
          return $ GoIdentifier varName
        Nothing -> do
          let newVarName = "_cse_" <> T.pack (show $ HashMap.size cache)
          modify $ \s -> s { osSubexpressions = HashMap.insert exprKey newVarName (osSubexpressions s) }
          return $ GoBinaryOp op newLeft newRight
    
    GoUnaryOp op operand -> do
      newOperand <- cseGoExpr operand
      return $ GoUnaryOp op newOperand
    
    GoCall func args -> do
      newFunc <- cseGoExpr func
      newArgs <- mapM cseGoExpr args
      return $ GoCall newFunc newArgs
    
    _ -> return expr
  return $ Located span newExpr

goExprToText :: GoExpr -> Text
goExprToText (GoIdentifier name) = name
goExprToText (GoLiteral lit) = T.pack $ show lit
goExprToText _ = "_complex_"

-- ============================================================================
-- PEEPHOLE OPTIMIZATION PASS
-- ============================================================================

peepholeOptimizationPass :: Either PythonAST GoAST -> OptimizationM (Either PythonAST GoAST)
peepholeOptimizationPass (Left pyAST) = Left <$> peepholePython pyAST
peepholeOptimizationPass (Right goAST) = Right <$> peepholeGo goAST

peepholePython :: PythonAST -> OptimizationM PythonAST
peepholePython (PythonModule stmts) = do
  newStmts <- peepholeOptimizeStmts stmts
  return $ PythonModule newStmts

peepholeOptimizeStmts :: [Located PythonStmt] -> OptimizationM [Located PythonStmt]
peepholeOptimizeStmts [] = return []
peepholeOptimizeStmts [stmt] = (:[]) <$> peepholePyStmt stmt
peepholeOptimizeStmts (s1:s2:rest) = do
  -- Look for patterns in consecutive statements
  case (locatedValue s1, locatedValue s2) of
    -- Pattern: x = y; return x => return y
    (PyAssign [var] value, PyReturn (Just (Located _ (PyIdentifier returnVar))))
      | var == returnVar -> do
        recordOptimization "Peephole: Eliminated temporary variable before return"
        modify $ \s -> s { osChanged = True }
        let newReturn = Located (locatedSpan s2) $ PyReturn (Just value)
        peepholeOptimizeStmts (newReturn : rest)
    
    _ -> do
      newS1 <- peepholePyStmt s1
      restOptimized <- peepholeOptimizeStmts (s2:rest)
      return (newS1 : restOptimized)

peepholePyStmt :: Located PythonStmt -> OptimizationM (Located PythonStmt)
peepholePyStmt (Located span stmt) = do
  newStmt <- case stmt of
    PyExprStmt expr -> PyExprStmt <$> peepholePyExpr expr
    PyAssign targets value -> PyAssign targets <$> peepholePyExpr value
    PyIf cond thenStmts elseStmts -> do
      newCond <- peepholePyExpr cond
      newThenStmts <- peepholeOptimizeStmts thenStmts
      newElseStmts <- peepholeOptimizeStmts elseStmts
      return $ PyIf newCond newThenStmts newElseStmts
    PyFor target iter body -> do
      newIter <- peepholePyExpr iter
      newBody <- peepholeOptimizeStmts body
      return $ PyFor target newIter newBody
    PyFunctionDef name params body returnType -> do
      newBody <- peepholeOptimizeStmts body
      return $ PyFunctionDef name params newBody returnType
    PyReturn mexpr -> PyReturn <$> traverse peepholePyExpr mexpr
    PyBlock stmts -> PyBlock <$> peepholeOptimizeStmts stmts
    _ -> return stmt
  return $ Located span newStmt

peepholePyExpr :: Located PythonExpr -> OptimizationM (Located PythonExpr)
peepholePyExpr (Located span expr) = do
  newExpr <- case expr of
    -- Pattern: not (x == y) => x != y
    PyUnaryOp OpNot (Located _ (PyBinaryOp OpEq left right)) -> do
      recordOptimization "Peephole: Converted not (x == y) to x != y"
      modify $ \s -> s { osChanged = True }
      return $ PyBinaryOp OpNe left right
    
    -- Pattern: not (x != y) => x == y
    PyUnaryOp OpNot (Located _ (PyBinaryOp OpNe left right)) -> do
      recordOptimization "Peephole: Converted not (x != y) to x == y"
      modify $ \s -> s { osChanged = True }
      return $ PyBinaryOp OpEq left right
    
    -- Pattern: not (x < y) => x >= y
    PyUnaryOp OpNot (Located _ (PyBinaryOp OpLt left right)) -> do
      recordOptimization "Peephole: Converted not (x < y) to x >= y"
      modify $ \s -> s { osChanged = True }
      return $ PyBinaryOp OpGe left right
    
    -- Pattern: not (x > y) => x <= y
    PyUnaryOp OpNot (Located _ (PyBinaryOp OpGt left right)) -> do
      recordOptimization "Peephole: Converted not (x > y) to x <= y"
      modify $ \s -> s { osChanged = True }
      return $ PyBinaryOp OpLe left right
    
    PyBinaryOp op left right -> do
      newLeft <- peepholePyExpr left
      newRight <- peepholePyExpr right
      return $ PyBinaryOp op newLeft newRight
    
    PyUnaryOp op operand -> do
      newOperand <- peepholePyExpr operand
      return $ PyUnaryOp op newOperand
    
    PyCall func args -> do
      newFunc <- peepholePyExpr func
      newArgs <- mapM peepholePyArg args
      return $ PyCall newFunc newArgs
    
    PyIndex container idx -> do
      newContainer <- peepholePyExpr container
      newIdx <- peepholePyExpr idx
      return $ PyIndex newContainer newIdx
    
    PyList elems -> PyList <$> mapM peepholePyExpr elems
    PyTuple elems -> PyTuple <$> mapM peepholePyExpr elems
    PyDict pairs -> PyDict <$> mapM (KATEX_INLINE_OPENk, v) -> (,) <$> peepholePyExpr k <*> peepholePyExpr v) pairs
    
    _ -> return expr
  return $ Located span newExpr

peepholePyArg :: PythonArgument -> OptimizationM PythonArgument
peepholePyArg (ArgPositional expr) = ArgPositional <$> peepholePyExpr expr
peepholePyArg (ArgKeyword kw expr) = ArgKeyword kw <$> peepholePyExpr expr
peepholePyArg (ArgStarred expr) = ArgStarred <$> peepholePyExpr expr

peepholeGo :: GoAST -> OptimizationM GoAST
peepholeGo (GoPackage packageName imports decls) = do
  newDecls <- mapM peepholeGoDecl decls
  return $ GoPackage packageName imports newDecls

peepholeGoDecl :: Located GoDecl -> OptimizationM (Located GoDecl)
peepholeGoDecl (Located span decl) = case decl of
  GoFunctionDecl name params results body -> do
    newBody <- peepholeGoStmt body
    return $ Located span $ GoFunctionDecl name params results newBody
  _ -> return $ Located span decl

peepholeGoStmt :: Located GoStmt -> OptimizationM (Located GoStmt)
peepholeGoStmt (Located span stmt) = do
  newStmt <- case stmt of
    GoExprStmt expr -> GoExprStmt <$> peepholeGoExpr expr
    GoAssignment lhs rhs -> GoAssignment lhs <$> peepholeGoExpr rhs
    GoIf cond thenStmt elseStmt -> do
      newCond <- peepholeGoExpr cond
      newThenStmt <- peepholeGoStmt thenStmt
      newElseStmt <- traverse peepholeGoStmt elseStmt
      return $ GoIf newCond newThenStmt newElseStmt
    GoFor forClause body -> do
      newBody <- peepholeGoStmt body
      return $ GoFor forClause newBody
    GoReturn mexpr -> GoReturn <$> traverse peepholeGoExpr mexpr
    GoBlock stmts -> do
      newStmts <- peepholeOptimizeGoStmts stmts
      return $ GoBlock newStmts
    _ -> return stmt
  return $ Located span newStmt

peepholeOptimizeGoStmts :: [Located GoStmt] -> OptimizationM [Located GoStmt]
peepholeOptimizeGoStmts [] = return []
peepholeOptimizeGoStmts [stmt] = (:[]) <$> peepholeGoStmt stmt
peepholeOptimizeGoStmts (s1:s2:rest) = do
  case (locatedValue s1, locatedValue s2) of
    (GoAssignment var value, GoReturn (Just (Located _ (GoIdentifier returnVar))))
      | var == returnVar -> do
        recordOptimization "Peephole: Eliminated Go temporary variable before return"
        modify $ \s -> s { osChanged = True }
        let newReturn = Located (locatedSpan s2) $ GoReturn (Just value)
        peepholeOptimizeGoStmts (newReturn : rest)
    _ -> do
      newS1 <- peepholeGoStmt s1
      restOptimized <- peepholeOptimizeGoStmts (s2:rest)
      return (newS1 : restOptimized)

peepholeGoExpr :: Located GoExpr -> OptimizationM (Located GoExpr)
peepholeGoExpr (Located span expr) = do
  newExpr <- case expr of
    GoUnaryOp GoOpNot (Located _ (GoBinaryOp GoOpEq left right)) -> do
      recordOptimization "Peephole: Converted Go !(x == y) to x != y"
      modify $ \s -> s { osChanged = True }
      return $ GoBinaryOp GoOpNe left right
    
    GoUnaryOp GoOpNot (Located _ (GoBinaryOp GoOpNe left right)) -> do
      recordOptimization "Peephole: Converted Go !(x != y) to x == y"
      modify $ \s -> s { osChanged = True }
      return $ GoBinaryOp GoOpEq left right
    
    GoBinaryOp op left right -> do
      newLeft <- peepholeGoExpr left
      newRight <- peepholeGoExpr right
      return $ GoBinaryOp op newLeft newRight
    
    GoUnaryOp op operand -> do
      newOperand <- peepholeGoExpr operand
      return $ GoUnaryOp op newOperand
    
    GoCall func args -> do
      newFunc <- peepholeGoExpr func
      newArgs <- mapM peepholeGoExpr args
      return $ GoCall newFunc newArgs
    
    _ -> return expr
  return $ Located span newExpr

-- ============================================================================
-- PUBLIC INTERFACES
-- ============================================================================

recordOptimization :: Text -> OptimizationM ()
recordOptimization opt = modify $ \s -> s { osOptimizations = opt : osOptimizations s }

constantFolding :: Either PythonAST GoAST -> OptimizationResult
constantFolding = runBasicOptimizations $ defaultConfig { 
    bpcEnableDeadCodeElimination = False,
    bpcEnableConstantPropagation = False,
    bpcEnableAlgebraicSimplification = False,
    bpcEnablePeepholeOptimization = False,
    bpcEnableCSE = False,
    bpcEnableStrengthReduction = False
  }

deadCodeElimination :: Either PythonAST GoAST -> OptimizationResult
deadCodeElimination = runBasicOptimizations $ defaultConfig {
    bpcEnableConstantFolding = False,
    bpcEnableConstantPropagation = False,
    bpcEnableAlgebraicSimplification = False,
    bpcEnablePeepholeOptimization = False,
    bpcEnableCSE = False,
    bpcEnableStrengthReduction = False
  }

constantPropagation :: Either PythonAST GoAST -> OptimizationResult
constantPropagation = runBasicOptimizations $ defaultConfig {
    bpcEnableConstantFolding = False,
    bpcEnableDeadCodeElimination = False,
    bpcEnableAlgebraicSimplification = False,
    bpcEnablePeepholeOptimization = False,
    bpcEnableCSE = False,
    bpcEnableStrengthReduction = False
  }

algebraicSimplification :: Either PythonAST GoAST -> OptimizationResult
algebraicSimplification = runBasicOptimizations $ defaultConfig {
    bpcEnableConstantFolding = False,
    bpcEnableDeadCodeElimination = False,
    bpcEnableConstantPropagation = False,
    bpcEnablePeepholeOptimization = False,
    bpcEnableCSE = False,
    bpcEnableStrengthReduction = False
  }

peepholeOptimization :: Either PythonAST GoAST -> OptimizationResult
peepholeOptimization = runBasicOptimizations $ defaultConfig {
    bpcEnableConstantFolding = False,
    bpcEnableDeadCodeElimination = False,
    bpcEnableConstantPropagation = False,
    bpcEnableAlgebraicSimplification = False,
    bpcEnableCSE = False,
    bpcEnableStrengthReduction = False
  }

commonSubexpressionElimination :: Either PythonAST GoAST -> OptimizationResult
commonSubexpressionElimination = runBasicOptimizations $ defaultConfig {
    bpcEnableConstantFolding = False,
    bpcEnableDeadCodeElimination = False,
    bpcEnableConstantPropagation = False,
    bpcEnableAlgebraicSimplification = False,
    bpcEnablePeepholeOptimization = False,
    bpcEnableStrengthReduction = False
  }

strengthReduction :: Either PythonAST GoAST -> OptimizationResult
strengthReduction = runBasicOptimizations $ defaultConfig {
    bpcEnableConstantFolding = False,
    bpcEnableDeadCodeElimination = False,
    bpcEnableConstantPropagation = False,
    bpcEnableAlgebraicSimplification = False,
    bpcEnablePeepholeOptimization = False,
    bpcEnableCSE = False
  }