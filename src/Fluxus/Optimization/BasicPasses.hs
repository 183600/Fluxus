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

import qualified Fluxus.AST.Common as Common
import qualified Fluxus.AST.Python as Python
import qualified Fluxus.AST.Go as Go
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
  { scopeConstants :: !(HashMap Common.Identifier Common.Literal)
  , scopeLiveVars :: !(Set Common.Identifier)
  , scopeDefinedVars :: !(Set Common.Identifier)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Common expression type for CSE
data BasicCommonExpr = BasicCommonExpr
  { ceOp :: !Text
  , ceOperands :: ![Text]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

instance Hashable BasicCommonExpr where
  hashWithSalt salt (BasicCommonExpr op operands) = 
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
  , osGlobalConstants :: !(HashMap Common.Identifier Common.Literal)   -- Global constants
  , osDeadCode :: ![Text]                                -- Dead code identified
  , osSubexpressions :: !(HashMap BasicCommonExpr Common.Identifier) -- Common subexpressions
  , osOptimizations :: ![Text]                           -- Applied optimizations
  , osIterationCount :: !Int                             -- Current iteration
  , osChanged :: !Bool                                   -- Whether changes were made
  , osConstantsFoldedCount :: !Int                       -- Count of constants folded
  , osDeadCodeRemovedCount :: !Int                       -- Count of dead code removed
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- | Result of optimization
data OptimizationResult = OptimizationResult  
  { orPythonAST :: !(Maybe Python.PythonAST)
  , orGoAST :: !(Maybe Go.GoAST)
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

lookupConstant :: Common.Identifier -> OptimizationM (Maybe Common.Literal)
lookupConstant ident = do
  scopes <- gets osScopeStack
  let searchScopes [] = Nothing
      searchScopes (s:ss) = case HashMap.lookup ident (scopeConstants s) of
        Just val -> Just val
        Nothing -> searchScopes ss
  return $ searchScopes scopes

addConstant :: Common.Identifier -> Common.Literal -> OptimizationM ()
addConstant ident lit = modify $ \s ->
  case osScopeStack s of
    [] -> s
    (scope:rest) -> s { osScopeStack = scope { scopeConstants = HashMap.insert ident lit (scopeConstants scope) } : rest }

markLive :: Common.Identifier -> OptimizationM ()
markLive ident = modify $ \s ->
  case osScopeStack s of
    [] -> s
    (scope:rest) -> s { osScopeStack = scope { scopeLiveVars = Set.insert ident (scopeLiveVars scope) } : rest }

isLive :: Common.Identifier -> OptimizationM Bool
isLive ident = do
  scopes <- gets osScopeStack
  return $ any (Set.member ident . scopeLiveVars) scopes

markDefined :: Common.Identifier -> OptimizationM ()
markDefined ident = modify $ \s ->
  case osScopeStack s of
    [] -> s
    (scope:rest) -> s { osScopeStack = scope { scopeDefinedVars = Set.insert ident (scopeDefinedVars scope) } : rest }

isDefined :: Common.Identifier -> OptimizationM Bool
isDefined ident = do
  scopes <- gets osScopeStack
  return $ any (Set.member ident . scopeDefinedVars) scopes

-- | Run basic optimizations on either Python or Go AST
runBasicOptimizations :: BasicPassConfig -> Either Python.PythonAST Go.GoAST -> OptimizationResult
runBasicOptimizations config ast = 
  let (result, finalState) = runState (runReaderT (optimizeAST ast) config) initialState
  in result { orOptimizations = reverse $ osOptimizations finalState
           , orIterations = osIterationCount finalState
           , orConstantsFolded = osConstantsFoldedCount finalState
           , orDeadCodeRemoved = osDeadCodeRemovedCount finalState
           }

-- | Main optimization function that runs all passes iteratively
optimizeAST :: Either Python.PythonAST Go.GoAST -> OptimizationM OptimizationResult
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

constantFoldingPass :: Either Python.PythonAST Go.GoAST -> OptimizationM (Either Python.PythonAST Go.GoAST)
constantFoldingPass (Left pyAST) = Left <$> constantFoldingPython pyAST
constantFoldingPass (Right goAST) = Right <$> constantFoldingGo goAST

constantFoldingPython :: Python.PythonAST -> OptimizationM Python.PythonAST
constantFoldingPython (Python.PythonModule stmts) = do
  newStmts <- mapM constantFoldPythonStmt stmts
  return $ PythonModule newStmts

constantFoldPythonStmt :: Common.Located Python.PythonStmt -> OptimizationM (Common.Located Python.PythonStmt)
constantFoldPythonStmt (Common.Located span stmt) = do
  newStmt <- case stmt of
    Python.PyExprStmt expr -> Python.PyExprStmt <$> constantFoldPythonExpr expr
    
    Python.PyAssign targets value -> do
      newValue <- constantFoldPythonExpr value
      return $ Python.PyAssign targets newValue
    
    Python.PyIf condition thenStmts elseStmts -> do
      newCondition <- constantFoldPythonExpr condition
      newThenStmts <- mapM constantFoldPythonStmt thenStmts
      newElseStmts <- mapM constantFoldPythonStmt elseStmts
      return $ Python.PyIf newCondition newThenStmts newElseStmts
    
    Python.PyFor target iter body -> do
      newIter <- constantFoldPythonExpr iter
      newBody <- withNewScope $ mapM constantFoldPythonStmt body
      return $ Python.PyFor target newIter newBody
    
    Python.PyFuncDef name params body returnType -> do
      newBody <- withNewScope $ mapM constantFoldPythonStmt body
      return $ Python.PyFuncDef name params newBody returnType
    
    Python.PyReturn mexpr -> Python.PyReturn <$> traverse constantFoldPythonExpr mexpr
    
    _ -> return stmt
    
  return $ Common.Located span newStmt

constantFoldPythonExpr :: Common.Located Python.PythonExpr -> OptimizationM (Common.Located Python.PythonExpr)
constantFoldPythonExpr (Common.Located span expr) = do
  newExpr <- case expr of
    Python.PyBinaryOp op left right -> do
      newLeft <- constantFoldPythonExpr left
      newRight <- constantFoldPythonExpr right
      
      case (Common.locatedValue newLeft, Common.locatedValue newRight) of
        (Python.PyLiteral leftLit, Python.PyLiteral rightLit) -> do
          case constantFoldBinaryOp op leftLit rightLit of
            Just result -> do
              recordOptimization $ "Folded constant expression: " <> T.pack (show op)
              modify $ \s -> s { osChanged = True, osConstantsFoldedCount = osConstantsFoldedCount s + 1 }
              return $ Python.PyLiteral result
            Nothing -> return $ Python.PyBinaryOp op newLeft newRight
        _ -> return $ Python.PyBinaryOp op newLeft newRight
    
    Python.PyUnaryOp op operand -> do
      newOperand <- constantFoldPythonExpr operand
      case Common.locatedValue newOperand of
        Python.PyLiteral lit -> do
          case constantFoldUnaryOp op lit of
            Just result -> do
              recordOptimization $ "Folded constant unary operation: " <> T.pack (show op)
              modify $ \s -> s { osChanged = True, osConstantsFoldedCount = osConstantsFoldedCount s + 1 }
              return $ Python.PyLiteral result
            Nothing -> return $ Python.PyUnaryOp op newOperand
        _ -> return $ Python.PyUnaryOp op newOperand
    
    Python.PyCall func args -> do
      newFunc <- constantFoldPythonExpr func
      newArgs <- mapM constantFoldPythonArg args
      return $ Python.PyCall newFunc newArgs
    
    Python.PySubscript container slice -> do
      newContainer <- constantFoldPythonExpr container
      newSlice <- constantFoldPythonSlice slice
      -- Try to fold list/string indexing with constants
      case (Common.locatedValue newContainer, Common.locatedValue newSlice) of
        (Python.PyList elems, Python.SliceIndex (Python.PyLiteral (Common.LInt idx))) | idx >= 0 && fromIntegral idx < length elems ->
          return $ Common.locatedValue (elems !! fromIntegral idx)
        (Python.PyLiteral (Common.LString str), Python.SliceIndex (Python.PyLiteral (Common.LInt idx))) | idx >= 0 && fromIntegral idx < T.length str ->
          return $ Python.PyLiteral $ Common.LString $ T.singleton $ T.index str (fromIntegral idx)
        _ -> return $ Python.PySubscript newContainer newSlice
    
    Python.PyList elems -> Python.PyList <$> mapM constantFoldPythonExpr elems
    Python.PyTuple elems -> Python.PyTuple <$> mapM constantFoldPythonExpr elems
    Python.PyDict pairs -> Python.PyDict <$> mapM (\(k, v) -> (,) <$> constantFoldPythonExpr k <*> constantFoldPythonExpr v) pairs
    
    _ -> return expr
    
  return $ Common.Located span newExpr

constantFoldPythonArg :: Python.PythonArgument -> OptimizationM Python.PythonArgument
constantFoldPythonArg (Python.ArgPositional expr) = Python.ArgPositional <$> constantFoldPythonExpr expr
constantFoldPythonArg (Python.ArgKeyword kw expr) = Python.ArgKeyword kw <$> constantFoldPythonExpr expr
constantFoldPythonArg (Python.ArgStarred expr) = Python.ArgStarred <$> constantFoldPythonExpr expr

constantFoldPythonSlice :: Common.Located Python.PythonSlice -> OptimizationM (Common.Located Python.PythonSlice)
constantFoldPythonSlice (Common.Located span slice) = do
  newSlice <- case slice of
    Python.SliceIndex index -> Python.SliceIndex <$> constantFoldPythonExpr index
    Python.SliceSlice start end step -> do
      newStart <- traverse constantFoldPythonExpr start
      newEnd <- traverse constantFoldPythonExpr end
      newStep <- traverse constantFoldPythonExpr step
      return $ Python.SliceSlice newStart newEnd newStep
    Python.SliceExtSlice slices -> Python.SliceExtSlice <$> mapM constantFoldPythonSlice slices
  return $ Common.Located span newSlice

constantFoldingGo :: Go.GoAST -> OptimizationM Go.GoAST
constantFoldingGo (Go.GoPackage packageName imports decls) = do
  newDecls <- mapM constantFoldGoDecl decls
  return $ Go.GoPackage packageName imports newDecls

constantFoldGoDecl :: Common.Located Go.GoDecl -> OptimizationM (Common.Located Go.GoDecl)
constantFoldGoDecl (Common.Located span decl) = do
  newDecl <- case decl of
    Go.GoFuncDecl name params results body -> do
      newBody <- withNewScope $ constantFoldGoStmt body
      return $ Go.GoFuncDecl name params results newBody
    _ -> return decl
  return $ Common.Located span newDecl

constantFoldGoStmt :: Common.Located Go.GoStmt -> OptimizationM (Common.Located Go.GoStmt)
constantFoldGoStmt (Common.Located span stmt) = do
  newStmt <- case stmt of
    Go.GoExprStmt expr -> Go.GoExprStmt <$> constantFoldGoExpr expr
    Go.GoBind binding -> Go.GoBind <$> (constantFoldGoBinding binding)
    Go.GoIf condition thenStmt elseStmt -> do
      newCondition <- constantFoldGoExpr condition
      newThenStmt <- constantFoldGoStmt thenStmt
      newElseStmt <- traverse constantFoldGoStmt elseStmt
      return $ Go.GoIf newCondition newThenStmt newElseStmt
    Go.GoFor forClause body -> do
      newBody <- withNewScope $ constantFoldGoStmt body
      return $ Go.GoFor forClause newBody
    Go.GoReturn mexpr -> Go.GoReturn <$> traverse constantFoldGoExpr mexpr
    Go.GoBlock stmts -> Go.GoBlock <$> mapM constantFoldGoStmt stmts
    _ -> return stmt
  return $ Common.Located span newStmt

constantFoldGoExpr :: Common.Located Go.GoExpr -> OptimizationM (Common.Located Go.GoExpr)
constantFoldGoExpr (Common.Located span expr) = do
  newExpr <- case expr of
    Go.GoBinaryOp op left right -> do
      newLeft <- constantFoldGoExpr left
      newRight <- constantFoldGoExpr right
      
      case (Common.locatedValue newLeft, Common.locatedValue newRight) of
        (Go.GoLiteral leftLit, Go.GoLiteral rightLit) -> do
          case constantFoldBinaryOpGo op leftLit rightLit of
            Just result -> do
              recordOptimization $ "Folded Go constant expression: " <> T.pack (show op)
              modify $ \s -> s { osChanged = True, osConstantsFoldedCount = osConstantsFoldedCount s + 1 }
              return $ Go.GoLiteral result
            Nothing -> return $ Go.GoBinaryOp op newLeft newRight
        _ -> return $ Go.GoBinaryOp op newLeft newRight
    
    Go.GoUnaryOp op operand -> do
      newOperand <- constantFoldGoExpr operand
      case Common.locatedValue newOperand of
        Go.GoLiteral lit -> do
          case constantFoldUnaryOpGo op lit of
            Just result -> do
              recordOptimization $ "Folded Go constant unary operation: " <> T.pack (show op)
              modify $ \s -> s { osChanged = True, osConstantsFoldedCount = osConstantsFoldedCount s + 1 }
              return $ Go.GoLiteral result
            Nothing -> return $ Go.GoUnaryOp op newOperand
        _ -> return $ Go.GoUnaryOp op newOperand
    
    Go.GoCall func args -> do
      newFunc <- constantFoldGoExpr func
      newArgs <- mapM constantFoldGoExpr args
      return $ Go.GoCall newFunc newArgs
    
    _ -> return expr
    
  return $ Common.Located span newExpr

-- Extended constant folding for binary operations
constantFoldBinaryOp :: Common.BinaryOp -> Common.Literal -> Common.Literal -> Maybe Common.Literal
constantFoldBinaryOp Common.OpAdd (Common.LInt a) (Common.LInt b) = Just $ Common.LInt (a + b)
constantFoldBinaryOp Common.OpAdd (Common.LFloat a) (Common.LFloat b) = Just $ Common.LFloat (a + b)
constantFoldBinaryOp Common.OpAdd (Common.LString a) (Common.LString b) = Just $ Common.LString (a <> b)
constantFoldBinaryOp Common.OpSub (Common.LInt a) (Common.LInt b) = Just $ Common.LInt (a - b)
constantFoldBinaryOp Common.OpSub (Common.LFloat a) (Common.LFloat b) = Just $ Common.LFloat (a - b)
constantFoldBinaryOp Common.OpMul (Common.LInt a) (Common.LInt b) = Just $ Common.LInt (a * b)
constantFoldBinaryOp Common.OpMul (Common.LFloat a) (Common.LFloat b) = Just $ Common.LFloat (a * b)
constantFoldBinaryOp Common.OpDiv (Common.LFloat a) (Common.LFloat b) | b /= 0 = Just $ Common.LFloat (a / b)
constantFoldBinaryOp Common.OpFloorDiv (Common.LInt a) (Common.LInt b) | b /= 0 = Just $ Common.LInt (a `div` b)
constantFoldBinaryOp Common.OpMod (Common.LInt a) (Common.LInt b) | b /= 0 = Just $ Common.LInt (a `mod` b)
constantFoldBinaryOp Common.OpPow (Common.LInt a) (Common.LInt b) | b >= 0 = Just $ Common.LInt (a ^ b)
constantFoldBinaryOp Common.OpAnd (Common.LBool a) (Common.LBool b) = Just $ Common.LBool (a && b)
constantFoldBinaryOp Common.OpOr (Common.LBool a) (Common.LBool b) = Just $ Common.LBool (a || b)
constantFoldBinaryOp Common.OpEq a b = Just $ Common.LBool (a == b)
constantFoldBinaryOp Common.OpNe a b = Just $ Common.LBool (a /= b)
constantFoldBinaryOp Common.OpLt (Common.LInt a) (Common.LInt b) = Just $ Common.LBool (a < b)
constantFoldBinaryOp Common.OpLt (Common.LFloat a) (Common.LFloat b) = Just $ Common.LBool (a < b)
constantFoldBinaryOp Common.OpLe (Common.LInt a) (Common.LInt b) = Just $ Common.LBool (a <= b)
constantFoldBinaryOp Common.OpLe (Common.LFloat a) (Common.LFloat b) = Just $ Common.LBool (a <= b)
constantFoldBinaryOp Common.OpGt (Common.LInt a) (Common.LInt b) = Just $ Common.LBool (a > b)
constantFoldBinaryOp Common.OpGt (Common.LFloat a) (Common.LFloat b) = Just $ Common.LBool (a > b)
constantFoldBinaryOp Common.OpGe (Common.LInt a) (Common.LInt b) = Just $ Common.LBool (a >= b)
constantFoldBinaryOp Common.OpGe (Common.LFloat a) (Common.LFloat b) = Just $ Common.LBool (a >= b)
constantFoldBinaryOp _ _ _ = Nothing

constantFoldUnaryOp :: Common.UnaryOp -> Common.Literal -> Maybe Common.Literal
constantFoldUnaryOp Common.OpNot (Common.LBool b) = Just $ Common.LBool (not b)
constantFoldUnaryOp Common.OpNegate (Common.LInt i) = Just $ Common.LInt (-i)
constantFoldUnaryOp Common.OpNegate (Common.LFloat f) = Just $ Common.LFloat (-f)
constantFoldUnaryOp _ _ = Nothing

constantFoldBinaryOpGo :: Go.BinaryOp -> Go.GoLiteral -> Go.GoLiteral -> Maybe Go.GoLiteral
constantFoldBinaryOpGo Go.OpAdd (Go.GoInt a) (Go.GoInt b) = Just $ Go.GoInt (a + b)
constantFoldBinaryOpGo Go.OpAdd (Go.GoFloat a) (Go.GoFloat b) = Just $ Go.GoFloat (a + b)
constantFoldBinaryOpGo Go.OpAdd (Go.GoString a) (Go.GoString b) = Just $ Go.GoString (a <> b)
constantFoldBinaryOpGo Go.OpSub (Go.GoInt a) (Go.GoInt b) = Just $ Go.GoInt (a - b)
constantFoldBinaryOpGo Go.OpSub (Go.GoFloat a) (Go.GoFloat b) = Just $ Go.GoFloat (a - b)
constantFoldBinaryOpGo Go.OpMul (Go.GoInt a) (Go.GoInt b) = Just $ Go.GoInt (a * b)
constantFoldBinaryOpGo Go.OpMul (Go.GoFloat a) (Go.GoFloat b) = Just $ Go.GoFloat (a * b)
constantFoldBinaryOpGo Go.OpQuo (Go.GoInt a) (Go.GoInt b) | b /= 0 = Just $ Go.GoInt (a `div` b)
constantFoldBinaryOpGo Go.OpQuo (Go.GoFloat a) (Go.GoFloat b) | b /= 0 = Just $ Go.GoFloat (a / b)
constantFoldBinaryOpGo Go.OpRem (Go.GoInt a) (Go.GoInt b) | b /= 0 = Just $ Go.GoInt (a `mod` b)
constantFoldBinaryOpGo Go.OpEql a b = Just $ Go.GoBool (a == b)
constantFoldBinaryOpGo Go.OpNeq a b = Just $ Go.GoBool (a /= b)
constantFoldBinaryOpGo Go.OpLss (Go.GoInt a) (Go.GoInt b) = Just $ Go.GoBool (a < b)
constantFoldBinaryOpGo Go.OpLss (Go.GoFloat a) (Go.GoFloat b) = Just $ Go.GoBool (a < b)
constantFoldBinaryOpGo Go.OpLeq (Go.GoInt a) (Go.GoInt b) = Just $ Go.GoBool (a <= b)
constantFoldBinaryOpGo Go.OpLeq (Go.GoFloat a) (Go.GoFloat b) = Just $ Go.GoBool (a <= b)
constantFoldBinaryOpGo Go.OpGtr (Go.GoInt a) (Go.GoInt b) = Just $ Go.GoBool (a > b)
constantFoldBinaryOpGo Go.OpGtr (Go.GoFloat a) (Go.GoFloat b) = Just $ Go.GoBool (a > b)
constantFoldBinaryOpGo Go.OpGeq (Go.GoInt a) (Go.GoInt b) = Just $ Go.GoBool (a >= b)
constantFoldBinaryOpGo Go.OpGeq (Go.GoFloat a) (Go.GoFloat b) = Just $ Go.GoBool (a >= b)
constantFoldBinaryOpGo Go.OpLand (Go.GoBool a) (Go.GoBool b) = Just $ Go.GoBool (a && b)
constantFoldBinaryOpGo Go.OpLor (Go.GoBool a) (Go.GoBool b) = Just $ Go.GoBool (a || b)
constantFoldBinaryOpGo _ _ _ = Nothing

constantFoldUnaryOpGo :: Go.UnaryOp -> Go.GoLiteral -> Maybe Go.GoLiteral
constantFoldUnaryOpGo Go.OpSub (Go.GoInt i) = Just $ Go.GoInt (-i)
constantFoldUnaryOpGo Go.OpSub (Go.GoFloat f) = Just $ Go.GoFloat (-f)
constantFoldUnaryOpGo Go.OpNot (Go.GoBool b) = Just $ Go.GoBool (not b)
constantFoldUnaryOpGo _ _ = Nothing

constantFoldGoBinding :: Go.GoBinding -> OptimizationM Go.GoBinding
constantFoldGoBinding binding = do
  newRHS <- mapM constantFoldGoExpr (Go.bindRHS binding)
  return $ binding { Go.bindRHS = newRHS }

-- ============================================================================
-- DEAD CODE ELIMINATION PASS
-- ============================================================================

deadCodeEliminationPass :: Either Python.PythonAST Go.GoAST -> OptimizationM (Either Python.PythonAST Go.GoAST)
deadCodeEliminationPass (Left pyAST) = do
  -- First pass: collect live variables
  collectLiveVariablesPython pyAST
  -- Second pass: eliminate dead code
  Left <$> eliminateDeadCodePython pyAST
deadCodeEliminationPass (Right goAST) = do
  collectLiveVariablesGo goAST
  Right <$> eliminateDeadCodeGo goAST

collectLiveVariablesPython :: Python.PythonAST -> OptimizationM ()
collectLiveVariablesPython (Python.PythonModule stmts) = mapM_ collectLiveVarsPyStmt (reverse stmts)

collectLiveVarsPyStmt :: Common.Located Python.PythonStmt -> OptimizationM ()
collectLiveVarsPyStmt (Common.Located _ stmt) = case stmt of
  Python.PyExprStmt expr -> collectLiveVarsPyExpr expr
  Python.PyAssign targets value -> do
    collectLiveVarsPyExpr value
    mapM_ markDefined targets
  Python.PyReturn (Just expr) -> collectLiveVarsPyExpr expr
  Python.PyIf cond thenStmts elseStmts -> do
    collectLiveVarsPyExpr cond
    mapM_ collectLiveVarsPyStmt thenStmts
    mapM_ collectLiveVarsPyStmt elseStmts
  Python.PyFor target iter body -> do
    collectLiveVarsPyExpr iter
    markDefined target
    mapM_ collectLiveVarsPyStmt body
  Python.PyFuncDef name _ body _ -> do
    markDefined name
    withNewScope $ mapM_ collectLiveVarsPyStmt body
  _ -> return ()

collectLiveVarsPyExpr :: Common.Located Python.PythonExpr -> OptimizationM ()
collectLiveVarsPyExpr (Common.Located _ expr) = case expr of
  Python.PyIdentifier ident -> markLive ident
  Python.PyBinaryOp _ left right -> do
    collectLiveVarsPyExpr left
    collectLiveVarsPyExpr right
  Python.PyUnaryOp _ operand -> collectLiveVarsPyExpr operand
  Python.PyCall func args -> do
    collectLiveVarsPyExpr func
    mapM_ collectLiveVarsPyArg args
  Python.PySubscript container slice -> do
    collectLiveVarsPyExpr container
    collectLiveVarsPySlice slice
  Python.PyList elems -> mapM_ collectLiveVarsPyExpr elems
  Python.PyTuple elems -> mapM_ collectLiveVarsPyExpr elems
  Python.PyDict pairs -> mapM_ (\(k, v) -> collectLiveVarsPyExpr k >> collectLiveVarsPyExpr v) pairs
  _ -> return ()

collectLiveVarsPyArg :: Python.PythonArgument -> OptimizationM ()
collectLiveVarsPyArg (Python.ArgPositional expr) = collectLiveVarsPyExpr expr
collectLiveVarsPyArg (Python.ArgKeyword _ expr) = collectLiveVarsPyExpr expr
collectLiveVarsPyArg (Python.ArgStarred expr) = collectLiveVarsPyExpr expr

eliminateDeadCodePython :: Python.PythonAST -> OptimizationM Python.PythonAST
eliminateDeadCodePython (Python.PythonModule stmts) = do
  newStmts <- catMaybes <$> mapM eliminateDeadPyStmt stmts
  return $ Python.PythonModule newStmts

eliminateDeadPyStmt :: Common.Located Python.PythonStmt -> OptimizationM (Maybe (Common.Located Python.PythonStmt))
eliminateDeadPyStmt stmt@(Common.Located span s) = case s of
  Python.PyAssign [target] _ -> do
    live <- isLive target
    if not live
      then do
        recordOptimization $ "Eliminated dead assignment to " <> target
        modify $ \st -> st { osChanged = True, osDeadCodeRemovedCount = osDeadCodeRemovedCount st + 1 }
        return Nothing
      else return $ Just stmt
  Python.PyBlock stmts -> do
    newStmts <- catMaybes <$> mapM eliminateDeadPyStmt stmts
    return $ Just $ Common.Located span $ Python.PyBlock newStmts
  Python.PyIf cond thenStmts elseStmts -> do
    newThenStmts <- catMaybes <$> mapM eliminateDeadPyStmt thenStmts
    newElseStmts <- catMaybes <$> mapM eliminateDeadPyStmt elseStmts
    return $ Just $ Common.Located span $ Python.PyIf cond newThenStmts newElseStmts
  _ -> return $ Just stmt

collectLiveVariablesGo :: Go.GoAST -> OptimizationM ()
collectLiveVariablesGo (Go.GoPackage _ _ decls) = mapM_ collectLiveVarsGoDecl decls

collectLiveVarsGoDecl :: Common.Located Go.GoDecl -> OptimizationM ()
collectLiveVarsGoDecl (Common.Located _ decl) = case decl of
  Go.GoFuncDecl name _ _ body -> do
    markDefined name
    withNewScope $ collectLiveVarsGoStmt body
  _ -> return ()

collectLiveVarsGoStmt :: Common.Located Go.GoStmt -> OptimizationM ()
collectLiveVarsGoStmt (Common.Located _ stmt) = case stmt of
  Go.GoExprStmt expr -> collectLiveVarsGoExpr expr
  Go.GoAssignment lhs rhs -> do
    collectLiveVarsGoExpr rhs
    markDefined lhs
  Go.GoReturn (Just expr) -> collectLiveVarsGoExpr expr
  Go.GoIf cond thenStmt elseStmt -> do
    collectLiveVarsGoExpr cond
    collectLiveVarsGoStmt thenStmt
    traverse_ collectLiveVarsGoStmt elseStmt
  Go.GoFor _ body -> collectLiveVarsGoStmt body
  Go.GoBlock stmts -> mapM_ collectLiveVarsGoStmt stmts
  _ -> return ()

collectLiveVarsGoExpr :: Common.Located Go.GoExpr -> OptimizationM ()
collectLiveVarsGoExpr (Common.Located _ expr) = case expr of
  Go.GoIdentifier ident -> markLive ident
  Go.GoBinaryOp _ left right -> do
    collectLiveVarsGoExpr left
    collectLiveVarsGoExpr right
  Go.GoUnaryOp _ operand -> collectLiveVarsGoExpr operand
  Go.GoCall func args -> do
    collectLiveVarsGoExpr func
    mapM_ collectLiveVarsGoExpr args
  _ -> return ()

eliminateDeadCodeGo :: Go.GoAST -> OptimizationM Go.GoAST
eliminateDeadCodeGo (Go.GoPackage packageName imports decls) = do
  newDecls <- mapM eliminateDeadGoDecl decls
  return $ Go.GoPackage packageName imports newDecls

eliminateDeadGoDecl :: Common.Located Go.GoDecl -> OptimizationM (Common.Located Go.GoDecl)
eliminateDeadGoDecl (Common.Located span decl) = case decl of
  Go.GoFuncDecl name params results body -> do
    newBody <- withNewScope $ eliminateDeadGoStmt body
    return $ Common.Located span $ Go.GoFuncDecl name params results newBody
  _ -> return $ Common.Located span decl

eliminateDeadGoStmt :: Common.Located Go.GoStmt -> OptimizationM (Common.Located Go.GoStmt)
eliminateDeadGoStmt stmt@(Common.Located span s) = case s of
  Go.GoAssignment target _ -> do
    live <- isLive target
    if not live
      then do
        recordOptimization $ "Eliminated dead Go assignment to " <> target
        modify $ \st -> st { osChanged = True, osDeadCodeRemovedCount = osDeadCodeRemovedCount st + 1 }
        return $ Common.Located span Go.GoNop
      else return stmt
  Go.GoBlock stmts -> do
    newStmts <- mapM eliminateDeadGoStmt stmts
    return $ Common.Located span $ Go.GoBlock (filter (not . isNop) newStmts)
  _ -> return stmt
  where
    isNop (Common.Located _ Go.GoNop) = True
    isNop _ = False

-- ============================================================================
-- CONSTANT PROPAGATION PASS
-- ============================================================================

constantPropagationPass :: Either Python.PythonAST Go.GoAST -> OptimizationM (Either Python.PythonAST Go.GoAST)
constantPropagationPass (Left pyAST) = Left <$> constantPropagationPython pyAST
constantPropagationPass (Right goAST) = Right <$> constantPropagationGo goAST

constantPropagationPython :: Python.PythonAST -> OptimizationM Python.PythonAST
constantPropagationPython (Python.PythonModule stmts) = do
  newStmts <- mapM propagateConstantsPyStmt stmts
  return $ Python.PythonModule newStmts

propagateConstantsPyStmt :: Common.Located Python.PythonStmt -> OptimizationM (Common.Located Python.PythonStmt)
propagateConstantsPyStmt (Common.Located span stmt) = do
  newStmt <- case stmt of
    Python.PyAssign [target] value -> do
      newValue <- propagateConstantsPyExpr value
      -- If the value is a constant, record it
      case Common.locatedValue newValue of
        Python.PyLiteral lit -> addConstant target lit
        _ -> return ()
      return $ Python.PyAssign [target] newValue
    
    Python.PyExprStmt expr -> Python.PyExprStmt <$> propagateConstantsPyExpr expr
    
    Python.PyIf cond thenStmts elseStmts -> do
      newCond <- propagateConstantsPyExpr cond
      newThenStmts <- withNewScope $ mapM propagateConstantsPyStmt thenStmts
      newElseStmts <- withNewScope $ mapM propagateConstantsPyStmt elseStmts
      return $ Python.PyIf newCond newThenStmts newElseStmts
    
    Python.PyFor target iter body -> do
      newIter <- propagateConstantsPyExpr iter
      newBody <- withNewScope $ mapM propagateConstantsPyStmt body
      return $ Python.PyFor target newIter newBody
    
    Python.PyFuncDef name params body returnType -> do
      newBody <- withNewScope $ mapM propagateConstantsPyStmt body
      return $ Python.PyFuncDef name params newBody returnType
    
    Python.PyReturn mexpr -> Python.PyReturn <$> traverse propagateConstantsPyExpr mexpr
    
    Python.PyBlock stmts -> Python.PyBlock <$> mapM propagateConstantsPyStmt stmts
    
    _ -> return stmt
    
  return $ Located span newStmt

propagateConstantsPyExpr :: Common.Located Python.PythonExpr -> OptimizationM (Common.Located Python.PythonExpr)
propagateConstantsPyExpr (Common.Located span expr) = do
  newExpr <- case expr of
    Python.PyIdentifier ident -> do
      mval <- lookupConstant ident
      case mval of
        Just lit -> do
          recordOptimization $ "Propagated constant " <> ident
          modify $ \s -> s { osChanged = True }
          return $ Python.PyLiteral lit
        Nothing -> return expr
    
    Python.PyBinaryOp op left right -> do
      newLeft <- propagateConstantsPyExpr left
      newRight <- propagateConstantsPyExpr right
      -- Try constant folding after propagation
      case (Common.locatedValue newLeft, Common.locatedValue newRight) of
        (Python.PyLiteral leftLit, Python.PyLiteral rightLit) ->
          case constantFoldBinaryOp op leftLit rightLit of
            Just result -> return $ Python.PyLiteral result
            Nothing -> return $ Python.PyBinaryOp op newLeft newRight
        _ -> return $ Python.PyBinaryOp op newLeft newRight
    
    Python.PyUnaryOp op operand -> do
      newOperand <- propagateConstantsPyExpr operand
      case Common.locatedValue newOperand of
        Python.PyLiteral lit ->
          case constantFoldUnaryOp op lit of
            Just result -> return $ Python.PyLiteral result
            Nothing -> return $ Python.PyUnaryOp op newOperand
        _ -> return $ Python.PyUnaryOp op newOperand
    
    Python.PyCall func args -> do
      newFunc <- propagateConstantsPyExpr func
      newArgs <- mapM propagateConstantsPyArg args
      return $ Python.PyCall newFunc newArgs
    
    Python.PySubscript container slice -> do
      newContainer <- propagateConstantsPyExpr container
      newSlice <- propagateConstantsPyExpr idx
      return $ Python.PySubscript newContainer newSlice
    
    Python.PyList elems -> Python.PyList <$> mapM propagateConstantsPyExpr elems
    Python.PyTuple elems -> Python.PyTuple <$> mapM propagateConstantsPyExpr elems
    Python.PyDict pairs -> Python.PyDict <$> mapM (\(k, v) -> (,) <$> propagateConstantsPyExpr k <*> propagateConstantsPyExpr v) pairs
    
    _ -> return expr
    
  return $ Located span newExpr

propagateConstantsPyArg :: Python.PythonArgument -> OptimizationM Python.PythonArgument
propagateConstantsPyArg (Python.ArgPositional expr) = Python.ArgPositional <$> propagateConstantsPyExpr expr
propagateConstantsPyArg (Python.ArgKeyword kw expr) = Python.ArgKeyword kw <$> propagateConstantsPyExpr expr
propagateConstantsPyArg (Python.ArgStarred expr) = Python.ArgStarred <$> propagateConstantsPyExpr expr

constantPropagationGo :: Go.GoAST -> OptimizationM Go.GoAST
constantPropagationGo (Go.GoPackage packageName imports decls) = do
  newDecls <- mapM propagateConstantsGoDecl decls
  return $ Go.GoPackage packageName imports newDecls

propagateConstantsGoDecl :: Common.Located Go.GoDecl -> OptimizationM (Common.Located Go.GoDecl)
propagateConstantsGoDecl (Common.Located span decl) = case decl of
  Go.GoFuncDecl name params results body -> do
    newBody <- withNewScope $ propagateConstantsGoStmt body
    return $ Common.Located span $ Go.GoFuncDecl name params results newBody
  _ -> return $ Common.Located span decl

propagateConstantsGoStmt :: Common.Located Go.GoStmt -> OptimizationM (Common.Located Go.GoStmt)
propagateConstantsGoStmt (Common.Located span stmt) = do
  newStmt <- case stmt of
    Go.GoAssignment target value -> do
      newValue <- propagateConstantsGoExpr value
      -- Record constant if applicable
      case Common.locatedValue newValue of
        Go.GoLiteral lit -> addConstant target (goLiteralToLiteral lit)
        _ -> return ()
      return $ Go.GoAssignment target newValue
    
    Go.GoExprStmt expr -> Go.GoExprStmt <$> propagateConstantsGoExpr expr
    Go.GoIf cond thenStmt elseStmt -> do
      newCond <- propagateConstantsGoExpr cond
      newThenStmt <- propagateConstantsGoStmt thenStmt
      newElseStmt <- traverse propagateConstantsGoStmt elseStmt
      return $ Go.GoIf newCond newThenStmt newElseStmt
    Go.GoFor forClause body -> do
      newBody <- withNewScope $ propagateConstantsGoStmt body
      return $ Go.GoFor forClause newBody
    Go.GoReturn mexpr -> Go.GoReturn <$> traverse propagateConstantsGoExpr mexpr
    Go.GoBlock stmts -> Go.GoBlock <$> mapM propagateConstantsGoStmt stmts
    _ -> return stmt
  return $ Located span newStmt

propagateConstantsGoExpr :: Common.Located Go.GoExpr -> OptimizationM (Common.Located Go.GoExpr)
propagateConstantsGoExpr (Common.Located span expr) = do
  newExpr <- case expr of
    Go.GoIdentifier ident -> do
      mval <- lookupConstant ident
      case mval of
        Just lit -> do
          recordOptimization $ "Propagated Go constant " <> ident
          modify $ \s -> s { osChanged = True }
          return $ Go.GoLiteral (literalToGoLiteral lit)
        Nothing -> return expr
    
    Go.GoBinaryOp op left right -> do
      newLeft <- propagateConstantsGoExpr left
      newRight <- propagateConstantsGoExpr right
      return $ Go.GoBinaryOp op newLeft newRight
    
    Go.GoUnaryOp op operand -> do
      newOperand <- propagateConstantsGoExpr operand
      return $ Go.GoUnaryOp op newOperand
    
    Go.GoCall func args -> do
      newFunc <- propagateConstantsGoExpr func
      newArgs <- mapM propagateConstantsGoExpr args
      return $ Go.GoCall newFunc newArgs
    
    _ -> return expr
  return $ Located span newExpr

-- Helper conversions
goLiteralToLiteral :: Go.GoLiteral -> Common.Literal
goLiteralToLiteral (Go.GoInt i) = Common.LInt i
goLiteralToLiteral (Go.GoFloat f) = Common.LFloat f
goLiteralToLiteral (Go.GoString s) = Common.LString s
goLiteralToLiteral (Go.GoBool b) = Common.LBool b
goLiteralToLiteral Go.GoNil = Common.LNone

literalToGoLiteral :: Common.Literal -> Go.GoLiteral
literalToGoLiteral (Common.LInt i) = Go.GoInt i
literalToGoLiteral (Common.LFloat f) = Go.GoFloat f
literalToGoLiteral (Common.LString s) = Go.GoString s
literalToGoLiteral (Common.LBool b) = Go.GoBool b
literalToGoLiteral Common.LNone = Go.GoNil

-- ============================================================================
-- ALGEBRAIC SIMPLIFICATION PASS
-- ============================================================================

algebraicSimplificationPass :: Either Python.PythonAST Go.GoAST -> OptimizationM (Either Python.PythonAST Go.GoAST)
algebraicSimplificationPass (Left pyAST) = Left <$> algebraicSimplificationPython pyAST
algebraicSimplificationPass (Right goAST) = Right <$> algebraicSimplificationGo goAST

algebraicSimplificationPython :: Python.PythonAST -> OptimizationM Python.PythonAST
algebraicSimplificationPython (Python.PythonModule stmts) = do
  newStmts <- mapM simplifyAlgebraicPyStmt stmts
  return $ Python.PythonModule newStmts

simplifyAlgebraicPyStmt :: Common.Located Python.PythonStmt -> OptimizationM (Common.Located Python.PythonStmt)
simplifyAlgebraicPyStmt (Common.Located span stmt) = do
  newStmt <- case stmt of
    Python.PyExprStmt expr -> Python.PyExprStmt <$> simplifyAlgebraicPyExpr expr
    Python.PyAssign targets value -> Python.PyAssign targets <$> simplifyAlgebraicPyExpr value
    Python.PyIf cond thenStmts elseStmts -> do
      newCond <- simplifyAlgebraicPyExpr cond
      newThenStmts <- mapM simplifyAlgebraicPyStmt thenStmts
      newElseStmts <- mapM simplifyAlgebraicPyStmt elseStmts
      return $ Python.PyIf newCond newThenStmts newElseStmts
    Python.PyFor target iter body -> do
      newIter <- simplifyAlgebraicPyExpr iter
      newBody <- mapM simplifyAlgebraicPyStmt body
      return $ Python.PyFor target newIter newBody
    Python.PyFuncDef name params body returnType -> do
      newBody <- mapM simplifyAlgebraicPyStmt body
      return $ Python.PyFuncDef name params newBody returnType
    Python.PyReturn mexpr -> Python.PyReturn <$> traverse simplifyAlgebraicPyExpr mexpr
    Python.PyBlock stmts -> Python.PyBlock <$> mapM simplifyAlgebraicPyStmt stmts
    _ -> return stmt
  return $ Located span newStmt

simplifyAlgebraicPyExpr :: Common.Located Python.PythonExpr -> OptimizationM (Common.Located Python.PythonExpr)
simplifyAlgebraicPyExpr (Common.Located span expr) = do
  newExpr <- case expr of
    Python.PyBinaryOp op left right -> do
      newLeft <- simplifyAlgebraicPyExpr left
      newRight <- simplifyAlgebraicPyExpr right
      
      -- Algebraic simplifications
      case op of
        -- x + 0 = x, 0 + x = x
        Common.OpAdd | isZero (Common.locatedValue newRight) -> do
          recordOptimization "Simplified x + 0 to x"
          modify $ \s -> s { osChanged = True }
          return $ Common.locatedValue newLeft
        Common.OpAdd | isZero (Common.locatedValue newLeft) -> do
          recordOptimization "Simplified 0 + x to x"
          modify $ \s -> s { osChanged = True }
          return $ Common.locatedValue newRight
        
        -- x - 0 = x
        Common.OpSub | isZero (Common.locatedValue newRight) -> do
          recordOptimization "Simplified x - 0 to x"
          modify $ \s -> s { osChanged = True }
          return $ Common.locatedValue newLeft
        
        -- x * 1 = x, 1 * x = x
        Common.OpMul | isOne (Common.locatedValue newRight) -> do
          recordOptimization "Simplified x * 1 to x"
          modify $ \s -> s { osChanged = True }
          return $ Common.locatedValue newLeft
        Common.OpMul | isOne (Common.locatedValue newLeft) -> do
          recordOptimization "Simplified 1 * x to x"
          modify $ \s -> s { osChanged = True }
          return $ Common.locatedValue newRight
        
        -- x * 0 = 0, 0 * x = 0
        Common.OpMul | isZero (Common.locatedValue newRight) || isZero (Common.locatedValue newLeft) -> do
          recordOptimization "Simplified x * 0 to 0"
          modify $ \s -> s { osChanged = True }
          return $ Python.PyLiteral (Common.LInt 0)
        
        -- x / 1 = x
        Common.OpDiv | isOne (Common.locatedValue newRight) -> do
          recordOptimization "Simplified x / 1 to x"
          modify $ \s -> s { osChanged = True }
          return $ Common.locatedValue newLeft
        
        -- x ** 1 = x
        Common.OpPow | isOne (Common.locatedValue newRight) -> do
          recordOptimization "Simplified x ** 1 to x"
          modify $ \s -> s { osChanged = True }
          return $ Common.locatedValue newLeft
        
        -- x ** 0 = 1
        Common.OpPow | isZero (Common.locatedValue newRight) -> do
          recordOptimization "Simplified x ** 0 to 1"
          modify $ \s -> s { osChanged = True }
          return $ Python.PyLiteral (Common.LInt 1)
        
        _ -> return $ Python.PyBinaryOp op newLeft newRight
    
    Python.PyUnaryOp op operand -> do
      newOperand <- simplifyAlgebraicPyExpr operand
      
      -- Double negation elimination
      case (op, Common.locatedValue newOperand) of
        (Common.OpNegate, Python.PyUnaryOp Common.OpNegate inner) -> do
          recordOptimization "Eliminated double negation"
          modify $ \s -> s { osChanged = True }
          return $ Common.locatedValue inner
        (Common.OpNot, Python.PyUnaryOp Common.OpNot inner) -> do
          recordOptimization "Eliminated double logical negation"
          modify $ \s -> s { osChanged = True }
          return $ Common.locatedValue inner
        _ -> return $ Python.PyUnaryOp op newOperand
    
    Python.PyCall func args -> do
      newFunc <- simplifyAlgebraicPyExpr func
      newArgs <- mapM simplifyAlgebraicPyArg args
      return $ Python.PyCall newFunc newArgs
    
    Python.PySubscript container slice -> do
      newContainer <- simplifyAlgebraicPyExpr container
      newSlice <- simplifyAlgebraicPyExpr idx
      return $ Python.PySubscript newContainer newSlice
    
    Python.PyList elems -> Python.PyList <$> mapM simplifyAlgebraicPyExpr elems
    Python.PyTuple elems -> Python.PyTuple <$> mapM simplifyAlgebraicPyExpr elems
    Python.PyDict pairs -> Python.PyDict <$> mapM (\(k, v) -> (,) <$> simplifyAlgebraicPyExpr k <*> simplifyAlgebraicPyExpr v) pairs
    
    _ -> return expr
  return $ Located span newExpr

simplifyAlgebraicPyArg :: Python.PythonArgument -> OptimizationM Python.PythonArgument
simplifyAlgebraicPyArg (Python.ArgPositional expr) = Python.ArgPositional <$> simplifyAlgebraicPyExpr expr
simplifyAlgebraicPyArg (Python.ArgKeyword kw expr) = Python.ArgKeyword kw <$> simplifyAlgebraicPyExpr expr
simplifyAlgebraicPyArg (Python.ArgStarred expr) = Python.ArgStarred <$> simplifyAlgebraicPyExpr expr

isZero :: Python.PythonExpr -> Bool
isZero (Python.PyLiteral (Common.LInt 0)) = True
isZero (Python.PyLiteral (Common.LFloat 0.0)) = True
isZero _ = False

isOne :: Python.PythonExpr -> Bool
isOne (Python.PyLiteral (Common.LInt 1)) = True
isOne (Python.PyLiteral (Common.LFloat 1.0)) = True
isOne _ = False

algebraicSimplificationGo :: Go.GoAST -> OptimizationM Go.GoAST
algebraicSimplificationGo (Go.GoPackage packageName imports decls) = do
  newDecls <- mapM simplifyAlgebraicGoDecl decls
  return $ Go.GoPackage packageName imports newDecls

simplifyAlgebraicGoDecl :: Common.Located Go.GoDecl -> OptimizationM (Common.Located Go.GoDecl)
simplifyAlgebraicGoDecl (Common.Located span decl) = case decl of
  Go.GoFuncDecl name params results body -> do
    newBody <- simplifyAlgebraicGoStmt body
    return $ Common.Located span $ Go.GoFuncDecl name params results newBody
  _ -> return $ Common.Located span decl

simplifyAlgebraicGoStmt :: Common.Located Go.GoStmt -> OptimizationM (Common.Located Go.GoStmt)
simplifyAlgebraicGoStmt (Common.Located span stmt) = do
  newStmt <- case stmt of
    Go.GoExprStmt expr -> Go.GoExprStmt <$> simplifyAlgebraicGoExpr expr
    Go.GoAssignment lhs rhs -> Go.GoAssignment lhs <$> simplifyAlgebraicGoExpr rhs
    Go.GoIf cond thenStmt elseStmt -> do
      newCond <- simplifyAlgebraicGoExpr cond
      newThenStmt <- simplifyAlgebraicGoStmt thenStmt
      newElseStmt <- traverse simplifyAlgebraicGoStmt elseStmt
      return $ Go.GoIf newCond newThenStmt newElseStmt
    Go.GoFor forClause body -> do
      newBody <- simplifyAlgebraicGoStmt body
      return $ Go.GoFor forClause newBody
    Go.GoReturn mexpr -> Go.GoReturn <$> traverse simplifyAlgebraicGoExpr mexpr
    Go.GoBlock stmts -> Go.GoBlock <$> mapM simplifyAlgebraicGoStmt stmts
    _ -> return stmt
  return $ Located span newStmt

simplifyAlgebraicGoExpr :: Common.Located Go.GoExpr -> OptimizationM (Common.Located Go.GoExpr)
simplifyAlgebraicGoExpr (Common.Located span expr) = do
  newExpr <- case expr of
    Go.GoBinaryOp op left right -> do
      newLeft <- simplifyAlgebraicGoExpr left
      newRight <- simplifyAlgebraicGoExpr right
      
      case op of
        Go.GoOpAdd | isGoZero (Common.locatedValue newRight) -> do
          recordOptimization "Simplified Go x + 0 to x"
          modify $ \s -> s { osChanged = True }
          return $ Common.locatedValue newLeft
        Go.GoOpSub | isGoZero (Common.locatedValue newRight) -> do
          recordOptimization "Simplified Go x - 0 to x"
          modify $ \s -> s { osChanged = True }
          return $ Common.locatedValue newLeft
        Go.GoOpMul | isGoOne (Common.locatedValue newRight) -> do
          recordOptimization "Simplified Go x * 1 to x"
          modify $ \s -> s { osChanged = True }
          return $ Common.locatedValue newLeft
        Go.GoOpMul | isGoZero (Common.locatedValue newRight) || isGoZero (Common.locatedValue newLeft) -> do
          recordOptimization "Simplified Go x * 0 to 0"
          modify $ \s -> s { osChanged = True }
          return $ Go.GoLiteral (Go.GoInt 0)
        Go.GoOpDiv | isGoOne (Common.locatedValue newRight) -> do
          recordOptimization "Simplified Go x / 1 to x"
          modify $ \s -> s { osChanged = True }
          return $ Common.locatedValue newLeft
        _ -> return $ Go.GoBinaryOp op newLeft newRight
    
    Go.GoUnaryOp op operand -> do
      newOperand <- simplifyAlgebraicGoExpr operand
      case (op, Common.locatedValue newOperand) of
        (Go.GoOpNegate, Go.GoUnaryOp Go.GoOpNegate inner) -> do
          recordOptimization "Eliminated Go double negation"
          modify $ \s -> s { osChanged = True }
          return $ Common.locatedValue inner
        (Go.GoOpNot, Go.GoUnaryOp Go.GoOpNot inner) -> do
          recordOptimization "Eliminated Go double logical negation"
          modify $ \s -> s { osChanged = True }
          return $ Common.locatedValue inner
        _ -> return $ Go.GoUnaryOp op newOperand
    
    Go.GoCall func args -> do
      newFunc <- simplifyAlgebraicGoExpr func
      newArgs <- mapM simplifyAlgebraicGoExpr args
      return $ Go.GoCall newFunc newArgs
    
    _ -> return expr
  return $ Located span newExpr

isGoZero :: Go.GoExpr -> Bool
isGoZero (Go.GoLiteral (Go.GoInt 0)) = True
isGoZero (Go.GoLiteral (Go.GoFloat 0.0)) = True
isGoZero _ = False

isGoOne :: Go.GoExpr -> Bool
isGoOne (Go.GoLiteral (Go.GoInt 1)) = True
isGoOne (Go.GoLiteral (Go.GoFloat 1.0)) = True
isGoOne _ = False

-- ============================================================================
-- STRENGTH REDUCTION PASS
-- ============================================================================

strengthReductionPass :: Either Python.PythonAST Go.GoAST -> OptimizationM (Either Python.PythonAST Go.GoAST)
strengthReductionPass (Left pyAST) = Left <$> strengthReductionPython pyAST
strengthReductionPass (Right goAST) = Right <$> strengthReductionGo goAST

strengthReductionPython :: Python.PythonAST -> OptimizationM Python.PythonAST
strengthReductionPython (Python.PythonModule stmts) = do
  newStmts <- mapM strengthReducePyStmt stmts
  return $ Python.PythonModule newStmts

strengthReducePyStmt :: Common.Located Python.PythonStmt -> OptimizationM (Common.Located Python.PythonStmt)
strengthReducePyStmt (Common.Located span stmt) = do
  newStmt <- case stmt of
    Python.PyExprStmt expr -> Python.PyExprStmt <$> strengthReducePyExpr expr
    Python.PyAssign targets value -> Python.PyAssign targets <$> strengthReducePyExpr value
    Python.PyIf cond thenStmts elseStmts -> do
      newCond <- strengthReducePyExpr cond
      newThenStmts <- mapM strengthReducePyStmt thenStmts
      newElseStmts <- mapM strengthReducePyStmt elseStmts
      return $ Python.PyIf newCond newThenStmts newElseStmts
    Python.PyFor target iter body -> do
      newIter <- strengthReducePyExpr iter
      newBody <- mapM strengthReducePyStmt body
      return $ Python.PyFor target newIter newBody
    Python.PyFuncDef name params body returnType -> do
      newBody <- mapM strengthReducePyStmt body
      return $ Python.PyFuncDef name params newBody returnType
    Python.PyReturn mexpr -> Python.PyReturn <$> traverse strengthReducePyExpr mexpr
    Python.PyBlock stmts -> Python.PyBlock <$> mapM strengthReducePyStmt stmts
    _ -> return stmt
  return $ Located span newStmt

strengthReducePyExpr :: Common.Located Python.PythonExpr -> OptimizationM (Common.Located Python.PythonExpr)
strengthReducePyExpr (Common.Located span expr) = do
  newExpr <- case expr of
    Python.PyBinaryOp op left right -> do
      newLeft <- strengthReducePyExpr left
      newRight <- strengthReducePyExpr right
      
      -- Strength reduction patterns
      case op of
        -- x * 2 => x + x (multiplication by 2 to addition)
        Common.OpMul | isPowerOfTwo (Common.locatedValue newRight) -> do
          let power = getPowerOfTwo (Common.locatedValue newRight)
          if power == 1
            then do
              recordOptimization "Reduced x * 2 to x + x"
              modify $ \s -> s { osChanged = True }
              return $ Python.PyBinaryOp Common.OpAdd newLeft newLeft
            else if power > 1 && power <= 4
              then do
                recordOptimization $ "Reduced x * " <> T.pack (show (2^power)) <> " to left shift"
                modify $ \s -> s { osChanged = True }
                return $ Python.PyBinaryOp Common.OpLShift newLeft (Common.Located span $ Python.PyLiteral $ Common.LInt power)
              else return $ Python.PyBinaryOp op newLeft newRight
        
        -- x / 2^n => x >> n (division by power of 2 to right shift)
        Common.OpFloorDiv | isPowerOfTwo (Common.locatedValue newRight) -> do
          let power = getPowerOfTwo (Common.locatedValue newRight)
          if power > 0
            then do
              recordOptimization $ "Reduced x // " <> T.pack (show (2^power)) <> " to right shift"
              modify $ \s -> s { osChanged = True }
              return $ Python.PyBinaryOp Common.OpRShift newLeft (Common.Located span $ Python.PyLiteral $ Common.LInt power)
            else return $ Python.PyBinaryOp op newLeft newRight
        
        -- x % 2^n => x & (2^n - 1) (modulo by power of 2 to bitwise and)
        Common.OpMod | isPowerOfTwo (Common.locatedValue newRight) -> do
          let power = getPowerOfTwo (Common.locatedValue newRight)
          if power > 0
            then do
              recordOptimization $ "Reduced x % " <> T.pack (show (2^power)) <> " to bitwise and"
              modify $ \s -> s { osChanged = True }
              let mask = (2^power) - 1
              return $ Python.PyBinaryOp Common.OpBitAnd newLeft (Common.Located span $ Python.PyLiteral $ Common.LInt mask)
            else return $ Python.PyBinaryOp op newLeft newRight
        
        _ -> return $ Python.PyBinaryOp op newLeft newRight
    
    Python.PyUnaryOp op operand -> do
      newOperand <- strengthReducePyExpr operand
      return $ Python.PyUnaryOp op newOperand
    
    Python.PyCall func args -> do
      newFunc <- strengthReducePyExpr func
      newArgs <- mapM strengthReducePyArg args
      return $ Python.PyCall newFunc newArgs
    
    Python.PySubscript container slice -> do
      newContainer <- strengthReducePyExpr container
      newSlice <- strengthReducePyExpr idx
      return $ Python.PySubscript newContainer newSlice
    
    Python.PyList elems -> Python.PyList <$> mapM strengthReducePyExpr elems
    Python.PyTuple elems -> Python.PyTuple <$> mapM strengthReducePyExpr elems
    Python.PyDict pairs -> Python.PyDict <$> mapM (\(k, v) -> (,) <$> strengthReducePyExpr k <*> strengthReducePyExpr v) pairs
    
    _ -> return expr
  return $ Located span newExpr

strengthReducePyArg :: Python.PythonArgument -> OptimizationM Python.PythonArgument
strengthReducePyArg (Python.ArgPositional expr) = Python.ArgPositional <$> strengthReducePyExpr expr
strengthReducePyArg (Python.ArgKeyword kw expr) = Python.ArgKeyword kw <$> strengthReducePyExpr expr
strengthReducePyArg (Python.ArgStarred expr) = Python.ArgStarred <$> strengthReducePyExpr expr

isPowerOfTwo :: Python.PythonExpr -> Bool
isPowerOfTwo (Python.PyLiteral (Common.LInt n)) = n > 0 && (n .&. (n - 1)) == 0
isPowerOfTwo _ = False

getPowerOfTwo :: Python.PythonExpr -> Integer
getPowerOfTwo (Python.PyLiteral (Common.LInt n)) = 
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

strengthReduceGoExpr :: Common.Located Go.GoExpr -> OptimizationM (Common.Located Go.GoExpr)
strengthReduceGoExpr (Common.Located span expr) = do
  newExpr <- case expr of
    Go.GoBinaryOp op left right -> do
      newLeft <- strengthReduceGoExpr left
      newRight <- strengthReduceGoExpr right
      
      case op of
        Go.GoOpMul | isGoPowerOfTwo (Common.locatedValue newRight) -> do
          let power = getGoPowerOfTwo (Common.locatedValue newRight)
          if power == 1
            then do
              recordOptimization "Reduced Go x * 2 to x + x"
              modify $ \s -> s { osChanged = True }
              return $ Go.GoBinaryOp Go.GoOpAdd newLeft newLeft
            else if power > 1 && power <= 4
              then do
                recordOptimization $ "Reduced Go x * " <> T.pack (show (2^power)) <> " to left shift"
                modify $ \s -> s { osChanged = True }
                return $ Go.GoBinaryOp Go.GoOpLShift newLeft (Common.Located span $ Go.GoLiteral $ Go.GoInt power)
              else return $ Go.GoBinaryOp op newLeft newRight
        _ -> return $ Go.GoBinaryOp op newLeft newRight
    
    Go.GoUnaryOp op operand -> do
      newOperand <- strengthReduceGoExpr operand
      return $ Go.GoUnaryOp op newOperand
    
    Go.GoCall func args -> do
      newFunc <- strengthReduceGoExpr func
      newArgs <- mapM strengthReduceGoExpr args
      return $ Go.GoCall newFunc newArgs
    
    _ -> return expr
  return $ Located span newExpr

isGoPowerOfTwo :: Go.GoExpr -> Bool
isGoPowerOfTwo (Go.GoLiteral (Go.GoInt n)) = n > 0 && (n .&. (n - 1)) == 0
isGoPowerOfTwo _ = False

getGoPowerOfTwo :: Go.GoExpr -> Integer
getGoPowerOfTwo (Go.GoLiteral (Go.GoInt n)) = 
  let countBits x p = if x == 1 then p else countBits (x `div` 2) (p + 1)
  in if n > 0 && (n .&. (n - 1)) == 0 then countBits n 0 else 0
getGoPowerOfTwo _ = 0

-- ============================================================================
-- COMMON SUBEXPRESSION ELIMINATION PASS
-- ============================================================================

commonSubexpressionEliminationPass :: Either Python.PythonAST Go.GoAST -> OptimizationM (Either Python.PythonAST Go.GoAST)
commonSubexpressionEliminationPass (Left pyAST) = Left <$> csePython pyAST
commonSubexpressionEliminationPass (Right goAST) = Right <$> cseGo goAST

csePython :: Python.PythonAST -> OptimizationM Python.PythonAST
csePython (Python.PythonModule stmts) = do
  newStmts <- mapM csePyStmt stmts
  return $ Python.PythonModule newStmts

csePyStmt :: Common.Located Python.PythonStmt -> OptimizationM (Common.Located Python.PythonStmt)
csePyStmt (Common.Located span stmt) = do
  newStmt <- case stmt of
    Python.PyExprStmt expr -> Python.PyExprStmt <$> csePyExpr expr
    Python.PyAssign targets value -> Python.PyAssign targets <$> csePyExpr value
    Python.PyIf cond thenStmts elseStmts -> do
      newCond <- csePyExpr cond
      newThenStmts <- mapM csePyStmt thenStmts
      newElseStmts <- mapM csePyStmt elseStmts
      return $ Python.PyIf newCond newThenStmts newElseStmts
    Python.PyFor target iter body -> do
      newIter <- csePyExpr iter
      -- Clear CSE cache for loop body as it may execute multiple times
      modify $ \s -> s { osSubexpressions = HashMap.empty }
      newBody <- mapM csePyStmt body
      return $ Python.PyFor target newIter newBody
    Python.PyFuncDef name params body returnType -> do
      -- Clear CSE cache for function body
      modify $ \s -> s { osSubexpressions = HashMap.empty }
      newBody <- mapM csePyStmt body
      return $ Python.PyFuncDef name params newBody returnType
    Python.PyReturn mexpr -> Python.PyReturn <$> traverse csePyExpr mexpr
    Python.PyBlock stmts -> Python.PyBlock <$> mapM csePyStmt stmts
    _ -> return stmt
  return $ Located span newStmt

csePyExpr :: Common.Located Python.PythonExpr -> OptimizationM (Common.Located Python.PythonExpr)
csePyExpr (Common.Located span expr) = do
  newExpr <- case expr of
    Python.PyBinaryOp op left right -> do
      newLeft <- csePyExpr left
      newRight <- csePyExpr right
      
      -- Check if this expression has been computed before
      let exprKey = BasicCommonExpr (T.pack $ show op) [exprToText $ Common.locatedValue newLeft, exprToText $ Common.locatedValue newRight]
      cache <- gets osSubexpressions
      
      case HashMap.lookup exprKey cache of
        Just varName -> do
          recordOptimization $ "Reused common subexpression: " <> T.pack (show op)
          modify $ \s -> s { osChanged = True }
          return $ Python.PyIdentifier varName
        Nothing -> do
          -- Generate new variable name and cache it
          let newVarName = "_cse_" <> T.pack (show $ HashMap.size cache)
          modify $ \s -> s { osSubexpressions = HashMap.insert exprKey newVarName (osSubexpressions s) }
          return $ Python.PyBinaryOp op newLeft newRight
    
    Python.PyUnaryOp op operand -> do
      newOperand <- csePyExpr operand
      return $ Python.PyUnaryOp op newOperand
    
    Python.PyCall func args -> do
      newFunc <- csePyExpr func
      newArgs <- mapM csePyArg args
      return $ Python.PyCall newFunc newArgs
    
    Python.PySubscript container slice -> do
      newContainer <- csePyExpr container
      newSlice <- csePyExpr idx
      return $ Python.PySubscript newContainer newSlice
    
    Python.PyList elems -> Python.PyList <$> mapM csePyExpr elems
    Python.PyTuple elems -> Python.PyTuple <$> mapM csePyExpr elems
    Python.PyDict pairs -> Python.PyDict <$> mapM (\(k, v) -> (,) <$> csePyExpr k <*> csePyExpr v) pairs
    
    _ -> return expr
  return $ Located span newExpr

csePyArg :: Python.PythonArgument -> OptimizationM Python.PythonArgument
csePyArg (Python.ArgPositional expr) = Python.ArgPositional <$> csePyExpr expr
csePyArg (Python.ArgKeyword kw expr) = Python.ArgKeyword kw <$> csePyExpr expr
csePyArg (Python.ArgStarred expr) = Python.ArgStarred <$> csePyExpr expr

exprToText :: Python.PythonExpr -> Text
exprToText (Python.PyIdentifier name) = name
exprToText (Python.PyLiteral lit) = T.pack $ show lit
exprToText _ = "_complex_"

cseGo :: Go.GoAST -> OptimizationM Go.GoAST
cseGo (Go.GoPackage packageName imports decls) = do
  newDecls <- mapM cseGoDecl decls
  return $ Go.GoPackage packageName imports newDecls

cseGoDecl :: Common.Located Go.GoDecl -> OptimizationM (Common.Located Go.GoDecl)
cseGoDecl (Common.Located span decl) = case decl of
  Go.GoFuncDecl name params results body -> do
    modify $ \s -> s { osSubexpressions = HashMap.empty }
    newBody <- cseGoStmt body
    return $ Common.Located span $ Go.GoFuncDecl name params results newBody
  _ -> return $ Common.Located span decl

cseGoStmt :: Common.Located Go.GoStmt -> OptimizationM (Common.Located Go.GoStmt)
cseGoStmt (Common.Located span stmt) = do
  newStmt <- case stmt of
    Go.GoExprStmt expr -> Go.GoExprStmt <$> cseGoExpr expr
    Go.GoAssignment lhs rhs -> Go.GoAssignment lhs <$> cseGoExpr rhs
    Go.GoIf cond thenStmt elseStmt -> do
      newCond <- cseGoExpr cond
      newThenStmt <- cseGoStmt thenStmt
      newElseStmt <- traverse cseGoStmt elseStmt
      return $ Go.GoIf newCond newThenStmt newElseStmt
    Go.GoFor forClause body -> do
      modify $ \s -> s { osSubexpressions = HashMap.empty }
      newBody <- cseGoStmt body
      return $ Go.GoFor forClause newBody
    Go.GoReturn mexpr -> Go.GoReturn <$> traverse cseGoExpr mexpr
    Go.GoBlock stmts -> Go.GoBlock <$> mapM cseGoStmt stmts
    _ -> return stmt
  return $ Located span newStmt

cseGoExpr :: Common.Located Go.GoExpr -> OptimizationM (Common.Located Go.GoExpr)
cseGoExpr (Common.Located span expr) = do
  newExpr <- case expr of
    Go.GoBinaryOp op left right -> do
      newLeft <- cseGoExpr left
      newRight <- cseGoExpr right
      
      let exprKey = BasicCommonExpr (T.pack $ show op) [goExprToText $ Common.locatedValue newLeft, goExprToText $ Common.locatedValue newRight]
      cache <- gets osSubexpressions
      
      case HashMap.lookup exprKey cache of
        Just varName -> do
          recordOptimization $ "Reused Go common subexpression: " <> T.pack (show op)
          modify $ \s -> s { osChanged = True }
          return $ Go.GoIdentifier varName
        Nothing -> do
          let newVarName = "_cse_" <> T.pack (show $ HashMap.size cache)
          modify $ \s -> s { osSubexpressions = HashMap.insert exprKey newVarName (osSubexpressions s) }
          return $ Go.GoBinaryOp op newLeft newRight
    
    Go.GoUnaryOp op operand -> do
      newOperand <- cseGoExpr operand
      return $ Go.GoUnaryOp op newOperand
    
    Go.GoCall func args -> do
      newFunc <- cseGoExpr func
      newArgs <- mapM cseGoExpr args
      return $ Go.GoCall newFunc newArgs
    
    _ -> return expr
  return $ Located span newExpr

goExprToText :: Go.GoExpr -> Text
goExprToText (Go.GoIdentifier name) = name
goExprToText (Go.GoLiteral lit) = T.pack $ show lit
goExprToText _ = "_complex_"

-- ============================================================================
-- PEEPHOLE OPTIMIZATION PASS
-- ============================================================================

peepholeOptimizationPass :: Either Python.PythonAST Go.GoAST -> OptimizationM (Either Python.PythonAST Go.GoAST)
peepholeOptimizationPass (Left pyAST) = Left <$> peepholePython pyAST
peepholeOptimizationPass (Right goAST) = Right <$> peepholeGo goAST

peepholePython :: Python.PythonAST -> OptimizationM Python.PythonAST
peepholePython (Python.PythonModule stmts) = do
  newStmts <- peepholeOptimizeStmts stmts
  return $ Python.PythonModule newStmts

peepholeOptimizeStmts :: [Common.Located Python.PythonStmt] -> OptimizationM [Common.Located Python.PythonStmt]
peepholeOptimizeStmts [] = return []
peepholeOptimizeStmts [stmt] = (:[]) <$> peepholePyStmt stmt
peepholeOptimizeStmts (s1:s2:rest) = do
  -- Look for patterns in consecutive statements
  case (Common.locatedValue s1, Common.locatedValue s2) of
    -- Pattern: x = y; return x => return y
    (Python.PyAssign [var] value, Python.PyReturn (Just (Common.Located _ (Python.PyIdentifier returnVar))))
      | var == returnVar -> do
        recordOptimization "Peephole: Eliminated temporary variable before return"
        modify $ \s -> s { osChanged = True }
        let newReturn = Common.Located (Common.locatedSpan s2) $ Python.PyReturn (Just value)
        peepholeOptimizeStmts (newReturn : rest)
    
    _ -> do
      newS1 <- peepholePyStmt s1
      restOptimized <- peepholeOptimizeStmts (s2:rest)
      return (newS1 : restOptimized)

peepholePyStmt :: Common.Located Python.PythonStmt -> OptimizationM (Common.Located Python.PythonStmt)
peepholePyStmt (Common.Located span stmt) = do
  newStmt <- case stmt of
    Python.PyExprStmt expr -> Python.PyExprStmt <$> peepholePyExpr expr
    Python.PyAssign targets value -> Python.PyAssign targets <$> peepholePyExpr value
    Python.PyIf cond thenStmts elseStmts -> do
      newCond <- peepholePyExpr cond
      newThenStmts <- peepholeOptimizeStmts thenStmts
      newElseStmts <- peepholeOptimizeStmts elseStmts
      return $ Python.PyIf newCond newThenStmts newElseStmts
    Python.PyFor target iter body -> do
      newIter <- peepholePyExpr iter
      newBody <- peepholeOptimizeStmts body
      return $ Python.PyFor target newIter newBody
    Python.PyFuncDef name params body returnType -> do
      newBody <- peepholeOptimizeStmts body
      return $ Python.PyFuncDef name params newBody returnType
    Python.PyReturn mexpr -> Python.PyReturn <$> traverse peepholePyExpr mexpr
    Python.PyBlock stmts -> Python.PyBlock <$> peepholeOptimizeStmts stmts
    _ -> return stmt
  return $ Located span newStmt

peepholePyExpr :: Common.Located Python.PythonExpr -> OptimizationM (Common.Located Python.PythonExpr)
peepholePyExpr (Common.Located span expr) = do
  newExpr <- case expr of
    -- Pattern: not (x == y) => x != y
    Python.PyUnaryOp Common.OpNot (Common.Located _ (Python.PyBinaryOp Common.OpEq left right)) -> do
      recordOptimization "Peephole: Converted not (x == y) to x != y"
      modify $ \s -> s { osChanged = True }
      return $ Python.PyBinaryOp Common.OpNe left right
    
    -- Pattern: not (x != y) => x == y
    Python.PyUnaryOp Common.OpNot (Common.Located _ (Python.PyBinaryOp Common.OpNe left right)) -> do
      recordOptimization "Peephole: Converted not (x != y) to x == y"
      modify $ \s -> s { osChanged = True }
      return $ Python.PyBinaryOp Common.OpEq left right
    
    -- Pattern: not (x < y) => x >= y
    Python.PyUnaryOp Common.OpNot (Common.Located _ (Python.PyBinaryOp Common.OpLt left right)) -> do
      recordOptimization "Peephole: Converted not (x < y) to x >= y"
      modify $ \s -> s { osChanged = True }
      return $ Python.PyBinaryOp Common.OpGe left right
    
    -- Pattern: not (x > y) => x <= y
    Python.PyUnaryOp Common.OpNot (Common.Located _ (Python.PyBinaryOp Common.OpGt left right)) -> do
      recordOptimization "Peephole: Converted not (x > y) to x <= y"
      modify $ \s -> s { osChanged = True }
      return $ Python.PyBinaryOp Common.OpLe left right
    
    Python.PyBinaryOp op left right -> do
      newLeft <- peepholePyExpr left
      newRight <- peepholePyExpr right
      return $ Python.PyBinaryOp op newLeft newRight
    
    Python.PyUnaryOp op operand -> do
      newOperand <- peepholePyExpr operand
      return $ Python.PyUnaryOp op newOperand
    
    Python.PyCall func args -> do
      newFunc <- peepholePyExpr func
      newArgs <- mapM peepholePyArg args
      return $ Python.PyCall newFunc newArgs
    
    Python.PySubscript container slice -> do
      newContainer <- peepholePyExpr container
      newSlice <- peepholePyExpr idx
      return $ Python.PySubscript newContainer newSlice
    
    Python.PyList elems -> Python.PyList <$> mapM peepholePyExpr elems
    Python.PyTuple elems -> Python.PyTuple <$> mapM peepholePyExpr elems
    Python.PyDict pairs -> Python.PyDict <$> mapM (\(k, v) -> (,) <$> peepholePyExpr k <*> peepholePyExpr v) pairs
    
    _ -> return expr
  return $ Located span newExpr

peepholePyArg :: Python.PythonArgument -> OptimizationM Python.PythonArgument
peepholePyArg (Python.ArgPositional expr) = Python.ArgPositional <$> peepholePyExpr expr
peepholePyArg (Python.ArgKeyword kw expr) = Python.ArgKeyword kw <$> peepholePyExpr expr
peepholePyArg (Python.ArgStarred expr) = Python.ArgStarred <$> peepholePyExpr expr

peepholeGo :: Go.GoAST -> OptimizationM Go.GoAST
peepholeGo (Go.GoPackage packageName imports decls) = do
  newDecls <- mapM peepholeGoDecl decls
  return $ Go.GoPackage packageName imports newDecls

peepholeGoDecl :: Common.Located Go.GoDecl -> OptimizationM (Common.Located Go.GoDecl)
peepholeGoDecl (Common.Located span decl) = case decl of
  Go.GoFuncDecl name params results body -> do
    newBody <- peepholeGoStmt body
    return $ Common.Located span $ Go.GoFuncDecl name params results newBody
  _ -> return $ Common.Located span decl

peepholeGoStmt :: Common.Located Go.GoStmt -> OptimizationM (Common.Located Go.GoStmt)
peepholeGoStmt (Common.Located span stmt) = do
  newStmt <- case stmt of
    Go.GoExprStmt expr -> Go.GoExprStmt <$> peepholeGoExpr expr
    Go.GoAssignment lhs rhs -> Go.GoAssignment lhs <$> peepholeGoExpr rhs
    Go.GoIf cond thenStmt elseStmt -> do
      newCond <- peepholeGoExpr cond
      newThenStmt <- peepholeGoStmt thenStmt
      newElseStmt <- traverse peepholeGoStmt elseStmt
      return $ Go.GoIf newCond newThenStmt newElseStmt
    Go.GoFor forClause body -> do
      newBody <- peepholeGoStmt body
      return $ Go.GoFor forClause newBody
    Go.GoReturn mexpr -> Go.GoReturn <$> traverse peepholeGoExpr mexpr
    Go.GoBlock stmts -> do
      newStmts <- peepholeOptimizeGoStmts stmts
      return $ Go.GoBlock newStmts
    _ -> return stmt
  return $ Located span newStmt

peepholeOptimizeGoStmts :: [Common.Located Go.GoStmt] -> OptimizationM [Common.Located Go.GoStmt]
peepholeOptimizeGoStmts [] = return []
peepholeOptimizeGoStmts [stmt] = (:[]) <$> peepholeGoStmt stmt
peepholeOptimizeGoStmts (s1:s2:rest) = do
  case (Common.locatedValue s1, Common.locatedValue s2) of
    (Go.GoAssignment var value, Go.GoReturn (Just (Common.Located _ (Go.GoIdentifier returnVar))))
      | var == returnVar -> do
        recordOptimization "Peephole: Eliminated Go temporary variable before return"
        modify $ \s -> s { osChanged = True }
        let newReturn = Common.Located (Common.locatedSpan s2) $ Go.GoReturn (Just value)
        peepholeOptimizeGoStmts (newReturn : rest)
    _ -> do
      newS1 <- peepholeGoStmt s1
      restOptimized <- peepholeOptimizeGoStmts (s2:rest)
      return (newS1 : restOptimized)

peepholeGoExpr :: Common.Located Go.GoExpr -> OptimizationM (Common.Located Go.GoExpr)
peepholeGoExpr (Common.Located span expr) = do
  newExpr <- case expr of
    Go.GoUnaryOp Go.GoOpNot (Common.Located _ (Go.GoBinaryOp Go.GoOpEq left right)) -> do
      recordOptimization "Peephole: Converted Go !(x == y) to x != y"
      modify $ \s -> s { osChanged = True }
      return $ Go.GoBinaryOp Go.GoOpNe left right
    
    Go.GoUnaryOp Go.GoOpNot (Common.Located _ (Go.GoBinaryOp Go.GoOpNe left right)) -> do
      recordOptimization "Peephole: Converted Go !(x != y) to x == y"
      modify $ \s -> s { osChanged = True }
      return $ Go.GoBinaryOp Go.GoOpEq left right
    
    Go.GoBinaryOp op left right -> do
      newLeft <- peepholeGoExpr left
      newRight <- peepholeGoExpr right
      return $ Go.GoBinaryOp op newLeft newRight
    
    Go.GoUnaryOp op operand -> do
      newOperand <- peepholeGoExpr operand
      return $ Go.GoUnaryOp op newOperand
    
    Go.GoCall func args -> do
      newFunc <- peepholeGoExpr func
      newArgs <- mapM peepholeGoExpr args
      return $ Go.GoCall newFunc newArgs
    
    _ -> return expr
  return $ Located span newExpr

-- ============================================================================
-- PUBLIC INTERFACES
-- ============================================================================

recordOptimization :: Text -> OptimizationM ()
recordOptimization opt = modify $ \s -> s { osOptimizations = opt : osOptimizations s }

constantFolding :: Either Python.PythonAST Go.GoAST -> OptimizationResult
constantFolding = runBasicOptimizations $ defaultConfig { 
    bpcEnableDeadCodeElimination = False,
    bpcEnableConstantPropagation = False,
    bpcEnableAlgebraicSimplification = False,
    bpcEnablePeepholeOptimization = False,
    bpcEnableCSE = False,
    bpcEnableStrengthReduction = False
  }

deadCodeElimination :: Either Python.PythonAST Go.GoAST -> OptimizationResult
deadCodeElimination = runBasicOptimizations $ defaultConfig {
    bpcEnableConstantFolding = False,
    bpcEnableConstantPropagation = False,
    bpcEnableAlgebraicSimplification = False,
    bpcEnablePeepholeOptimization = False,
    bpcEnableCSE = False,
    bpcEnableStrengthReduction = False
  }

constantPropagation :: Either Python.PythonAST Go.GoAST -> OptimizationResult
constantPropagation = runBasicOptimizations $ defaultConfig {
    bpcEnableConstantFolding = False,
    bpcEnableDeadCodeElimination = False,
    bpcEnableAlgebraicSimplification = False,
    bpcEnablePeepholeOptimization = False,
    bpcEnableCSE = False,
    bpcEnableStrengthReduction = False
  }

algebraicSimplification :: Either Python.PythonAST Go.GoAST -> OptimizationResult
algebraicSimplification = runBasicOptimizations $ defaultConfig {
    bpcEnableConstantFolding = False,
    bpcEnableDeadCodeElimination = False,
    bpcEnableConstantPropagation = False,
    bpcEnablePeepholeOptimization = False,
    bpcEnableCSE = False,
    bpcEnableStrengthReduction = False
  }

peepholeOptimization :: Either Python.PythonAST Go.GoAST -> OptimizationResult
peepholeOptimization = runBasicOptimizations $ defaultConfig {
    bpcEnableConstantFolding = False,
    bpcEnableDeadCodeElimination = False,
    bpcEnableConstantPropagation = False,
    bpcEnableAlgebraicSimplification = False,
    bpcEnableCSE = False,
    bpcEnableStrengthReduction = False
  }

commonSubexpressionElimination :: Either Python.PythonAST Go.GoAST -> OptimizationResult
commonSubexpressionElimination = runBasicOptimizations $ defaultConfig {
    bpcEnableConstantFolding = False,
    bpcEnableDeadCodeElimination = False,
    bpcEnableConstantPropagation = False,
    bpcEnableAlgebraicSimplification = False,
    bpcEnablePeepholeOptimization = False,
    bpcEnableStrengthReduction = False
  }

strengthReduction :: Either Python.PythonAST Go.GoAST -> OptimizationResult
strengthReduction = runBasicOptimizations $ defaultConfig {
    bpcEnableConstantFolding = False,
    bpcEnableDeadCodeElimination = False,
    bpcEnableConstantPropagation = False,
    bpcEnableAlgebraicSimplification = False,
    bpcEnablePeepholeOptimization = False,
    bpcEnableCSE = False
  }