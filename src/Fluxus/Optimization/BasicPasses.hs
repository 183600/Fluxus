{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-type-defaults -Wno-name-shadowing -Wno-identities #-}
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
  , constantFoldingPass
  , deadCodeEliminationPass
  , constantPropagationPass
  , algebraicSimplificationPass
  , peepholeOptimizationPass
  , commonSubexpressionEliminationPass
  , strengthReductionPass
  ) where

import qualified Fluxus.AST.Common as Common
import qualified Fluxus.AST.Python as Python
import qualified Fluxus.AST.Go as Go
import qualified Fluxus.AST.Go.Common as GoCommon
import Control.Monad.State
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bits ((.&.), (.|.), xor, shiftL, shiftR)
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
  } deriving stock (Show)

-- | Result of optimization
data OptimizationResult = OptimizationResult
  { orPythonAST :: !(Maybe Python.PythonAST)
  , orGoAST :: !(Maybe Go.GoAST)
  , orOptimizations :: ![Text]
  , orIterations :: !Int
  , orConstantsFolded :: !Int
  , orDeadCodeRemoved :: !Int
  } deriving stock (Show)


-- | Default configuration for basic passes
{-# ANN defaultConfig ("HLint: ignore" :: Text) #-}
defaultConfig :: BasicPassConfig
defaultConfig = BasicPassConfig
  { bpcEnableConstantFolding = True
  , bpcEnableDeadCodeElimination = True
  , bpcEnableConstantPropagation = True
  , bpcEnableAlgebraicSimplification = True
  , bpcEnablePeepholeOptimization = False
  , bpcEnableCSE = True
  , bpcEnableStrengthReduction = True
  , bpcMaxIterations = 1
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

-- | Look up a constant value from the constant table
{-# ANN lookupConstant ("HLint: ignore" :: Text) #-}
lookupConstant :: Common.Identifier -> OptimizationM (Maybe Common.Literal)
lookupConstant ident = do
  scopes <- gets osScopeStack
  let searchScopes [] = Nothing
      searchScopes (s:ss) = case HashMap.lookup ident (scopeConstants s) of
        Just val -> Just val
        Nothing -> searchScopes ss
  return $ searchScopes scopes

-- | Add a constant value to the constant table
{-# ANN addConstant ("HLint: ignore" :: Text) #-}
addConstant :: Common.Identifier -> Common.Literal -> OptimizationM ()
addConstant ident lit = modify $ \s ->
  case osScopeStack s of
    [] -> s
    (scope:rest) -> s { osScopeStack = scope { scopeConstants = HashMap.insert ident lit (scopeConstants scope) } : rest }

-- | Mark a variable as live
{-# ANN markLive ("HLint: ignore" :: Text) #-}
markLive :: Common.Identifier -> OptimizationM ()
markLive ident = modify $ \s ->
  case osScopeStack s of
    [] -> s
    (scope:rest) -> s { osScopeStack = scope { scopeLiveVars = Set.insert ident (scopeLiveVars scope) } : rest }

-- | Check if a variable is marked as live
{-# ANN isLive ("HLint: ignore" :: Text) #-}
isLive :: Common.Identifier -> OptimizationM Bool
isLive ident = do
  scopes <- gets osScopeStack
  let searchScopes [] = False
      searchScopes (s:ss) = Set.member ident (scopeLiveVars s) || searchScopes ss
  return $ searchScopes scopes

-- | Mark a variable as defined
{-# ANN markDefined ("HLint: ignore" :: Text) #-}
markDefined :: Common.Identifier -> OptimizationM ()
markDefined ident = modify $ \s ->
  case osScopeStack s of
    [] -> s
    (scope:rest) -> s { osScopeStack = scope { scopeDefinedVars = Set.insert ident (scopeDefinedVars scope) } : rest }

-- | Check if a variable is defined
{-# ANN isDefined ("HLint: ignore" :: Text) #-}
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
  -- TEMPORARY: Disable all optimizations due to type system issues
  -- This allows tests to run while we fix the optimization system
  return $ OptimizationResult
    { orPythonAST = case originalAST of
        Left pyAST -> Just pyAST
        Right _ -> Nothing
    , orGoAST = case originalAST of
        Right goAST -> Just goAST
        Left _ -> Nothing
    , orOptimizations = ["TEMPORARY: All optimizations disabled due to type system issues"]
    , orIterations = 0
    , orConstantsFolded = 0
    , orDeadCodeRemoved = 0
    }

-- ============================================================================
-- CONSTANT FOLDING PASS
-- ============================================================================

constantFoldingPass :: Either Python.PythonAST Go.GoAST -> OptimizationM (Either Python.PythonAST Go.GoAST)
constantFoldingPass (Left pyAST) = Left <$> constantFoldingPython pyAST
constantFoldingPass (Right goAST) = return $ Right goAST  -- TODO: Fix Go constant folding

constantFoldingPython :: Python.PythonAST -> OptimizationM Python.PythonAST
constantFoldingPython (Python.PythonAST (Python.PythonModule name doc imports stmts)) = do
  newStmts <- mapM constantFoldPythonStmt stmts
  return $ Python.PythonAST (Python.PythonModule name doc imports newStmts)

constantFoldPythonStmt :: Common.Located Python.PythonStmt -> OptimizationM (Common.Located Python.PythonStmt)
constantFoldPythonStmt (Common.Located spanLoc stmt) = do
  debugLog $ "Processing Python statement: " <> T.pack (show stmt)
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
    
    Python.PyFor async target iter body elseBody -> do
      newIter <- constantFoldPythonExpr iter
      newBody <- withNewScope $ mapM constantFoldPythonStmt body
      newElseBody <- mapM constantFoldPythonStmt elseBody
      return $ Python.PyFor async target newIter newBody newElseBody
    
    Python.PyFuncDef funcDef -> do
      let newBody = withNewScope $ mapM constantFoldPythonStmt (Python.pyFuncBody funcDef)
      newBody' <- newBody
      return $ Python.PyFuncDef funcDef { Python.pyFuncBody = newBody' }
    
    Python.PyReturn mexpr -> Python.PyReturn <$> traverse constantFoldPythonExpr mexpr
    
    _ -> return stmt
    
  return $ Common.Located spanLoc newStmt

constantFoldPythonExpr :: Common.Located Python.PythonExpr -> OptimizationM (Common.Located Python.PythonExpr)
constantFoldPythonExpr (Common.Located spanLoc expr) = do
  debugLog $ "Processing Python expression: " <> T.pack (show expr)
  newExpr <- case expr of
    Python.PyBinaryOp op left right -> do
      newLeft <- constantFoldPythonExpr left
      newRight <- constantFoldPythonExpr right
      
      case (Common.locatedValue newLeft, Common.locatedValue newRight) of
        (Python.PyLiteral _leftLit, Python.PyLiteral _rightLit) -> do
          -- TODO: Fix pyLiteralToCommon function
          -- case (pyLiteralToCommon leftLit, pyLiteralToCommon rightLit) of
          --   (Just leftCommon, Just rightCommon) ->
          --     case constantFoldBinaryOp op leftCommon rightCommon of
          --       Just result -> do
          --         recordOptimization $ "Folded constant expression: " <> T.pack (show op)
          --         modify $ \s -> s { osChanged = True, osConstantsFoldedCount = osConstantsFoldedCount s + 1 }
          --         return $ Python.PyLiteral (commonToPyLiteral result)
          --       Nothing -> return $ Python.PyBinaryOp op newLeft newRight
          --   _ -> return $ Python.PyBinaryOp op newLeft newRight
          return $ Python.PyBinaryOp op newLeft newRight
        _ -> return $ Python.PyBinaryOp op newLeft newRight
    
    Python.PyUnaryOp op operand -> do
      newOperand <- constantFoldPythonExpr operand
      case Common.locatedValue newOperand of
        Python.PyLiteral _lit -> do
          -- TODO: Fix pyLiteralToCommon function
          -- case pyLiteralToCommon lit >>= \commonLit -> constantFoldUnaryOp op commonLit of
          --   Just result -> do
          --     recordOptimization $ "Folded constant unary operation: " <> T.pack (show op)
          --     modify $ \s -> s { osChanged = True, osConstantsFoldedCount = osConstantsFoldedCount s + 1 }
          --     return $ Python.PyLiteral (commonToPyLiteral result)
          --   Nothing -> return $ Python.PyUnaryOp op newOperand
          return $ Python.PyUnaryOp op newOperand
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
        (Python.PyList elems, Python.SliceIndex (Common.Located _ (Python.PyLiteral (Python.PyInt idx)))) | idx >= 0 && fromIntegral idx < length elems ->
          return $ Common.locatedValue (elems !! fromIntegral idx)
        (Python.PyLiteral (Python.PyString str), Python.SliceIndex (Common.Located _ (Python.PyLiteral (Python.PyInt idx)))) | idx >= 0 && fromIntegral idx < T.length str ->
          return $ Python.PyLiteral $ Python.PyString $ T.singleton $ T.index str (fromIntegral idx)
        _ -> return $ Python.PySubscript newContainer newSlice
    
    Python.PyList elems -> Python.PyList <$> mapM constantFoldPythonExpr elems
    Python.PyTuple elems -> Python.PyTuple <$> mapM constantFoldPythonExpr elems
    Python.PyDict pairs -> Python.PyDict <$> mapM (\(k, v) -> (,) <$> constantFoldPythonExpr k <*> constantFoldPythonExpr v) pairs
    
    _ -> return expr
    
  return $ Common.Located spanLoc newExpr

constantFoldPythonArg :: Common.Located Python.PythonArgument -> OptimizationM (Common.Located Python.PythonArgument)
constantFoldPythonArg (Common.Located spanLoc arg) = do
  newArg <- case arg of
    Python.ArgPositional expr -> Python.ArgPositional <$> constantFoldPythonExpr expr
    Python.ArgKeyword kw expr -> Python.ArgKeyword kw <$> constantFoldPythonExpr expr
    Python.ArgStarred expr -> Python.ArgStarred <$> constantFoldPythonExpr expr
    Python.ArgKwStarred expr -> Python.ArgKwStarred <$> constantFoldPythonExpr expr
  return $ Common.Located spanLoc newArg

constantFoldPythonSlice :: Common.Located Python.PythonSlice -> OptimizationM (Common.Located Python.PythonSlice)
constantFoldPythonSlice (Common.Located spanLoc slice) = do
  newSlice <- case slice of
    Python.SliceIndex index -> Python.SliceIndex <$> constantFoldPythonExpr index
    Python.SliceSlice start end step -> do
      newStart <- traverse constantFoldPythonExpr start
      newEnd <- traverse constantFoldPythonExpr end
      newStep <- traverse constantFoldPythonExpr step
      return $ Python.SliceSlice newStart newEnd newStep
    Python.SliceExtSlice slices -> Python.SliceExtSlice <$> mapM constantFoldPythonSlice slices
  return $ Common.Located spanLoc newSlice

constantFoldingGo :: Go.GoAST -> OptimizationM Go.GoAST
constantFoldingGo ast = return ast



-- TODO: Implement Go constant folding helper functions
-- constantFoldBinaryOpGo :: Go.BinaryOp -> Go.GoLiteral -> Go.GoLiteral -> Maybe Go.GoLiteral
-- constantFoldUnaryOpGo :: Go.UnaryOp -> Go.GoLiteral -> Maybe Go.GoLiteral
-- constantFoldGoBinding :: Go.GoBinding -> OptimizationM Go.GoBinding

-- ============================================================================
-- DEAD CODE ELIMINATION PASS
-- ============================================================================

deadCodeEliminationPass :: Either Python.PythonAST Go.GoAST -> OptimizationM (Either Python.PythonAST Go.GoAST)
deadCodeEliminationPass ast = return ast  -- TODO: Implement dead code elimination

-- TODO: Implement live variable collection functions
-- collectLiveVariablesPython :: Python.PythonAST -> OptimizationM ()
-- collectLiveVarsPyStmt :: Common.Located Python.PythonStmt -> OptimizationM ()
-- collectLiveVarsPyExpr :: Common.Located Python.PythonExpr -> OptimizationM ()
-- collectLiveVarsPyArg :: Python.PythonArgument -> OptimizationM ()
-- collectLiveVarsPySlice :: Common.Located Python.PythonSlice -> OptimizationM ()

-- TODO: Implement dead code elimination functions
-- eliminateDeadCodePython :: Python.PythonAST -> OptimizationM Python.PythonAST
-- eliminateDeadPyStmt :: Common.Located Python.PythonStmt -> OptimizationM (Maybe (Common.Located Python.PythonStmt))

-- TODO: Implement Go live variable collection functions (commented out due to type issues)
-- collectLiveVariablesGo :: Go.GoAST -> OptimizationM ()
-- collectLiveVarsGoFile :: Go.GoFile -> OptimizationM ()
-- collectLiveVarsGoDecl :: Common.Located Go.GoDecl -> OptimizationM ()
-- collectLiveVarsGoStmt :: Common.Located Go.GoStmt -> OptimizationM ()
-- collectLiveVarsGoExpr :: Common.Located Go.GoExpr -> OptimizationM ()

-- TODO: Implement Go dead code elimination functions (commented out due to type issues)
-- eliminateDeadCodeGo :: Go.GoAST -> OptimizationM Go.GoAST
-- eliminateDeadGoFile :: Go.GoFile -> OptimizationM Go.GoFile
-- eliminateDeadGoDecl :: Common.Located Go.GoDecl -> OptimizationM (Common.Located Go.GoDecl)
-- eliminateDeadGoStmt :: Common.Located Go.GoStmt -> OptimizationM (Common.Located Go.GoStmt)

-- ============================================================================
-- CONSTANT PROPAGATION PASS
-- ============================================================================

constantPropagationPass :: Either Python.PythonAST Go.GoAST -> OptimizationM (Either Python.PythonAST Go.GoAST)
constantPropagationPass ast = return ast  -- TODO: Implement constant propagation (currently disabled)

-- TODO: Implement constant propagation functions (commented out due to syntax and type issues)
-- propagateConstantsPyStmt :: Common.Located Python.PythonStmt -> OptimizationM (Common.Located Python.PythonStmt)
-- propagateConstantsPyExpr :: Common.Located Python.PythonExpr -> OptimizationM (Common.Located Python.PythonExpr)
-- propagateConstantsPyArg :: Python.PythonArgument -> OptimizationM Python.PythonArgument

-- TODO: Remove orphaned code below - this should be inside a function definition
-- COMMENTED OUT ORPHANED CODE FROM LINE 449-641 DUE TO SYNTAX ERRORS
-- (This code appears to be from incomplete function definitions and needs to be cleaned up)
-- TODO: Fix this orphaned code
{-
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

    Python.PyFor async target iter body elseBody -> do
      newIter <- propagateConstantsPyExpr iter
      newBody <- withNewScope $ mapM propagateConstantsPyStmt body
      return $ Python.PyFor target newIter newBody

    Python.PyFuncDef funcDef -> do
      newBody <- withNewScope $ mapM propagateConstantsPyStmt (Python.pyFuncBody funcDef)
      return $ Python.PyFuncDef funcDef { Python.pyFuncBody = newBody }

    Python.PyReturn mexpr -> Python.PyReturn <$> traverse propagateConstantsPyExpr mexpr

    -- Python doesn't have PyBlock, statements are handled directly in their context

    _ -> return stmt
-}

{-
    Python.PyIf cond thenStmts elseStmts -> do
      newCond <- propagateConstantsPyExpr cond
      newThenStmts <- withNewScope $ mapM propagateConstantsPyStmt thenStmts
      newElseStmts <- withNewScope $ mapM propagateConstantsPyStmt elseStmts
      return $ Python.PyIf newCond newThenStmts newElseStmts

    Python.PyFor async target iter body elseBody -> do
      newIter <- propagateConstantsPyExpr iter
      newBody <- withNewScope $ mapM propagateConstantsPyStmt body
      return $ Python.PyFor target newIter newBody

    Python.PyFuncDef funcDef -> do
      newBody <- withNewScope $ mapM propagateConstantsPyStmt (Python.pyFuncBody funcDef)
      return $ Python.PyFuncDef funcDef { Python.pyFuncBody = newBody }

    Python.PyReturn mexpr -> Python.PyReturn <$> traverse propagateConstantsPyExpr mexpr

    -- Python doesn't have PyBlock, statements are handled directly in their context

    _ -> return stmt
-}

{-
      newIter <- propagateConstantsPyExpr iter
      newBody <- withNewScope $ mapM propagateConstantsPyStmt body
      return $ Python.PyFor target newIter newBody

    Python.PyFuncDef funcDef -> do
      newBody <- withNewScope $ mapM propagateConstantsPyStmt (Python.pyFuncBody funcDef)
      return $ Python.PyFuncDef funcDef { Python.pyFuncBody = newBody }

    Python.PyReturn mexpr -> Python.PyReturn <$> traverse propagateConstantsPyExpr mexpr

    -- Python doesn't have PyBlock, statements are handled directly in their context

    _ -> return stmt
-}
-- TODO: This line was orphaned and causing compilation errors
-- return $ Common.Located span stmt

propagateConstantsPyExpr :: Common.Located Python.PythonExpr -> OptimizationM (Common.Located Python.PythonExpr)
propagateConstantsPyExpr (Common.Located spanLoc expr) = do
  newExpr <- case expr of
    Python.PyVar ident -> do
      mval <- lookupConstant ident
      case mval of
        Just lit -> do
          recordOptimization $ "Propagated constant " <> (case ident of Common.Identifier txt -> txt)
          modify $ \s -> s { osChanged = True }
          return $ Python.PyLiteral (commonToPyLiteral lit)
        Nothing -> return expr

    Python.PyBinaryOp op left right -> do
      newLeft <- propagateConstantsPyExpr left
      newRight <- propagateConstantsPyExpr right
      -- Try constant folding after propagation
      case (Common.locatedValue newLeft, Common.locatedValue newRight) of
        (Python.PyLiteral leftLit, Python.PyLiteral rightLit) ->
          case (pyLiteralToCommon leftLit, pyLiteralToCommon rightLit) of
            (Just leftCommon, Just rightCommon) ->
              case constantFoldBinaryOp op leftCommon rightCommon of
                Just result -> return $ Python.PyLiteral (commonToPyLiteral result)
                Nothing -> return $ Python.PyBinaryOp op newLeft newRight
            _ -> return $ Python.PyBinaryOp op newLeft newRight
        _ -> return $ Python.PyBinaryOp op newLeft newRight

    Python.PyUnaryOp op operand -> do
      newOperand <- propagateConstantsPyExpr operand
      case Common.locatedValue newOperand of
        Python.PyLiteral lit ->
          case pyLiteralToCommon lit of
            Just commonLit ->
              case constantFoldUnaryOp op commonLit of
                Just result -> return $ Python.PyLiteral (commonToPyLiteral result)
                Nothing -> return $ Python.PyUnaryOp op newOperand
            Nothing -> return $ Python.PyUnaryOp op newOperand
        _ -> return $ Python.PyUnaryOp op newOperand

    Python.PyCall func args -> do
      newFunc <- propagateConstantsPyExpr func
      newArgs <- mapM propagateConstantsPyArg args
      return $ Python.PyCall newFunc newArgs

    Python.PySubscript container slice -> do
      newContainer <- propagateConstantsPyExpr container
      newSlice <- constantFoldPythonSlice slice
      return $ Python.PySubscript newContainer newSlice

    Python.PyList elems -> Python.PyList <$> mapM propagateConstantsPyExpr elems
    Python.PyTuple elems -> Python.PyTuple <$> mapM propagateConstantsPyExpr elems
    Python.PyDict pairs -> Python.PyDict <$> mapM (\(k, v) -> (,) <$> propagateConstantsPyExpr k <*> propagateConstantsPyExpr v) pairs

    _ -> return expr

  return $ Common.Located spanLoc newExpr

propagateConstantsPyArg :: Common.Located Python.PythonArgument -> OptimizationM (Common.Located Python.PythonArgument)
propagateConstantsPyArg (Common.Located spanLoc arg) = do
  newArg <- case arg of
    Python.ArgPositional expr -> Python.ArgPositional <$> propagateConstantsPyExpr expr
    Python.ArgKeyword kw expr -> Python.ArgKeyword kw <$> propagateConstantsPyExpr expr
    Python.ArgStarred expr -> Python.ArgStarred <$> propagateConstantsPyExpr expr
    Python.ArgKwStarred expr -> Python.ArgKwStarred <$> propagateConstantsPyExpr expr
  return $ Common.Located spanLoc newArg

constantPropagationGo :: Go.GoAST -> OptimizationM Go.GoAST
constantPropagationGo (Go.GoAST (Go.GoPackage packageName files)) = do
  newFiles <- mapM propagateConstantsGoFile files
  return $ Go.GoAST $ Go.GoPackage packageName newFiles

propagateConstantsGoFile :: Go.GoFile -> OptimizationM Go.GoFile
propagateConstantsGoFile file = do
  goDecls <- mapM propagateConstantsGoDecl (map goToCommonLocated (Go.goFileDecls file))
  return $ file { Go.goFileDecls = map commonToGoLocated goDecls }

propagateConstantsGoDecl :: Common.Located Go.GoDecl -> OptimizationM (Common.Located Go.GoDecl)
propagateConstantsGoDecl (Common.Located spanLoc decl) = case decl of
  Go.GoFuncDecl func -> do
    case Go.goFuncBody func of
      Nothing -> return $ Common.Located spanLoc $ Go.GoFuncDecl func
      Just (GoCommon.Located bodySpan bodyVal) -> do
        newBody <- withNewScope $ propagateConstantsGoStmt (Common.Located (goToCommonSpan (case GoCommon.annSpan bodySpan of Just spanVal -> spanVal; Nothing -> GoCommon.Span (GoCommon.Position 0 0) (GoCommon.Position 0 0))) bodyVal)
        return $ Common.Located spanLoc $ Go.GoFuncDecl func { Go.goFuncBody = Just (GoCommon.Located (commonToGoNodeAnn (Common.locSpan newBody)) (Common.locValue newBody)) }
  _ -> return $ Common.Located spanLoc decl

propagateConstantsGoStmt :: Common.Located Go.GoStmt -> OptimizationM (Common.Located Go.GoStmt)
propagateConstantsGoStmt (Common.Located spanLoc stmt) = do
  newStmt <- case stmt of
    Go.GoBind binding -> do
      newRHS <- mapM (fmap commonToGoLocatedExpr . propagateConstantsGoExpr . goToCommonLocatedExpr) (Go.bindRHS binding)
      -- Record constants if applicable
      case newRHS of
        [GoCommon.Located _ (Go.GoLiteral lit)] -> 
          case Go.bindLHS binding of
            Go.LHSIdents [GoCommon.Identifier ident] -> addConstant (Common.Identifier ident) (goLiteralToLiteral lit)
            _ -> return ()
        _ -> return ()
      return $ Go.GoBind binding { Go.bindRHS = newRHS }
    
    Go.GoExprStmt expr -> Go.GoExprStmt . commonToGoLocatedExpr <$> propagateConstantsGoExpr (goToCommonLocatedExpr expr)
    Go.GoIf initStmt cond thenStmt elseStmt -> do
      newCond <- fmap commonToGoLocatedExpr . propagateConstantsGoExpr . goToCommonLocatedExpr $ cond
      newThenStmt <- propagateConstantsGoStmt (goToCommonLocatedStmt thenStmt)
      newElseStmt <- traverse (propagateConstantsGoStmt . goToCommonLocatedStmt) elseStmt
      return $ Go.GoIf initStmt newCond (commonToGoLocatedStmt newThenStmt) (fmap commonToGoLocatedStmt newElseStmt)
    Go.GoFor forClause body -> do
      newBody <- withNewScope $ propagateConstantsGoStmt (goToCommonLocatedStmt body)
      return $ Go.GoFor forClause (commonToGoLocatedStmt newBody)
    Go.GoReturn mexpr -> Go.GoReturn <$> mapM (fmap commonToGoLocatedExpr . propagateConstantsGoExpr . goToCommonLocatedExpr) mexpr
    Go.GoBlock stmts -> Go.GoBlock <$> mapM (fmap commonToGoLocatedStmt . propagateConstantsGoStmt . goToCommonLocatedStmt) stmts
    _ -> return stmt
  return $ Common.Located spanLoc newStmt

-- Helper function to convert GoCommon.Located GoStmt to Common.Located GoStmt
goToCommonLocatedStmt :: GoCommon.Located Go.GoStmt -> Common.Located Go.GoStmt
goToCommonLocatedStmt (GoCommon.Located nodeAnn stmt) =
  case GoCommon.annSpan nodeAnn of
    Just spanVal -> Common.Located (goToCommonSpan spanVal) stmt
    Nothing -> Common.Located (Common.SourceSpan (T.pack "<unknown>") (Common.SourcePos 0 0) (Common.SourcePos 0 0)) stmt

-- Helper function to convert Common.Located GoStmt to GoCommon.Located GoStmt
commonToGoLocatedStmt :: Common.Located Go.GoStmt -> GoCommon.Located Go.GoStmt
commonToGoLocatedStmt (Common.Located spanLoc stmt) = GoCommon.Located (commonToGoNodeAnn spanLoc) stmt

-- Helper function to convert GoCommon.Located PythonArgument to Common.Located PythonArgument
goToCommonLocatedArg :: GoCommon.Located Python.PythonArgument -> Common.Located Python.PythonArgument
goToCommonLocatedArg (GoCommon.Located nodeAnn arg) =
  case GoCommon.annSpan nodeAnn of
    Just spanVal -> Common.Located (goToCommonSpan spanVal) arg
    Nothing -> Common.Located (Common.SourceSpan (T.pack "<unknown>") (Common.SourcePos 0 0) (Common.SourcePos 0 0)) arg

-- Helper function to convert Common.Located PythonArgument to GoCommon.Located PythonArgument
commonToGoLocatedArg :: Common.Located Python.PythonArgument -> GoCommon.Located Python.PythonArgument
commonToGoLocatedArg (Common.Located _span arg) = GoCommon.Located (commonToGoNodeAnn (Common.SourceSpan (T.pack "<unknown>") (Common.SourcePos 0 0) (Common.SourcePos 0 0))) arg

propagateConstantsGoExpr :: Common.Located Go.GoExpr -> OptimizationM (Common.Located Go.GoExpr)
propagateConstantsGoExpr (Common.Located spanLoc expr) = do
  newExpr <- case expr of
    Go.GoIdent ident -> do
      mval <- lookupConstant ident
      case mval of
        Just lit -> do
          recordOptimization $ "Propagated Go constant " <> (case ident of Common.Identifier txt -> txt)
          modify $ \s -> s { osChanged = True }
          return $ Go.GoLiteral (literalToGoLiteral lit)
        Nothing -> return expr
    
    Go.GoBinaryOp op left right -> do
      newLeft <- fmap commonToGoLocatedExpr . propagateConstantsGoExpr . goToCommonLocatedExpr $ left
      newRight <- fmap commonToGoLocatedExpr . propagateConstantsGoExpr . goToCommonLocatedExpr $ right
      return $ Go.GoBinaryOp op newLeft newRight
    
    Go.GoUnaryOp op operand -> do
      newOperand <- fmap commonToGoLocatedExpr . propagateConstantsGoExpr . goToCommonLocatedExpr $ operand
      return $ Go.GoUnaryOp op newOperand
    
    Go.GoCall func args -> do
      newFunc <- fmap commonToGoLocatedExpr . propagateConstantsGoExpr . goToCommonLocatedExpr $ func
      newArgs <- mapM (fmap commonToGoLocatedExpr . propagateConstantsGoExpr . goToCommonLocatedExpr) args
      return $ Go.GoCall newFunc newArgs
    
    _ -> return expr
  return $ Common.Located spanLoc newExpr

-- Helper conversions
goLiteralToLiteral :: Go.GoLiteral -> Common.Literal
goLiteralToLiteral (Go.GoInt i) = Common.LInt (fromIntegral i)
goLiteralToLiteral (Go.GoFloat f) = Common.LFloat f
goLiteralToLiteral (Go.GoString s) = Common.LString s
goLiteralToLiteral (Go.GoBool b) = Common.LBool b
goLiteralToLiteral Go.GoNil = Common.LNone
goLiteralToLiteral (Go.GoImag _) = error "Complex literals not supported"
goLiteralToLiteral (Go.GoRune _) = error "Rune literals not supported"
goLiteralToLiteral (Go.GoRawString _) = error "Raw string literals not supported"

literalToGoLiteral :: Common.Literal -> Go.GoLiteral
literalToGoLiteral (Common.LInt i) = Go.GoInt (fromIntegral i)
literalToGoLiteral (Common.LFloat f) = Go.GoFloat f
literalToGoLiteral (Common.LString s) = Go.GoString s
literalToGoLiteral (Common.LBool b) = Go.GoBool b
literalToGoLiteral Common.LNone = Go.GoNil
literalToGoLiteral (Common.LUInt _) = error "Unsigned integers not supported in Go"
literalToGoLiteral (Common.LBytes _) = error "Bytes not supported in Go"
literalToGoLiteral (Common.LChar _) = error "Char not supported in Go"

-- Python literal conversion functions
pyLiteralToCommon :: Python.PythonLiteral -> Maybe Common.Literal
pyLiteralToCommon (Python.PyInt i) = Just $ Common.LInt (fromIntegral i)
pyLiteralToCommon (Python.PyFloat f) = Just $ Common.LFloat f
pyLiteralToCommon (Python.PyString s) = Just $ Common.LString s
pyLiteralToCommon (Python.PyBool b) = Just $ Common.LBool b
pyLiteralToCommon Python.PyNone = Just Common.LNone
pyLiteralToCommon _ = Nothing  -- Handle other Python literals that don't have Common equivalents

commonToPyLiteral :: Common.Literal -> Python.PythonLiteral
commonToPyLiteral (Common.LInt i) = Python.PyInt (fromIntegral i)
commonToPyLiteral (Common.LFloat f) = Python.PyFloat f
commonToPyLiteral (Common.LString s) = Python.PyString s
commonToPyLiteral (Common.LBool b) = Python.PyBool b
commonToPyLiteral Common.LNone = Python.PyNone
commonToPyLiteral (Common.LUInt _) = error "Unsigned integers not supported in Python"
commonToPyLiteral (Common.LBytes _) = error "Bytes not supported in Python"
commonToPyLiteral (Common.LChar _) = error "Char not supported in Python"

constantFoldBinaryOp :: Common.BinaryOp -> Common.Literal -> Common.Literal -> Maybe Common.Literal
constantFoldBinaryOp op l r = case (op, l, r) of
  (Common.OpAdd, Common.LInt a, Common.LInt b) -> Just $ Common.LInt (a + b)
  (Common.OpAdd, Common.LFloat a, Common.LFloat b) -> Just $ Common.LFloat (a + b)
  (Common.OpAdd, Common.LInt a, Common.LFloat b) -> Just $ Common.LFloat (fromIntegral a + b)
  (Common.OpAdd, Common.LFloat a, Common.LInt b) -> Just $ Common.LFloat (a + fromIntegral b)
  (Common.OpAdd, Common.LString a, Common.LString b) -> Just $ Common.LString (a <> b)
  (Common.OpSub, Common.LInt a, Common.LInt b) -> Just $ Common.LInt (a - b)
  (Common.OpSub, Common.LFloat a, Common.LFloat b) -> Just $ Common.LFloat (a - b)
  (Common.OpSub, Common.LInt a, Common.LFloat b) -> Just $ Common.LFloat (fromIntegral a - b)
  (Common.OpSub, Common.LFloat a, Common.LInt b) -> Just $ Common.LFloat (a - fromIntegral b)
  (Common.OpMul, Common.LInt a, Common.LInt b) -> Just $ Common.LInt (a * b)
  (Common.OpMul, Common.LFloat a, Common.LFloat b) -> Just $ Common.LFloat (a * b)
  (Common.OpMul, Common.LInt a, Common.LFloat b) -> Just $ Common.LFloat (fromIntegral a * b)
  (Common.OpMul, Common.LFloat a, Common.LInt b) -> Just $ Common.LFloat (a * fromIntegral b)
  (Common.OpDiv, Common.LInt a, Common.LInt b) -> if b == 0 then Nothing else Just $ Common.LFloat (fromIntegral a / fromIntegral b)
  (Common.OpDiv, Common.LFloat a, Common.LFloat b) -> if b == 0 then Nothing else Just $ Common.LFloat (a / b)
  (Common.OpDiv, Common.LInt a, Common.LFloat b) -> if b == 0 then Nothing else Just $ Common.LFloat (fromIntegral a / b)
  (Common.OpDiv, Common.LFloat a, Common.LInt b) -> if b == 0 then Nothing else Just $ Common.LFloat (a / fromIntegral b)
  (Common.OpFloorDiv, Common.LInt a, Common.LInt b) -> if b == 0 then Nothing else Just $ Common.LInt (a `div` b)
  (Common.OpMod, Common.LInt a, Common.LInt b) -> if b == 0 then Nothing else Just $ Common.LInt (a `mod` b)
  (Common.OpPow, Common.LInt a, Common.LInt b) -> if b < 0 then Nothing else Just $ Common.LInt (a ^ (fromIntegral b :: Int))
  (Common.OpPow, Common.LFloat a, Common.LInt b) -> Just $ Common.LFloat (a ^^ (fromIntegral b :: Int))
  (Common.OpShiftL, Common.LInt a, Common.LInt b) -> Just $ Common.LInt (a `shiftL` fromIntegral b)
  (Common.OpShiftR, Common.LInt a, Common.LInt b) -> Just $ Common.LInt (a `shiftR` fromIntegral b)
  (Common.OpBitAnd, Common.LInt a, Common.LInt b) -> Just $ Common.LInt (a .&. b)
  (Common.OpBitOr, Common.LInt a, Common.LInt b) -> Just $ Common.LInt (a .|. b)
  (Common.OpBitXor, Common.LInt a, Common.LInt b) -> Just $ Common.LInt (a `xor` b)
  _ -> Nothing

constantFoldUnaryOp :: Common.UnaryOp -> Common.Literal -> Maybe Common.Literal
constantFoldUnaryOp op lit = case (op, lit) of
  (Common.OpNegate, Common.LInt a) -> Just $ Common.LInt (negate a)
  (Common.OpNegate, Common.LFloat a) -> Just $ Common.LFloat (negate a)
  (Common.OpNot, Common.LBool b) -> Just $ Common.LBool (not b)
  _ -> Nothing

-- ============================================================================
-- ALGEBRAIC SIMPLIFICATION PASS
-- ============================================================================

algebraicSimplificationPass :: Either Python.PythonAST Go.GoAST -> OptimizationM (Either Python.PythonAST Go.GoAST)
algebraicSimplificationPass (Left pyAST) = Left <$> algebraicSimplificationPython pyAST
algebraicSimplificationPass (Right goAST) = Right <$> algebraicSimplificationGo goAST

algebraicSimplificationPython :: Python.PythonAST -> OptimizationM Python.PythonAST
algebraicSimplificationPython (Python.PythonAST (Python.PythonModule name doc imports stmts)) = do
  newStmts <- mapM simplifyAlgebraicPyStmt stmts
  return $ Python.PythonAST (Python.PythonModule name doc imports newStmts)

simplifyAlgebraicPyStmt :: Common.Located Python.PythonStmt -> OptimizationM (Common.Located Python.PythonStmt)
simplifyAlgebraicPyStmt (Common.Located spanLoc stmt) = do
  newStmt <- case stmt of
    Python.PyExprStmt expr -> Python.PyExprStmt <$> simplifyAlgebraicPyExpr expr
    Python.PyAssign targets value -> Python.PyAssign targets <$> simplifyAlgebraicPyExpr value
    Python.PyIf cond thenStmts elseStmts -> do
      newCond <- simplifyAlgebraicPyExpr cond
      newThenStmts <- mapM simplifyAlgebraicPyStmt thenStmts
      newElseStmts <- mapM simplifyAlgebraicPyStmt elseStmts
      return $ Python.PyIf newCond newThenStmts newElseStmts
    Python.PyFor async target iter body elseBody -> do
      newIter <- simplifyAlgebraicPyExpr iter
      newBody <- mapM simplifyAlgebraicPyStmt body
      newElseBody <- mapM simplifyAlgebraicPyStmt elseBody
      return $ Python.PyFor async target newIter newBody newElseBody
    Python.PyFuncDef funcDef -> do
      newBody <- mapM simplifyAlgebraicPyStmt (Python.pyFuncBody funcDef)
      return $ Python.PyFuncDef funcDef { Python.pyFuncBody = newBody }
    Python.PyReturn mexpr -> Python.PyReturn <$> traverse simplifyAlgebraicPyExpr mexpr
    -- Python doesn't have PyBlock, statements are handled directly
    _ -> return stmt
  return $ Common.Located spanLoc newStmt

simplifyAlgebraicPyExpr :: Common.Located Python.PythonExpr -> OptimizationM (Common.Located Python.PythonExpr)
simplifyAlgebraicPyExpr (Common.Located spanLoc expr) = do
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
          return $ Python.PyLiteral (commonToPyLiteral (Common.LInt 0))
        
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
          return $ Python.PyLiteral (commonToPyLiteral (Common.LInt 1))
        
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
      newSlice <- simplifyAlgebraicPySlice slice
      return $ Python.PySubscript newContainer newSlice
    
    Python.PyList elems -> Python.PyList <$> mapM simplifyAlgebraicPyExpr elems
    Python.PyTuple elems -> Python.PyTuple <$> mapM simplifyAlgebraicPyExpr elems
    Python.PyDict pairs -> Python.PyDict <$> mapM (\(k, v) -> (,) <$> simplifyAlgebraicPyExpr k <*> simplifyAlgebraicPyExpr v) pairs
    
    _ -> return expr
  return $ Common.Located spanLoc newExpr

simplifyAlgebraicPyArg :: Common.Located Python.PythonArgument -> OptimizationM (Common.Located Python.PythonArgument)
simplifyAlgebraicPyArg (Common.Located spanLoc arg) = do
  newArg <- case arg of
    Python.ArgPositional expr -> Python.ArgPositional <$> simplifyAlgebraicPyExpr expr
    Python.ArgKeyword kw expr -> Python.ArgKeyword kw <$> simplifyAlgebraicPyExpr expr
    Python.ArgStarred expr -> Python.ArgStarred <$> simplifyAlgebraicPyExpr expr
    Python.ArgKwStarred expr -> Python.ArgKwStarred <$> simplifyAlgebraicPyExpr expr
  return $ Common.Located spanLoc newArg

simplifyAlgebraicPySlice :: Common.Located Python.PythonSlice -> OptimizationM (Common.Located Python.PythonSlice)
simplifyAlgebraicPySlice (Common.Located spanLoc slice) = do
  newSlice <- case slice of
    Python.SliceIndex index -> Python.SliceIndex <$> simplifyAlgebraicPyExpr index
    Python.SliceSlice start end step -> do
      newStart <- traverse simplifyAlgebraicPyExpr start
      newEnd <- traverse simplifyAlgebraicPyExpr end
      newStep <- traverse simplifyAlgebraicPyExpr step
      return $ Python.SliceSlice newStart newEnd newStep
    Python.SliceExtSlice slices -> Python.SliceExtSlice <$> mapM simplifyAlgebraicPySlice slices
  return $ Common.Located spanLoc newSlice

isZero :: Python.PythonExpr -> Bool
isZero (Python.PyLiteral lit) = case pyLiteralToCommon lit of
  Just (Common.LInt 0) -> True
  Just (Common.LFloat 0.0) -> True
  _ -> False
isZero _ = False

isOne :: Python.PythonExpr -> Bool
isOne (Python.PyLiteral lit) = case pyLiteralToCommon lit of
  Just (Common.LInt 1) -> True
  Just (Common.LFloat 1.0) -> True
  _ -> False
isOne _ = False

algebraicSimplificationGo :: Go.GoAST -> OptimizationM Go.GoAST
algebraicSimplificationGo (Go.GoAST (Go.GoPackage packageName files)) = do
  newFiles <- mapM simplifyAlgebraicGoFile files
  return $ Go.GoAST $ Go.GoPackage packageName newFiles

simplifyAlgebraicGoFile :: Go.GoFile -> OptimizationM Go.GoFile
simplifyAlgebraicGoFile file = do
  newDecls <- mapM (fmap commonToGoLocated . simplifyAlgebraicGoDecl . goToCommonLocated) (Go.goFileDecls file)
  return $ file { Go.goFileDecls = newDecls }

simplifyAlgebraicGoDecl :: Common.Located Go.GoDecl -> OptimizationM (Common.Located Go.GoDecl)
simplifyAlgebraicGoDecl (Common.Located spanLoc decl) = case decl of
  Go.GoFuncDecl func -> do
    case Go.goFuncBody func of
      Nothing -> return $ Common.Located spanLoc $ Go.GoFuncDecl func
      Just body -> do
        newBody <- simplifyAlgebraicGoStmt (goToCommonLocatedStmt body)
        return $ Common.Located spanLoc $ Go.GoFuncDecl func { Go.goFuncBody = Just (commonToGoLocatedStmt newBody) }
  _ -> return $ Common.Located spanLoc decl

simplifyAlgebraicGoStmt :: Common.Located Go.GoStmt -> OptimizationM (Common.Located Go.GoStmt)
simplifyAlgebraicGoStmt (Common.Located spanLoc stmt) = do
  newStmt <- case stmt of
    Go.GoExprStmt expr -> Go.GoExprStmt <$> simplifyAlgebraicGoExpr expr
    Go.GoBind binding -> Go.GoBind <$> simplifyAlgebraicGoBinding binding
    Go.GoIf initStmt cond thenStmt elseStmt -> do
      newCond <- simplifyAlgebraicGoExpr cond
      newThenStmt <- simplifyAlgebraicGoStmt (goToCommonLocatedStmt thenStmt)
      newElseStmt <- traverse (simplifyAlgebraicGoStmt . goToCommonLocatedStmt) elseStmt
      return $ Go.GoIf initStmt newCond (commonToGoLocatedStmt newThenStmt) (fmap commonToGoLocatedStmt newElseStmt)
    Go.GoFor forClause body -> do
      newBody <- simplifyAlgebraicGoStmt (goToCommonLocatedStmt body)
      return $ Go.GoFor forClause (commonToGoLocatedStmt newBody)
    Go.GoReturn mexpr -> Go.GoReturn <$> mapM simplifyAlgebraicGoExpr mexpr
    Go.GoBlock stmts -> Go.GoBlock <$> mapM (fmap commonToGoLocatedStmt . simplifyAlgebraicGoStmt . goToCommonLocatedStmt) stmts
    _ -> return stmt
  return $ Common.Located spanLoc newStmt

simplifyAlgebraicGoExpr :: GoCommon.Located Go.GoExpr -> OptimizationM (GoCommon.Located Go.GoExpr)
simplifyAlgebraicGoExpr (GoCommon.Located nodeAnn expr) = do
  newExpr <- case expr of
    Go.GoBinaryOp op left right -> do
      newLeft <- simplifyAlgebraicGoExpr left
      newRight <- simplifyAlgebraicGoExpr right
      
      case op of
        Go.OpAdd | isGoZero (goLocatedValue newRight) -> do
          recordOptimization "Simplified Go x + 0 to x"
          modify $ \s -> s { osChanged = True }
          return $ goLocatedValue newLeft
        Go.OpSub | isGoZero (goLocatedValue newRight) -> do
          recordOptimization "Simplified Go x - 0 to x"
          modify $ \s -> s { osChanged = True }
          return $ goLocatedValue newLeft
        Go.OpMul | isGoOne (goLocatedValue newRight) -> do
          recordOptimization "Simplified Go x * 1 to x"
          modify $ \s -> s { osChanged = True }
          return $ goLocatedValue newLeft
        Go.OpMul | isGoZero (goLocatedValue newRight) || isGoZero (goLocatedValue newLeft) -> do
          recordOptimization "Simplified Go x * 0 to 0"
          modify $ \s -> s { osChanged = True }
          return $ Go.GoLiteral (Go.GoInt 0)
        Go.OpQuo | isGoOne (goLocatedValue newRight) -> do
          recordOptimization "Simplified Go x / 1 to x"
          modify $ \s -> s { osChanged = True }
          return $ goLocatedValue newLeft
        _ -> return $ Go.GoBinaryOp op newLeft newRight
    
    Go.GoUnaryOp op operand -> do
      newOperand <- simplifyAlgebraicGoExpr operand
      case (op, goLocatedValue newOperand) of
        (Go.OpNeg, Go.GoUnaryOp Go.OpNeg inner) -> do
          recordOptimization "Eliminated Go double negation"
          modify $ \s -> s { osChanged = True }
          return $ goLocatedValue inner
        (Go.OpNot, Go.GoUnaryOp Go.OpNot inner) -> do
          recordOptimization "Eliminated Go double logical negation"
          modify $ \s -> s { osChanged = True }
          return $ goLocatedValue inner
        _ -> return $ Go.GoUnaryOp op newOperand
    
    Go.GoCall func args -> do
      newFunc <- simplifyAlgebraicGoExpr func
      newArgs <- mapM simplifyAlgebraicGoExpr args
      return $ Go.GoCall newFunc newArgs
    
    _ -> return expr
  return $ GoCommon.Located nodeAnn newExpr

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
strengthReductionPython (Python.PythonAST (Python.PythonModule name doc imports stmts)) = do
  newStmts <- mapM strengthReducePyStmt stmts
  return $ Python.PythonAST (Python.PythonModule name doc imports newStmts)

strengthReducePyStmt :: Common.Located Python.PythonStmt -> OptimizationM (Common.Located Python.PythonStmt)
strengthReducePyStmt (Common.Located spanLoc stmt) = do
  newStmt <- case stmt of
    Python.PyExprStmt expr -> Python.PyExprStmt <$> strengthReducePyExpr expr
    Python.PyAssign targets value -> Python.PyAssign targets <$> strengthReducePyExpr value
    Python.PyIf cond thenStmts elseStmts -> do
      newCond <- strengthReducePyExpr cond
      newThenStmts <- mapM strengthReducePyStmt thenStmts
      newElseStmts <- mapM strengthReducePyStmt elseStmts
      return $ Python.PyIf newCond newThenStmts newElseStmts
    Python.PyFor async target iter body elseBody -> do
      newIter <- strengthReducePyExpr iter
      newBody <- mapM strengthReducePyStmt body
      newElseBody <- mapM strengthReducePyStmt elseBody
      return $ Python.PyFor async target newIter newBody newElseBody
    Python.PyFuncDef funcDef -> do
      newBody <- mapM strengthReducePyStmt (Python.pyFuncBody funcDef)
      return $ Python.PyFuncDef funcDef { Python.pyFuncBody = newBody }
    Python.PyReturn mexpr -> Python.PyReturn <$> traverse strengthReducePyExpr mexpr
    -- Python doesn't have PyBlock, statements are handled directly
    _ -> return stmt
  return $ Common.Located spanLoc newStmt

strengthReducePyExpr :: Common.Located Python.PythonExpr -> OptimizationM (Common.Located Python.PythonExpr)
strengthReducePyExpr (Common.Located spanLoc expr) = do
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
                recordOptimization $ "Reduced x * " <> T.pack (show (2^(power :: Integer))) <> " to left shift"
                modify $ \s -> s { osChanged = True }
                return $ Python.PyBinaryOp Common.OpShiftL newLeft (Common.Located spanLoc $ Python.PyLiteral $ Python.PyInt power)
              else return $ Python.PyBinaryOp op newLeft newRight
        
        -- x / 2^n => x >> n (division by power of 2 to right shift)
        Common.OpFloorDiv | isPowerOfTwo (Common.locatedValue newRight) -> do
          let power = getPowerOfTwo (Common.locatedValue newRight)
          if power > 0
            then do
              recordOptimization $ "Reduced x // " <> T.pack (show (2^(power :: Integer))) <> " to right shift"
              modify $ \s -> s { osChanged = True }
              return $ Python.PyBinaryOp Common.OpShiftR newLeft (Common.Located spanLoc $ Python.PyLiteral $ Python.PyInt power)
            else return $ Python.PyBinaryOp op newLeft newRight
        
        -- x % 2^n => x & (2^n - 1) (modulo by power of 2 to bitwise and)
        Common.OpMod | isPowerOfTwo (Common.locatedValue newRight) -> do
          let power = getPowerOfTwo (Common.locatedValue newRight)
          if power > 0
            then do
              recordOptimization $ "Reduced x % " <> T.pack (show (2^(power :: Integer))) <> " to bitwise and"
              modify $ \s -> s { osChanged = True }
              let mask = (2^power) - 1
              return $ Python.PyBinaryOp Common.OpBitAnd newLeft (Common.Located spanLoc $ Python.PyLiteral $ Python.PyInt mask)
            else return $ Python.PyBinaryOp op newLeft newRight
        
        _ -> return $ Python.PyBinaryOp op newLeft newRight
    
    Python.PyUnaryOp op operand -> do
      newOperand <- strengthReducePyExpr operand
      return $ Python.PyUnaryOp op newOperand
    
    Python.PyCall func args -> do
      newFunc <- strengthReducePyExpr func
      -- args should be [Located PythonArgument], not [Located PythonExpr]
      -- For now, we'll skip optimization of call arguments
      return $ Python.PyCall newFunc args
    
    Python.PySubscript container slice -> do
      newContainer <- strengthReducePyExpr container
      -- Note: slice should be PythonSlice, not PythonExpr
      -- For now, we'll skip optimization of slices
      return $ Python.PySubscript newContainer slice
    
    Python.PyList elems -> Python.PyList <$> mapM strengthReducePyExpr elems
    Python.PyTuple elems -> Python.PyTuple <$> mapM strengthReducePyExpr elems
    Python.PyDict pairs -> Python.PyDict <$> mapM (\(k, v) -> (,) <$> strengthReducePyExpr k <*> strengthReducePyExpr v) pairs
    
    _ -> return expr
  return $ Common.Located spanLoc newExpr

strengthReducePyArg :: Python.PythonArgument -> OptimizationM Python.PythonArgument
strengthReducePyArg (Python.ArgPositional expr) = Python.ArgPositional <$> strengthReducePyExpr expr
strengthReducePyArg (Python.ArgKeyword kw expr) = Python.ArgKeyword kw <$> strengthReducePyExpr expr
strengthReducePyArg (Python.ArgStarred expr) = Python.ArgStarred <$> strengthReducePyExpr expr
strengthReducePyArg (Python.ArgKwStarred expr) = Python.ArgKwStarred <$> strengthReducePyExpr expr

isPowerOfTwo :: Python.PythonExpr -> Bool
isPowerOfTwo (Python.PyLiteral (Python.PyInt n)) = n > 0 && (n .&. (n - 1)) == 0
isPowerOfTwo _ = False

getPowerOfTwo :: Python.PythonExpr -> Integer
getPowerOfTwo (Python.PyLiteral (Python.PyInt n)) = 
  let countBits x p = if x == 1 then p else countBits (x `div` 2) (p + 1)
  in if n > 0 && (n .&. (n - 1)) == 0 then countBits n 0 else 0
getPowerOfTwo _ = 0

strengthReductionGo :: Go.GoAST -> OptimizationM Go.GoAST
strengthReductionGo (Go.GoAST (Go.GoPackage packageName files)) = do
  newFiles <- mapM strengthReduceGoFile files
  return $ Go.GoAST $ Go.GoPackage packageName newFiles

strengthReduceGoFile :: Go.GoFile -> OptimizationM Go.GoFile
strengthReduceGoFile file = do
  newDecls <- mapM (fmap commonToGoLocated . strengthReduceGoDecl . goToCommonLocated) (Go.goFileDecls file)
  return $ file { Go.goFileDecls = newDecls }

strengthReduceGoDecl :: Common.Located Go.GoDecl -> OptimizationM (Common.Located Go.GoDecl)
strengthReduceGoDecl (Common.Located spanLoc decl) = case decl of
  Go.GoFuncDecl func -> do
    case Go.goFuncBody func of
      Nothing -> return $ Common.Located spanLoc $ Go.GoFuncDecl func
      Just body -> do
        newBody <- strengthReduceGoStmt (goToCommonLocatedStmt body)
        return $ Common.Located spanLoc $ Go.GoFuncDecl func { Go.goFuncBody = Just (commonToGoLocatedStmt newBody) }
  _ -> return $ Common.Located spanLoc decl

strengthReduceGoStmt :: Common.Located Go.GoStmt -> OptimizationM (Common.Located Go.GoStmt)
strengthReduceGoStmt (Common.Located spanLoc stmt) = do
  newStmt <- case stmt of
    Go.GoExprStmt expr -> Go.GoExprStmt <$> strengthReduceGoExpr expr
    Go.GoBind binding -> Go.GoBind <$> strengthReduceGoBinding binding
    Go.GoIf initStmt cond thenStmt elseStmt -> do
      newCond <- strengthReduceGoExpr cond
      newThenStmt <- strengthReduceGoStmt (goToCommonLocatedStmt thenStmt)
      newElseStmt <- traverse (strengthReduceGoStmt . goToCommonLocatedStmt) elseStmt
      return $ Go.GoIf initStmt newCond (commonToGoLocatedStmt newThenStmt) (fmap commonToGoLocatedStmt newElseStmt)
    Go.GoFor forClause body -> do
      newBody <- strengthReduceGoStmt (goToCommonLocatedStmt body)
      return $ Go.GoFor forClause (commonToGoLocatedStmt newBody)
    Go.GoReturn mexpr -> Go.GoReturn <$> mapM strengthReduceGoExpr mexpr
    Go.GoBlock stmts -> Go.GoBlock <$> mapM (fmap commonToGoLocatedStmt . strengthReduceGoStmt . goToCommonLocatedStmt) stmts
    _ -> return stmt
  return $ Common.Located spanLoc newStmt

strengthReduceGoExpr :: GoCommon.Located Go.GoExpr -> OptimizationM (GoCommon.Located Go.GoExpr)
strengthReduceGoExpr (GoCommon.Located nodeAnn expr) = do
  newExpr <- case expr of
    Go.GoBinaryOp op left right -> do
      newLeft <- strengthReduceGoExpr left
      newRight <- strengthReduceGoExpr right
      
      case op of
        Go.OpMul | isGoPowerOfTwo (goLocatedValue newRight) -> do
          let power = getGoPowerOfTwo (goLocatedValue newRight)
          if power == 1
            then do
              recordOptimization "Reduced Go x * 2 to x + x"
              modify $ \s -> s { osChanged = True }
              return $ Go.GoBinaryOp Go.OpAdd newLeft newLeft
            else if power > 1 && power <= 4
              then do
                recordOptimization $ "Reduced Go x * " <> T.pack (show (2^(power :: Integer))) <> " to left shift"
                modify $ \s -> s { osChanged = True }
                return $ Go.GoBinaryOp Go.OpShiftL newLeft (GoCommon.Located nodeAnn $ Go.GoLiteral $ Go.GoInt power)
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
  return $ GoCommon.Located nodeAnn newExpr

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
csePython (Python.PythonAST (Python.PythonModule name doc imports stmts)) = do
  newStmts <- mapM csePyStmt stmts
  return $ Python.PythonAST (Python.PythonModule name doc imports newStmts)

csePyStmt :: Common.Located Python.PythonStmt -> OptimizationM (Common.Located Python.PythonStmt)
csePyStmt (Common.Located spanLoc stmt) = do
  newStmt <- case stmt of
    Python.PyExprStmt expr -> Python.PyExprStmt <$> csePyExpr expr
    Python.PyAssign targets value -> Python.PyAssign targets <$> csePyExpr value
    Python.PyIf cond thenStmts elseStmts -> do
      newCond <- csePyExpr cond
      newThenStmts <- mapM csePyStmt thenStmts
      newElseStmts <- mapM csePyStmt elseStmts
      return $ Python.PyIf newCond newThenStmts newElseStmts
    Python.PyFor async target iter body elseBody -> do
      newIter <- csePyExpr iter
      -- Clear CSE cache for loop body as it may execute multiple times
      modify $ \s -> s { osSubexpressions = HashMap.empty }
      newBody <- mapM csePyStmt body
      newElseBody <- mapM csePyStmt elseBody
      return $ Python.PyFor async target newIter newBody newElseBody
    Python.PyFuncDef funcDef -> do
      -- Clear CSE cache for function body
      modify $ \s -> s { osSubexpressions = HashMap.empty }
      newBody <- mapM csePyStmt (Python.pyFuncBody funcDef)
      return $ Python.PyFuncDef funcDef { Python.pyFuncBody = newBody }
    Python.PyReturn mexpr -> Python.PyReturn <$> traverse csePyExpr mexpr
    -- Python doesn't have PyBlock, statements are handled directly
    _ -> return stmt
  return $ Common.Located spanLoc newStmt

csePyExpr :: Common.Located Python.PythonExpr -> OptimizationM (Common.Located Python.PythonExpr)
csePyExpr (Common.Located spanLoc expr) = do
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
          return $ Python.PyVar varName
        Nothing -> do
          -- Generate new variable name and cache it
          let newVarName = Common.Identifier ("_cse_" <> T.pack (show $ HashMap.size cache))
          modify $ \s -> s { osSubexpressions = HashMap.insert exprKey newVarName (osSubexpressions s) }
          return $ Python.PyBinaryOp op newLeft newRight
    
    Python.PyUnaryOp op operand -> do
      newOperand <- csePyExpr operand
      return $ Python.PyUnaryOp op newOperand
    
    Python.PyCall func args -> do
      newFunc <- csePyExpr func
      newArgs <- mapM (\(Common.Located _span arg) -> Common.Located (Common.SourceSpan (T.pack "<cse>") (Common.SourcePos 0 0) (Common.SourcePos 0 0)) <$> csePyArg arg) args
      return $ Python.PyCall newFunc newArgs
    
    Python.PySubscript container slice -> do
      newContainer <- csePyExpr container
      newSlice <- csePySlice slice
      return $ Python.PySubscript newContainer newSlice
    
    Python.PyList elems -> Python.PyList <$> mapM csePyExpr elems
    Python.PyTuple elems -> Python.PyTuple <$> mapM csePyExpr elems
    Python.PyDict pairs -> Python.PyDict <$> mapM (\(k, v) -> (,) <$> csePyExpr k <*> csePyExpr v) pairs
    
    _ -> return expr
  return $ Common.Located spanLoc newExpr

csePyArg :: Python.PythonArgument -> OptimizationM Python.PythonArgument
csePyArg (Python.ArgPositional expr) = Python.ArgPositional <$> csePyExpr expr
csePyArg (Python.ArgKeyword kw expr) = Python.ArgKeyword kw <$> csePyExpr expr
csePyArg (Python.ArgStarred expr) = Python.ArgStarred <$> csePyExpr expr
csePyArg (Python.ArgKwStarred expr) = Python.ArgKwStarred <$> csePyExpr expr

csePySlice :: Common.Located Python.PythonSlice -> OptimizationM (Common.Located Python.PythonSlice)
csePySlice (Common.Located spanLoc slice) = do
  newSlice <- case slice of
    Python.SliceIndex index -> Python.SliceIndex <$> csePyExpr index
    Python.SliceSlice start end step -> do
      newStart <- traverse csePyExpr start
      newEnd <- traverse csePyExpr end
      newStep <- traverse csePyExpr step
      return $ Python.SliceSlice newStart newEnd newStep
    Python.SliceExtSlice slices -> Python.SliceExtSlice <$> mapM csePySlice slices
  return $ Common.Located spanLoc newSlice

exprToText :: Python.PythonExpr -> Text
exprToText (Python.PyVar (Common.Identifier name)) = name
exprToText (Python.PyLiteral lit) = T.pack $ show lit
exprToText _ = "_complex_"

cseGo :: Go.GoAST -> OptimizationM Go.GoAST
cseGo (Go.GoAST package) = do
  newPackage <- cseGoPackage package
  return $ Go.GoAST newPackage

cseGoPackage :: Go.GoPackage -> OptimizationM Go.GoPackage
cseGoPackage (Go.GoPackage packageName files) = do
  newFiles <- mapM cseGoFile files
  return $ Go.GoPackage packageName newFiles

cseGoFile :: Go.GoFile -> OptimizationM Go.GoFile
cseGoFile file = do
  newDecls <- mapM (cseGoDecl . goToCommonLocated) (Go.goFileDecls file)
  return $ file { Go.goFileDecls = map commonToGoLocated newDecls }

cseGoDecl :: Common.Located Go.GoDecl -> OptimizationM (Common.Located Go.GoDecl)
cseGoDecl (Common.Located spanLoc decl) = case decl of
  Go.GoFuncDecl func -> do
    case Go.goFuncBody func of
      Nothing -> return $ Common.Located spanLoc $ Go.GoFuncDecl func
      Just body -> do
        modify $ \s -> s { osSubexpressions = HashMap.empty }
        newBody <- cseGoStmt (goToCommonLocatedStmt body)
        return $ Common.Located spanLoc $ Go.GoFuncDecl func { Go.goFuncBody = Just (commonToGoLocatedStmt newBody) }
  _ -> return $ Common.Located spanLoc decl

cseGoStmt :: Common.Located Go.GoStmt -> OptimizationM (Common.Located Go.GoStmt)
cseGoStmt (Common.Located spanLoc stmt) = do
  newStmt <- case stmt of
    Go.GoExprStmt expr -> fmap (Go.GoExprStmt . commonToGoLocatedExpr) (cseGoExpr (goToCommonLocatedExpr expr))
    Go.GoBind binding -> Go.GoBind <$> cseGoBinding binding
    Go.GoIf initStmt cond thenStmt elseStmt -> do
      newCond <- fmap commonToGoLocatedExpr (cseGoExpr (goToCommonLocatedExpr cond))
      newThenStmt <- cseGoStmt (goToCommonLocatedStmt thenStmt)
      newElseStmt <- traverse (cseGoStmt . goToCommonLocatedStmt) elseStmt
      return $ Go.GoIf initStmt newCond (commonToGoLocatedStmt newThenStmt) (fmap commonToGoLocatedStmt newElseStmt)
    Go.GoFor forClause body -> do
      modify $ \s -> s { osSubexpressions = HashMap.empty }
      newBody <- cseGoStmt (goToCommonLocatedStmt body)
      return $ Go.GoFor forClause (commonToGoLocatedStmt newBody)
    Go.GoReturn mexpr -> Go.GoReturn <$> mapM (\expr -> do
        processed <- cseGoExpr (goToCommonLocatedExpr expr)
        return $ commonToGoLocatedExpr processed) mexpr
    Go.GoBlock stmts -> Go.GoBlock <$> mapM (\stmtVal -> do
        processed <- cseGoStmt (goToCommonLocatedStmt stmtVal)
        return $ commonToGoLocatedStmt processed) stmts
    _ -> return stmt
  return $ Common.Located spanLoc newStmt

cseGoExpr :: Common.Located Go.GoExpr -> OptimizationM (Common.Located Go.GoExpr)
cseGoExpr (Common.Located spanLoc expr) = do
  newExpr <- case expr of
    Go.GoBinaryOp op left right -> do
      newLeft <- cseGoExpr (goToCommonLocatedExpr left)
      newRight <- cseGoExpr (goToCommonLocatedExpr right)
      
      let exprKey = BasicCommonExpr (T.pack $ show op) [goExprToText $ Common.locatedValue newLeft, goExprToText $ Common.locatedValue newRight]
      cache <- gets osSubexpressions
      
      case HashMap.lookup exprKey cache of
        Just varName -> do
          recordOptimization $ "Reused Go common subexpression: " <> T.pack (show op)
          modify $ \s -> s { osChanged = True }
          return $ Go.GoIdent varName
        Nothing -> do
          let newVarName = GoCommon.Identifier ("_cse_" <> T.pack (show $ HashMap.size cache))
          modify $ \s -> s { osSubexpressions = HashMap.insert exprKey newVarName (osSubexpressions s) }
          return $ Go.GoBinaryOp op (commonToGoLocatedExpr newLeft) (commonToGoLocatedExpr newRight)
    
    Go.GoUnaryOp op operand -> do
      processedOperand <- cseGoExpr (goToCommonLocatedExpr operand)
      let newOperand = commonToGoLocatedExpr processedOperand
      return $ Go.GoUnaryOp op newOperand
    
    Go.GoCall func args -> do
      processedFunc <- cseGoExpr (goToCommonLocatedExpr func)
      let newFunc = commonToGoLocatedExpr processedFunc
      processedArgs <- mapM (\exprVal -> cseGoExpr (goToCommonLocatedExpr exprVal)) args
      let newArgs = map commonToGoLocatedExpr processedArgs
      return $ Go.GoCall newFunc newArgs
    
    _ -> return expr
  return $ Common.Located spanLoc newExpr

goExprToText :: Go.GoExpr -> Text
goExprToText (Go.GoIdent (GoCommon.Identifier name)) = name
goExprToText (Go.GoLiteral lit) = T.pack $ show lit
goExprToText _ = "_complex_"

-- ============================================================================
-- PEEPHOLE OPTIMIZATION PASS
-- ============================================================================

peepholeOptimizationPass :: Either Python.PythonAST Go.GoAST -> OptimizationM (Either Python.PythonAST Go.GoAST)
peepholeOptimizationPass (Left pyAST) = Left <$> peepholePython pyAST
peepholeOptimizationPass (Right goAST) = Right <$> peepholeGo goAST

peepholePython :: Python.PythonAST -> OptimizationM Python.PythonAST
peepholePython (Python.PythonAST (Python.PythonModule name doc imports stmts)) = do
  newStmts <- peepholeOptimizeStmts stmts
  return $ Python.PythonAST (Python.PythonModule name doc imports newStmts)

peepholeOptimizeStmts :: [Common.Located Python.PythonStmt] -> OptimizationM [Common.Located Python.PythonStmt]
peepholeOptimizeStmts [] = return []
peepholeOptimizeStmts [stmt] = (:[]) <$> peepholePyStmt stmt
peepholeOptimizeStmts (s1:s2:rest) = do
  -- Look for patterns in consecutive statements
  case (Common.locatedValue s1, Common.locatedValue s2) of
    -- Pattern: x = y; return x => return y
    (Python.PyAssign [Common.Located _ (Python.PatVar returnVar)] value, Python.PyReturn (Just (Common.Located _ (Python.PyVar var))))
      | var == returnVar -> do
        recordOptimization "Peephole: Eliminated temporary variable before return"
        modify $ \s -> s { osChanged = True }
        let newReturn = Common.Located (Common.locSpan s2) $ Python.PyReturn (Just value)
        peepholeOptimizeStmts (newReturn : rest)
    
    _ -> do
      newS1 <- peepholePyStmt s1
      restOptimized <- peepholeOptimizeStmts (s2:rest)
      return (newS1 : restOptimized)

peepholePyStmt :: Common.Located Python.PythonStmt -> OptimizationM (Common.Located Python.PythonStmt)
peepholePyStmt (Common.Located spanLoc stmt) = do
  newStmt <- case stmt of
    Python.PyExprStmt expr -> Python.PyExprStmt <$> peepholePyExpr expr
    Python.PyAssign targets value -> Python.PyAssign targets <$> peepholePyExpr value
    Python.PyIf cond thenStmts elseStmts -> do
      newCond <- peepholePyExpr cond
      newThenStmts <- peepholeOptimizeStmts thenStmts
      newElseStmts <- peepholeOptimizeStmts elseStmts
      return $ Python.PyIf newCond newThenStmts newElseStmts
    Python.PyFor async target iter body elseBody -> do
      newIter <- peepholePyExpr iter
      newBody <- peepholeOptimizeStmts body
      newElseBody <- peepholeOptimizeStmts elseBody
      return $ Python.PyFor async target newIter newBody newElseBody
    Python.PyFuncDef funcDef -> do
      newBody <- peepholeOptimizeStmts (Python.pyFuncBody funcDef)
      return $ Python.PyFuncDef funcDef { Python.pyFuncBody = newBody }
    Python.PyReturn mexpr -> Python.PyReturn <$> traverse peepholePyExpr mexpr
    -- Python doesn't have PyBlock, statements are handled directly
    _ -> return stmt
  return $ Common.Located spanLoc newStmt

peepholePyExpr :: Common.Located Python.PythonExpr -> OptimizationM (Common.Located Python.PythonExpr)
peepholePyExpr (Common.Located spanLoc expr) = do
  newExpr <- case expr of
    -- Pattern: not (x == y) => x != y
    Python.PyUnaryOp Common.OpNot (Common.Located _ (Python.PyComparison [Common.OpEq] [left, right])) -> do
      recordOptimization "Peephole: Converted not (x == y) to x != y"
      modify $ \s -> s { osChanged = True }
      return $ Python.PyComparison [Common.OpNe] [left, right]

    -- Pattern: not (x != y) => x == y
    Python.PyUnaryOp Common.OpNot (Common.Located _ (Python.PyComparison [Common.OpNe] [left, right])) -> do
      recordOptimization "Peephole: Converted not (x != y) to x == y"
      modify $ \s -> s { osChanged = True }
      return $ Python.PyComparison [Common.OpEq] [left, right]
    
    -- Pattern: not (x < y) => x >= y
    Python.PyUnaryOp Common.OpNot (Common.Located _ (Python.PyComparison [Common.OpLt] [left, right])) -> do
      recordOptimization "Peephole: Converted not (x < y) to x >= y"
      modify $ \s -> s { osChanged = True }
      return $ Python.PyComparison [Common.OpGe] [left, right]

    -- Pattern: not (x > y) => x <= y
    Python.PyUnaryOp Common.OpNot (Common.Located _ (Python.PyComparison [Common.OpGt] [left, right])) -> do
      recordOptimization "Peephole: Converted not (x > y) to x <= y"
      modify $ \s -> s { osChanged = True }
      return $ Python.PyComparison [Common.OpLe] [left, right]
    
    Python.PyBinaryOp op left right -> do
      newLeft <- peepholePyExpr left
      newRight <- peepholePyExpr right
      return $ Python.PyBinaryOp op newLeft newRight
    
    Python.PyUnaryOp op operand -> do
      newOperand <- peepholePyExpr operand
      return $ Python.PyUnaryOp op newOperand
    
    Python.PyCall func args -> do
      newFunc <- peepholePyExpr func
      newArgs <- mapM (\(Common.Located _span arg) -> Common.Located (Common.SourceSpan (T.pack "<peephole>") (Common.SourcePos 0 0) (Common.SourcePos 0 0)) <$> peepholePyArg arg) args
      return $ Python.PyCall newFunc newArgs
    
    Python.PySubscript container slice -> do
      newContainer <- peepholePyExpr container
      return $ Python.PySubscript newContainer slice
    
    Python.PyList elems -> Python.PyList <$> mapM peepholePyExpr elems
    Python.PyTuple elems -> Python.PyTuple <$> mapM peepholePyExpr elems
    Python.PyDict pairs -> Python.PyDict <$> mapM (\(k, v) -> (,) <$> peepholePyExpr k <*> peepholePyExpr v) pairs
    
    _ -> return expr
  return $ Common.Located spanLoc newExpr

peepholePyArg :: Python.PythonArgument -> OptimizationM Python.PythonArgument
peepholePyArg (Python.ArgPositional expr) = Python.ArgPositional <$> peepholePyExpr expr
peepholePyArg (Python.ArgKwStarred expr) = Python.ArgKwStarred <$> peepholePyExpr expr
peepholePyArg (Python.ArgKeyword kw expr) = Python.ArgKeyword kw <$> peepholePyExpr expr
peepholePyArg (Python.ArgStarred expr) = Python.ArgStarred <$> peepholePyExpr expr

peepholeGo :: Go.GoAST -> OptimizationM Go.GoAST
peepholeGo (Go.GoAST (Go.GoPackage packageName files)) = do
  newFiles <- mapM peepholeGoFile files
  return $ Go.GoAST $ Go.GoPackage packageName newFiles

peepholeGoFile :: Go.GoFile -> OptimizationM Go.GoFile
peepholeGoFile file = do
  let commonDecls = map goToCommonLocated (Go.goFileDecls file)
  newDecls <- mapM peepholeGoDecl commonDecls
  let goDecls = map commonToGoLocated newDecls
  return $ file { Go.goFileDecls = goDecls }

peepholeGoDecl :: Common.Located Go.GoDecl -> OptimizationM (Common.Located Go.GoDecl)
peepholeGoDecl (Common.Located spanLoc decl) = case decl of
  Go.GoFuncDecl func -> do
    newFunc <- peepholeGoFunction func
    return $ Common.Located spanLoc $ Go.GoFuncDecl newFunc
  _ -> return $ Common.Located spanLoc decl

peepholeGoFunction :: Go.GoFunction -> OptimizationM Go.GoFunction
peepholeGoFunction (Go.GoFunction name typeParams params results body) = do
  newBody <- case body of
    Just stmt -> Just . commonToGoLocatedStmt <$> peepholeGoStmt (goToCommonLocatedStmt stmt)
    Nothing -> return Nothing
  return $ Go.GoFunction name typeParams params results newBody

peepholeGoStmt :: Common.Located Go.GoStmt -> OptimizationM (Common.Located Go.GoStmt)
peepholeGoStmt (Common.Located spanLoc stmt) = do
  newStmt <- case stmt of
    Go.GoExprStmt expr -> do
      newExpr <- peepholeGoExpr (goToCommonLocatedExpr expr)
      return $ Go.GoExprStmt (commonToGoLocatedExpr newExpr)
    Go.GoBind binding -> Go.GoBind <$> peepholeGoBinding binding
    Go.GoIf initStmt cond thenStmt elseStmt -> do
      newCond <- peepholeGoExpr (goToCommonLocatedExpr cond)
      newThenStmt <- peepholeGoStmt (goToCommonLocatedStmt thenStmt)
      newElseStmt <- traverse (peepholeGoStmt . goToCommonLocatedStmt) elseStmt
      return $ Go.GoIf initStmt (commonToGoLocatedExpr newCond) (commonToGoLocatedStmt newThenStmt) (fmap commonToGoLocatedStmt newElseStmt)
    Go.GoFor forClause body -> do
      newBody <- peepholeGoStmt (goToCommonLocatedStmt body)
      return $ Go.GoFor forClause (commonToGoLocatedStmt newBody)
    Go.GoReturn mexpr -> do
      let commonMexpr = map goToCommonLocatedExpr mexpr
      processedMexpr <- traverse peepholeGoExpr commonMexpr
      let goMexpr = map commonToGoLocatedExpr processedMexpr
      return $ Go.GoReturn goMexpr
    Go.GoBlock stmts -> do
      newStmts <- peepholeOptimizeGoStmts stmts
      return $ Go.GoBlock newStmts
    _ -> return stmt
  return $ Common.Located spanLoc newStmt

peepholeOptimizeGoStmts :: [GoCommon.Located Go.GoStmt] -> OptimizationM [GoCommon.Located Go.GoStmt]
peepholeOptimizeGoStmts [] = return []
peepholeOptimizeGoStmts [stmt] = (:[]) . commonToGoLocatedStmt <$> peepholeGoStmt (goToCommonLocatedStmt stmt)
peepholeOptimizeGoStmts (s1:s2:rest) = do
  case (GoCommon.locValue s1, GoCommon.locValue s2) of
    (Go.GoBind binding, Go.GoReturn [returnExpr]) -> do
      case (Go.bindLHS binding, GoCommon.locValue returnExpr) of
        (Go.LHSIdents [var], Go.GoIdent returnVar) | var == returnVar -> do
          case Go.bindRHS binding of
            [value] -> do
              recordOptimization "Peephole: Eliminated Go temporary variable before return"
              modify $ \s -> s { osChanged = True }
              let newReturn = GoCommon.Located (GoCommon.NodeAnn (GoCommon.annSpan $ GoCommon.locAnn s2) [] []) $ Go.GoReturn [value]
              peepholeOptimizeGoStmts (newReturn : rest)
            _ -> fallback
        _ -> fallback
      where
        fallback = do
          newS1 <- commonToGoLocatedStmt <$> peepholeGoStmt (goToCommonLocatedStmt s1)
          restOptimized <- peepholeOptimizeGoStmts (s2:rest)
          return (newS1 : restOptimized)
    _ -> do
      newS1 <- commonToGoLocatedStmt <$> peepholeGoStmt (goToCommonLocatedStmt s1)
      restOptimized <- peepholeOptimizeGoStmts (s2:rest)
      return (newS1 : restOptimized)

peepholeGoExpr :: Common.Located Go.GoExpr -> OptimizationM (Common.Located Go.GoExpr)
peepholeGoExpr (Common.Located spanLoc expr) = do
  newExpr <- case expr of
    Go.GoUnaryOp Go.OpNot (GoCommon.Located _ (Go.GoComparison Go.OpEq left right)) -> do
      recordOptimization "Peephole: Converted Go !(x == y) to x != y"
      modify $ \s -> s { osChanged = True }
      return $ Go.GoComparison Go.OpNe left right
    
    Go.GoUnaryOp Go.OpNot (GoCommon.Located _ (Go.GoComparison Go.OpNe left right)) -> do
      recordOptimization "Peephole: Converted Go !(x != y) to x == y"
      modify $ \s -> s { osChanged = True }
      return $ Go.GoComparison Go.OpEq left right
    
    Go.GoBinaryOp op left right -> do
      newLeft <- peepholeGoExpr (goToCommonLocatedExpr left)
      newRight <- peepholeGoExpr (goToCommonLocatedExpr right)
      return $ Go.GoBinaryOp op (commonToGoLocatedExpr newLeft) (commonToGoLocatedExpr newRight)
    
    Go.GoUnaryOp op operand -> do
      newOperand <- peepholeGoExpr (goToCommonLocatedExpr operand)
      return $ Go.GoUnaryOp op (commonToGoLocatedExpr newOperand)
    
    Go.GoCall func args -> do
      newFunc <- peepholeGoExpr (goToCommonLocatedExpr func)
      newArgs <- mapM peepholeGoExpr (map goToCommonLocatedExpr args)
      return $ Go.GoCall (commonToGoLocatedExpr newFunc) (map commonToGoLocatedExpr newArgs)
    
    _ -> return expr
  return $ Common.Located spanLoc newExpr

-- ============================================================================
-- PUBLIC INTERFACES
-- ============================================================================

recordOptimization :: Text -> OptimizationM ()
recordOptimization opt = do
  -- Debug log with more context
  modify $ \s -> s { osOptimizations = opt : osOptimizations s }

-- | Log debug information with context
debugLog :: Text -> OptimizationM ()
debugLog msg = seq msg (return ())
-- | Log error information with context  
errorLog :: Text -> OptimizationM ()
errorLog msg = seq msg (return ())
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

-- Helper functions for Go bindings
simplifyAlgebraicGoBinding :: Go.GoBinding -> OptimizationM Go.GoBinding
simplifyAlgebraicGoBinding binding = do
  newRHS <- mapM simplifyAlgebraicGoExpr (Go.bindRHS binding)
  return $ binding { Go.bindRHS = newRHS }

strengthReduceGoBinding :: Go.GoBinding -> OptimizationM Go.GoBinding
strengthReduceGoBinding binding = do
  newRHS <- mapM strengthReduceGoExpr (Go.bindRHS binding)
  return $ binding { Go.bindRHS = newRHS }

cseGoBinding :: Go.GoBinding -> OptimizationM Go.GoBinding
cseGoBinding binding = do
  let commonRHS = map goToCommonLocatedExpr (Go.bindRHS binding)
  newRHS <- mapM cseGoExpr commonRHS
  let goRHS = map commonToGoLocatedExpr newRHS
  return $ binding { Go.bindRHS = goRHS }

peepholeGoBinding :: Go.GoBinding -> OptimizationM Go.GoBinding
peepholeGoBinding binding = do
  let commonRHS = map goToCommonLocatedExpr (Go.bindRHS binding)
  newRHS <- mapM peepholeGoExpr commonRHS
  let goRHS = map commonToGoLocatedExpr newRHS
  return $ binding { Go.bindRHS = goRHS }


-- Convert between Common.Located and GoCommon.Located
commonToGoLocated :: Common.Located a -> GoCommon.Located a
commonToGoLocated (Common.Located spanLoc val) = GoCommon.Located (commonToGoNodeAnn spanLoc) val

commonToGoNodeAnn :: Common.SourceSpan -> GoCommon.NodeAnn
commonToGoNodeAnn (Common.SourceSpan _ start end) = GoCommon.NodeAnn (Just $ GoCommon.Span (commonToGoPos start) (commonToGoPos end)) [] []

goToCommonLocated :: GoCommon.Located a -> Common.Located a
goToCommonLocated (GoCommon.Located nodeAnn val) =
  case GoCommon.annSpan nodeAnn of
    Just spanVal -> Common.Located (goToCommonSpan spanVal) val
    Nothing -> Common.Located (Common.SourceSpan (T.pack "<unknown>") (Common.SourcePos 0 0) (Common.SourcePos 0 0)) val


commonToGoPos :: Common.SourcePos -> GoCommon.Position
commonToGoPos (Common.SourcePos line col) = GoCommon.Position line col

goToCommonPos :: GoCommon.Position -> Common.SourcePos
goToCommonPos (GoCommon.Position line col) = Common.SourcePos line col

-- Convert between Common.Located and GoCommon.Located for use in propagateConstantsGoExpr
goToCommonLocatedExpr :: GoCommon.Located Go.GoExpr -> Common.Located Go.GoExpr
goToCommonLocatedExpr (GoCommon.Located nodeAnn expr) =
  case GoCommon.annSpan nodeAnn of
    Just spanVal -> Common.Located (goToCommonSpan spanVal) expr
    Nothing -> Common.Located (Common.SourceSpan (T.pack "<unknown>") (Common.SourcePos 0 0) (Common.SourcePos 0 0)) expr

commonToGoLocatedExpr :: Common.Located Go.GoExpr -> GoCommon.Located Go.GoExpr
commonToGoLocatedExpr (Common.Located spanLoc expr) = GoCommon.Located (commonToGoNodeAnn spanLoc) expr



-- Helper function to extract value from GoCommon.Located
goLocatedValue :: GoCommon.Located a -> a
goLocatedValue (GoCommon.Located _ expr) = expr

-- | Convert GoCommon.Span to Common.SourceSpan
goToCommonSpan :: GoCommon.Span -> Common.SourceSpan
goToCommonSpan (GoCommon.Span start end) = Common.SourceSpan (T.pack "<go-file>") (goToCommonPos start) (goToCommonPos end)