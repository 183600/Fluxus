{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

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
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

type OptimizationM = ReaderT BasicPassConfig (State OptimizationState)

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
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | State for optimization passes
data OptimizationState = OptimizationState
  { osConstants :: !(HashMap Identifier Literal)         -- Known constant values
  , osLiveVariables :: !(Set Identifier)                 -- Variables that are live
  , osDeadCode :: ![Text]                                 -- Dead code identified
  , osSubexpressions :: !(HashMap CommonExpr Identifier) -- Common subexpressions
  , osOptimizations :: ![Text]                           -- Applied optimizations
  , osIterationCount :: !Int                             -- Current iteration
  , osChanged :: !Bool                                   -- Whether changes were made
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
  }

-- | Initial state
initialState :: OptimizationState
initialState = OptimizationState
  { osConstants = HashMap.empty
  , osLiveVariables = Set.empty
  , osDeadCode = []
  , osSubexpressions = HashMap.empty
  , osOptimizations = []
  , osIterationCount = 0
  , osChanged = False
  }

-- | Run basic optimizations on either Python or Go AST
runBasicOptimizations :: BasicPassConfig -> Either PythonAST GoAST -> OptimizationResult
runBasicOptimizations config ast = 
  let (result, finalState) = runState (runReaderT (optimizeAST ast) config) initialState
  in result { orOptimizations = osOptimizations finalState
           , orIterations = osIterationCount finalState
           }

-- | Main optimization function that runs all passes iteratively
optimizeAST :: Either PythonAST GoAST -> OptimizationM OptimizationResult
optimizeAST originalAST = do
  config <- ask
  
  let runPasses ast = do
        modify $ \s -> s { osChanged = False }
        
        -- Run all enabled passes
        ast1 <- if bpcEnableConstantFolding config then constantFoldingPass ast else return ast
        ast2 <- if bpcEnableConstantPropagation config then constantPropagationPass ast1 else return ast1
        ast3 <- if bpcEnableAlgebraicSimplification config then algebraicSimplificationPass ast2 else return ast2
        ast4 <- if bpcEnableStrengthReduction config then strengthReductionPass ast3 else return ast3
        ast5 <- if bpcEnableCSE config then csePass ast4 else return ast4
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
    , orOptimizations = osOptimizations state
    , orIterations = osIterationCount state
    , orConstantsFolded = HashMap.size (osConstants state)
    , orDeadCodeRemoved = length (osDeadCode state)
    }

-- | Constant folding pass
constantFoldingPass :: Either PythonAST GoAST -> OptimizationM (Either PythonAST GoAST)
constantFoldingPass (Left pyAST) = do
  newAST <- constantFoldingPython pyAST
  return $ Left newAST
constantFoldingPass (Right goAST) = do
  newAST <- constantFoldingGo goAST
  return $ Right newAST

-- | Constant folding for Python AST
constantFoldingPython :: PythonAST -> OptimizationM PythonAST
constantFoldingPython (PythonModule stmts) = do
  newStmts <- mapM constantFoldPythonStmt stmts
  return $ PythonModule newStmts

-- | Constant folding for Python statements
constantFoldPythonStmt :: Located PythonStmt -> OptimizationM (Located PythonStmt)
constantFoldPythonStmt (Located span stmt) = do
  newStmt <- case stmt of
    PyExprStmt expr -> do
      newExpr <- constantFoldPythonExpr expr
      return $ PyExprStmt newExpr
    PyAssign targets value -> do
      newValue <- constantFoldPythonExpr value
      return $ PyAssign targets newValue
    PyIf condition thenStmts elseStmts -> do
      newCondition <- constantFoldPythonExpr condition
      newThenStmts <- mapM constantFoldPythonStmt thenStmts
      newElseStmts <- mapM constantFoldPythonStmt elseStmts
      -- Check if condition is constant
      case locatedValue newCondition of
        PyLiteral (LBool True) -> do
          recordOptimization "Eliminated always-true if statement"
          modify $ \s -> s { osChanged = True }
          -- Return then branch as block
          return $ PyBlock newThenStmts
        PyLiteral (LBool False) -> do
          recordOptimization "Eliminated always-false if statement"
          modify $ \s -> s { osChanged = True }
          -- Return else branch as block  
          return $ PyBlock newElseStmts
        _ -> return $ PyIf newCondition newThenStmts newElseStmts
    PyFor target iter body -> do
      newIter <- constantFoldPythonExpr iter
      newBody <- mapM constantFoldPythonStmt body
      return $ PyFor target newIter newBody
    PyFunctionDef name params body returnType -> do
      newBody <- mapM constantFoldPythonStmt body
      return $ PyFunctionDef name params newBody returnType
    PyReturn mexpr -> do
      newExpr <- case mexpr of
        Just expr -> Just <$> constantFoldPythonExpr expr
        Nothing -> return Nothing
      return $ PyReturn newExpr
    PyBlock stmts -> do
      newStmts <- mapM constantFoldPythonStmt stmts
      return $ PyBlock newStmts
    _ -> return stmt
  return $ Located span newStmt

-- | Constant folding for Python expressions
constantFoldPythonExpr :: Located PythonExpr -> OptimizationM (Located PythonExpr)
constantFoldPythonExpr (Located span expr) = do
  newExpr <- case expr of
    PyBinaryOp op left right -> do
      newLeft <- constantFoldPythonExpr left
      newRight <- constantFoldPythonExpr right
      
      -- Try to fold constants
      case (locatedValue newLeft, locatedValue newRight) of
        (PyLiteral leftLit, PyLiteral rightLit) -> do
          case constantFoldBinaryOp op leftLit rightLit of
            Just result -> do
              recordOptimization $ "Folded constant expression: " <> T.pack (show op)
              modify $ \s -> s { osChanged = True }
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
              modify $ \s -> s { osChanged = True }
              return $ PyLiteral result
            Nothing -> return $ PyUnaryOp op newOperand
        _ -> return $ PyUnaryOp op newOperand
        
    PyCall func args -> do
      newFunc <- constantFoldPythonExpr func
      newArgs <- mapM (mapM constantFoldPythonExpr . pyArgExpr) args
      let newPyArgs = zipWith (\oldArg newExpr -> case oldArg of
            ArgPositional _ -> ArgPositional newExpr
            ArgKeyword kw _ -> ArgKeyword kw newExpr
            ArgStarred _ -> ArgStarred newExpr) args newArgs
      return $ PyCall newFunc newPyArgs
      
    PyIndex container index -> do
      newContainer <- constantFoldPythonExpr container
      newIndex <- constantFoldPythonExpr index
      return $ PyIndex newContainer newIndex
      
    _ -> return expr
    
  return $ Located span newExpr

-- Helper to extract expression from argument
pyArgExpr :: PythonArgument -> Located PythonExpr
pyArgExpr (ArgPositional expr) = expr
pyArgExpr (ArgKeyword _ expr) = expr  
pyArgExpr (ArgStarred expr) = expr

-- | Constant folding for Go AST
constantFoldingGo :: GoAST -> OptimizationM GoAST
constantFoldingGo (GoPackage packageName imports decls) = do
  newDecls <- mapM constantFoldGoDecl decls
  return $ GoPackage packageName imports newDecls

-- | Constant folding for Go declarations
constantFoldGoDecl :: Located GoDecl -> OptimizationM (Located GoDecl)
constantFoldGoDecl (Located span decl) = do
  newDecl <- case decl of
    GoFunctionDecl name params results body -> do
      newBody <- constantFoldGoStmt body
      return $ GoFunctionDecl name params results newBody
    _ -> return decl
  return $ Located span newDecl

-- | Constant folding for Go statements  
constantFoldGoStmt :: Located GoStmt -> OptimizationM (Located GoStmt)
constantFoldGoStmt (Located span stmt) = do
  newStmt <- case stmt of
    GoExprStmt expr -> do
      newExpr <- constantFoldGoExpr expr
      return $ GoExprStmt newExpr
    GoAssignment lhs rhs -> do
      newRhs <- constantFoldGoExpr rhs
      return $ GoAssignment lhs newRhs
    GoIf condition thenStmt elseStmt -> do
      newCondition <- constantFoldGoExpr condition
      newThenStmt <- constantFoldGoStmt thenStmt  
      newElseStmt <- case elseStmt of
        Just elseS -> Just <$> constantFoldGoStmt elseS
        Nothing -> return Nothing
      return $ GoIf newCondition newThenStmt newElseStmt
    GoFor forClause body -> do
      newBody <- constantFoldGoStmt body
      return $ GoFor forClause newBody
    GoReturn mexpr -> do
      newExpr <- case mexpr of
        Just expr -> Just <$> constantFoldGoExpr expr
        Nothing -> return Nothing
      return $ GoReturn newExpr
    GoBlock stmts -> do
      newStmts <- mapM constantFoldGoStmt stmts
      return $ GoBlock newStmts
    _ -> return stmt
  return $ Located span newStmt

-- | Constant folding for Go expressions
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
              modify $ \s -> s { osChanged = True }
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
              modify $ \s -> s { osChanged = True }
              return $ GoLiteral result
            Nothing -> return $ GoUnaryOp op newOperand
        _ -> return $ GoUnaryOp op newOperand
        
    GoCall func args -> do
      newFunc <- constantFoldGoExpr func
      newArgs <- mapM constantFoldGoExpr args
      return $ GoCall newFunc newArgs
      
    _ -> return expr
    
  return $ Located span newExpr

-- | Fold binary operations on constants
constantFoldBinaryOp :: BinaryOp -> Literal -> Literal -> Maybe Literal
constantFoldBinaryOp OpAdd (LInt a) (LInt b) = Just $ LInt (a + b)
constantFoldBinaryOp OpAdd (LFloat a) (LFloat b) = Just $ LFloat (a + b)
constantFoldBinaryOp OpSub (LInt a) (LInt b) = Just $ LInt (a - b)
constantFoldBinaryOp OpSub (LFloat a) (LFloat b) = Just $ LFloat (a - b)
constantFoldBinaryOp OpMul (LInt a) (LInt b) = Just $ LInt (a * b)
constantFoldBinaryOp OpMul (LFloat a) (LFloat b) = Just $ LFloat (a * b)
constantFoldBinaryOp OpDiv (LFloat a) (LFloat b) | b /= 0 = Just $ LFloat (a / b)
constantFoldBinaryOp OpAnd (LBool a) (LBool b) = Just $ LBool (a && b)
constantFoldBinaryOp OpOr (LBool a) (LBool b) = Just $ LBool (a || b)
constantFoldBinaryOp _ _ _ = Nothing

-- | Fold unary operations on constants  
constantFoldUnaryOp :: UnaryOp -> Literal -> Maybe Literal
constantFoldUnaryOp OpNot (LBool b) = Just $ LBool (not b)
constantFoldUnaryOp OpNegate (LInt i) = Just $ LInt (-i)
constantFoldUnaryOp OpNegate (LFloat f) = Just $ LFloat (-f)
constantFoldUnaryOp _ _ = Nothing

-- | Fold Go binary operations on constants
constantFoldBinaryOpGo :: GoBinaryOp -> GoLiteral -> GoLiteral -> Maybe GoLiteral
constantFoldBinaryOpGo GoOpAdd (GoInt a) (GoInt b) = Just $ GoInt (a + b)
constantFoldBinaryOpGo GoOpAdd (GoFloat a) (GoFloat b) = Just $ GoFloat (a + b)
constantFoldBinaryOpGo GoOpSub (GoInt a) (GoInt b) = Just $ GoInt (a - b)
constantFoldBinaryOpGo GoOpSub (GoFloat a) (GoFloat b) = Just $ GoFloat (a - b)
constantFoldBinaryOpGo GoOpMul (GoInt a) (GoInt b) = Just $ GoInt (a * b)
constantFoldBinaryOpGo GoOpMul (GoFloat a) (GoFloat b) = Just $ GoFloat (a * b)
constantFoldBinaryOpGo GoOpDiv (GoFloat a) (GoFloat b) | b /= 0 = Just $ GoFloat (a / b)
constantFoldBinaryOpGo _ _ _ = Nothing

-- | Fold Go unary operations on constants
constantFoldUnaryOpGo :: GoUnaryOp -> GoLiteral -> Maybe GoLiteral
constantFoldUnaryOpGo GoOpNegate (GoInt i) = Just $ GoInt (-i)
constantFoldUnaryOpGo GoOpNegate (GoFloat f) = Just $ GoFloat (-f)
constantFoldUnaryOpGo _ _ = Nothing

-- | Dead code elimination pass (placeholder)
deadCodeEliminationPass :: Either PythonAST GoAST -> OptimizationM (Either PythonAST GoAST)
deadCodeEliminationPass ast = do
  recordOptimization "Dead code elimination analysis"
  return ast

-- | Constant propagation pass (placeholder)
constantPropagationPass :: Either PythonAST GoAST -> OptimizationM (Either PythonAST GoAST)
constantPropagationPass ast = do
  recordOptimization "Constant propagation analysis"  
  return ast

-- | Algebraic simplification pass (placeholder)
algebraicSimplificationPass :: Either PythonAST GoAST -> OptimizationM (Either PythonAST GoAST)
algebraicSimplificationPass ast = do
  recordOptimization "Algebraic simplification analysis"
  return ast

-- | Strength reduction pass (placeholder) 
strengthReductionPass :: Either PythonAST GoAST -> OptimizationM (Either PythonAST GoAST)
strengthReductionPass ast = do
  recordOptimization "Strength reduction analysis"
  return ast

-- | Common subexpression elimination pass (placeholder)
csePass :: Either PythonAST GoAST -> OptimizationM (Either PythonAST GoAST)  
csePass ast = do
  recordOptimization "Common subexpression elimination analysis"
  return ast

-- | Peephole optimization pass (placeholder)
peepholeOptimizationPass :: Either PythonAST GoAST -> OptimizationM (Either PythonAST GoAST)
peepholeOptimizationPass ast = do
  recordOptimization "Peephole optimization analysis"
  return ast

-- | Public interfaces for individual optimizations
constantFolding :: Either PythonAST GoAST -> OptimizationResult
constantFolding ast = runBasicOptimizations config ast
  where config = defaultConfig { bpcEnableDeadCodeElimination = False
                              , bpcEnableConstantPropagation = False  
                              , bpcEnableAlgebraicSimplification = False
                              , bpcEnablePeepholeOptimization = False
                              , bpcEnableCSE = False
                              , bpcEnableStrengthReduction = False
                              }

deadCodeElimination :: Either PythonAST GoAST -> OptimizationResult
deadCodeElimination ast = runBasicOptimizations config ast
  where config = defaultConfig { bpcEnableConstantFolding = False
                              , bpcEnableConstantPropagation = False
                              , bpcEnableAlgebraicSimplification = False
                              , bpcEnablePeepholeOptimization = False
                              , bpcEnableCSE = False
                              , bpcEnableStrengthReduction = False
                              }

constantPropagation :: Either PythonAST GoAST -> OptimizationResult  
constantPropagation ast = runBasicOptimizations config ast
  where config = defaultConfig { bpcEnableConstantFolding = False
                              , bpcEnableDeadCodeElimination = False
                              , bpcEnableAlgebraicSimplification = False
                              , bpcEnablePeepholeOptimization = False
                              , bpcEnableCSE = False
                              , bpcEnableStrengthReduction = False
                              }

algebraicSimplification :: Either PythonAST GoAST -> OptimizationResult
algebraicSimplification ast = runBasicOptimizations config ast
  where config = defaultConfig { bpcEnableConstantFolding = False
                              , bpcEnableDeadCodeElimination = False
                              , bpcEnableConstantPropagation = False
                              , bpcEnablePeepholeOptimization = False
                              , bpcEnableCSE = False
                              , bpcEnableStrengthReduction = False
                              }

peepholeOptimization :: Either PythonAST GoAST -> OptimizationResult
peepholeOptimization ast = runBasicOptimizations config ast
  where config = defaultConfig { bpcEnableConstantFolding = False
                              , bpcEnableDeadCodeElimination = False
                              , bpcEnableConstantPropagation = False
                              , bpcEnableAlgebraicSimplification = False
                              , bpcEnableCSE = False
                              , bpcEnableStrengthReduction = False
                              }

commonSubexpressionElimination :: Either PythonAST GoAST -> OptimizationResult
commonSubexpressionElimination ast = runBasicOptimizations config ast
  where config = defaultConfig { bpcEnableConstantFolding = False
                              , bpcEnableDeadCodeElimination = False
                              , bpcEnableConstantPropagation = False
                              , bpcEnableAlgebraicSimplification = False
                              , bpcEnablePeepholeOptimization = False
                              , bpcEnableStrengthReduction = False
                              }

strengthReduction :: Either PythonAST GoAST -> OptimizationResult
strengthReduction ast = runBasicOptimizations config ast
  where config = defaultConfig { bpcEnableConstantFolding = False
                              , bpcEnableDeadCodeElimination = False
                              , bpcEnableConstantPropagation = False
                              , bpcEnableAlgebraicSimplification = False
                              , bpcEnablePeepholeOptimization = False
                              , bpcEnableCSE = False
                              }

-- | Record an optimization for reporting
recordOptimization :: Text -> OptimizationM ()
recordOptimization opt = do
  modify $ \s -> s { osOptimizations = opt : osOptimizations s }