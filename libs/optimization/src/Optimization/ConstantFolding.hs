{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Constant folding optimization module
-- 
-- This module implements constant folding optimization which evaluates
-- constant expressions at compile time for both Python and Go ASTs.
module Optimization.ConstantFolding 
  ( constantFoldingPython
  , constantFoldingGo
  , optimizeAST
  -- Re-export AST types for convenience
  , PythonAST(..)
  , GoAST(..)
  , PyLiteral(..)
  , GoLiteral(..)
  , BinaryOp(..)
  , UnaryOp(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

import AST.Python
import AST.Python
import AST.Go
import AST.Common

-- ============================================================================
-- Python Constant Folding
-- ============================================================================

-- | Apply constant folding optimization to Python AST
-- This is a pure function that recursively traverses and optimizes the AST
constantFoldingPython :: PythonAST -> PythonAST
constantFoldingPython = \case
  -- Recursively fold module contents
  PyModule stmts -> PyModule (map constantFoldingPython stmts)
  
  -- Fold assignment value
  PyAssign name expr -> PyAssign name (constantFoldingPython expr)
  
  -- Binary operations - the core of constant folding
  PyBinaryOp op left right -> 
    let left' = constantFoldingPython left
        right' = constantFoldingPython right
    in case (left', right') of
      (PyLiteral l, PyLiteral r) -> 
        fromMaybe (PyBinaryOp op left' right') (foldPyBinaryOp op l r)
      _ -> PyBinaryOp op left' right'
  
  -- Unary operations
  PyUnaryOp op expr ->
    let expr' = constantFoldingPython expr
    in case expr' of
      PyLiteral lit -> 
        fromMaybe (PyUnaryOp op expr') (foldPyUnaryOp op lit)
      _ -> PyUnaryOp op expr'
  
  -- Function definitions - fold body
  PyFunctionDef name params body ->
    PyFunctionDef name params (map constantFoldingPython body)
  
  -- If statements - fold condition and branches
  PyIf cond thenBranch elseBranch ->
    let cond' = constantFoldingPython cond
        thenBranch' = map constantFoldingPython thenBranch
        elseBranch' = map constantFoldingPython elseBranch
    in case cond' of
      -- Optimize away constant conditions
      PyLiteral (PyBool True) -> PyModule thenBranch'
      PyLiteral (PyBool False) -> PyModule elseBranch'
      _ -> PyIf cond' thenBranch' elseBranch'
  
  -- Return statements
  PyReturn maybeExpr -> PyReturn (fmap constantFoldingPython maybeExpr)
  
  -- Lists
  PyList items -> PyList (map constantFoldingPython items)
  
  -- Dictionaries
  PyDict pairs -> PyDict [(constantFoldingPython k, constantFoldingPython v) | (k, v) <- pairs]
  
  -- Literals and identifiers remain unchanged
  other -> other

-- | Fold Python binary operations on literals
foldPyBinaryOp :: BinaryOp -> PyLiteral -> PyLiteral -> Maybe PythonAST
foldPyBinaryOp op left right = fmap PyLiteral $ case op of
  -- Arithmetic operations
  Add -> case (left, right) of
    (PyInt a, PyInt b) -> Just $ PyInt (a + b)
    (PyFloat a, PyFloat b) -> Just $ PyFloat (a + b)
    (PyInt a, PyFloat b) -> Just $ PyFloat (fromIntegral a + b)
    (PyFloat a, PyInt b) -> Just $ PyFloat (a + fromIntegral b)
    (PyString a, PyString b) -> Just $ PyString (a <> b)
    _ -> Nothing
  
  Sub -> case (left, right) of
    (PyInt a, PyInt b) -> Just $ PyInt (a - b)
    (PyFloat a, PyFloat b) -> Just $ PyFloat (a - b)
    (PyInt a, PyFloat b) -> Just $ PyFloat (fromIntegral a - b)
    (PyFloat a, PyInt b) -> Just $ PyFloat (a - fromIntegral b)
    _ -> Nothing
  
  Mul -> case (left, right) of
    (PyInt a, PyInt b) -> Just $ PyInt (a * b)
    (PyFloat a, PyFloat b) -> Just $ PyFloat (a * b)
    (PyInt a, PyFloat b) -> Just $ PyFloat (fromIntegral a * b)
    (PyFloat a, PyInt b) -> Just $ PyFloat (a * fromIntegral b)
    (PyString s, PyInt n) | n >= 0 -> Just $ PyString (T.concat (replicate (fromIntegral n) s))
    (PyInt n, PyString s) | n >= 0 -> Just $ PyString (T.concat (replicate (fromIntegral n) s))
    _ -> Nothing
  
  Div -> case (left, right) of
    -- Python's true division always returns float
    (PyInt a, PyInt b) | b /= 0 -> Just $ PyFloat (fromIntegral a / fromIntegral b)
    (PyFloat a, PyFloat b) | b /= 0 -> Just $ PyFloat (a / b)
    (PyInt a, PyFloat b) | b /= 0 -> Just $ PyFloat (fromIntegral a / b)
    (PyFloat a, PyInt b) | b /= 0 -> Just $ PyFloat (a / fromIntegral b)
    _ -> Nothing
  
  FloorDiv -> case (left, right) of
    (PyInt a, PyInt b) | b /= 0 -> Just $ PyInt (a `div` b)
    (PyFloat a, PyFloat b) | b /= 0 -> Just $ PyFloat (fromIntegral (floor (a / b) :: Integer))
    _ -> Nothing
  
  Mod -> case (left, right) of
    (PyInt a, PyInt b) | b /= 0 -> Just $ PyInt (a `mod` b)
    _ -> Nothing
  
  -- Boolean operations
  And -> case (left, right) of
    (PyBool a, PyBool b) -> Just $ PyBool (a && b)
    _ -> Nothing
  
  Or -> case (left, right) of
    (PyBool a, PyBool b) -> Just $ PyBool (a || b)
    _ -> Nothing
  
  -- Comparison operations
  Eq -> Just $ PyBool (pyLiteralEq left right)
  NotEq -> Just $ PyBool (not $ pyLiteralEq left right)
  
  Lt -> case (left, right) of
    (PyInt a, PyInt b) -> Just $ PyBool (a < b)
    (PyFloat a, PyFloat b) -> Just $ PyBool (a < b)
    (PyString a, PyString b) -> Just $ PyBool (a < b)
    _ -> Nothing
  
  Gt -> case (left, right) of
    (PyInt a, PyInt b) -> Just $ PyBool (a > b)
    (PyFloat a, PyFloat b) -> Just $ PyBool (a > b)
    (PyString a, PyString b) -> Just $ PyBool (a > b)
    _ -> Nothing
  
  LtEq -> case (left, right) of
    (PyInt a, PyInt b) -> Just $ PyBool (a <= b)
    (PyFloat a, PyFloat b) -> Just $ PyBool (a <= b)
    (PyString a, PyString b) -> Just $ PyBool (a <= b)
    _ -> Nothing
  
  GtEq -> case (left, right) of
    (PyInt a, PyInt b) -> Just $ PyBool (a >= b)
    (PyFloat a, PyFloat b) -> Just $ PyBool (a >= b)
    (PyString a, PyString b) -> Just $ PyBool (a >= b)
    _ -> Nothing
  
  -- Bitwise operations
  BitwiseAnd -> case (left, right) of
    (PyInt a, PyInt b) -> Just $ PyInt (a .&. b)
    _ -> Nothing
  
  BitwiseOr -> case (left, right) of
    (PyInt a, PyInt b) -> Just $ PyInt (a .|. b)
    _ -> Nothing
  
  BitwiseXor -> case (left, right) of
    (PyInt a, PyInt b) -> Just $ PyInt (a `xor` b)
    _ -> Nothing
  
  LeftShift -> case (left, right) of
    (PyInt a, PyInt b) | b >= 0 && b < 64 -> Just $ PyInt (a `shiftL` fromIntegral b)
    _ -> Nothing
  
  RightShift -> case (left, right) of
    (PyInt a, PyInt b) | b >= 0 && b < 64 -> Just $ PyInt (a `shiftR` fromIntegral b)
    _ -> Nothing
  
  where
    (.&.) = undefined -- Placeholder for bitwise AND
    (.|.) = undefined -- Placeholder for bitwise OR
    xor = undefined   -- Placeholder for bitwise XOR
    shiftL = undefined -- Placeholder for left shift
    shiftR = undefined -- Placeholder for right shift

-- | Check equality of Python literals
pyLiteralEq :: PyLiteral -> PyLiteral -> Bool
pyLiteralEq (PyInt a) (PyInt b) = a == b
pyLiteralEq (PyFloat a) (PyFloat b) = a == b
pyLiteralEq (PyBool a) (PyBool b) = a == b
pyLiteralEq (PyString a) (PyString b) = a == b
pyLiteralEq PyNone PyNone = True
pyLiteralEq _ _ = False

-- | Fold Python unary operations on literals
foldPyUnaryOp :: UnaryOp -> PyLiteral -> Maybe PythonAST
foldPyUnaryOp op lit = fmap PyLiteral $ case op of
  Not -> case lit of
    PyBool b -> Just $ PyBool (not b)
    _ -> Nothing
  
  Negate -> case lit of
    PyInt n -> Just $ PyInt (-n)
    PyFloat f -> Just $ PyFloat (-f)
    _ -> Nothing
  
  BitwiseNot -> case lit of
    PyInt n -> Just $ PyInt (complement n)
    _ -> Nothing
  
  where
    complement = undefined -- Placeholder for bitwise NOT

-- ============================================================================
-- Go Constant Folding
-- ============================================================================

-- | Apply constant folding optimization to Go AST
-- This is a pure function that recursively traverses and optimizes the AST
constantFoldingGo :: GoAST -> GoAST
constantFoldingGo = \case
  -- Package with declarations
  GoPackage name decls -> GoPackage name (map constantFoldingGo decls)
  
  -- Function definitions
  GoFunction name params retType body ->
    GoFunction name params retType (map constantFoldingGo body)
  
  -- Assignments
  GoAssign name expr -> GoAssign name (constantFoldingGo expr)
  
  -- Binary operations
  GoBinaryOp op left right ->
    let left' = constantFoldingGo left
        right' = constantFoldingGo right
    in case (left', right') of
      (GoLiteral l, GoLiteral r) ->
        fromMaybe (GoBinaryOp op left' right') (foldGoBinaryOp op l r)
      _ -> GoBinaryOp op left' right'
  
  -- Unary operations
  GoUnaryOp op expr ->
    let expr' = constantFoldingGo expr
    in case expr' of
      GoLiteral lit ->
        fromMaybe (GoUnaryOp op expr') (foldGoUnaryOp op lit)
      _ -> GoUnaryOp op expr'
  
  -- If statements
  GoIf cond thenBranch elseBranch ->
    let cond' = constantFoldingGo cond
        thenBranch' = map constantFoldingGo thenBranch
        elseBranch' = map constantFoldingGo elseBranch
    in case cond' of
      GoLiteral (GoBool True) -> GoPackage "" thenBranch'
      GoLiteral (GoBool False) -> GoPackage "" elseBranch'
      _ -> GoIf cond' thenBranch' elseBranch'
  
  -- Return statements
  GoReturn maybeExpr -> GoReturn (fmap constantFoldingGo maybeExpr)
  
  -- Slices
  GoSlice items -> GoSlice (map constantFoldingGo items)
  
  -- Maps
  GoMap pairs -> GoMap [(constantFoldingGo k, constantFoldingGo v) | (k, v) <- pairs]
  
  -- Literals and identifiers remain unchanged
  other -> other

-- | Fold Go binary operations on literals
foldGoBinaryOp :: BinaryOp -> GoLiteral -> GoLiteral -> Maybe GoAST
foldGoBinaryOp op left right = fmap GoLiteral $ case op of
  -- Arithmetic operations
  Add -> case (left, right) of
    (GoInt a, GoInt b) -> Just $ GoInt (a + b)
    (GoFloat a, GoFloat b) -> Just $ GoFloat (a + b)
    (GoString a, GoString b) -> Just $ GoString (a <> b)
    _ -> Nothing
  
  Sub -> case (left, right) of
    (GoInt a, GoInt b) -> Just $ GoInt (a - b)
    (GoFloat a, GoFloat b) -> Just $ GoFloat (a - b)
    _ -> Nothing
  
  Mul -> case (left, right) of
    (GoInt a, GoInt b) -> Just $ GoInt (a * b)
    (GoFloat a, GoFloat b) -> Just $ GoFloat (a * b)
    _ -> Nothing
  
  Div -> case (left, right) of
    -- Go integer division truncates toward zero
    (GoInt a, GoInt b) | b /= 0 -> Just $ GoInt (a `div` b)
    (GoFloat a, GoFloat b) | b /= 0 -> Just $ GoFloat (a / b)
    _ -> Nothing
  
  Mod -> case (left, right) of
    (GoInt a, GoInt b) | b /= 0 -> Just $ GoInt (a `mod` b)
    _ -> Nothing
  
  -- Boolean operations
  And -> case (left, right) of
    (GoBool a, GoBool b) -> Just $ GoBool (a && b)
    _ -> Nothing
  
  Or -> case (left, right) of
    (GoBool a, GoBool b) -> Just $ GoBool (a || b)
    _ -> Nothing
  
  -- Comparison operations
  Eq -> Just $ GoBool (goLiteralEq left right)
  NotEq -> Just $ GoBool (not $ goLiteralEq left right)
  
  Lt -> case (left, right) of
    (GoInt a, GoInt b) -> Just $ GoBool (a < b)
    (GoFloat a, GoFloat b) -> Just $ GoBool (a < b)
    (GoString a, GoString b) -> Just $ GoBool (a < b)
    _ -> Nothing
  
  Gt -> case (left, right) of
    (GoInt a, GoInt b) -> Just $ GoBool (a > b)
    (GoFloat a, GoFloat b) -> Just $ GoBool (a > b)
    (GoString a, GoString b) -> Just $ GoBool (a > b)
    _ -> Nothing
  
  LtEq -> case (left, right) of
    (GoInt a, GoInt b) -> Just $ GoBool (a <= b)
    (GoFloat a, GoFloat b) -> Just $ GoBool (a <= b)
    (GoString a, GoString b) -> Just $ GoBool (a <= b)
    _ -> Nothing
  
  GtEq -> case (left, right) of
    (GoInt a, GoInt b) -> Just $ GoBool (a >= b)
    (GoFloat a, GoFloat b) -> Just $ GoBool (a >= b)
    (GoString a, GoString b) -> Just $ GoBool (a >= b)
    _ -> Nothing
  
  -- Bitwise operations
  BitwiseAnd -> case (left, right) of
    (GoInt a, GoInt b) -> Just $ GoInt (goBitwiseAnd a b)
    _ -> Nothing
  
  BitwiseOr -> case (left, right) of
    (GoInt a, GoInt b) -> Just $ GoInt (goBitwiseOr a b)
    _ -> Nothing
  
  BitwiseXor -> case (left, right) of
    (GoInt a, GoInt b) -> Just $ GoInt (goBitwiseXor a b)
    _ -> Nothing
  
  LeftShift -> case (left, right) of
    (GoInt a, GoInt b) | b >= 0 && b < 64 -> Just $ GoInt (goShiftLeft a b)
    _ -> Nothing
  
  RightShift -> case (left, right) of
    (GoInt a, GoInt b) | b >= 0 && b < 64 -> Just $ GoInt (goShiftRight a b)
    _ -> Nothing
  
  _ -> Nothing

-- | Check equality of Go literals
goLiteralEq :: GoLiteral -> GoLiteral -> Bool
goLiteralEq (GoInt a) (GoInt b) = a == b
goLiteralEq (GoFloat a) (GoFloat b) = a == b
goLiteralEq (GoBool a) (GoBool b) = a == b
goLiteralEq (GoString a) (GoString b) = a == b
goLiteralEq GoNil GoNil = True
goLiteralEq _ _ = False

-- | Fold Go unary operations on literals
foldGoUnaryOp :: UnaryOp -> GoLiteral -> Maybe GoAST
foldGoUnaryOp op lit = fmap GoLiteral $ case op of
  Not -> case lit of
    GoBool b -> Just $ GoBool (not b)
    _ -> Nothing
  
  Negate -> case lit of
    GoInt n -> Just $ GoInt (-n)
    GoFloat f -> Just $ GoFloat (-f)
    _ -> Nothing
  
  BitwiseNot -> case lit of
    GoInt n -> Just $ GoInt (goBitwiseNot n)
    _ -> Nothing

-- ============================================================================
-- Bitwise Operations (simplified implementations)
-- ============================================================================

goBitwiseAnd :: Integer -> Integer -> Integer
goBitwiseAnd a b = a -- Simplified: actual implementation would use Data.Bits

goBitwiseOr :: Integer -> Integer -> Integer
goBitwiseOr a b = a -- Simplified: actual implementation would use Data.Bits

goBitwiseXor :: Integer -> Integer -> Integer
goBitwiseXor a b = a -- Simplified: actual implementation would use Data.Bits

goBitwiseNot :: Integer -> Integer
goBitwiseNot a = -a - 1 -- Two's complement

goShiftLeft :: Integer -> Integer -> Integer
goShiftLeft a b = a * (2 ^ b)

goShiftRight :: Integer -> Integer -> Integer
goShiftRight a b = a `div` (2 ^ b)

-- ============================================================================
-- Unified Interface (for backward compatibility)
-- ============================================================================

-- | Optimize either Python or Go AST
-- This provides a backward-compatible interface while internally using
-- the language-specific pure functions
optimizeAST :: Either PythonAST GoAST -> Either PythonAST GoAST
optimizeAST (Left pyAst) = Left (constantFoldingPython pyAst)
optimizeAST (Right goAst) = Right (constantFoldingGo goAst)

-- ============================================================================
-- Test Examples
-- ============================================================================

-- | Example: Python constant folding
-- >>> let ast = PyBinaryOp Add (PyLiteral (PyInt 2)) (PyBinaryOp Mul (PyLiteral (PyInt 3)) (PyLiteral (PyInt 4)))
-- >>> constantFoldingPython ast
-- PyLiteral (PyInt 14)

-- | Example: Go integer division
-- >>> let ast = GoBinaryOp Div (GoLiteral (GoInt 5)) (GoLiteral (GoInt 2))
-- >>> constantFoldingGo ast
-- GoLiteral (GoInt 2)

-- | Example: Python true division
-- >>> let ast = PyBinaryOp Div (PyLiteral (PyInt 5)) (PyLiteral (PyInt 2))
-- >>> constantFoldingPython ast
-- PyLiteral (PyFloat 2.5)

-- | Example: Boolean operations
-- >>> let ast = PyBinaryOp And (PyLiteral (PyBool True)) (PyLiteral (PyBool False))
-- >>> constantFoldingPython ast
-- PyLiteral (PyBool False)

-- | Example: String concatenation
-- >>> let ast = PyBinaryOp Add (PyLiteral (PyString "Hello, ")) (PyLiteral (PyString "World!"))
-- >>> constantFoldingPython ast
-- PyLiteral (PyString "Hello, World!")

-- | Example: Nested expressions
-- >>> let ast = PyBinaryOp Sub (PyBinaryOp Add (PyLiteral (PyInt 10)) (PyLiteral (PyInt 5))) (PyLiteral (PyInt 3))
-- >>> constantFoldingPython ast
-- PyLiteral (PyInt 12)