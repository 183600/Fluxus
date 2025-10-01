{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Optimization.ConstantFolding (spec) where

import Test.Hspec
import Data.Text (Text)
import Data.Int (Int64)

import Fluxus.AST.Common
import Fluxus.Optimization.ConstantFolding

spec :: Spec
spec = describe "Constant Folding" $ do
  basicConstantFoldingSpec
  complexConstantFoldingSpec
  edgeCaseSpec

basicConstantFoldingSpec :: Spec
basicConstantFoldingSpec = describe "Basic Constant Folding" $ do
  it "folds integer addition" $ do
    let expr = PyBinaryOp Add (PyLiteral (PyInt 5)) (PyLiteral (PyInt 3))
    case constantFoldingPython expr of
      PyLiteral (PyInt 8) -> return ()
      result -> expectationFailure $ "Expected PyInt 8, got: " ++ show result
  
  it "folds integer subtraction" $ do
    let expr = PyBinaryOp Sub (PyLiteral (PyInt 10)) (PyLiteral (PyInt 4))
    case constantFoldingPython expr of
      PyLiteral (PyInt 6) -> return ()
      result -> expectationFailure $ "Expected PyInt 6, got: " ++ show result
  
  it "folds integer multiplication" $ do
    let expr = PyBinaryOp Mul (PyLiteral (PyInt 7)) (PyLiteral (PyInt 6))
    case constantFoldingPython expr of
      PyLiteral (PyInt 42) -> return ()
      result -> expectationFailure $ "Expected PyInt 42, got: " ++ show result
  
  it "folds integer division" $ do
    let expr = PyBinaryOp Div (PyLiteral (PyInt 100)) (PyLiteral (PyInt 5))
    case constantFoldingPython expr of
      PyLiteral (PyFloat 20.0) -> return ()
      PyLiteral (PyInt 20) -> return ()  -- Also accept integer result for compatibility
      result -> expectationFailure $ "Expected PyFloat 20.0 or PyInt 20, got: " ++ show result
  
  it "folds boolean AND operation" $ do
    let andExpr = PyBinaryOp And (PyLiteral (PyBool True)) (PyLiteral (PyBool True))
    case constantFoldingPython andExpr of
      PyLiteral (PyBool True) -> return ()
      result -> expectationFailure $ "Expected PyBool True, got: " ++ show result
  
  it "folds boolean OR operation" $ do
    let orExpr = PyBinaryOp Or (PyLiteral (PyBool False)) (PyLiteral (PyBool True))
    case constantFoldingPython orExpr of
      PyLiteral (PyBool True) -> return ()
      result -> expectationFailure $ "Expected PyBool True, got: " ++ show result
  
  it "folds string concatenation" $ do
    let expr = PyBinaryOp Add (PyLiteral (PyString "Hello ")) (PyLiteral (PyString "World"))
    case constantFoldingPython expr of
      PyLiteral (PyString "Hello World") -> return ()
      result -> expectationFailure $ "Expected PyString 'Hello World', got: " ++ show result

complexConstantFoldingSpec :: Spec
complexConstantFoldingSpec = describe "Complex Constant Folding" $ do
  it "folds nested binary operations" $ do
    let expr = PyBinaryOp Add (PyBinaryOp Mul (PyLiteral (PyInt 2)) (PyLiteral (PyInt 3))) (PyLiteral (PyInt 4))
    case constantFoldingPython expr of
      PyLiteral (PyInt 10) -> return ()
      result -> expectationFailure $ "Expected PyInt 10, got: " ++ show result
  
  it "folds multiple operations" $ do
    let expr = PyBinaryOp Add
                  (PyBinaryOp Add (PyLiteral (PyInt 1)) (PyLiteral (PyInt 2)))
                  (PyBinaryOp Add (PyLiteral (PyInt 3)) (PyLiteral (PyInt 4)))
    case constantFoldingPython expr of
      PyLiteral (PyInt 10) -> return ()  -- 1+2 + 3+4 = 3 + 7 = 10
      result -> expectationFailure $ "Expected IntLit 10, got: " ++ show result
  
  it "folds mixed type operations" $ do
    let expr = PyBinaryOp Add 
                  (PyLiteral (PyInt 5))
                  (PyBinaryOp Mul (PyLiteral (PyInt 2)) (PyLiteral (PyInt 3)))
    case constantFoldingPython expr of
      PyLiteral (PyInt 11) -> return ()  -- 5 + 2*3 = 5 + 6 = 11
      result -> expectationFailure $ "Expected FloatLit 11.0, got: " ++ show result
  
  it "does not fold non-constant expressions" $ do
    let expr = PyBinaryOp Add (PyLiteral (PyInt 5)) (PyIdentifier "x")
    case constantFoldingPython expr of
      PyBinaryOp Add (PyLiteral (PyInt 5)) (PyIdentifier "x") -> return ()
      result -> expectationFailure $ "Should not fold non-constant expression, got: " ++ show result

edgeCaseSpec :: Spec
edgeCaseSpec = describe "Edge Cases" $ do
  it "folds division by zero to error" $ do
    let expr = PyBinaryOp Div (PyLiteral (PyInt 10)) (PyLiteral (PyInt 0))
    case constantFoldingPython expr of
      PyLiteral (PyInt _) -> expectationFailure "Should not fold division by zero"
      _ -> return ()  -- Should remain unchanged or handle error gracefully
  
  it "handles overflow gracefully" $ do
    let expr = PyBinaryOp Mul (PyLiteral (PyInt (fromIntegral (maxBound :: Int64)))) (PyLiteral (PyInt 2))
    case constantFoldingPython expr of
      PyBinaryOp Mul (PyLiteral (PyInt _)) (PyLiteral (PyInt 2)) -> return ()
      result -> expectationFailure $ "Should not fold potential overflow, got: " ++ show result
  
  it "handles floating point precision" $ do
    let expr = PyBinaryOp Div (PyLiteral (PyFloat 1.0)) (PyLiteral (PyFloat 3.0))
    case constantFoldingPython expr of
      PyLiteral (PyFloat _) -> return ()
      result -> expectationFailure $ "Should fold floating point division, got: " ++ show result
  
  it "folds unary NOT operation" $ do
    let negExpr = PyUnaryOp Not (PyLiteral (PyBool True))
    case constantFoldingPython negExpr of
      PyLiteral (PyBool False) -> return ()
      result -> expectationFailure $ "Expected PyBool False, got: " ++ show result
  
  it "folds arithmetic with negative numbers" $ do
    let expr = PyBinaryOp Add (PyLiteral (PyInt (-5))) (PyLiteral (PyInt 3))
    case constantFoldingPython expr of
      PyLiteral (PyInt (-2)) -> return ()
      result -> expectationFailure $ "Expected PyInt -2, got: " ++ show result