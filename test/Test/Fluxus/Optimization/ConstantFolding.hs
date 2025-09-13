{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Optimization.ConstantFolding (spec) where

import Test.Hspec
import Data.Text (Text)

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
    let expr = BinaryOp "+" (Literal (IntLit 5)) (Literal (IntLit 3))
    case foldConstants expr of
      Literal (IntLit 8) -> return ()
      result -> expectationFailure $ "Expected IntLit 8, got: " ++ show result
  
  it "folds integer subtraction" $ do
    let expr = BinaryOp "-" (Literal (IntLit 10)) (Literal (IntLit 4))
    case foldConstants expr of
      Literal (IntLit 6) -> return ()
      result -> expectationFailure $ "Expected IntLit 6, got: " ++ show result
  
  it "folds integer multiplication" $ do
    let expr = BinaryOp "*" (Literal (IntLit 7)) (Literal (IntLit 6))
    case foldConstants expr of
      Literal (IntLit 42) -> return ()
      result -> expectationFailure $ "Expected IntLit 42, got: " ++ show result
  
  it "folds integer division" $ do
    let expr = BinaryOp "/" (Literal (IntLit 100)) (Literal (IntLit 5))
    case foldConstants expr of
      Literal (IntLit 20) -> return ()
      result -> expectationFailure $ "Expected IntLit 20, got: " ++ show result
  
  it "folds boolean operations" $ do
    let andExpr = BinaryOp "&&" (Literal (BoolLit True)) (Literal (BoolLit True))
    let orExpr = BinaryOp "||" (Literal (BoolLit False)) (Literal (BoolLit True))
    
    case foldConstants andExpr of
      Literal (BoolLit True) -> return ()
      result -> expectationFailure $ "Expected BoolLit True, got: " ++ show result
      
    case foldConstants orExpr of
      Literal (BoolLit True) -> return ()
      result -> expectationFailure $ "Expected BoolLit True, got: " ++ show result
  
  it "folds string concatenation" $ do
    let expr = BinaryOp "++" (Literal (StringLit "Hello ")) (Literal (StringLit "World"))
    case foldConstants expr of
      Literal (StringLit "Hello World") -> return ()
      result -> expectationFailure $ "Expected StringLit \"Hello World\", got: " ++ show result

complexConstantFoldingSpec :: Spec
complexConstantFoldingSpec = describe "Complex Constant Folding" $ do
  it "folds nested expressions" $ do
    let expr = BinaryOp "+" 
                  (BinaryOp "*" (Literal (IntLit 2)) (Literal (IntLit 3)))
                  (BinaryOp "/" (Literal (IntLit 20)) (Literal (IntLit 4)))
    case foldConstants expr of
      Literal (IntLit 11) -> return ()  -- 2*3 + 20/4 = 6 + 5 = 11
      result -> expectationFailure $ "Expected IntLit 11, got: " ++ show result
  
  it "folds multiple operations" $ do
    let expr = BinaryOp "+" 
                  (BinaryOp "+" (Literal (IntLit 1)) (Literal (IntLit 2)))
                  (BinaryOp "+" (Literal (IntLit 3)) (Literal (IntLit 4)))
    case foldConstants expr of
      Literal (IntLit 10) -> return ()  -- 1+2 + 3+4 = 3 + 7 = 10
      result -> expectationFailure $ "Expected IntLit 10, got: " ++ show result
  
  it "folds mixed type operations" $ do
    let expr = BinaryOp "+" 
                  (Literal (IntLit 5))
                  (BinaryOp "*" (Literal (FloatLit 2.0)) (Literal (FloatLit 3.0)))
    case foldConstants expr of
      Literal (FloatLit 11.0) -> return ()  -- 5 + 2.0*3.0 = 5 + 6.0 = 11.0
      result -> expectationFailure $ "Expected FloatLit 11.0, got: " ++ show result
  
  it "preserves non-constant subexpressions" $ do
    let expr = BinaryOp "+" (Literal (IntLit 5)) (Variable "x")
    case foldConstants expr of
      BinaryOp "+" (Literal (IntLit 5)) (Variable "x") -> return ()
      result -> expectationFailure $ "Expected partially folded expression, got: " ++ show result

edgeCaseSpec :: Spec
edgeCaseSpec = describe "Edge Cases" $ do
  it "handles division by zero" $ do
    let expr = BinaryOp "/" (Literal (IntLit 10)) (Literal (IntLit 0))
    case foldConstants expr of
      BinaryOp "/" (Literal (IntLit 10)) (Literal (IntLit 0)) -> return ()
      result -> expectationFailure $ "Should not fold division by zero, got: " ++ show result
  
  it "handles overflow gracefully" $ do
    let expr = BinaryOp "*" (Literal (IntLit maxBound)) (Literal (IntLit 2))
    case foldConstants expr of
      BinaryOp "*" (Literal (IntLit maxBound)) (Literal (IntLit 2)) -> return ()
      result -> expectationFailure $ "Should not fold potential overflow, got: " ++ show result
  
  it "handles floating point precision" $ do
    let expr = BinaryOp "/" (Literal (FloatLit 1.0)) (Literal (FloatLit 3.0))
    case foldConstants expr of
      Literal (FloatLit _) -> return ()
      result -> expectationFailure $ "Should fold floating point division, got: " ++ show result
  
  it "handles unary operations" $ do
    let negExpr = UnaryOp "-" (Literal (IntLit 42))
    let notExpr = UnaryOp "!" (Literal (BoolLit True))
    
    case foldConstants negExpr of
      Literal (IntLit (-42)) -> return ()
      result -> expectationFailure $ "Expected IntLit -42, got: " ++ show result
      
    case foldConstants notExpr of
      Literal (BoolLit False) -> return ()
      result -> expectationFailure $ "Expected BoolLit False, got: " ++ show result