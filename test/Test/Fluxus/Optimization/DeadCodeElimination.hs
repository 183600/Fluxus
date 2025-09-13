{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Optimization.DeadCodeElimination (spec) where

import Test.Hspec
import Data.Text (Text)

import Fluxus.AST.Common
import Fluxus.Optimization.DeadCodeElimination

spec :: Spec
spec = describe "Dead Code Elimination" $ do
  basicDeadCodeEliminationSpec
  controlFlowSpec
  functionOptimizationSpec

basicDeadCodeEliminationSpec :: Spec
basicDeadCodeEliminationSpec = describe "Basic Dead Code Elimination" $ do
  it "eliminates unreachable statements after return" $ do
    let stmts = [
          Return (Just (Literal (IntLit 0))),
          Assignment "x" (Literal (IntLit 42)),
          ExprStmt (Call (Variable "printf") [Literal (StringLit "Hello")])
          ]
    case eliminateDeadCode stmts of
      [Return (Just (Literal (IntLit 0)))] -> return ()
      result -> expectationFailure $ "Expected only return statement, got: " ++ show result
  
  it "eliminates unreachable code in if branches" $ do
    let ifStmt = If 
          (Literal (BoolLit True))
          [Return (Just (Literal (IntLit 1))), Assignment "x" (Literal (IntLit 2))]
          [Return (Just (Literal (IntLit 3))), Assignment "y" (Literal (IntLit 4))]
    
    case eliminateDeadCode [ifStmt] of
      [If (Literal (BoolLit True)) 
          [Return (Just (Literal (IntLit 1)))] 
          [Return (Just (Literal (IntLit 3)))]] -> return ()
      result -> expectationFailure $ "Expected if with optimized branches, got: " ++ show result
  
  it "eliminates unused variable assignments" $ do
    let stmts = [
          Assignment "unused_var" (Literal (IntLit 42)),
          Assignment "used_var" (Literal (IntLit 100)),
          Return (Just (Variable "used_var"))
          ]
    case eliminateDeadCode stmts of
      [Assignment "used_var" (Literal (IntLit 100)), Return (Just (Variable "used_var"))] -> return ()
      result -> expectationFailure $ "Expected only used variable assignment, got: " ++ show result

controlFlowSpec :: Spec
controlFlowSpec = describe "Control Flow Optimization" $ do
  it "eliminates redundant if statements" $ do
    let ifStmt = If (Literal (BoolLit True)) [Return (Just (Literal (IntLit 1)))] []
    case eliminateDeadCode [ifStmt] of
      [Return (Just (Literal (IntLit 1)))] -> return ()
      result -> expectationFailure $ "Expected return statement without if, got: " ++ show result
  
  it "eliminates always-false if statements" $ do
    let ifStmt = If (Literal (BoolLit False)) [Assignment "x" (Literal (IntLit 1))] []
    case eliminateDeadCode [ifStmt] of
      [] -> return ()
      result -> expectationFailure $ "Expected empty statement list, got: " ++ show result
  
  it "optimizes if-else with constant condition" $ do
    let ifStmt = If 
          (Literal (BoolLit False))
          [Assignment "x" (Literal (IntLit 1))]
          [Assignment "y" (Literal (IntLit 2))]
    case eliminateDeadCode [ifStmt] of
      [Assignment "y" (Literal (IntLit 2))] -> return ()
      result -> expectationFailure $ "Expected else branch only, got: " ++ show result
  
  it "eliminates empty loops" $ do
    let whileStmt = While (Literal (BoolLit True)) []
    case eliminateDeadCode [whileStmt] of
      [] -> return ()
      result -> expectationFailure $ "Expected empty statement list, got: " ++ show result
  
  it "eliminates unreachable loop bodies" $ do
    let whileStmt = While (Literal (BoolLit False)) [Assignment "x" (Literal (IntLit 1))]
    case eliminateDeadCode [whileStmt] of
      [] -> return ()
      result -> expectationFailure $ "Expected empty statement list, got: " ++ show result

functionOptimizationSpec :: Spec
functionOptimizationSpec = describe "Function Optimization" $ do
  it "eliminates unused functions" $ do
    let func1 = Function "unused_func" [] TInt [Return (Just (Literal (IntLit 0)))]
    let func2 = Function "used_func" [] TInt [Return (Just (Literal (IntLit 42)))]
    let callStmt = ExprStmt (Call (Variable "used_func") [])
    
    case eliminateDeadCodeFunctions [func1, func2] [callStmt] of
      [Function "used_func" [] TInt [Return (Just (Literal (IntLit 42)))]] -> return ()
      result -> expectationFailure $ "Expected only used function, got: " ++ show result
  
  it "eliminates unreachable code after early returns" $ do
    let func = Function "test_func" [] TInt [
          If (Variable "condition") 
             [Return (Just (Literal (IntLit 1)))]
             [],
          Assignment "x" (Literal (IntLit 2)),
          Return (Just (Variable "x"))
          ]
    
    case eliminateDeadCode [func] of
      [Function "test_func" [] TInt [
          If (Variable "condition") 
             [Return (Just (Literal (IntLit 1)))]
             [],
          Assignment "x" (Literal (IntLit 2)),
          Return (Just (Variable "x"))
          ]] -> return ()
      result -> expectationFailure $ "Expected optimized function, got: " ++ show result
  
  it "preserves side-effecting statements" $ do
    let stmts = [
          Return (Just (Literal (IntLit 0))),
          ExprStmt (Call (Variable "side_effect_func") [])
          ]
    case eliminateDeadCode stmts of
      [Return (Just (Literal (IntLit 0)))] -> return ()
      result -> expectationFailure $ "Expected only return statement, got: " ++ show result