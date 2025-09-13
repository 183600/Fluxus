{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Optimization.Inlining (spec) where

import Test.Hspec
import Data.Text (Text)

import Fluxus.AST.Common
import Fluxus.Optimization.Inlining

spec :: Spec
spec = describe "Function Inlining" $ do
  basicInliningSpec
  optimizationSpec
  edgeCaseSpec

basicInliningSpec :: Spec
basicInliningSpec = describe "Basic Function Inlining" $ do
  it "inlines simple function calls" $ do
    let simpleFunc = Function "add_one" [Param "x" TInt] TInt [
          Return (Just (BinaryOp "+" (Variable "x") (Literal (IntLit 1))))
          ]
    let call = Call (Variable "add_one") [Literal (IntLit 5)]
    
    case inlineFunctions [simpleFunc] call of
      BinaryOp "+" (Literal (IntLit 5)) (Literal (IntLit 1)) -> return ()
      result -> expectationFailure $ "Expected inlined addition, got: " ++ show result
  
  it "inlines functions with multiple parameters" $ do
    let addFunc = Function "add" [Param "a" TInt, Param "b" TInt] TInt [
          Return (Just (BinaryOp "+" (Variable "a") (Variable "b")))
          ]
    let call = Call (Variable "add") [Literal (IntLit 3), Literal (IntLit 4)]
    
    case inlineFunctions [addFunc] call of
      BinaryOp "+" (Literal (IntLit 3)) (Literal (IntLit 4)) -> return ()
      result -> expectationFailure $ "Expected inlined addition, got: " ++ show result
  
  it "inlines functions with multiple statements" $ do
    let multiStmtFunc = Function "compute" [Param "x" TInt] TInt [
          Assignment "temp" (BinaryOp "*" (Variable "x") (Literal (IntLit 2))),
          Return (Just (BinaryOp "+" (Variable "temp") (Literal (IntLit 1))))
          ]
    let call = Call (Variable "compute") [Literal (IntLit 5)]
    
    case inlineFunctions [multiStmtFunc] call of
      BinaryOp "+" (BinaryOp "*" (Literal (IntLit 5)) (Literal (IntLit 2))) (Literal (IntLit 1)) -> return ()
      result -> expectationFailure $ "Expected inlined computation, got: " ++ show result
  
  it "preserves function calls that cannot be inlined" $ do
    let externalFunc = Function "external" [Param "x" TInt] TInt [
          Return (Just (Call (Variable "runtime_call") [Variable "x"]))
          ]
    let call = Call (Variable "external") [Literal (IntLit 42))]
    
    case inlineFunctions [externalFunc] call of
      Call (Variable "external") [Literal (IntLit 42)] -> return ()
      result -> expectationFailure $ "Expected original call to be preserved, got: " ++ show result

optimizationSpec :: Spec
optimizationSpec = describe "Inlining Optimizations" $ do
  it "applies constant propagation after inlining" $ do
    let constFunc = Function "get_value" [] TInt [
          Return (Just (Literal (IntLit 42)))
          ]
    let call = Call (Variable "get_value") []
    
    case inlineFunctions [constFunc] call of
      Literal (IntLit 42) -> return ()
      result -> expectationFailure $ "Expected constant value after inlining, got: " ++ show result
  
  it "inlines recursive functions with depth limit" $ do
    let recursiveFunc = Function "factorial" [Param "n" TInt] TInt [
          If (BinaryOp "<=" (Variable "n") (Literal (IntLit 1)))
             [Return (Just (Literal (IntLit 1)))]
             [Return (Just (BinaryOp "*" 
                             (Variable "n") 
                             (Call (Variable "factorial") [BinaryOp "-" (Variable "n") (Literal (IntLit 1))])
                         ))]
          ]
    let call = Call (Variable "factorial") [Literal (IntLit 5)]
    
    case inlineFunctions [recursiveFunc] call of
      Call (Variable "factorial") [Literal (IntLit 5)] -> return ()
      result -> expectationFailure $ "Expected recursive call to be preserved, got: " ++ show result
  
  it "inlines nested function calls" $ do
    let func1 = Function "square" [Param "x" TInt] TInt [
          Return (Just (BinaryOp "*" (Variable "x") (Variable "x")))
          ]
    let func2 = Function "add_square" [Param "a" TInt, Param "b" TInt] TInt [
          Return (Just (BinaryOp "+" 
                         (Call (Variable "square") [Variable "a"]) 
                         (Call (Variable "square") [Variable "b"])
                     ))
          ]
    let call = Call (Variable "add_square") [Literal (IntLit 3), Literal (IntLit 4)]
    
    case inlineFunctions [func1, func2] call of
      BinaryOp "+" 
        (BinaryOp "*" (Literal (IntLit 3)) (Literal (IntLit 3)))
        (BinaryOp "*" (Literal (IntLit 4)) (Literal (IntLit 4))) -> return ()
      result -> expectationFailure $ "Expected fully inlined nested calls, got: " ++ show result

edgeCaseSpec :: Spec
edgeCaseSpec = describe "Edge Cases" $ do
  it "handles functions with side effects" $ do
    let sideEffectFunc = Function "with_side_effect" [Param "x" TInt] TInt [
          ExprStmt (Call (Variable "log") [Literal (StringLit "called")]),
          Return (Just (Variable "x"))
          ]
    let call = Call (Variable "with_side_effect") [Literal (IntLit 10))]
    
    case inlineFunctions [sideEffectFunc] call of
      Call (Variable "with_side_effect") [Literal (IntLit 10)] -> return ()
      result -> expectationFailure $ "Expected side-effecting function call to be preserved, got: " ++ show result
  
  it "handles functions with complex control flow" $ do
    let complexFunc = Function "complex" [Param "x" TInt] TInt [
          If (BinaryOp ">" (Variable "x") (Literal (IntLit 0)))
             [Return (Just (BinaryOp "*" (Variable "x") (Literal (IntLit 2))))]
             [Return (Just (Literal (IntLit 0)))]
          ]
    let call = Call (Variable "complex") [Literal (IntLit 5))]
    
    case inlineFunctions [complexFunc] call of
      If (BinaryOp ">" (Literal (IntLit 5)) (Literal (IntLit 0)))
         [BinaryOp "*" (Literal (IntLit 5)) (Literal (IntLit 2))]
         [Literal (IntLit 0)] -> return ()
      result -> expectationFailure $ "Expected inlined control flow, got: " ++ show result
  
  it "handles function calls in expressions" $ do
    let func = Function "double" [Param "x" TInt] TInt [
          Return (Just (BinaryOp "*" (Variable "x") (Literal (IntLit 2))))
          ]
    let expr = BinaryOp "+" 
                  (Call (Variable "double") [Literal (IntLit 3)])
                  (Call (Variable "double") [Literal (IntLit 4)])
    
    case inlineFunctions [func] expr of
      BinaryOp "+" 
        (BinaryOp "*" (Literal (IntLit 3)) (Literal (IntLit 2)))
        (BinaryOp "*" (Literal (IntLit 4)) (Literal (IntLit 2))) -> return ()
      result -> expectationFailure $ "Expected inlined function calls in expression, got: " ++ show result