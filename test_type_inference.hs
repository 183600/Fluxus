{-# LANGUAGE OverloadedStrings #-}
import Fluxus.Analysis.TypeInference
import Fluxus.AST.Common
import qualified Data.HashMap.Strict as HM
import Data.Either (isRight)

main :: IO ()
main = do
  putStrLn "Testing type inference..."
  
  -- Create a simple environment with basic types
  let env = HM.fromList
        [ (Identifier "x", TInt 32)
        , (Identifier "y", TInt 32)
        ]
  
  -- Test 1: Simple type inference
  putStrLn "\n1. Testing simple type inference..."
  let result1 = runTypeInference env $ do
        t <- inferType (CEBinaryOp OpAdd (Located (SourceSpan "" (SourcePos 0 0) (SourcePos 0 0)) (CEVar (Identifier "x"))) 
                                          (Located (SourceSpan "" (SourcePos 0 0) (SourcePos 0 0)) (CEVar (Identifier "y"))))
        solveConstraints
        checkTypes
  putStrLn $ "Result: " ++ show result1
  putStrLn $ "Expected: Right True"
  
  -- Test 2: Type inference with literals
  putStrLn "\n2. Testing type inference with literals..."
  let result2 = runTypeInference env $ do
        t <- inferType (CEBinaryOp OpAdd (Located (SourceSpan "" (SourcePos 0 0) (SourcePos 0 0)) (CEVar (Identifier "x"))) 
                                          (Located (SourceSpan "" (SourcePos 0 0) (SourcePos 0 0)) (CELiteral (LInt 5))))
        solveConstraints
        checkTypes
  putStrLn $ "Result: " ++ show result2
  putStrLn $ "Expected: Right True"
  
  -- Test 3: Type inference failure
  putStrLn "\n3. Testing type inference with incompatible types..."
  let env3 = HM.fromList
        [ (Identifier "a", TInt 32)
        , (Identifier "b", TString)
        ]
  let result3 = runTypeInference env3 $ do
        t <- inferType (CEBinaryOp OpAdd (Located (SourceSpan "" (SourcePos 0 0) (SourcePos 0 0)) (CEVar (Identifier "a"))) 
                                          (Located (SourceSpan "" (SourcePos 0 0) (SourcePos 0 0)) (CEVar (Identifier "b"))))
        solveConstraints
        checkTypes
  putStrLn $ "Result: " ++ show result3
  putStrLn $ "Expected: Left \"...\" (type error)"