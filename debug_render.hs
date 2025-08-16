{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import Fluxus.Compiler.Driver
import Fluxus.CodeGen.CPP
import Fluxus.AST.Common
import Fluxus.AST.Python

-- Create a simple CppCall expression for testing
testExpr :: CppExpr
testExpr = CppCall (CppVar "add") [CppLiteral (CppIntLit 1), CppLiteral (CppIntLit 2)]

main :: IO ()
main = do
  putStrLn "Testing C++ expression rendering..."
  let rendered = renderCppExpr testExpr
  putStrLn $ "Rendered expression: " ++ show rendered