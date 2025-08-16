{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import Fluxus.CodeGen.CPP
import Fluxus.AST.Common
import Fluxus.AST.Python
import Data.List (intercalate)
import qualified Data.Text as T

-- Create a simple CppCall expression for testing
testExpr :: CppExpr
testExpr = CppCall (CppVar "add") [CppLiteral (CppIntLit 1), CppLiteral (CppIntLit 2)]

-- Custom render function with debug output
renderCppExprDebug :: CppExpr -> String
renderCppExprDebug = \case
  CppVar name -> T.unpack name
  CppLiteral lit -> renderCppLiteralDebug lit
  CppBinary op left right -> 
    (renderCppExprDebug left) ++ " " ++ (T.unpack op) ++ " " ++ (renderCppExprDebug right)
  CppCall func args -> 
    let funcStr = renderCppExprDebug func
        argsStr = intercalate ", " (map renderCppExprDebug args)
    in do
      -- Debug output
      -- putStrLn $ "DEBUG: Rendering function call - func: " ++ funcStr ++ ", args: [" ++ argsStr ++ "]"
      funcStr ++ "(" ++ argsStr ++ ")"
  _ -> "/* unimplemented expr */"

renderCppLiteralDebug :: CppLiteral -> String
renderCppLiteralDebug = \case
  CppIntLit i -> show i
  CppFloatLit f -> show f
  CppBoolLit True -> "true"
  CppBoolLit False -> "false" 
  CppStringLit s -> "\"" ++ (T.unpack s) ++ "\""
  _ -> "/* unimplemented literal */"

main :: IO ()
main = do
  putStrLn "Testing C++ expression rendering with debug..."
  let result = renderCppExprDebug testExpr
  putStrLn $ "Final result: " ++ result
