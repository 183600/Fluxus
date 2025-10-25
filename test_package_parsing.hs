#!/usr/bin/env stack
{- stack script --resolver lts-22.28 --package fluxus -}

import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Go
import Fluxus.AST.Common
import qualified Data.Text.IO as TIO
import System.Exit

testFile :: FilePath -> String -> IO Bool
testFile path expectedPkg = do
  putStrLn $ "\n测试文件: " ++ path
  content <- TIO.readFile path
  case runGoLexer path content of
    Left err -> do
      putStrLn $ "  ❌ 词法分析失败: " ++ show err
      return False
    Right tokens -> do
      putStrLn $ "  ✓ 词法分析成功，生成 " ++ show (length tokens) ++ " 个 token"
      case runGoParser path tokens of
        Left err -> do
          putStrLn $ "  ❌ 语法分析失败: " ++ show err
          return False
        Right (GoAST package_) -> do
          let Identifier pkgName = goPackageName package_
          putStrLn $ "  ✓ 语法分析成功"
          putStrLn $ "  包名: " ++ pkgName
          if pkgName == expectedPkg
            then do
              putStrLn $ "  ✅ 包名匹配: " ++ expectedPkg
              return True
            else do
              putStrLn $ "  ❌ 包名不匹配，期望: " ++ expectedPkg ++ "，实际: " ++ pkgName
              return False

main :: IO ()
main = do
  putStrLn "=========================================="
  putStrLn "Go 包声明测试"
  putStrLn "=========================================="
  
  results <- sequence
    [ testFile "test_package_simple.go" "main"
    , testFile "test_package_custom.go" "mypackage"
    , testFile "test_package_with_import.go" "main"
    ]
  
  putStrLn "\n=========================================="
  putStrLn "测试总结"
  putStrLn "=========================================="
  let passed = length $ filter id results
  let total = length results
  putStrLn $ "通过: " ++ show passed ++ "/" ++ show total
  
  if all id results
    then do
      putStrLn "✅ 所有测试通过！"
      exitSuccess
    else do
      putStrLn "❌ 部分测试失败"
      exitFailure
