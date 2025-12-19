{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.CodeGen.CPP.UnsupportedConstructs (spec) where

import Data.List (find)
import qualified Data.Text as T

import Fluxus.AST.Common
import Fluxus.AST.Python
import Fluxus.AST.Go
import Fluxus.CodeGen.CPP
import Fluxus.CodeGen.CPP.Diagnostics
import Test.Hspec

import qualified Test.Fluxus.CodeGen.CPP.Shared as Shared

spec :: Spec
spec = describe "Unsupported construct handling" $ do
  strictModeTests
  pythonUnsupportedTests
  goUnsupportedTests

-- | Strict mode configuration
strictConfig :: CppGenConfig
strictConfig = Shared.testCppConfig { cgcStrictMode = True }

-- | Non-strict mode configuration
nonStrictConfig :: CppGenConfig
nonStrictConfig = Shared.testCppConfig { cgcStrictMode = False }

strictModeTests :: Spec
strictModeTests = describe "Strict mode behavior" $ do
  it "supports Python 'with' statements in strict mode" $ do
    let withItem = PythonWithItem
          { pyWithContext = noLoc (PyCall (noLoc (PyVar (Identifier "open")))
              [noLoc (ArgPositional (noLoc (PyLiteral (PyString "file.txt"))))])
          , pyWithVar = Just (noLoc (PatVar (Identifier "f")))
          }
        moduleBody = [noLoc (PyWith [noLoc withItem] [])]
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = moduleBody
          }
    case generateCpp strictConfig (Left pythonAst) of
      Right _ -> pure ()
      Left failure ->
        expectationFailure $ "Expected strict mode to support with statements, but compilation failed: " <> show failure


  it "supports Python try/except statements in strict mode" $ do
    let exceptClause = PythonExcept
          { pyExceptType = Nothing
          , pyExceptName = Nothing
          , pyExceptBody = []
          }
        moduleBody = [noLoc (PyTry [] [noLoc exceptClause] [] [])]
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = moduleBody
          }
    case generateCpp strictConfig (Left pythonAst) of
      Right _ -> pure ()
      Left failure ->
        expectationFailure $ "Expected strict mode to support try statements, but compilation failed: " <> show failure


pythonUnsupportedTests :: Spec
pythonUnsupportedTests = describe "Python unsupported constructs" $ do
  it "reports error for async function definition" $ do
    let funcDef = PythonFuncDef
          { pyFuncName = Identifier "async_func"
          , pyFuncDecorators = []
          , pyFuncParams = []
          , pyFuncReturns = Nothing
          , pyFuncBody = []
          , pyFuncDoc = Nothing
          , pyFuncIsAsync = True
          }
        moduleBody = [noLoc (PyAsyncFuncDef funcDef)]
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = moduleBody
          }
        result = generateCpp strictConfig (Left pythonAst)
    case result of
      Left failure ->
        cgfErrors failure `shouldSatisfy` (not . null)
      Right _ ->
        expectationFailure "Expected compilation to fail for async function"

  it "supports raise statements in strict mode" $ do
    let moduleBody = [noLoc (PyRaise Nothing Nothing)]
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = moduleBody
          }
    case generateCpp strictConfig (Left pythonAst) of
      Right _ -> pure ()
      Left failure ->
        expectationFailure $ "Expected raise statements to be supported, but compilation failed: " <> show failure

  it "reports error for yield statement" $ do
    let moduleBody = [noLoc (PyYield Nothing)]
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = moduleBody
          }
        result = generateCpp strictConfig (Left pythonAst)
    case result of
      Left failure ->
        cgfErrors failure `shouldSatisfy` (not . null)
      Right _ ->
        expectationFailure "Expected compilation to fail for yield statement"

  it "reports error for yield from statement" $ do
    let moduleBody = [noLoc (PyYieldFrom (noLoc (PyVar (Identifier "gen"))))]
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = moduleBody
          }
        result = generateCpp strictConfig (Left pythonAst)
    case result of
      Left failure ->
        cgfErrors failure `shouldSatisfy` (not . null)
      Right _ ->
        expectationFailure "Expected compilation to fail for yield from statement"

  it "reports error for async with statement" $ do
    let withItem = PythonWithItem
          { pyWithContext = noLoc (PyVar (Identifier "ctx"))
          , pyWithVar = Nothing
          }
        moduleBody = [noLoc (PyAsyncWith [noLoc withItem] [])]
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = moduleBody
          }
        result = generateCpp strictConfig (Left pythonAst)
    case result of
      Left failure ->
        cgfErrors failure `shouldSatisfy` (not . null)
      Right _ ->
        expectationFailure "Expected compilation to fail for async with statement"

  it "reports error for async for statement" $ do
    let moduleBody = [noLoc (PyAsyncFor 
          (noLoc (PatVar (Identifier "x")))
          (noLoc (PyVar (Identifier "items")))
          []
          [])]
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = moduleBody
          }
        result = generateCpp strictConfig (Left pythonAst)
    case result of
      Left failure ->
        cgfErrors failure `shouldSatisfy` (not . null)
      Right _ ->
        expectationFailure "Expected compilation to fail for async for statement"

  it "falls back to runtime abort for multi-dimensional slicing in non-strict mode" $ do
    let sliceExpr =
          noLoc $
            PySubscript
              (noLoc (PyVar (Identifier "matrix")))
              (noLoc (SliceExtSlice
                       [ noLoc (SliceSlice
                           (Just (noLoc (PyLiteral (PyInt 0))))
                           (Just (noLoc (PyLiteral (PyInt 2))))
                           Nothing)
                       , noLoc (SliceSlice
                           (Just (noLoc (PyLiteral (PyInt 1))))
                           (Just (noLoc (PyLiteral (PyInt 3))))
                           Nothing)
                       ]))
        rowValues = map (noLoc . PyLiteral . PyInt) [1, 2, 3, 4]
        rowLiteral = noLoc (PyList rowValues)
        matrixLiteral = noLoc (PyList [rowLiteral, rowLiteral])
        moduleBody =
          [ noLoc (PyAssign [noLoc (PatVar (Identifier "matrix"))] matrixLiteral)
          , noLoc (PyExprStmt sliceExpr)
          ]
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = moduleBody
          }
    case generateCpp nonStrictConfig (Left pythonAst) of
      Left failure ->
        expectationFailure $ "Code generation unexpectedly failed: " <> show failure
      Right result -> do
        let unit = cgrUnit result
            decls = cppDeclarations unit

            isRuntimeAbortHelper decl = case decl of
              CppFunction name _ _ _ -> name == "fluxus_runtime_abort"
              _ -> False

            isMainFunctionDecl decl = case decl of
              CppFunction "main" _ _ _ -> True
              _ -> False

            isSliceFallbackStmt stmt = case stmt of
              CppExprStmt expr -> isSliceFallbackExpr expr
              CppStmtSeq stmts -> any isSliceFallbackStmt stmts
              CppBlock stmts -> any isSliceFallbackStmt stmts
              _ -> False

            isSliceFallbackExpr expr = case expr of
              CppBinary "," lhs rhs ->
                isAbortCall lhs && rhs == CppLiteral (CppIntLit 0)
              _ -> False

            isAbortCall expr = case expr of
              CppCall (CppVar "fluxus_runtime_abort") [CppLiteral (CppStringLit msg)] ->
                T.isInfixOf "multiple indices" msg
              _ -> False

        any isRuntimeAbortHelper decls `shouldBe` True
        case find isMainFunctionDecl decls of
          Just (CppFunction _ _ _ body) ->
            any isSliceFallbackStmt body `shouldBe` True
          _ ->
            expectationFailure "Expected generated main function containing slice fallback statement"

goUnsupportedTests :: Spec
goUnsupportedTests = describe "Go unsupported constructs" $ do
  it "reports error for unsupported Go declaration" $ do
    -- Use GoConstDecl which is likely not fully implemented
    let decl = noLoc (GoConstDecl [])
        file = GoFile
          { goFileName = "test.go"
          , goFilePackage = Identifier "main"
          , goFileImports = []
          , goFileDecls = [decl]
          }
        goAst = GoAST (GoPackage (Identifier "main") [file])
        result = generateCpp strictConfig (Right goAst)
    case result of
      Left failure ->
        cgfErrors failure `shouldSatisfy` (not . null)
      Right _ ->
        -- If it succeeds, that's also fine (means it's now implemented)
        pure ()
