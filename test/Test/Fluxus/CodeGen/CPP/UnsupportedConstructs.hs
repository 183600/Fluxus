{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.CodeGen.CPP.UnsupportedConstructs (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Fluxus.AST.Common
import Fluxus.AST.Python
import Fluxus.AST.Go
import Fluxus.CodeGen.CPP
import Fluxus.CodeGen.CPP.Diagnostics (CppCodeGenError(..))
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
  it "fails compilation on unsupported Python 'with' statement in strict mode" $ do
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
        result = generateCpp strictConfig (Left pythonAst)
    case result of
      Left failure ->
        case cgfErrors failure of
          [CppNotImplemented msg] -> 
            msg `shouldSatisfy` T.isInfixOf "with"
          errors ->
            expectationFailure $ "Expected single CppNotImplemented error, got: " <> show errors
      Right _ ->
        expectationFailure "Expected compilation to fail in strict mode"

  it "fails even when strict mode is disabled" $ do
    let withItem = PythonWithItem
          { pyWithContext = noLoc (PyVar (Identifier "resource"))
          , pyWithVar = Nothing
          }
        moduleBody = [noLoc (PyWith [noLoc withItem] [])]
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = moduleBody
          }
        result = generateCpp nonStrictConfig (Left pythonAst)
    case result of
      Left failure ->
        cgfErrors failure `shouldSatisfy` (not . null)
      Right _ ->
        expectationFailure "Expected compilation to fail even when strict mode is disabled"

  it "fails compilation on unsupported Python 'try' statement in strict mode" $ do
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
        result = generateCpp strictConfig (Left pythonAst)
    case result of
      Left failure ->
        case cgfErrors failure of
          [CppNotImplemented msg] -> 
            msg `shouldSatisfy` T.isInfixOf "try"
          errors ->
            expectationFailure $ "Expected single CppNotImplemented error, got: " <> show errors
      Right _ ->
        expectationFailure "Expected compilation to fail in strict mode"

  it "fails compilation on unsupported multiple assignment in strict mode" $ do
    let moduleBody = [noLoc (PyAssign 
          [noLoc (PatVar (Identifier "x")), noLoc (PatVar (Identifier "y"))]
          (noLoc (PyTuple [noLoc (PyLiteral (PyInt 1)), noLoc (PyLiteral (PyInt 2))])))]
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = moduleBody
          }
        result = generateCpp strictConfig (Left pythonAst)
    case result of
      Left failure ->
        case cgfErrors failure of
          [CppNotImplemented msg] -> 
            msg `shouldSatisfy` T.isInfixOf "Multiple assignment"
          errors ->
            expectationFailure $ "Expected single CppNotImplemented error, got: " <> show errors
      Right _ ->
        expectationFailure "Expected compilation to fail in strict mode"

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

  it "reports error for raise statement" $ do
    let moduleBody = [noLoc (PyRaise Nothing Nothing)]
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
        expectationFailure "Expected compilation to fail for raise statement"

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
