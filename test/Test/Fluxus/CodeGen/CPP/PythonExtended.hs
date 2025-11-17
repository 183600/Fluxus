{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.CodeGen.CPP.PythonExtended (spec) where

import Data.List (find)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Fluxus.AST.Common
import Fluxus.AST.Python
import Fluxus.CodeGen.CPP
import Fluxus.CodeGen.CPP.AST
import Fluxus.CodeGen.CPP.Diagnostics (diagMessage)
import Test.Hspec

import qualified Test.Fluxus.CodeGen.CPP.Shared as Shared

spec :: Spec
spec = describe "Extended Python code generation" $ do
  tupleUnpackingSpec
  collectionLiteralsSpec
  lambdaExpressionSpec
  rangeHelperSpec
  keywordArgumentsSpec

-- Test tuple unpacking in assignments
tupleUnpackingSpec :: Spec
tupleUnpackingSpec = describe "Tuple unpacking" $ do
  it "translates simple tuple unpacking to std::tie" $ do
    let moduleBody =
          [ noLoc
              ( PyAssign
                  [noLoc (PatTuple [noLoc (PatVar (Identifier "x")), noLoc (PatVar (Identifier "y"))])]
                  (noLoc (PyCall (noLoc (PyVar (Identifier "get_pair"))) []))
              )
          ]
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = moduleBody
              }
        result = generateCpp Shared.testCppConfig (Left pythonAst)
    case result of
      Right res -> do
        let unit = cgrUnit res
        cppIncludes unit `shouldSatisfy` (elem (T.pack "<tuple>"))
        case find Shared.isMainFunction (cppDeclarations unit) of
          Just (CppFunction _ _ _ body) -> do
            let hasTie stmt = case stmt of
                  CppExprStmt (CppBinary "=" (CppCall (CppVar "std::tie") _) _) -> True
                  CppStmtSeq stmts -> any hasTie stmts
                  _ -> False
            body `shouldSatisfy` any hasTie
          _ -> expectationFailure "Expected generated main function"
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

  it "declares variables when unpacking for the first time" $ do
    let moduleBody =
          [ noLoc
              ( PyAssign
                  [noLoc (PatTuple [noLoc (PatVar (Identifier "a")), noLoc (PatVar (Identifier "b"))])]
                  (noLoc (PyTuple [noLoc (PyLiteral (PyInt 1)), noLoc (PyLiteral (PyInt 2))]))
              )
          ]
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = moduleBody
              }
        result = generateCpp Shared.testCppConfig (Left pythonAst)
    case result of
      Right _ -> return ()  -- Success
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

-- Test collection literals
collectionLiteralsSpec :: Spec
collectionLiteralsSpec = describe "Collection literals" $ do
  it "generates std::tuple for Python tuple literals" $ do
    let moduleBody =
          [ noLoc
              ( PyAssign
                  [noLoc (PatVar (Identifier "t"))]
                  (noLoc (PyTuple [noLoc (PyLiteral (PyInt 1)), noLoc (PyLiteral (PyInt 2))]))
              )
          ]
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = moduleBody
              }
        result = generateCpp Shared.testCppConfig (Left pythonAst)
    case result of
      Right res -> do
        let unit = cgrUnit res
        cppIncludes unit `shouldSatisfy` (elem (T.pack "<tuple>"))
        let decls = cppDeclarations unit
            hasMakeTuple (CppVariable _ _ (Just expr)) = containsMakeTuple expr
            hasMakeTuple _ = False
            containsMakeTuple (CppCall (CppVar "std::make_tuple") _) = True
            containsMakeTuple _ = False
        decls `shouldSatisfy` any hasMakeTuple
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

  it "generates std::set for Python set literals" $ do
    let moduleBody =
          [ noLoc
              ( PyAssign
                  [noLoc (PatVar (Identifier "s"))]
                  (noLoc (PySet [noLoc (PyLiteral (PyInt 1)), noLoc (PyLiteral (PyInt 2))]))
              )
          ]
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = moduleBody
              }
        result = generateCpp Shared.testCppConfig (Left pythonAst)
    case result of
      Right res -> do
        let unit = cgrUnit res
        cppIncludes unit `shouldSatisfy` (elem (T.pack "<set>"))
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

  it "generates std::map for Python dict literals" $ do
    let moduleBody =
          [ noLoc
              ( PyAssign
                  [noLoc (PatVar (Identifier "d"))]
                  (noLoc (PyDict [(noLoc (PyLiteral (PyString "key")), noLoc (PyLiteral (PyInt 42)))]))
              )
          ]
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = moduleBody
              }
        result = generateCpp Shared.testCppConfig (Left pythonAst)
    case result of
      Right res -> do
        let unit = cgrUnit res
        cppIncludes unit `shouldSatisfy` (elem (T.pack "<map>"))
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

-- Test lambda expressions
lambdaExpressionSpec :: Spec
lambdaExpressionSpec = describe "Lambda expressions" $ do
  it "translates Python lambdas to C++ lambdas" $ do
    let moduleBody =
          [ noLoc
              ( PyAssign
                  [noLoc (PatVar (Identifier "f"))]
                  (noLoc (PyLambda [noLoc (ParamNormal (Identifier "x") Nothing Nothing)]
                                    (noLoc (PyBinaryOp OpAdd (noLoc (PyVar (Identifier "x"))) (noLoc (PyLiteral (PyInt 1)))))))
              )
          ]
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = moduleBody
              }
        result = generateCpp Shared.testCppConfig (Left pythonAst)
    case result of
      Right res -> do
        let hasLambda (CppVariable _ _ (Just expr)) = containsLambda expr
            hasLambda _ = False
            containsLambda (CppLambda _ _) = True
            containsLambda _ = False
            decls = cppDeclarations (cgrUnit res)
        decls `shouldSatisfy` any hasLambda
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

-- Test range() helper generation
rangeHelperSpec :: Spec
rangeHelperSpec = describe "Range helper function" $ do
  it "generates range() helper function when used" $ do
    let moduleBody =
          [ noLoc
              ( PyFor
                  (noLoc (PatVar (Identifier "i")))
                  (noLoc (PyCall (noLoc (PyVar (Identifier "range"))) [noLoc (ArgPositional (noLoc (PyLiteral (PyInt 5))))]))
                  [noLoc (PyExprStmt (noLoc (PyCall (noLoc (PyVar (Identifier "print"))) [noLoc (ArgPositional (noLoc (PyVar (Identifier "i"))))])))]
                  []
              )
          ]
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = moduleBody
              }
        result = generateCpp Shared.testCppConfig (Left pythonAst)
    case result of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
            isRangeFunction (CppFunction "range" _ _ _) = True
            isRangeFunction _ = False
        decls `shouldSatisfy` any isRangeFunction
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

-- Test keyword argument warnings
keywordArgumentsSpec :: Spec
keywordArgumentsSpec = describe "Keyword arguments" $ do
  it "emits warnings for keyword arguments" $ do
    let moduleBody =
          [ noLoc
              ( PyExprStmt
                  (noLoc (PyCall (noLoc (PyVar (Identifier "print")))
                                [noLoc (ArgKeyword (Identifier "end") (noLoc (PyLiteral (PyString " "))))]))
              )
          ]
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = moduleBody
              }
        result = generateCpp Shared.testCppConfig (Left pythonAst)
    case result of
      Right res -> do
        let warnings = cgrDiagnostics res
        length warnings `shouldSatisfy` (> 0)
        warnings `shouldSatisfy` any (T.isInfixOf "Keyword argument" . diagMessage)
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

  it "emits warnings for *args unpacking" $ do
    let moduleBody =
          [ noLoc
              ( PyExprStmt
                  (noLoc (PyCall (noLoc (PyVar (Identifier "print")))
                                [noLoc (ArgStarred (noLoc (PyVar (Identifier "args"))))]))
              )
          ]
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = moduleBody
              }
        result = generateCpp Shared.testCppConfig (Left pythonAst)
    case result of
      Right res -> do
        let warnings = cgrDiagnostics res
        warnings `shouldSatisfy` any (T.isInfixOf "*args" . diagMessage)
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

  it "emits warnings for **kwargs unpacking" $ do
    let moduleBody =
          [ noLoc
              ( PyExprStmt
                  (noLoc (PyCall (noLoc (PyVar (Identifier "print")))
                                [noLoc (ArgKwStarred (noLoc (PyVar (Identifier "kwargs"))))]))
              )
          ]
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = moduleBody
              }
        result = generateCpp Shared.testCppConfig (Left pythonAst)
    case result of
      Right res -> do
        let warnings = cgrDiagnostics res
        warnings `shouldSatisfy` any (T.isInfixOf "**kwargs" . diagMessage)
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure
