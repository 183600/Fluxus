{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.CodeGen.CPP.PythonExtended (spec) where

import Data.List (find)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Fluxus.AST.Common
import Fluxus.AST.Python
import Fluxus.CodeGen.CPP
import Fluxus.CodeGen.CPP.AST()
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
  chainedAssignmentSpec
  generatorExpressionSpec
  joinedStringSpec
  walrusExpressionSpec
  slicingSpec

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

chainedAssignmentSpec :: Spec
chainedAssignmentSpec = describe "Chained assignments" $ do
  it "hoists module-level chains while evaluating the RHS once" $ do
    let moduleBody =
          [ noLoc
              ( PyAssign
                  [ noLoc (PatVar (Identifier "first"))
                  , noLoc (PatVar (Identifier "second"))
                  ]
                  (noLoc (PyLiteral (PyInt 7)))
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
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
            lookupVar name = [decl | decl@(CppVariable varName _ _) <- decls, varName == name]
        case lookupVar "second" of
          [CppVariable _ ty (Just initExpr)] -> do
            ty `shouldBe` CppLongLong
            initExpr `shouldBe` CppLiteral (CppIntLit 7)
          other -> expectationFailure $ "Expected declaration for 'second', found " <> show other
        case lookupVar "first" of
          [CppVariable _ ty (Just initExpr)] -> do
            ty `shouldBe` CppLongLong
            initExpr `shouldBe` CppVar "second"
          other -> expectationFailure $ "Expected declaration for 'first', found " <> show other
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

  it "lowers chained assignments inside functions" $ do
    let assignStmt =
          noLoc
            ( PyAssign
                [ noLoc (PatVar (Identifier "outer"))
                , noLoc (PatVar (Identifier "inner"))
                ]
                (noLoc (PyLiteral (PyInt 99)))
            )
        funcDef = PythonFuncDef
          { pyFuncName = Identifier "assign_chain"
          , pyFuncDecorators = []
          , pyFuncParams = []
          , pyFuncReturns = Nothing
          , pyFuncBody = [assignStmt]
          , pyFuncDoc = Nothing
          , pyFuncIsAsync = False
          }
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = [noLoc (PyFuncDef funcDef)]
              }
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
        case [body | CppFunction "assign_chain" _ _ body <- decls] of
          [body] -> do
            let extractDecls stmt = case stmt of
                            CppDecl decl -> [decl]
                            CppStmtSeq stmts -> concatMap extractDecls stmts
                            _ -> []
                chainDecls = concatMap extractDecls body
            chainDecls `shouldSatisfy` ((>= 2) . length)
            case chainDecls of
              (CppVariable name1 ty1 (Just init1) : CppVariable name2 ty2 (Just init2) : _) -> do
                name1 `shouldBe` "inner"
                ty1 `shouldBe` CppLongLong
                init1 `shouldBe` CppLiteral (CppIntLit 99)
                name2 `shouldBe` "outer"
                ty2 `shouldBe` CppLongLong
                init2 `shouldBe` CppVar "inner"
              other -> expectationFailure $ "Unexpected declaration sequence: " <> show other
          _ -> expectationFailure "Expected generated function 'assign_chain'"
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

generatorExpressionSpec :: Spec
generatorExpressionSpec = describe "Generator expressions" $ do
  it "materializes generator expressions eagerly" $ do
    let sourceAssign =
          noLoc
            ( PyAssign
                [noLoc (PatVar (Identifier "source"))]
                (noLoc (PyList (map (noLoc . PyLiteral . PyInt) [1, 2, 3])))
            )
        comp = PythonComprehension
          { pyCompTarget = noLoc (PatVar (Identifier "item"))
          , pyCompIter = noLoc (PyVar (Identifier "source"))
          , pyCompFilters = []
          , pyCompAsync = False
          }
        generatorAssign =
          noLoc
            ( PyAssign
                [noLoc (PatVar (Identifier "result"))]
                (noLoc (PyGenComp (noLoc (PyVar (Identifier "item"))) [comp]))
            )
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = [sourceAssign, generatorAssign]
              }
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let warnings = cgrDiagnostics res
        warnings `shouldSatisfy` any (T.isInfixOf "generator expression" . diagMessage)
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

joinedStringSpec :: Spec
joinedStringSpec = describe "Joined strings" $ do
  it "lowers PyJoinedStr via stringstreams" $ do
    let moduleBody =
          [ noLoc
              ( PyAssign
                  [noLoc (PatVar (Identifier "name"))]
                  (noLoc (PyLiteral (PyString "Fluxus")))
              )
          , noLoc
              ( PyAssign
                  [noLoc (PatVar (Identifier "message"))]
                  (noLoc (PyJoinedStr
                    [ noLoc (PyLiteral (PyString "Hello "))
                    , noLoc (PyVar (Identifier "name"))
                    ]))
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
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let includes = cppIncludes (cgrUnit res)
        includes `shouldSatisfy` (elem "<sstream>")
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

walrusExpressionSpec :: Spec
walrusExpressionSpec = describe "Assignment expressions" $ do
  it "reuses existing bindings for walrus assignments" $ do
    let moduleBody =
          [ noLoc
              ( PyAssign
                  [noLoc (PatVar (Identifier "existing"))]
                  (noLoc (PyLiteral (PyInt 0)))
              )
          , noLoc
              ( PyAssign
                  [noLoc (PatVar (Identifier "result"))]
                  (noLoc (PyNamedExpr
                    (noLoc (PatVar (Identifier "existing")))
                    (noLoc (PyLiteral (PyInt 5))))))
          ]
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = moduleBody
              }
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
            resultDecl = listToMaybe [decl | decl@(CppVariable name _ _) <- decls, name == "result"]
        case resultDecl of
          Just (CppVariable _ _ (Just initExpr)) ->
            initExpr `shouldSatisfy` exprContainsLambda
          other -> expectationFailure $ "Expected hoisted declaration for 'result', found " <> show other
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

slicingSpec :: Spec
slicingSpec = describe "Sequence slicing" $ do
  it "lowers list slicing via fluxus_slice" $ do
    let valuesAssign =
          noLoc
            ( PyAssign
                [noLoc (PatVar (Identifier "values"))]
                (noLoc (PyList (map literalInt [1, 2, 3, 4])))
            )
        sliceAssign =
          noLoc
            ( PyAssign
                [noLoc (PatVar (Identifier "middle"))]
                (noLoc (PySubscript
                  (noLoc (PyVar (Identifier "values")))
                  (sliceRange 1 3)))
            )
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = [valuesAssign, sliceAssign]
              }
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
        decls `shouldSatisfy` any isSliceHelperDecl
        case lookupInitializer decls "middle" of
          Just expr -> expr `shouldSatisfy` isSliceHelperCall
          Nothing -> expectationFailure "Expected initializer for 'middle'"
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

  it "materializes string slicing without runtime fallback" $ do
    let stringAssign =
          noLoc
            ( PyAssign
                [noLoc (PatVar (Identifier "word"))]
                (noLoc (PyLiteral (PyString "fluxus")))
            )
        chunkAssign =
          noLoc
            ( PyAssign
                [noLoc (PatVar (Identifier "chunk"))]
                (noLoc (PySubscript
                  (noLoc (PyVar (Identifier "word")))
                  (sliceRange 0 3)))
            )
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = [stringAssign, chunkAssign]
              }
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
        case lookupInitializer decls "chunk" of
          Just expr -> expr `shouldSatisfy` isSliceHelperCall
          Nothing -> expectationFailure "Expected initializer for 'chunk'"
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure
  where
    isSliceHelperDecl decl = case decl of
      CppTemplate _ (CppFunction name _ _ _) -> name == "fluxus_slice"
      _ -> False

    lookupInitializer decls name =
      listToMaybe [expr | CppVariable varName _ (Just expr) <- decls, varName == name]

    isSliceHelperCall expr = case expr of
      CppCall (CppVar name) args -> name == "fluxus_slice" && length args == 4
      _ -> False

    literalInt n = noLoc (PyLiteral (PyInt n))

    sliceRange start stop =
      noLoc (SliceSlice (Just (literalInt start)) (Just (literalInt stop)) Nothing)

exprContainsLambda :: CppExpr -> Bool
exprContainsLambda expr = case expr of
  CppLambda {} -> True
  CppCall func args ->
    isLambda func
      || exprContainsLambda func
      || any exprContainsLambda args
  CppBinary _ lhs rhs -> exprContainsLambda lhs || exprContainsLambda rhs
  CppConditional a b c -> any exprContainsLambda [a, b, c]
  CppUnary _ inner -> exprContainsLambda inner
  CppIndex base idx -> exprContainsLambda base || exprContainsLambda idx
  CppBracedInit _ exprs -> any exprContainsLambda exprs
  _ -> False
  where
    isLambda (CppLambda _ _) = True
    isLambda _ = False
