{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.CodeGen.CPP.Python (spec) where

import Data.Foldable (for_)
import Data.List (find)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Fluxus.AST.Common
import Fluxus.AST.Python
import qualified Fluxus.AST.Python as Py
import Fluxus.Analysis.CommonExprLowering (pythonExprToCommon, renderCommonExpr)
import Fluxus.CodeGen.CPP
import Fluxus.CodeGen.CPP.AST (CppCatch(..))
import Fluxus.CodeGen.CPP.Diagnostics (CppDiagnostic(..), DiagnosticSeverity(..))
import Fluxus.Compiler.Driver
  ( CompilerConfig(..)
  , SourceLanguage(..)
  , compileFile
  , defaultConfig
  , runCompiler
  , setupCompilerEnvironment
  )
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), replaceExtension)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec

import qualified Test.Fluxus.CodeGen.CPP.Shared as Shared

spec :: Spec
spec = describe "Python code generation" $ do
  typeMappingIndependentSpecs
  pythonRuntimeSpec

-- Translation-oriented specs --------------------------------------------------

typeMappingIndependentSpecs :: Spec
typeMappingIndependentSpecs = do
  expressionGenerationSpec
  statementGenerationSpec
  fallbackHandlingSpec
  declarationGenerationSpec
  pythonGlobalSpec
  analysisFeedbackSpec

expressionGenerationSpec :: Spec
expressionGenerationSpec = describe "Expression generation" $ do
  it "translates Python arithmetic expressions when hoisting globals" $ do
    let moduleBody =
          [ noLoc (PyAssign [noLoc (PatVar (Identifier "total"))]
              (noLoc (PyBinaryOp OpAdd
                        (noLoc (PyLiteral (PyInt 1)))
                        (noLoc (PyLiteral (PyInt 2))))))
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
        isTotalVar decl = case decl of
          CppVariable name _ _ -> name == "total"
          _ -> False
    case result of
      Right res ->
        case find isTotalVar (cppDeclarations (cgrUnit res)) of
          Just (CppVariable _ _ (Just initializer)) ->
            initializer `shouldBe`
              CppBinary "+"
                (CppLiteral (CppIntLit 1))
                (CppLiteral (CppIntLit 2))
          _ ->
            expectationFailure "Expected hoisted declaration for variable 'total'"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "turns Python print into std::cout streaming" $ do
    let moduleBody =
          [ noLoc
              ( PyExprStmt
                  ( noLoc
                      ( PyCall
                          (noLoc (PyVar (Identifier "print")))
                          [ noLoc (ArgPositional (noLoc (PyLiteral (PyString "hello")))) ]
                      )
                  )
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
      Right res ->
        case find Shared.isMainFunction (cppDeclarations (cgrUnit res)) of
          Just (CppFunction _ _ _ body) ->
            case listToMaybe [expr | CppExprStmt expr <- body] of
              Just expr ->
                expr `shouldBe`
                  CppBinary "<<"
                    (CppBinary "<<" (CppVar "std::cout") (CppLiteral (CppStringLit "hello")))
                    (CppVar "std::endl")
              Nothing ->
                expectationFailure "Expected print statement in generated main body"
          _ ->
            expectationFailure "Expected generated main function"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

statementGenerationSpec :: Spec
statementGenerationSpec = describe "Statement generation" $ do
  it "lowers Python if statements to CppIf" $ do
    let moduleBody =
          [ noLoc
              ( PyIf
                  (noLoc (PyLiteral (PyBool True)))
                  [ noLoc
                      ( PyExprStmt
                          ( noLoc
                              ( PyCall
                                  (noLoc (PyVar (Identifier "print")))
                                  [ noLoc (ArgPositional (noLoc (PyLiteral (PyString "then")))) ]
                              )
                          )
                      )
                  ]
                  [ noLoc
                      ( PyExprStmt
                          ( noLoc
                              ( PyCall
                                  (noLoc (PyVar (Identifier "print")))
                                  [ noLoc (ArgPositional (noLoc (PyLiteral (PyString "else")))) ]
                              )
                          )
                      )
                  ]
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
      Right res ->
        case find Shared.isMainFunction (cppDeclarations (cgrUnit res)) of
          Just (CppFunction _ _ _ body) ->
            case listToMaybe [(cond, thenStmts, elseStmts) | CppIf cond thenStmts elseStmts <- body] of
              Just (cond, thenStmts, elseStmts) -> do
                cond `shouldBe` CppLiteral (CppBoolLit True)
                let expectedStream text =
                      CppBinary "<<"
                        (CppBinary "<<" (CppVar "std::cout") (CppLiteral (CppStringLit text)))
                        (CppVar "std::endl")
                listToMaybe [expr | CppExprStmt expr <- thenStmts]
                  `shouldBe` Just (expectedStream "then")
                listToMaybe [expr | CppExprStmt expr <- elseStmts]
                  `shouldBe` Just (expectedStream "else")
              Nothing ->
                expectationFailure "Expected an if statement in generated main body"
          _ ->
            expectationFailure "Expected generated main function"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "emits CppWhile nodes for Python while loops" $ do
    let moduleBody =
          [ noLoc (PyAssign [noLoc (PatVar (Identifier "n"))] (noLoc (PyLiteral (PyInt 0))))
          , noLoc
              ( PyWhile
                  ( noLoc
                      ( PyComparison
                          [OpLt]
                          [ noLoc (PyVar (Identifier "n"))
                          , noLoc (PyLiteral (PyInt 3))
                          ]
                      )
                  )
                  [ noLoc
                      ( PyExprStmt
                          ( noLoc
                              ( PyCall
                                  (noLoc (PyVar (Identifier "print")))
                                  [ noLoc (ArgPositional (noLoc (PyVar (Identifier "n")))) ]
                              )
                          )
                      )
                  , noLoc
                      ( PyAssign
                          [noLoc (PatVar (Identifier "n"))]
                          ( noLoc
                              ( PyBinaryOp OpAdd
                                  (noLoc (PyVar (Identifier "n")))
                                  (noLoc (PyLiteral (PyInt 1)))
                              )
                          )
                      )
                  ]
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
      Right res ->
        case find Shared.isMainFunction (cppDeclarations (cgrUnit res)) of
          Just (CppFunction _ _ _ body) ->
            case listToMaybe [(cond, loopStmts) | CppWhile cond loopStmts <- body] of
              Just (cond, loopStmts) -> do
                cond `shouldBe`
                  CppBinary "<" (CppVar "n") (CppLiteral (CppIntLit 3))
                let hasIncrement = any incrementsN loopStmts
                    incrementsN stmt =
                      case stmt of
                        CppExprStmt (CppBinary "=" (CppVar "n") (CppBinary "+" (CppVar "n") (CppLiteral (CppIntLit 1)))) -> True
                        CppStmtSeq inner -> any incrementsN inner
                        CppBlock inner -> any incrementsN inner
                        _ -> False
                hasIncrement `shouldBe` True
              Nothing ->
                expectationFailure "Expected a while loop in generated main body"
          _ ->
            expectationFailure "Expected generated main function"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

fallbackHandlingSpec :: Spec
fallbackHandlingSpec = describe "Runtime fallback handling" $ do
  it "injects runtime fallback for with statements in non-strict mode" $ do
    let withItem = PythonWithItem
          { pyWithContext = noLoc (PyCall (noLoc (PyVar (Identifier "make_resource"))) [])
          , pyWithVar = Just (noLoc (PatVar (Identifier "resource")))
          }
        bodyStmt = noLoc
          ( PyExprStmt
              ( noLoc
                  ( PyCall
                      (noLoc (PyVar (Identifier "use")))
                      [ noLoc (ArgPositional (noLoc (PyVar (Identifier "resource")))) ]
                  )
              )
          )
        moduleBody = [noLoc (PyWith [noLoc withItem] [bodyStmt])]
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = moduleBody
          }
        fallbackConfig = Shared.testCppConfig
    case generateCpp fallbackConfig (Left pythonAst) of
      Right res -> do
        let warnings = filter ((== SeverityWarning) . diagSeverity) (cgrDiagnostics res)
        warnings `shouldSatisfy`
          any (T.isInfixOf "'with' statement" . diagMessage)
        case find Shared.isMainFunction (cppDeclarations (cgrUnit res)) of
          Just (CppFunction _ _ _ body) ->
            containsRuntimeAbort body `shouldBe` True
          _ -> expectationFailure "Expected generated main function"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "reports runtime fallback for try statements in non-strict mode" $ do
    let exceptBlock = PythonExcept
          { pyExceptType = Just (noLoc (PyVar (Identifier "Exception")))
          , pyExceptName = Just (Identifier "exc")
          , pyExceptBody = [noLoc PyPass]
          }
        tryStmt = noLoc (PyTry [noLoc PyPass] [noLoc exceptBlock] [] [])
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = [tryStmt]
          }
        fallbackConfig = Shared.testCppConfig
    case generateCpp fallbackConfig (Left pythonAst) of
      Right res -> do
        let warnings = filter ((== SeverityWarning) . diagSeverity) (cgrDiagnostics res)
        warnings `shouldSatisfy`
          any (T.isInfixOf "'try' statement" . diagMessage)
        case find Shared.isMainFunction (cppDeclarations (cgrUnit res)) of
          Just (CppFunction _ _ _ body) ->
            containsRuntimeAbort body `shouldBe` True
          _ -> expectationFailure "Expected generated main function"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "reports runtime fallback for raise statements in non-strict mode" $ do
    let raiseStmt = noLoc
          ( PyRaise
              ( Just
                  ( noLoc
                      ( PyCall
                          (noLoc (PyVar (Identifier "ValueError")))
                          [ noLoc (ArgPositional (noLoc (PyLiteral (PyString "boom")))) ]
                      )
                  )
              )
              Nothing
          )
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = [raiseStmt]
          }
        fallbackConfig = Shared.testCppConfig
    case generateCpp fallbackConfig (Left pythonAst) of
      Right res -> do
        let warnings = filter ((== SeverityWarning) . diagSeverity) (cgrDiagnostics res)
        warnings `shouldSatisfy`
          any (T.isInfixOf "'raise' statement" . diagMessage)
        case find Shared.isMainFunction (cppDeclarations (cgrUnit res)) of
          Just (CppFunction _ _ _ body) ->
            containsRuntimeAbort body `shouldBe` True
          _ -> expectationFailure "Expected generated main function"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "generates runtime fallback stub for async functions in non-strict mode" $ do
    let asyncFunc = PythonFuncDef
          { pyFuncName = Identifier "worker"
          , pyFuncDecorators = []
          , pyFuncParams = []
          , pyFuncReturns = Nothing
          , pyFuncBody = [noLoc PyPass]
          , pyFuncDoc = Nothing
          , pyFuncIsAsync = True
          }
        moduleBody = [noLoc (PyAsyncFuncDef asyncFunc)]
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = moduleBody
          }
        fallbackConfig = Shared.testCppConfig
    case generateCpp fallbackConfig (Left pythonAst) of
      Right res -> do
        let warnings = filter ((== SeverityWarning) . diagSeverity) (cgrDiagnostics res)
        warnings `shouldSatisfy`
          any (T.isInfixOf "async function" . diagMessage)
        let decls = cppDeclarations (cgrUnit res)
            isWorker decl = case decl of
              CppFunction name _ _ body | name == "worker" -> containsRuntimeAbort body
              _ -> False
        decls `shouldSatisfy` any isWorker
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure
  where
    containsRuntimeAbort :: [CppStmt] -> Bool
    containsRuntimeAbort = any stmtHasRuntimeAbort

    stmtHasRuntimeAbort :: CppStmt -> Bool
    stmtHasRuntimeAbort stmt =
      case stmt of
        CppExprStmt (CppCall (CppVar "fluxus_runtime_abort") _) -> True
        CppStmtSeq stmts -> any stmtHasRuntimeAbort stmts
        CppBlock stmts -> any stmtHasRuntimeAbort stmts
        CppIf _ thenStmts elseStmts -> any stmtHasRuntimeAbort (thenStmts ++ elseStmts)
        CppWhile _ body -> any stmtHasRuntimeAbort body
        CppFor mInit _ _ body ->
          maybe False stmtHasRuntimeAbort mInit || any stmtHasRuntimeAbort body
        CppTry tryStmts catches finallyStmts ->
          any stmtHasRuntimeAbort tryStmts
            || any catchHasRuntimeAbort catches
            || any stmtHasRuntimeAbort finallyStmts
        _ -> False

    catchHasRuntimeAbort :: CppCatch -> Bool
    catchHasRuntimeAbort (CppCatch _ _ body) = any stmtHasRuntimeAbort body

declarationGenerationSpec :: Spec
declarationGenerationSpec = describe "Declaration generation" $ do
  it "emits CppFunction declarations for Python defs" $ do
    let funcDef = PythonFuncDef
          { pyFuncName = Identifier "add"
          , pyFuncDecorators = []
          , pyFuncParams =
              [ noLoc (ParamNormal (Identifier "x") Nothing Nothing)
              , noLoc (ParamNormal (Identifier "y") Nothing Nothing)
              ]
          , pyFuncReturns = Nothing
          , pyFuncBody =
              [ noLoc
                  ( PyReturn
                      ( Just
                          ( noLoc
                              ( PyBinaryOp OpAdd
                                  (noLoc (PyVar (Identifier "x")))
                                  (noLoc (PyVar (Identifier "y")))
                              )
                          )
                      )
                  )
              ]
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
        result = generateCpp Shared.testCppConfig (Left pythonAst)
        isAdd decl = case decl of
          CppFunction name _ _ _ -> name == "add"
          _ -> False
    case result of
      Right res ->
        case find isAdd (cppDeclarations (cgrUnit res)) of
          Just (CppFunction _ returnType params body) -> do
            returnType `shouldBe` CppAuto
            params `shouldBe`
              [ CppParam "x" CppAuto Nothing
              , CppParam "y" CppAuto Nothing
              ]
            listToMaybe [expr | CppReturn (Just expr) <- body]
              `shouldBe`
                Just (CppBinary "+" (CppVar "x") (CppVar "y"))
          _ ->
            expectationFailure "Expected generated declaration for function 'add'"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "honors Python type annotations for parameters and returns" $ do
    let intType = noLoc (TypeName (QualifiedName [] (Identifier "int")))
        listIntType =
          noLoc
            ( TypeSubscript
                (noLoc (TypeName (QualifiedName [] (Identifier "list"))))
                [intType]
            )
        optionalIntType =
          noLoc
            ( TypeUnion
                [ intType
                , noLoc (TypeName (QualifiedName [] (Identifier "None")))
                ]
            )
        funcDef = PythonFuncDef
          { pyFuncName = Identifier "process"
          , pyFuncDecorators = []
          , pyFuncParams =
              [ noLoc (ParamNormal (Identifier "value") (Just intType) Nothing)
              , noLoc (ParamNormal (Identifier "items") (Just listIntType) Nothing)
              ]
          , pyFuncReturns = Just optionalIntType
          , pyFuncBody =
              [ noLoc
                  ( PyReturn
                      (Just (noLoc (PyVar (Identifier "value"))))
                  )
              ]
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
        isProcess decl = case decl of
          CppFunction name _ _ _ -> name == "process"
          _ -> False
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res ->
        case find isProcess (cppDeclarations (cgrUnit res)) of
          Just (CppFunction _ returnType params _) -> do
            returnType `shouldBe` CppOptional CppLongLong
            params `shouldBe`
              [ CppParam "value" CppLongLong Nothing
              , CppParam "items" (CppVector CppLongLong) Nothing
              ]
          _ ->
            expectationFailure "Expected generated function 'process'"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "reports std::any fallback for unsupported annotations" $ do
    let typeVarT = noLoc (Py.TypeVar "T")
        funcDef = PythonFuncDef
          { pyFuncName = Identifier "wrap"
          , pyFuncDecorators = []
          , pyFuncParams =
              [ noLoc (ParamNormal (Identifier "payload") (Just typeVarT) Nothing) ]
          , pyFuncReturns = Nothing
          , pyFuncBody = [noLoc (PyReturn Nothing)]
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
        isWrap decl = case decl of
          CppFunction name _ _ _ -> name == "wrap"
          _ -> False
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        case find isWrap (cppDeclarations (cgrUnit res)) of
          Just (CppFunction _ _ params _) ->
            params `shouldBe` [CppParam "payload" (CppClassType "std::any" []) Nothing]
          _ ->
            expectationFailure "Expected generated function 'wrap'"
        let warnings =
              filter ((== SeverityWarning) . diagSeverity) (cgrDiagnostics res)
        warnings `shouldSatisfy`
          any (T.isInfixOf "std::any" . diagMessage)
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "emits CppClass declarations for Python classes" $ do
    let classDef = PythonClassDef
          { pyClassName = Identifier "Sample"
          , pyClassDecorators = []
          , pyClassBases = []
          , pyClassKeywords = []
          , pyClassBody = []
          , pyClassDoc = Nothing
          }
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = [noLoc (PyClassDef classDef)]
              }
        result = generateCpp Shared.testCppConfig (Left pythonAst)
        isSample decl = case decl of
          CppClass name _ _ -> name == "Sample"
          _ -> False
    case result of
      Right res ->
        case find isSample (cppDeclarations (cgrUnit res)) of
          Just (CppClass _ bases members) -> do
            bases `shouldBe` []
            members `shouldBe` []
          _ ->
            expectationFailure "Expected generated declaration for class 'Sample'"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "maps Python base classes to C++ inheritance list" $ do
    let baseExpr =
          noLoc
            ( PyAttribute
                (noLoc (PyVar (Identifier "parent")))
                (Identifier "Base")
            )
        classDef = PythonClassDef
          { pyClassName = Identifier "Child"
          , pyClassDecorators = []
          , pyClassBases = [baseExpr]
          , pyClassKeywords = []
          , pyClassBody = []
          , pyClassDoc = Nothing
          }
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = [noLoc (PyClassDef classDef)]
              }
        isChild decl = case decl of
          CppClass name _ _ -> name == "Child"
          _ -> False
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res ->
        case find isChild (cppDeclarations (cgrUnit res)) of
          Just (CppClass _ bases members) -> do
            bases `shouldBe` ["parent::Base"]
            members `shouldBe` []
          _ -> expectationFailure "Expected generated declaration for class 'Child'"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "generates class attributes and instance methods" $ do
    let attributeAssign =
          noLoc
            ( PyAssign
                [noLoc (PatVar (Identifier "value"))]
                (noLoc (PyLiteral (PyInt 10)))
            )
        methodDef = PythonFuncDef
          { pyFuncName = Identifier "double"
          , pyFuncDecorators = []
          , pyFuncParams =
              [ noLoc (ParamNormal (Identifier "self") Nothing Nothing)
              , noLoc (ParamNormal (Identifier "amount") Nothing Nothing)
              ]
          , pyFuncReturns = Nothing
          , pyFuncBody =
              [ noLoc
                  ( PyReturn
                      ( Just
                          ( noLoc
                              ( PyBinaryOp OpMul
                                  (noLoc (PyVar (Identifier "amount")))
                                  (noLoc (PyLiteral (PyInt 2)))
                              )
                          )
                      )
                  )
              ]
          , pyFuncDoc = Nothing
          , pyFuncIsAsync = False
          }
        classDef = PythonClassDef
          { pyClassName = Identifier "Rich"
          , pyClassDecorators = []
          , pyClassBases = []
          , pyClassKeywords = []
          , pyClassBody = [attributeAssign, noLoc (PyFuncDef methodDef)]
          , pyClassDoc = Nothing
          }
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = [noLoc (PyClassDef classDef)]
              }
        isRich decl = case decl of
          CppClass name _ _ -> name == "Rich"
          _ -> False
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res ->
        case find isRich (cppDeclarations (cgrUnit res)) of
          Just (CppClass _ bases members) -> do
            bases `shouldBe` []
            case members of
              [ CppAccessSpec "public"
                , CppVariable attrName attrType (Just initializer)
                , CppMethod methodName returnType params body False
                ] -> do
                  attrName `shouldBe` "value"
                  attrType `shouldBe` CppLongLong
                  initializer `shouldBe` CppLiteral (CppIntLit 10)
                  methodName `shouldBe` "double"
                  returnType `shouldBe` CppAuto
                  params `shouldBe` [CppParam "amount" CppAuto Nothing]
                  listToMaybe [expr | CppReturn (Just expr) <- body]
                    `shouldBe`
                      Just (CppBinary "*" (CppVar "amount") (CppLiteral (CppIntLit 2)))
              _ -> expectationFailure "Expected class members for attribute and method"
          _ -> expectationFailure "Expected generated declaration for class 'Rich'"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

pythonGlobalSpec :: Spec
pythonGlobalSpec = describe "Python module handling" $ do
  it "hoists module-level assignments to global declarations" $ do
    let result = generateCpp Shared.testCppConfig (Left pythonAst)
    case result of
      Right res ->
        let decls = cppDeclarations (cgrUnit res) in
        case decls of
          (CppVariable name _ _) : rest -> do
            name `shouldBe` "x"
            case find Shared.isFooFunction rest of
              Just _ -> pure ()
              Nothing -> expectationFailure "Expected foo function declaration"
            case find Shared.isMainFunction rest of
              Just (CppFunction _ _ _ body) ->
                any (declaresVar "x") body `shouldBe` False
              _ -> expectationFailure "Expected generated main function"
          _ -> expectationFailure "Expected module-level variable declaration"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure
  where
    pythonAst = PythonAST PythonModule
      { pyModuleName = Just (ModuleName "sample")
      , pyModuleDoc = Nothing
      , pyModuleImports = []
      , pyModuleBody = moduleBody
      }
    moduleBody =
      [ noLoc (PyAssign [noLoc (PatVar (Identifier "x"))] (noLoc (PyLiteral (PyInt 10))))
      , noLoc (PyFuncDef fooDef)
      , noLoc (PyExprStmt (noLoc printCall))
      ]
    fooDef = PythonFuncDef
      { pyFuncName = Identifier "foo"
      , pyFuncDecorators = []
      , pyFuncParams = []
      , pyFuncReturns = Nothing
      , pyFuncBody = [noLoc (PyReturn (Just (noLoc (PyVar (Identifier "x")))))]
      , pyFuncDoc = Nothing
      , pyFuncIsAsync = False
      }
    fooCallExpr = noLoc $ PyCall (noLoc (PyVar (Identifier "foo"))) []
    printCall = PyCall (noLoc (PyVar (Identifier "print")))
      [noLoc (ArgPositional fooCallExpr)]
    declaresVar target stmt = case stmt of
      CppDecl (CppVariable name _ _) -> name == target
      CppStmtSeq stmts -> any (declaresVar target) stmts
      CppBlock stmts -> any (declaresVar target) stmts
      _ -> False


analysisFeedbackSpec :: Spec
analysisFeedbackSpec = describe "Analysis annotation integration" $ do
  let annotationsFor expr =
        case pythonExprToCommon expr of
          Left err -> error ("Failed to lower expression: " <> show err)
          Right common ->
            insertAnnotations (renderCommonExpr common) exprAnnotation emptyAnnotations

      exprAnnotation =
        ExprAnnotations
          { eaInferredType = Just (TOwned TString)
          , eaOwnership = Just ownershipInfo
          , eaEscapeInfo = Just EscapeToHeap
          , eaOptimizationNotes = ["factory result escapes to heap"]
          }

      ownershipInfo =
        OwnershipInfo
          { ownsMemory = True
          , canMove = True
          , refCount = Just 1
          , escapes = EscapeToHeap
          , memLocation = Heap
          }

  it "refines module-level variable declarations using analysis annotations" $ do
    let callExpr = noLoc (PyCall (noLoc (PyVar (Identifier "factory"))) [])
        assignment = noLoc (PyAssign [noLoc (PatVar (Identifier "value"))] callExpr)
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = [assignment]
          }
        annotations = annotationsFor callExpr
    case generateCppWithAnnotations Shared.testCppConfig annotations (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
            isValueDecl decl = case decl of
              CppVariable name _ _ -> name == "value"
              _ -> False
        case find isValueDecl decls of
          Just (CppVariable _ varType (Just initializer)) -> do
            varType `shouldBe` CppUniquePtr CppString
            initializer `shouldBe` CppCall (CppVar "factory") []
          _ -> expectationFailure "Expected annotated variable declaration for 'value'"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "refines function return types using analysis annotations" $ do
    let callExpr = noLoc (PyCall (noLoc (PyVar (Identifier "factory"))) [])
        funcDef = PythonFuncDef
          { pyFuncName = Identifier "make"
          , pyFuncDecorators = []
          , pyFuncParams = []
          , pyFuncReturns = Nothing
          , pyFuncBody = [noLoc (PyReturn (Just callExpr))]
          , pyFuncDoc = Nothing
          , pyFuncIsAsync = False
          }
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = [noLoc (PyFuncDef funcDef)]
          }
        annotations = annotationsFor callExpr
    case generateCppWithAnnotations Shared.testCppConfig annotations (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
            isMake decl = case decl of
              CppFunction name _ _ _ -> name == "make"
              _ -> False
        case find isMake decls of
          Just (CppFunction _ retType _ body) -> do
            retType `shouldBe` CppUniquePtr CppString
            listToMaybe [expr | CppReturn (Just expr) <- body]
              `shouldBe` Just (CppCall (CppVar "factory") [])
          _ -> expectationFailure "Expected annotated function 'make'"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

-- Runtime compilation specs ---------------------------------------------------

pythonRuntimeSpec :: Spec
pythonRuntimeSpec = describe "Python end-to-end compilation" $ do
  maybeCompiler <- runIO Shared.findCppCompiler
  case maybeCompiler of
    Nothing ->
      it "requires an available C++ compiler" $
        expectationFailure "No C++ compiler found in PATH"
    Just compiler ->
      for_ pythonRuntimeTests $ \testCase ->
        it (prtName testCase) $
          case prtPendingReason testCase of
            Just reason -> pendingWith reason
            Nothing -> runPythonRuntimeTest compiler testCase

data PythonRuntimeTest = PythonRuntimeTest
  { prtName :: String
  , prtSource :: [String]
  , prtExpectedStdOut :: String
  , prtPendingReason :: Maybe String
  }

runPythonRuntimeTest :: FilePath -> PythonRuntimeTest -> Expectation
runPythonRuntimeTest compiler PythonRuntimeTest { prtName = name, prtSource = sourceLines, prtExpectedStdOut = expectedStdOut } =
  withSystemTempDirectory ("fluxus-python-cpp-" ++ Shared.sanitizeName name) $ \tmpDir -> do
    let sourcePath = tmpDir </> "input.py"
        outputBinary = tmpDir </> "program"
        pythonSource = unlines sourceLines
        config =
          defaultConfig
            { ccSourceLanguage = Python
            , ccCppCompiler = T.pack compiler
            , ccOutputPath = Just outputBinary
            , ccVerboseLevel = 0
            , ccWorkDirectory = Just tmpDir
            , ccKeepIntermediates = True
            }
    writeFile sourcePath pythonSource
    compileResult <- runCompiler config $ do
      setupCompilerEnvironment
      compileFile sourcePath
    case compileResult of
      Left err ->
        expectationFailure $ "Compilation failed: " <> show err
      Right (finalBinary, _) -> do
        finalBinary `shouldBe` outputBinary
        cppExists <- doesFileExist (replaceExtension sourcePath ".cpp")
        cppExists `shouldBe` True
        binaryExists <- doesFileExist finalBinary
        binaryExists `shouldBe` True
        (exitCode, stdOut, _) <- readProcessWithExitCode finalBinary [] ""
        exitCode `shouldBe` ExitSuccess
        stdOut `shouldBe` expectedStdOut

pythonRuntimeTests :: [PythonRuntimeTest]
pythonRuntimeTests =
  [ PythonRuntimeTest
      { prtName = "compiles simple print"
      , prtSource =
          [ "print(\"hello fluxus\")"
          ]
      , prtExpectedStdOut = "hello fluxus\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles integer addition"
      , prtSource =
          [ "result = 21 + 21"
          , "print(result)"
          ]
      , prtExpectedStdOut = "42\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles arithmetic subtraction"
      , prtSource =
          [ "value = 30"
          , "value = value - 12"
          , "print(value)"
          ]
      , prtExpectedStdOut = "18\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles arithmetic multiplication"
      , prtSource =
          [ "print(6 * 7)"
          ]
      , prtExpectedStdOut = "42\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles division"
      , prtSource =
          [ "print(84 / 2)"
          ]
      , prtExpectedStdOut = "42.0\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles modulo"
      , prtSource =
          [ "print(41 % 6)"
          ]
      , prtExpectedStdOut = "5\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles exponentiation"
      , prtSource =
          [ "print(2 ** 5)"
          ]
      , prtExpectedStdOut = "32\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles chained arithmetic"
      , prtSource =
          [ "value = (2 + 3) * 4"
          , "print(value)"
          ]
      , prtExpectedStdOut = "20\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles boolean expression"
      , prtSource =
          [ "print(True and not False)"
          ]
      , prtExpectedStdOut = "True\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles equality branch"
      , prtSource =
          [ "value = 10"
          , "if value == 10:"
          , "    print(\"ten\")"
          , "else:"
          , "    print(\"other\")"
          ]
      , prtExpectedStdOut = "ten\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles nested conditional branch"
      , prtSource =
          [ "a = 5"
          , "b = 7"
          , "c = 6"
          , "if a > b:"
          , "    print(\"a\")"
          , "else:"
          , "    if b > c:"
          , "        print(\"b\")"
          , "    else:"
          , "        print(\"c\")"
          ]
      , prtExpectedStdOut = "b\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles simple loop"
      , prtSource =
          [ "count = 0"
          , "for i in range(3):"
          , "    count = count + 1"
          , "print(count)"
          ]
      , prtExpectedStdOut = "3\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles nested loops accumulation"
      , prtSource =
          [ "count = 0"
          , "for i in range(2):"
          , "    for j in range(3):"
          , "        count = count + 1"
          , "print(count)"
          ]
      , prtExpectedStdOut = "6\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles string returning function"
      , prtSource =
          [ "def greet(name):"
          , "    return f\"Hello {name}\""
          , ""
          , "print(greet(\"Fluxus\"))"
          ]
      , prtExpectedStdOut = "Hello Fluxus\n"
      , prtPendingReason = Just "Python f-string expression evaluation is not yet supported in the C++ backend"
      }
  , PythonRuntimeTest
      { prtName = "compiles local variable function"
      , prtSource =
          [ "def compute():"
          , "    result = 10"
          , "    result = result + 5"
          , "    return result"
          , ""
          , "print(compute())"
          ]
      , prtExpectedStdOut = "15\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles dependent functions"
      , prtSource =
          [ "def square(x):"
          , "    return x * x"
          , ""
          , "def cube(x):"
          , "    return x * square(x)"
          , ""
          , "print(cube(3))"
          ]
      , prtExpectedStdOut = "27\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles indexed list summation"
      , prtSource =
          [ "values = [1, 2, 3, 4]"
          , "total = 0"
          , "for i in range(len(values)):"
          , "    total = total + values[i]"
          , "print(total)"
          ]
      , prtExpectedStdOut = "10\n"
      , prtPendingReason = Just "List literal parsing is not yet implemented in the Python frontend"
      }
  , PythonRuntimeTest
      { prtName = "compiles multi step string concatenation"
      , prtSource =
          [ "part = \"Flux\""
          , "part = part + \"us\""
          , "part = part + \" Rocks\""
          , "print(part)"
          ]
      , prtExpectedStdOut = "Fluxus Rocks\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles boolean or branch"
      , prtSource =
          [ "a = False"
          , "b = True"
          , "if a or b:"
          , "    print(\"pass\")"
          , "else:"
          , "    print(\"fail\")"
          ]
      , prtExpectedStdOut = "pass\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles ranged even summation"
      , prtSource =
          [ "total = 0"
          , "for i in range(2, 10, 2):"
          , "    total = total + i"
          , "print(total)"
          ]
      , prtExpectedStdOut = "20\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles descending range summation"
      , prtSource =
          [ "total = 0"
          , "for i in range(5, 0, -1):"
          , "    total = total + i"
          , "print(total)"
          ]
      , prtExpectedStdOut = "15\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles fibonacci recursion"
      , prtSource =
          [ "def fib(n):"
          , "    if n <= 1:"
          , "        return n"
          , "    return fib(n - 1) + fib(n - 2)"
          , ""
          , "print(fib(6))"
          ]
      , prtExpectedStdOut = "8\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles boolean returning function"
      , prtSource =
          [ "def is_positive(n):"
          , "    if n > 0:"
          , "        return True"
          , "    else:"
          , "        return False"
          , ""
          , "if is_positive(5):"
          , "    print(\"positive\")"
          , "else:"
          , "    print(\"non-positive\")"
          ]
      , prtExpectedStdOut = "positive\n"
      , prtPendingReason = Just "Identifiers that embed Python keywords (like is_positive) are not handled by the lexer"
      }
  , PythonRuntimeTest
      { prtName = "compiles countdown loop"
      , prtSource =
          [ "count = 3"
          , "while count > 0:"
          , "    print(count)"
          , "    count = count - 1"
          ]
      , prtExpectedStdOut = unlines ["3", "2", "1"]
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles list returning function"
      , prtSource =
          [ "def pair_sum(a, b):"
          , "    values = [a, b, a + b]"
          , "    return values[2]"
          , ""
          , "print(pair_sum(3, 4))"
          ]
      , prtExpectedStdOut = "7\n"
      , prtPendingReason = Just "List literal parsing is not yet implemented in the Python frontend"
      }
  , PythonRuntimeTest
      { prtName = "compiles string repetition helper"
      , prtSource =
          [ "def repeat_phrase(phrase, count):"
          , "    result = \"\""
          , "    i = 0"
          , "    while i < count:"
          , "        result = result + phrase"
          , "        i = i + 1"
          , "    return result"
          , ""
          , "print(repeat_phrase(\"ha\", 3))"
          ]
      , prtExpectedStdOut = "hahaha\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles doubled range accumulation"
      , prtSource =
          [ "def double_sum(limit):"
          , "    total = 0"
          , "    for i in range(limit):"
          , "        total = total + (i * 2)"
          , "    return total"
          , ""
          , "print(double_sum(4))"
          ]
      , prtExpectedStdOut = "12\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles floor division operation"
      , prtSource =
          [ "value = 41 // 3"
          , "print(value)"
          ]
      , prtExpectedStdOut = "13\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles nested while loops"
      , prtSource =
          [ "outer = 0"
          , "count = 0"
          , "while outer < 2:"
          , "    inner = 0"
          , "    while inner < 2:"
          , "        count = count + 1"
          , "        inner = inner + 1"
          , "    outer = outer + 1"
          , "print(count)"
          ]
      , prtExpectedStdOut = "4\n"
      , prtPendingReason = Just "Nested while loops are not yet supported in the Python frontend"
      }
  , PythonRuntimeTest
      { prtName = "compiles if elif chain"
      , prtSource =
          [ "value = 0"
          , "if value > 0:"
          , "    print(\"positive\")"
          , "elif value == 0:"
          , "    print(\"zero\")"
          , "else:"
          , "    print(\"negative\")"
          ]
      , prtExpectedStdOut = "zero\n"
      , prtPendingReason = Just "Python elif chain lowering is not yet implemented in the C++ backend"
      }
  , PythonRuntimeTest
      { prtName = "compiles string multiplication"
      , prtSource =
          [ "print(\"ha\" * 3)"
          ]
      , prtExpectedStdOut = "hahaha\n"
      , prtPendingReason = Just "Python string multiplication code generation is not yet implemented in the C++ backend"
      }
  , PythonRuntimeTest
      { prtName = "compiles boolean list counting"
      , prtSource =
          [ "values = [True, False, True, True]"
          , "count = 0"
          , "for value in values:"
          , "    if value:"
          , "        count = count + 1"
          , "print(count)"
          ]
      , prtExpectedStdOut = "3\n"
      , prtPendingReason = Just "List literal parsing is not yet implemented in the Python frontend"
      }
  , PythonRuntimeTest
      { prtName = "compiles min function loop"
      , prtSource =
          [ "def find_min(values):"
          , "    smallest = values[0]"
          , "    for value in values:"
          , "        if value < smallest:"
          , "            smallest = value"
          , "    return smallest"
          , ""
          , "print(find_min([5, 3, 7, 2]))"
          ]
      , prtExpectedStdOut = "2\n"
      , prtPendingReason = Just "List literal parsing is not yet implemented in the Python frontend"
      }
  , PythonRuntimeTest
      { prtName = "compiles even counter function"
      , prtSource =
          [ "def count_even(limit):"
          , "    total = 0"
          , "    for i in range(limit + 1):"
          , "        if i % 2 == 0:"
          , "            total = total + 1"
          , "    return total"
          , ""
          , "print(count_even(5))"
          ]
      , prtExpectedStdOut = "3\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles iterative factorial function"
      , prtSource =
          [ "def factorial_iterative(n):"
          , "    result = 1"
          , "    for i in range(2, n + 1):"
          , "        result = result * i"
          , "    return result"
          , ""
          , "print(factorial_iterative(5))"
          ]
      , prtExpectedStdOut = "120\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles loop string accumulation"
      , prtSource =
          [ "result = \"\""
          , "for i in range(3):"
          , "    result = result + f\"{i}\""
          , "print(result)"
          ]
      , prtExpectedStdOut = "012\n"
      , prtPendingReason = Just "Python f-string expression evaluation is not yet supported in the C++ backend"
      }
  ]
