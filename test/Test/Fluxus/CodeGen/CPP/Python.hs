{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.CodeGen.CPP.Python (spec) where

import Data.Foldable (foldl', for_)
import Data.List (find)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Fluxus.AST.Common
import Fluxus.AST.Python
import qualified Fluxus.AST.Python as Py
import Fluxus.Analysis.CommonExprLowering (pythonExprToLocatedCommon, fingerprintCommonExpr)
import Fluxus.CodeGen.CPP
import Fluxus.CodeGen.CPP.AST (CppCatch(..), CppDecl(..), CppExpr(..), CppLiteral(..), CppStmt(..), CppType(..), renderCppExpr)
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
  identifierSanitizationSpec

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

  it "emits the C++ conditional operator for Python ternary expressions" $ do
    let moduleBody =
          [ noLoc (PyAssign [noLoc (PatVar (Identifier "cond"))] (noLoc (PyLiteral (PyBool True))))
          , noLoc
              ( PyAssign
                  [noLoc (PatVar (Identifier "value"))]
                  ( noLoc
                      ( PyIfExp
                          (noLoc (PyVar (Identifier "cond")))
                          (noLoc (PyLiteral (PyInt 1)))
                          (noLoc (PyLiteral (PyInt 2)))
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
        isValueVar decl = case decl of
          CppVariable name _ _ -> name == "value"
          _ -> False
    case result of
      Right res ->
        case find isValueVar (cppDeclarations (cgrUnit res)) of
          Just (CppVariable _ _ (Just initializer)) ->
            initializer `shouldBe`
              CppConditional
                (CppVar "cond")
                (CppLiteral (CppIntLit 1))
                (CppLiteral (CppIntLit 2))
          _ ->
            expectationFailure "Expected hoisted declaration for variable 'value'"
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

  it "lowers Python list comprehensions into lambda-built vectors" $ do
    let numbersLiteral = noLoc (PyList (map (noLoc . PyLiteral . PyInt) [1, 2, 3, 4]))
        numbersAssign = noLoc (PyAssign [noLoc (PatVar (Identifier "numbers"))] numbersLiteral)
        filterExpr =
          noLoc
            ( PyComparison
                [OpEq]
                [ noLoc
                    ( PyBinaryOp
                        OpMod
                        (noLoc (PyVar (Identifier "n")))
                        (noLoc (PyLiteral (PyInt 2)))
                    )
                , noLoc (PyLiteral (PyInt 0))
                ]
            )
        comprehension = PythonComprehension
          { pyCompTarget = noLoc (PatVar (Identifier "n"))
          , pyCompIter = noLoc (PyVar (Identifier "numbers"))
          , pyCompFilters = [filterExpr]
          , pyCompAsync = False
          }
        listCompExpr =
          noLoc
            ( PyListComp
                ( noLoc
                    ( PyBinaryOp
                        OpMul
                        (noLoc (PyVar (Identifier "n")))
                        (noLoc (PyLiteral (PyInt 2)))
                    )
                )
                [comprehension]
            )
        evensAssign = noLoc (PyAssign [noLoc (PatVar (Identifier "evens"))] listCompExpr)
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = [numbersAssign, evensAssign]
              }
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
            isEvensDecl decl = case decl of
              CppVariable name _ _ -> name == "evens"
              _ -> False
        case find isEvensDecl decls of
          Just (CppVariable _ (CppVector elemType) (Just initializer)) -> do
            elemType `shouldBe` CppClassType "std::any" []
            case initializer of
              CppCall (CppLambda [] lambdaBody) [] ->
                case lambdaBody of
                  CppDecl (CppVariable builderName _ (Just (CppBracedInit _ [])))
                    : [ CppForRange loopVar rangeExpr loopBody
                      , CppReturn (Just (CppVar retVar))
                      ] -> do
                      loopVar `shouldBe` "n"
                      rangeExpr `shouldBe` CppVar "numbers"
                      retVar `shouldBe` builderName
                      case loopBody of
                        [CppIf cond thenStmts []] -> do
                          let expectedCond =
                                CppBinary "=="
                                  (CppBinary "%"
                                    (CppVar "n")
                                    (CppLiteral (CppIntLit 2)))
                                  (CppLiteral (CppIntLit 0))
                          cond `shouldBe` expectedCond
                          case thenStmts of
                            [CppExprStmt pushExpr] -> do
                              let expectedPush =
                                    CppCall
                                      (CppMember (CppVar builderName) "push_back")
                                      [ CppBinary "*"
                                          (CppVar "n")
                                          (CppLiteral (CppIntLit 2))
                                      ]
                              pushExpr `shouldBe` expectedPush
                            _ -> expectationFailure "Expected single push statement inside comprehension body"
                        _ -> expectationFailure "Expected filter guard inside comprehension body"
                  _ -> expectationFailure "Unexpected lambda body emitted for list comprehension"
              _ -> expectationFailure "Expected list comprehension initializer to be a lambda call"
          _ -> expectationFailure "Expected hoisted declaration for 'evens' list comprehension"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "lowers Python set comprehensions into lambda-built sets" $ do
    let numbersLiteral = noLoc (PyList (map (noLoc . PyLiteral . PyInt) [1, 2, 3]))
        numbersAssign = noLoc (PyAssign [noLoc (PatVar (Identifier "numbers"))] numbersLiteral)
        comprehension = PythonComprehension
          { pyCompTarget = noLoc (PatVar (Identifier "n"))
          , pyCompIter = noLoc (PyVar (Identifier "numbers"))
          , pyCompFilters = []
          , pyCompAsync = False
          }
        doubledExpr =
          noLoc
            ( PyBinaryOp
                OpMul
                (noLoc (PyVar (Identifier "n")))
                (noLoc (PyLiteral (PyInt 2)))
            )
        setCompExpr = noLoc (PySetComp doubledExpr [comprehension])
        doubledAssign = noLoc (PyAssign [noLoc (PatVar (Identifier "doubled"))] setCompExpr)
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = [numbersAssign, doubledAssign]
              }
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
            isDoubled decl = case decl of
              CppVariable name _ _ -> name == "doubled"
              _ -> False
        case find isDoubled decls of
          Just (CppVariable _ (CppClassType "std::set" [elemType]) (Just initializer)) -> do
            elemType `shouldBe` CppLongLong
            case initializer of
              CppCall (CppLambda [] lambdaBody) [] ->
                case lambdaBody of
                  CppDecl (CppVariable builderName builderType (Just (CppBracedInit _ [])))
                    : [ CppForRange loopVar rangeExpr loopBody
                      , CppReturn (Just (CppVar retVar))
                      ] -> do
                      builderType `shouldBe` CppClassType "std::set" [CppLongLong]
                      loopVar `shouldBe` "n"
                      rangeExpr `shouldBe` CppVar "numbers"
                      retVar `shouldBe` builderName
                      loopBody `shouldBe`
                        [ CppExprStmt
                            ( CppCall
                                (CppMember (CppVar builderName) "insert")
                                [ CppBinary "*"
                                    (CppVar "n")
                                    (CppLiteral (CppIntLit 2))
                                ]
                            )
                        ]
                  _ -> expectationFailure "Unexpected lambda body emitted for set comprehension"
              _ -> expectationFailure "Expected set comprehension initializer to be a lambda call"
          _ -> expectationFailure "Expected hoisted declaration for 'doubled' set comprehension"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "lowers Python dict comprehensions into lambda-built maps" $ do
    let numbersLiteral = noLoc (PyList (map (noLoc . PyLiteral . PyInt) [1, 2]))
        numbersAssign = noLoc (PyAssign [noLoc (PatVar (Identifier "numbers"))] numbersLiteral)
        comprehension = PythonComprehension
          { pyCompTarget = noLoc (PatVar (Identifier "n"))
          , pyCompIter = noLoc (PyVar (Identifier "numbers"))
          , pyCompFilters = []
          , pyCompAsync = False
          }
        keyExpr = noLoc (PyVar (Identifier "n"))
        valueExpr =
          noLoc
            ( PyBinaryOp
                OpMul
                (noLoc (PyVar (Identifier "n")))
                (noLoc (PyVar (Identifier "n")))
            )
        dictCompExpr = noLoc (PyDictComp keyExpr valueExpr [comprehension])
        squaresAssign = noLoc (PyAssign [noLoc (PatVar (Identifier "squares"))] dictCompExpr)
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = [numbersAssign, squaresAssign]
              }
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
            isSquares decl = case decl of
              CppVariable name _ _ -> name == "squares"
              _ -> False
        case find isSquares decls of
          Just (CppVariable _ (CppClassType "std::map" [keyType, valueType]) (Just initializer)) -> do
            keyType `shouldBe` CppLongLong
            valueType `shouldBe` CppLongLong
            case initializer of
              CppCall (CppLambda [] lambdaBody) [] ->
                case lambdaBody of
                  CppDecl (CppVariable builderName builderType (Just (CppBracedInit _ [])))
                    : [ CppForRange loopVar rangeExpr loopBody
                      , CppReturn (Just (CppVar retVar))
                      ] -> do
                      builderType `shouldBe` CppClassType "std::map" [CppLongLong, CppLongLong]
                      loopVar `shouldBe` "n"
                      rangeExpr `shouldBe` CppVar "numbers"
                      retVar `shouldBe` builderName
                      let expectedAssignment =
                            CppBinary "="
                              (CppIndex (CppVar builderName) (CppVar "n"))
                              (CppBinary "*"
                                (CppVar "n")
                                (CppVar "n"))
                      loopBody `shouldBe` [CppExprStmt expectedAssignment]
                  _ -> expectationFailure "Unexpected lambda body emitted for dict comprehension"
              _ -> expectationFailure "Expected dict comprehension initializer to be a lambda call"
          _ -> expectationFailure "Expected hoisted declaration for 'squares' dict comprehension"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "lowers string membership comparisons using std::string::find" $ do
    let vowelsAssign =
          noLoc
            ( PyAssign
                [noLoc (PatVar (Identifier "vowels"))]
                (noLoc (PyLiteral (PyString "aeiou")))
            )
        membershipExpr =
          noLoc
            ( PyComparison
                [OpIn]
                [ noLoc (PyLiteral (PyString "e"))
                , noLoc (PyVar (Identifier "vowels"))
                ]
            )
        resultAssign =
          noLoc
            ( PyAssign
                [noLoc (PatVar (Identifier "contains_vowel"))]
                membershipExpr
            )
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = [vowelsAssign, resultAssign]
              }
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
            isTarget decl = case decl of
              CppVariable name _ _ -> name == "contains_vowel"
              _ -> False
        case find isTarget decls of
          Just (CppVariable _ _ (Just (CppCall (CppLambda [] lambdaBody) []))) ->
            case lambdaBody of
              [ CppDecl (CppVariable haystackName haystackType (Just (CppVar "vowels")))
                , CppDecl (CppVariable needleName needleType (Just needleInit))
                , CppReturn (Just resultExpr)
                ] -> do
                haystackType `shouldBe` CppConst (CppReference CppAuto)
                needleType `shouldBe` CppConst (CppReference CppAuto)
                needleInit `shouldBe`
                  CppCall (CppVar "std::string") [CppLiteral (CppStringLit "e")]
                let expectedResult =
                      CppBinary "!="
                        (CppCall (CppMember (CppVar haystackName) "find") [CppVar needleName])
                        (CppVar "std::string::npos")
                resultExpr `shouldBe` expectedResult
              _ -> expectationFailure "Expected lambda with two bindings and a return"
          _ -> expectationFailure "Expected hoisted declaration for 'contains_vowel'"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "translates list membership into std::find searches" $ do
    let numbersLiteral = noLoc (PyList (map (noLoc . PyLiteral . PyInt) [1, 2, 3]))
        numbersAssign = noLoc (PyAssign [noLoc (PatVar (Identifier "numbers"))] numbersLiteral)
        membershipExpr =
          noLoc
            ( PyComparison
                [OpIn]
                [ noLoc (PyLiteral (PyInt 2))
                , noLoc (PyVar (Identifier "numbers"))
                ]
            )
        resultAssign =
          noLoc
            ( PyAssign
                [noLoc (PatVar (Identifier "has_two"))]
                membershipExpr
            )
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = [numbersAssign, resultAssign]
              }
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
            isTarget decl = case decl of
              CppVariable name _ _ -> name == "has_two"
              _ -> False
        case find isTarget decls of
          Just (CppVariable _ _ (Just (CppCall (CppLambda [] lambdaBody) []))) ->
            case lambdaBody of
              [ CppDecl (CppVariable haystackName haystackType (Just (CppVar "numbers")))
                , CppDecl (CppVariable needleName needleType (Just (CppLiteral (CppIntLit 2))))
                , CppDecl (CppVariable endName CppAuto (Just endInit))
                , CppReturn (Just resultExpr)
                ] -> do
                haystackType `shouldBe` CppConst (CppReference CppAuto)
                needleType `shouldBe` CppConst (CppReference CppAuto)
                endInit `shouldBe` CppCall (CppVar "std::end") [CppVar haystackName]
                let expectedFind =
                      CppCall (CppVar "std::find")
                        [ CppCall (CppVar "std::begin") [CppVar haystackName]
                        , CppVar endName
                        , CppVar needleName
                        ]
                    expectedResult = CppBinary "!=" expectedFind (CppVar endName)
                resultExpr `shouldBe` expectedResult
              _ -> expectationFailure "Expected lambda with three bindings and a return"
          _ -> expectationFailure "Expected hoisted declaration for 'has_two'"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "lowers Python string multiplication into helper calls" $ do
    let moduleBody =
          [ noLoc
              ( PyAssign
                  [noLoc (PatVar (Identifier "repeated"))]
                  ( noLoc
                      ( PyBinaryOp
                          OpMul
                          (noLoc (PyLiteral (PyString "ha")))
                          (noLoc (PyLiteral (PyInt 3)))
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
        isRepeated decl = case decl of
          CppVariable name _ _ -> name == "repeated"
          _ -> False
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
        case find isRepeated decls of
          Just (CppVariable _ _ (Just initializer)) ->
            initializer `shouldBe`
              CppCall (CppVar "fluxus_repeat_string")
                [ CppLiteral (CppStringLit "ha")
                , CppCast CppLongLong (CppLiteral (CppIntLit 3))
                ]
          _ -> expectationFailure "Expected declaration for 'repeated'"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

statementGenerationSpec :: Spec
statementGenerationSpec = describe "Statement generation" $ do
  it "lowers Python if statements" $ do
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

  it "lowers Python if/elif/else chains" $ do
    let valueIdent = Identifier "value"
        zeroLiteral = noLoc (PyLiteral (PyInt 0))
        valueVar = noLoc (PyVar valueIdent)
        makePrint text =
          noLoc
            ( PyExprStmt
                ( noLoc
                    ( PyCall
                        (noLoc (PyVar (Identifier "print")))
                        [noLoc (ArgPositional (noLoc (PyLiteral (PyString text))))]
                    )
                )
            )
        positiveCond =
          noLoc
            ( PyComparison
                [OpGt]
                [ valueVar
                , zeroLiteral
                ]
            )
        zeroCond =
          noLoc
            ( PyComparison
                [OpEq]
                [ valueVar
                , zeroLiteral
                ]
            )
        elifBranch =
          noLoc
            ( PyIf
                zeroCond
                [makePrint "zero"]
                [makePrint "negative"]
            )
        moduleBody =
          [ noLoc (PyAssign [noLoc (PatVar valueIdent)] zeroLiteral)
          , noLoc (PyIf positiveCond [makePrint "positive"] [elifBranch])
          ]
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = moduleBody
              }
        expectedStream text =
          CppBinary "<<"
            (CppBinary "<<" (CppVar "std::cout") (CppLiteral (CppStringLit text)))
            (CppVar "std::endl")
        expectedPositiveCond =
          CppBinary ">" (CppVar "value") (CppLiteral (CppIntLit 0))
        expectedZeroCond =
          CppBinary "==" (CppVar "value") (CppLiteral (CppIntLit 0))
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res ->
        case find Shared.isMainFunction (cppDeclarations (cgrUnit res)) of
          Just (CppFunction _ _ _ body) ->
            case listToMaybe [(cond, thenStmts, elseStmts) | CppIf cond thenStmts elseStmts <- body] of
              Just (cond, thenStmts, elseBranch) -> do
                cond `shouldBe` expectedPositiveCond
                listToMaybe [expr | CppExprStmt expr <- thenStmts]
                  `shouldBe` Just (expectedStream "positive")
                case elseBranch of
                  [CppIf elifCond elifThen elifElse] -> do
                    elifCond `shouldBe` expectedZeroCond
                    listToMaybe [expr | CppExprStmt expr <- elifThen]
                      `shouldBe` Just (expectedStream "zero")
                    case listToMaybe [expr | CppExprStmt expr <- elifElse] of
                      Just expr -> expr `shouldBe` expectedStream "negative"
                      Nothing -> expectationFailure "Expected final else print"
                  _ -> expectationFailure "Expected nested if for elif branch"
              Nothing -> expectationFailure "Expected top-level if statement"
          _ -> expectationFailure "Expected generated main function"
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

  it "lowers match statements on literals into nested conditionals" $ do
    let readyCase = PythonCase
          { pyCasePattern = noLoc (PatLiteral (PyString "ready"))
          , pyCaseGuard = Nothing
          , pyCaseBody = [noLoc (PyReturn (Just (noLoc (PyLiteral (PyInt 1)))))]
          }
        fallbackCase = PythonCase
          { pyCasePattern = noLoc PatWildcard
          , pyCaseGuard = Nothing
          , pyCaseBody = [noLoc (PyReturn (Just (noLoc (PyLiteral (PyInt 0)))))]
          }
        matchStmt = noLoc (PyMatch (noLoc (PyVar (Identifier "status"))) [noLoc readyCase, noLoc fallbackCase])
        funcDef = PythonFuncDef
          { pyFuncName = Identifier "classify"
          , pyFuncDecorators = []
          , pyFuncParams = [noLoc (ParamNormal (Identifier "status") Nothing Nothing)]
          , pyFuncReturns = Nothing
          , pyFuncBody = [matchStmt]
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
        initializesSubject name stmt = case stmt of
          CppDecl (CppVariable _ _ (Just (CppVar source))) -> source == name
          _ -> False
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let infoDiags = filter (\d -> diagSeverity d /= SeverityInfo) (cgrDiagnostics res)
        infoDiags `shouldBe` []
        let decls = cppDeclarations (cgrUnit res)
        case [body | CppFunction "classify" _ _ body <- decls] of
          [body] -> do
            let seqs = [stmts | CppStmtSeq stmts <- body, any (initializesSubject "status") stmts]
            case seqs of
              [subjectDecl, matchNode] : _ ->
                case (subjectDecl, matchNode) of
                  (CppDecl (CppVariable tmpName _ (Just (CppVar "status"))), CppIf cond thenStmts elseStmts) -> do
                    cond `shouldBe` CppBinary "==" (CppVar tmpName) (CppLiteral (CppStringLit "ready"))
                    thenStmts `shouldBe` [CppReturn (Just (CppLiteral (CppIntLit 1)))]
                    case elseStmts of
                      [CppIf finalCond finalBody []] -> do
                        finalCond `shouldBe` CppLiteral (CppBoolLit True)
                        finalBody `shouldBe` [CppReturn (Just (CppLiteral (CppIntLit 0)))]
                      other -> expectationFailure $ "Unexpected else branch: " <> show other
                  _ -> expectationFailure "Expected match lowering sequence"
              _ -> expectationFailure "Expected lowered match sequence"
          _ -> expectationFailure "Expected classify function in output"
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

  it "binds capture patterns inside match cases" $ do
    let captureCase = PythonCase
          { pyCasePattern = noLoc (PatVar (Identifier "value"))
          , pyCaseGuard = Nothing
          , pyCaseBody = [noLoc (PyReturn (Just (noLoc (PyVar (Identifier "value")))))]
          }
        matchStmt = noLoc (PyMatch (noLoc (PyVar (Identifier "metric"))) [noLoc captureCase])
        funcDef = PythonFuncDef
          { pyFuncName = Identifier "extract"
          , pyFuncDecorators = []
          , pyFuncParams = [noLoc (ParamNormal (Identifier "metric") Nothing Nothing)]
          , pyFuncReturns = Nothing
          , pyFuncBody = [matchStmt]
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
        initializesSubject name stmt = case stmt of
          CppDecl (CppVariable _ _ (Just (CppVar source))) -> source == name
          _ -> False
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
        case [body | CppFunction "extract" _ _ body <- decls] of
          [body] -> do
            let seqs = [stmts | CppStmtSeq stmts <- body, any (initializesSubject "metric") stmts]
            case seqs of
              [subjectDecl, CppIf cond thenStmts []] : _ ->
                case subjectDecl of
                  CppDecl (CppVariable tmpName _ (Just (CppVar "metric"))) -> do
                    cond `shouldBe` CppLiteral (CppBoolLit True)
                    case thenStmts of
                      (CppDecl (CppVariable boundName _ (Just (CppVar source))):CppReturn (Just (CppVar varName)):_)
                        | boundName == "value"
                        , source == tmpName
                        , varName == "value" -> pure ()
                      other -> expectationFailure $ "Unexpected match body: " <> show other
                  _ -> expectationFailure "Expected subject binding declaration"
              _ -> expectationFailure "Expected lowered match sequence"
          _ -> expectationFailure "Expected extract function in output"
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

  it "evaluates match guards after binding capture variables" $ do
    let guardExpr = noLoc
          ( PyComparison
              [OpGt]
              [ noLoc (PyVar (Identifier "value"))
              , noLoc (PyLiteral (PyInt 0))
              ]
          )
        guardedCase = PythonCase
          { pyCasePattern = noLoc (PatVar (Identifier "value"))
          , pyCaseGuard = Just guardExpr
          , pyCaseBody = [noLoc (PyReturn (Just (noLoc (PyVar (Identifier "value")))))]
          }
        defaultCase = PythonCase
          { pyCasePattern = noLoc PatWildcard
          , pyCaseGuard = Nothing
          , pyCaseBody = [noLoc (PyReturn (Just (noLoc (PyLiteral (PyInt 0)))))]
          }
        matchStmt = noLoc (PyMatch (noLoc (PyVar (Identifier "metric"))) [noLoc guardedCase, noLoc defaultCase])
        funcDef = PythonFuncDef
          { pyFuncName = Identifier "limit_check"
          , pyFuncDecorators = []
          , pyFuncParams = [noLoc (ParamNormal (Identifier "metric") Nothing Nothing)]
          , pyFuncReturns = Nothing
          , pyFuncBody = [matchStmt]
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
        initializesSubject name stmt = case stmt of
          CppDecl (CppVariable _ _ (Just (CppVar source))) -> source == name
          _ -> False
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
        case [body | CppFunction "limit_check" _ _ body <- decls] of
          [body] -> do
            let seqs = [stmts | CppStmtSeq stmts <- body, any (initializesSubject "metric") stmts]
            case seqs of
              [subjectDecl, CppIf cond thenStmts elseStmts] : _ ->
                case subjectDecl of
                  CppDecl (CppVariable tmpName _ (Just (CppVar "metric"))) -> do
                    cond `shouldBe` CppLiteral (CppBoolLit True)
                    case thenStmts of
                      (CppDecl (CppVariable boundName _ (Just (CppVar source))):CppIf guard body []:_) -> do
                        boundName `shouldBe` "value"
                        source `shouldBe` tmpName
                        guard `shouldBe` CppBinary ">" (CppVar "value") (CppLiteral (CppIntLit 0))
                        body `shouldBe` [CppReturn (Just (CppVar "value"))]
                      other -> expectationFailure $ "Unexpected guarded body: " <> show other
                    case elseStmts of
                      [CppIf fallbackCond fallbackBody []] -> do
                        fallbackCond `shouldBe` CppLiteral (CppBoolLit True)
                        fallbackBody `shouldBe` [CppReturn (Just (CppLiteral (CppIntLit 0)))]
                      other -> expectationFailure $ "Unexpected fallback branch: " <> show other
                  _ -> expectationFailure "Expected subject binding declaration"
              _ -> expectationFailure "Expected lowered match sequence"
          _ -> expectationFailure "Expected limit_check function in output"
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

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

  it "lowers Python augmented assignments to compound updates" $ do
    let moduleBody =
          [ noLoc (PyAssign [noLoc (PatVar (Identifier "total"))] (noLoc (PyLiteral (PyInt 10))))
          , noLoc (PyAugAssign (noLoc (PatVar (Identifier "total"))) OpAdd (noLoc (PyLiteral (PyInt 5))))
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
          Just (CppFunction _ _ _ body) -> do
            let matchesAugmented stmt = case stmt of
                  CppExprStmt (CppBinary "+=" (CppVar "total") (CppLiteral (CppIntLit 5))) -> True
                  CppStmtSeq inner -> any matchesAugmented inner
                  CppBlock inner -> any matchesAugmented inner
                  _ -> False
            any matchesAugmented body `shouldBe` True
          _ -> expectationFailure "Expected generated main function"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "lowers Python with statements using a scope guard" $ do
    let withItem = PythonWithItem
          { pyWithContext = noLoc (PyCall (noLoc (PyVar (Identifier "make_resource"))) [])
          , pyWithVar = Just (noLoc (PatVar (Identifier "resource")))
          }
        bodyStmt =
          noLoc
            ( PyExprStmt
                ( noLoc
                    ( PyCall
                        (noLoc (PyVar (Identifier "use")))
                        [ noLoc (ArgPositional (noLoc (PyVar (Identifier "resource")))) ]
                    )
                )
            )
        moduleBody = [noLoc (PyWith [noLoc withItem] [bodyStmt])]
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = moduleBody
              }
        isFinallyGuardStruct decl = case decl of
          CppStruct name _ -> name == "FluxusFinallyGuard"
          _ -> False
        isEnterCall expr = case expr of
          CppCall (CppMember _ "__enter__") [] -> True
          _ -> False
        declaresEnter stmt = case stmt of
          CppDecl (CppVariable _ _ (Just initExpr)) -> isEnterCall initExpr
          CppExprStmt (CppBinary "=" _ rhs) -> isEnterCall rhs
          _ -> False
        isGuardDecl stmt = case stmt of
          CppDecl (CppVariable _ (CppClassType name _) (Just initExpr))
            | name == "FluxusFinallyGuard"
            , Just _ <- extractGuardLambda initExpr -> True
          _ -> False
        isUseCall stmt = case stmt of
          CppExprStmt (CppCall (CppVar "use") [CppVar "resource"]) -> True
          _ -> False
        extractGuardLambda expr = case expr of
          CppCall (CppVar name) [CppLambda [] lambdaStmts]
            | name == "FluxusFinallyGuard" -> Just lambdaStmts
          _ -> Nothing
        isExitCall stmt = case stmt of
          CppExprStmt (CppCall (CppMember _ "__exit__") args) ->
            args == [CppLiteral CppNullPtr, CppLiteral CppNullPtr, CppLiteral CppNullPtr]
          _ -> False
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let unit = cgrUnit res
        any isFinallyGuardStruct (cppDeclarations unit) `shouldBe` True
        case find Shared.isMainFunction (cppDeclarations unit) of
          Just (CppFunction _ _ _ body) ->
            case listToMaybe [stmts | CppStmtSeq stmts <- body] of
              Just stmts -> do
                any declaresEnter stmts `shouldBe` True
                case [ blockStmts | CppBlock blockStmts <- stmts, any isGuardDecl blockStmts ] of
                  [blockStmts] -> do
                    any isUseCall blockStmts `shouldBe` True
                    case listToMaybe
                           [ lambdaStmts
                           | CppDecl (CppVariable _ (CppClassType "FluxusFinallyGuard" _) (Just initExpr)) <- blockStmts
                           , Just lambdaStmts <- [extractGuardLambda initExpr]
                           ] of
                      Just lambdaStmts -> lambdaStmts `shouldSatisfy` any isExitCall
                      Nothing -> expectationFailure "Expected guard declaration with destructor lambda"
                  _ -> expectationFailure "Expected scope block guarded by FluxusFinallyGuard"
              Nothing -> expectationFailure "Expected compound statement sequence in main body"
          _ -> expectationFailure "Expected generated main function"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "translates Python try/except/else/finally using a scope guard" $ do
    let bodyPrint =
          noLoc
            ( PyExprStmt
                ( noLoc
                    ( PyCall
                        (noLoc (PyVar (Identifier "print")))
                        [ noLoc (ArgPositional (noLoc (PyLiteral (PyString "try branch")))) ]
                    )
                )
            )
        exceptPrint =
          noLoc
            ( PyExprStmt
                ( noLoc
                    ( PyCall
                        (noLoc (PyVar (Identifier "print")))
                        [ noLoc (ArgPositional (noLoc (PyLiteral (PyString "except branch")))) ]
                    )
                )
            )
        elsePrint =
          noLoc
            ( PyExprStmt
                ( noLoc
                    ( PyCall
                        (noLoc (PyVar (Identifier "print")))
                        [ noLoc (ArgPositional (noLoc (PyLiteral (PyString "else branch")))) ]
                    )
                )
            )
        finallyPrint =
          noLoc
            ( PyExprStmt
                ( noLoc
                    ( PyCall
                        (noLoc (PyVar (Identifier "print")))
                        [ noLoc (ArgPositional (noLoc (PyLiteral (PyString "finally branch")))) ]
                    )
                )
            )
        exceptBlock =
          PythonExcept
            { pyExceptType = Just (noLoc (PyVar (Identifier "Exception")))
            , pyExceptName = Just (Identifier "exc")
            , pyExceptBody = [exceptPrint]
            }
        tryStmt = noLoc (PyTry [bodyPrint] [noLoc exceptBlock] [elsePrint] [finallyPrint])
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = [tryStmt]
              }
        expectedStream text =
          CppBinary "<<"
            (CppBinary "<<" (CppVar "std::cout") (CppLiteral (CppStringLit text)))
            (CppVar "std::endl")
        isPrintOf text stmt = case stmt of
          CppExprStmt expr -> expr == expectedStream text
          _ -> False
        isSuccessAssignment flag stmt = case stmt of
          CppExprStmt (CppBinary "=" (CppVar name) (CppLiteral (CppBoolLit True))) -> name == flag
          _ -> False
        isFinallyGuardStruct decl = case decl of
          CppStruct name _ -> name == "FluxusFinallyGuard"
          _ -> False
        extractGuardLambda expr = case expr of
          CppCall (CppVar name) [CppLambda [] lambdaStmts]
            | name == "FluxusFinallyGuard" -> Just lambdaStmts
          _ -> Nothing
        lambdaHasElseAndFinally flag lambdaStmts =
          any (isElseGuard flag) lambdaStmts && any (isPrintOf "finally branch") lambdaStmts
        isElseGuard flag stmt = case stmt of
          CppIf (CppVar condVar) thenStmts []
            | condVar == flag -> any (isPrintOf "else branch") thenStmts
          _ -> False
        isGuardDecl flag stmt = case stmt of
          CppDecl (CppVariable _ (CppClassType name _) (Just initExpr))
            | name == "FluxusFinallyGuard"
            , Just lambdaStmts <- extractGuardLambda initExpr -> lambdaHasElseAndFinally flag lambdaStmts
          _ -> False
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let unit = cgrUnit res
        any isFinallyGuardStruct (cppDeclarations unit) `shouldBe` True
        case find Shared.isMainFunction (cppDeclarations unit) of
          Just (CppFunction _ _ _ body) ->
            case listToMaybe [stmts | CppStmtSeq stmts <- body] of
              Just stmts ->
                case stmts of
                  CppDecl (CppVariable flagName CppBool (Just (CppLiteral (CppBoolLit False)))) : rest ->
                    case [blockStmts | CppBlock blockStmts <- rest] of
                      [blockStmts] ->
                        case blockStmts of
                          guardStmt : CppTry tryBody catches [] : _ -> do
                            guardStmt `shouldSatisfy` isGuardDecl flagName
                            tryBody `shouldSatisfy` any (isPrintOf "try branch")
                            tryBody `shouldSatisfy` any (isSuccessAssignment flagName)
                            case catches of
                              [CppCatch catchType catchVar catchBody] -> do
                                catchType `shouldBe` CppClassType "std::exception" []
                                catchVar `shouldBe` "exc"
                                catchBody `shouldSatisfy` any (isPrintOf "except branch")
                              _ -> expectationFailure "Expected single catch clause"
                          _ -> expectationFailure "Expected guard declaration followed by try block"
                      _ -> expectationFailure "Expected scoped block containing guarded try"
                  _ -> expectationFailure "Expected success flag declaration followed by scope block"
              Nothing -> expectationFailure "Expected compound statement sequence in main body"
          _ -> expectationFailure "Expected generated main function"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "translates Python raise ValueError into std::runtime_error throws" $ do
    let raiseStmt =
          noLoc
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
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = [raiseStmt]
              }
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res ->
        case find Shared.isMainFunction (cppDeclarations (cgrUnit res)) of
          Just (CppFunction _ _ _ body) ->
            case [expr | CppThrow (Just expr) <- body] of
              [CppCall (CppVar "std::runtime_error") [messageExpr]] -> do
                let rendered = renderCppExpr messageExpr
                rendered `shouldSatisfy` T.isInfixOf "ValueError"
                rendered `shouldSatisfy` T.isInfixOf "boom"
              other ->
                expectationFailure $ "Expected single std::runtime_error throw, got: " <> show other
          _ ->
            expectationFailure "Expected generated main function"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "rethrows current exception for bare raise statements" $ do
    let bareRaise = noLoc (PyRaise Nothing Nothing)
        exceptClause = PythonExcept
          { pyExceptType = Nothing
          , pyExceptName = Nothing
          , pyExceptBody = [bareRaise]
          }
        tryStmt = noLoc (PyTry [] [noLoc exceptClause] [] [])
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = [tryStmt]
              }
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res ->
        case find Shared.isMainFunction (cppDeclarations (cgrUnit res)) of
          Just (CppFunction _ _ _ body) ->
            case listToMaybe [catches | CppTry _ catches _ <- body] of
              Just [CppCatch _ _ catchBody] ->
                catchBody `shouldSatisfy` any (== CppThrow Nothing)
              other ->
                expectationFailure $ "Expected catch body with bare rethrow, got: " <> show other
          _ ->
            expectationFailure "Expected generated main function"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

fallbackHandlingSpec :: Spec
fallbackHandlingSpec = describe "Runtime fallback handling" $ do

  it "lowers raise statements to std::runtime_error in non-strict mode" $ do
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
          all (not . T.isInfixOf "'raise' statement" . diagMessage)
        case find Shared.isMainFunction (cppDeclarations (cgrUnit res)) of
          Just (CppFunction _ _ _ body) -> do
            containsRuntimeAbort body `shouldBe` False
            case [expr | CppThrow (Just expr) <- body] of
              [CppCall (CppVar "std::runtime_error") [messageExpr]] ->
                renderCppExpr messageExpr `shouldSatisfy` T.isInfixOf "boom"
              other ->
                expectationFailure $ "Expected lowered raise statement, got: " <> show other
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
        CppExprStmt (CppCall (CppVar funcName) _)
          | funcName == "fluxus_runtime_abort" || funcName == "fluxus_runtime_fallback" -> True
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
                  let sanitizedMethodName = "double_fluxus"
                  methodName `shouldBe` sanitizedMethodName
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
        case pythonExprToLocatedCommon expr of
          Left err -> error ("Failed to lower expression: " <> show err)
          Right commonLocated ->
            insertAnnotations (fingerprintCommonExpr commonLocated) exprAnnotation emptyAnnotations

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

      addAnnotation anns expr =
        case pythonExprToLocatedCommon expr of
          Left err -> error ("Failed to lower expression: " <> show err)
          Right commonLocated ->
            insertAnnotations (fingerprintCommonExpr commonLocated) exprAnnotation anns

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

  it "infers list literal element types using analysis annotations" $ do
    let callExpr1 = noLoc (PyCall (noLoc (PyVar (Identifier "factory"))) [])
        callExpr2 = noLoc (PyCall (noLoc (PyVar (Identifier "factory"))) [])
        listExpr = noLoc (PyList [callExpr1, callExpr2])
        assignment = noLoc (PyAssign [noLoc (PatVar (Identifier "values"))] listExpr)
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = [assignment]
          }
        annotations = foldl' addAnnotation emptyAnnotations [callExpr1, callExpr2]
    case generateCppWithAnnotations Shared.testCppConfig annotations (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
            isValues decl = case decl of
              CppVariable name _ _ -> name == "values"
              _ -> False
        case find isValues decls of
          Just (CppVariable _ varType (Just initializer)) -> do
            let expectedType = CppVector (CppUniquePtr CppString)
                expectedExpr = CppBracedInit expectedType
                  [ CppCall (CppVar "factory") []
                  , CppCall (CppVar "factory") []
                  ]
            varType `shouldBe` expectedType
            initializer `shouldBe` expectedExpr
          _ -> expectationFailure "Expected annotated list declaration for 'values'"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "falls back to std::variant for heterogeneous list literals" $ do
    let callExpr = noLoc (PyCall (noLoc (PyVar (Identifier "factory"))) [])
        listExpr = noLoc (PyList [callExpr, noLoc (PyLiteral (PyInt 1))])
        assignment = noLoc (PyAssign [noLoc (PatVar (Identifier "values"))] listExpr)
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = [assignment]
          }
        annotations = foldl' addAnnotation emptyAnnotations [callExpr]
    case generateCppWithAnnotations Shared.testCppConfig annotations (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
            isValues decl = case decl of
              CppVariable name _ _ -> name == "values"
              _ -> False
            infoMessages = [diagMessage diag | diag <- cgrDiagnostics res, diagSeverity diag == SeverityInfo]
        case find isValues decls of
          Just (CppVariable _ varType (Just initializer)) -> do
            let variantType = CppVariant [CppUniquePtr CppString, CppLongLong]
                expectedType = CppVector variantType
                expectedExpr = CppBracedInit expectedType
                  [ CppCall (CppVar "factory") []
                  , CppLiteral (CppIntLit 1)
                  ]
            varType `shouldBe` expectedType
            initializer `shouldBe` expectedExpr
            infoMessages `shouldSatisfy` any (T.isInfixOf "std::variant")
          _ -> expectationFailure "Expected variant-based list declaration for 'values'"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "falls back to std::any when list literal elements remain unresolved" $ do
    let listExpr = noLoc (PyList [noLoc (PyVar (Identifier "dynamic")), noLoc (PyLiteral (PyInt 1))])
        assignment = noLoc (PyAssign [noLoc (PatVar (Identifier "values"))] listExpr)
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = [assignment]
          }
        annotations = emptyAnnotations
    case generateCppWithAnnotations Shared.testCppConfig annotations (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
            isValues decl = case decl of
              CppVariable name _ _ -> name == "values"
              _ -> False
            infoMessages = [diagMessage diag | diag <- cgrDiagnostics res, diagSeverity diag == SeverityInfo]
        case find isValues decls of
          Just (CppVariable _ varType (Just initializer)) -> do
            let elemType = CppClassType "std::any" []
                expectedType = CppVector elemType
                expectedExpr = CppBracedInit expectedType
                  [ CppVar "dynamic"
                  , CppLiteral (CppIntLit 1)
                  ]
            varType `shouldBe` expectedType
            initializer `shouldBe` expectedExpr
            infoMessages `shouldSatisfy` any (T.isInfixOf "std::any")
          _ -> expectationFailure "Expected std::any-based list declaration for 'values'"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "uses list-level annotations before falling back to std::any" $ do
    let dynamicExpr = noLoc (PyVar (Identifier "dynamic"))
        aliasExpr = noLoc (PyVar (Identifier "alias"))
        listExpr = noLoc (PyList [dynamicExpr, aliasExpr])
        assignment = noLoc (PyAssign [noLoc (PatVar (Identifier "values"))] listExpr)
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = [assignment]
          }
        listAnnotation =
          ExprAnnotations
            { eaInferredType = Just (TList TString)
            , eaOwnership = Nothing
            , eaEscapeInfo = Nothing
            , eaOptimizationNotes = []
            }
        annotations =
          case pythonExprToLocatedCommon listExpr of
            Left err -> error ("Failed to lower expression: " <> show err)
            Right commonLocated ->
              insertAnnotations (fingerprintCommonExpr commonLocated) listAnnotation emptyAnnotations

    case generateCppWithAnnotations Shared.testCppConfig annotations (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
            isValues decl = case decl of
              CppVariable name _ _ -> name == "values"
              _ -> False
            infoMessages = [diagMessage diag | diag <- cgrDiagnostics res, diagSeverity diag == SeverityInfo]
        case find isValues decls of
          Just (CppVariable _ varType (Just initializer)) -> do
            let expectedType = CppVector CppString
                expectedExpr = CppBracedInit expectedType
                  [ CppVar "dynamic"
                  , CppVar "alias"
                  ]
            varType `shouldBe` expectedType
            initializer `shouldBe` expectedExpr
            infoMessages `shouldSatisfy` all (not . T.isInfixOf "std::any")
          _ -> expectationFailure "Expected annotated list declaration for 'values'"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

  it "falls back to std::any when list literal mixes heterogeneous and unresolved elements" $ do
    let callExpr = noLoc (PyCall (noLoc (PyVar (Identifier "factory"))) [])
        listExpr = noLoc (PyList [callExpr, noLoc (PyLiteral (PyInt 1)), noLoc (PyVar (Identifier "dynamic"))])
        assignment = noLoc (PyAssign [noLoc (PatVar (Identifier "values"))] listExpr)
        pythonAst = PythonAST PythonModule
          { pyModuleName = Nothing
          , pyModuleDoc = Nothing
          , pyModuleImports = []
          , pyModuleBody = [assignment]
          }
        annotations = foldl' addAnnotation emptyAnnotations [callExpr]
    case generateCppWithAnnotations Shared.testCppConfig annotations (Left pythonAst) of
      Right res -> do
        let decls = cppDeclarations (cgrUnit res)
            isValues decl = case decl of
              CppVariable name _ _ -> name == "values"
              _ -> False
            infoMessages = [diagMessage diag | diag <- cgrDiagnostics res, diagSeverity diag == SeverityInfo]
        case find isValues decls of
          Just (CppVariable _ varType (Just initializer)) -> do
            let elemType = CppClassType "std::any" []
                expectedType = CppVector elemType
                expectedExpr = CppBracedInit expectedType
                  [ CppCall (CppVar "factory") []
                  , CppLiteral (CppIntLit 1)
                  , CppVar "dynamic"
                  ]
            varType `shouldBe` expectedType
            initializer `shouldBe` expectedExpr
            infoMessages `shouldSatisfy` any (T.isInfixOf "std::any")
          _ -> expectationFailure "Expected std::any-based list declaration for 'values'"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

-- Runtime compilation specs ---------------------------------------------------

identifierSanitizationSpec :: Spec
identifierSanitizationSpec = describe "Identifier sanitization" $ do
  it "renames Python identifiers that collide with C++ keywords" $ do
    let funcDef = PythonFuncDef
          { pyFuncName = Identifier "double"
          , pyFuncDecorators = []
          , pyFuncParams = []
          , pyFuncReturns = Nothing
          , pyFuncBody = [noLoc (PyReturn (Just (noLoc (PyLiteral (PyInt 2)))))]
          , pyFuncDoc = Nothing
          , pyFuncIsAsync = False
          }
        moduleBody =
          [ noLoc (PyFuncDef funcDef)
          , noLoc (PyExprStmt (noLoc (PyCall (noLoc (PyVar (Identifier "double"))) [])))
          ]
        pythonAst =
          PythonAST
            PythonModule
              { pyModuleName = Nothing
              , pyModuleDoc = Nothing
              , pyModuleImports = []
              , pyModuleBody = moduleBody
              }
        sanitizedName = "double_fluxus"
        isSanitizedFunction decl = case decl of
          CppFunction name _ _ _ -> name == sanitizedName
          _ -> False
    case generateCpp Shared.testCppConfig (Left pythonAst) of
      Right res -> do
        let unit = cgrUnit res
            decls = cppDeclarations unit
        any isSanitizedFunction decls `shouldBe` True
        case find Shared.isMainFunction decls of
          Just (CppFunction _ _ _ body) -> do
            let callTargets =
                  [ func
                  | CppExprStmt (CppCall func args) <- body
                  , null args
                  ]
            (CppVar sanitizedName `elem` callTargets) `shouldBe` True
          _ -> expectationFailure "Expected generated main function"
      Left failure ->
        expectationFailure $ "Code generation failed: " <> show failure

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
      , prtPendingReason = Nothing
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
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles string multiplication"
      , prtSource =
          [ "print(\"ha\" * 3)"
          ]
      , prtExpectedStdOut = "hahaha\n"
      , prtPendingReason = Nothing
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
  , PythonRuntimeTest
      { prtName = "compiles with statement using scope guard"
      , prtSource =
          [ "class Context:"
          , "    def __enter__(self):"
          , "        print(\"enter\")"
          , "        return self"
          , "    def __exit__(self, exc_type, exc, tb):"
          , "        print(\"exit\")"
          , "        return False"
          , ""
          , "def run_with():"
          , "    with Context() as ctx:"
          , "        print(\"body\")"
          , "    print(\"done\")"
          , ""
          , "run_with()"
          ]
      , prtExpectedStdOut = unlines ["enter", "body", "exit", "done"]
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles try finally with else guard"
      , prtSource =
          [ "def run():"
          , "    try:"
          , "        print(\"try\")"
          , "    except Exception:"
          , "        print(\"except\")"
          , "    else:"
          , "        print(\"else\")"
          , "    finally:"
          , "        print(\"finally\")"
          , ""
          , "run()"
          ]
      , prtExpectedStdOut = unlines ["try", "else", "finally"]
      , prtPendingReason = Nothing
      }
  ]
