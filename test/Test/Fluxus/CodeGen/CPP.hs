{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.CodeGen.CPP (spec) where

import Test.Hspec
import Data.Char (isAlphaNum)
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (find)
import Data.Maybe (listToMaybe)
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), replaceExtension)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

import Fluxus.CodeGen.CPP
import Fluxus.AST.Common
import Fluxus.AST.Go
import Fluxus.AST.Python
import Fluxus.Compiler.Driver
  ( CompilerConfig(..)
  , SourceLanguage(..)
  , compileFile
  , defaultConfig
  , runCompiler
  , setupCompilerEnvironment
  )

spec :: Spec
spec = describe "C++ Code Generation" $ do
  typeMappingSpec
  expressionGenerationSpec
  statementGenerationSpec
  declarationGenerationSpec
  pythonGlobalSpec
  goPrintingSpec
  goEndToEndSpec
  pythonEndToEndSpec

typeMappingSpec :: Spec
typeMappingSpec = describe "Type Mapping" $ do
  it "maps basic types correctly" $ do
    mapCommonTypeToCpp (TInt 32) `shouldBe` CppInt
    mapCommonTypeToCpp TBool `shouldBe` CppBool
    mapCommonTypeToCpp TString `shouldBe` CppString
    mapCommonTypeToCpp (TFloat 64) `shouldBe` CppDouble
  
  it "maps container types correctly" $ do
    mapCommonTypeToCpp (TList (TInt 32)) `shouldBe` CppVector CppInt
    mapCommonTypeToCpp (TDict TString (TInt 32)) `shouldBe` CppUnorderedMap CppString CppInt
    mapCommonTypeToCpp (TOptional TString) `shouldBe` CppOptional CppString
  
  it "maps smart pointer types correctly" $ do
    mapCommonTypeToCpp (TOwned (TInt 32)) `shouldBe` CppUniquePtr CppInt
    mapCommonTypeToCpp (TShared (TString)) `shouldBe` CppSharedPtr CppString
  
  it "maps complex types correctly" $ do
    let complexType = TFunction [TInt 32, TString] TBool
    mapCommonTypeToCpp complexType `shouldBe` CppAuto  -- Fallback to auto for complex types

expressionGenerationSpec :: Spec
expressionGenerationSpec = describe "Expression Generation" $ do
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
        unit = generateCpp testCppConfig (Left pythonAst)
        isTotalVar decl = case decl of
          CppVariable name _ _ -> name == "total"
          _ -> False
    case find isTotalVar (cppDeclarations unit) of
      Just (CppVariable _ _ (Just initializer)) ->
        initializer `shouldBe`
          CppBinary "+"
            (CppLiteral (CppIntLit 1))
            (CppLiteral (CppIntLit 2))
      _ ->
        expectationFailure "Expected hoisted declaration for variable 'total'"

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
        unit = generateCpp testCppConfig (Left pythonAst)
    case find isMainFunction (cppDeclarations unit) of
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

statementGenerationSpec :: Spec
statementGenerationSpec = describe "Statement Generation" $ do
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
        unit = generateCpp testCppConfig (Left pythonAst)
    case find isMainFunction (cppDeclarations unit) of
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

  it "emits CppWhile nodes for Python while loops" $ do
    let moduleBody =
          [ noLoc (PyAssign [noLoc (PatVar (Identifier "n"))] (noLoc (PyLiteral (PyInt 0))))
          , noLoc
              ( PyWhile
                  (noLoc (PyBinaryOp OpLt (noLoc (PyVar (Identifier "n"))) (noLoc (PyLiteral (PyInt 3)))))
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
        unit = generateCpp testCppConfig (Left pythonAst)
    case find isMainFunction (cppDeclarations unit) of
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

declarationGenerationSpec :: Spec
declarationGenerationSpec = describe "Declaration Generation" $ do
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
        unit = generateCpp testCppConfig (Left pythonAst)
        isAdd decl = case decl of
          CppFunction name _ _ _ -> name == "add"
          _ -> False
    case find isAdd (cppDeclarations unit) of
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
        unit = generateCpp testCppConfig (Left pythonAst)
        isSample decl = case decl of
          CppClass name _ _ -> name == "Sample"
          _ -> False
    case find isSample (cppDeclarations unit) of
      Just (CppClass _ bases members) -> do
        bases `shouldBe` []
        members `shouldBe` []
      _ ->
        expectationFailure "Expected generated declaration for class 'Sample'"

pythonGlobalSpec :: Spec
pythonGlobalSpec = describe "Python module handling" $ do
  it "hoists module-level assignments to global declarations" $ do
    let unit = generateCpp testCppConfig (Left pythonAst)
        decls = cppDeclarations unit
    case decls of
      (CppVariable name _ _) : rest -> do
        name `shouldBe` "x"
        case find isFooFunction rest of
          Just _ -> pure ()
          Nothing -> expectationFailure "Expected foo function declaration"
        case find isMainFunction rest of
          Just (CppFunction _ _ _ body) ->
            any (declaresVar "x") body `shouldBe` False
          _ -> expectationFailure "Expected generated main function"
      _ -> expectationFailure "Expected module-level variable declaration"
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

goPrintingSpec :: Spec
goPrintingSpec = describe "Go fmt translation" $ do
  it "translates fmt.Println into std::cout stream chain" $ do
    let unit = generateCpp testCppConfig (Right goPrintlnAst)
        includes = cppIncludes unit
    ("<iostream>" `elem` includes) `shouldBe` True
    let maybeMain = find isMainFunction (cppDeclarations unit)
    case maybeMain of
      Just (CppFunction _ _ _ body) ->
        case listToMaybe body of
          Just (CppExprStmt expr) -> do
            isStreamFromCout expr `shouldBe` True
            containsStdEndl expr `shouldBe` True
            containsCoutCall expr `shouldBe` False
          _ -> expectationFailure "expected fmt.Println to produce expression statement"
      _ -> expectationFailure "expected generated main function"

  it "translates fmt.Printf into std::printf call" $ do
    let unit = generateCpp testCppConfig (Right goPrintfAst)
        includes = cppIncludes unit
    ("<cstdio>" `elem` includes) `shouldBe` True
    let maybeMain = find isMainFunction (cppDeclarations unit)
    case maybeMain of
      Just (CppFunction _ _ _ body) ->
        case listToMaybe body of
          Just (CppExprStmt expr) -> do
            isStdPrintfCall expr `shouldBe` True
            containsCoutCall expr `shouldBe` False
          _ -> expectationFailure "expected fmt.Printf to produce expression statement"
      _ -> expectationFailure "expected generated main function"

goEndToEndSpec :: Spec
goEndToEndSpec = describe "Go end-to-end compilation" $ do
  maybeCompiler <- runIO findCppCompiler
  case maybeCompiler of
    Nothing ->
      it "requires an available C++ compiler" $
        expectationFailure "No C++ compiler found in PATH"
    Just compiler ->
      for_ goRuntimeTests $ \testCase ->
        it (grtName testCase) $
          case grtPendingReason testCase of
            Just reason -> pendingWith reason
            Nothing -> runGoRuntimeTest compiler testCase

data GoRuntimeTest = GoRuntimeTest
  { grtName :: String
  , grtSource :: [String]
  , grtExpectedStdOut :: String
  , grtPendingReason :: Maybe String
  }

goRuntimeTests :: [GoRuntimeTest]
goRuntimeTests =
  [ GoRuntimeTest
      { grtName = "compiles go println constant"
      , grtSource =
          goProgram
            []
            [ "fmt.Println(\"hello from go\")"
            ]
      , grtExpectedStdOut = "hello from go\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go addition"
      , grtSource =
          goProgram
            []
            [ "fmt.Println(21 + 21)"
            ]
      , grtExpectedStdOut = "42\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go multiplication"
      , grtSource =
          goProgram
            []
            [ "fmt.Println(6 * 7)"
            ]
      , grtExpectedStdOut = "42\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go function call"
      , grtSource =
          goProgram
            [ "func double(x int) int {"
            , "    return x * 2"
            , "}"
            ]
            [ "fmt.Println(double(21))"
            ]
      , grtExpectedStdOut = "42\n"
      , grtPendingReason = Just "C++ codegen does not yet escape Go identifiers that collide with C++ keywords"
      }
  , GoRuntimeTest
      { grtName = "compiles go nested functions"
      , grtSource =
          goProgram
            [ "func double(x int) int {"
            , "    return x * 2"
            , "}"
            , ""
            , "func quad(x int) int {"
            , "    return double(double(x))"
            , "}"
            ]
            [ "fmt.Println(quad(3))"
            ]
      , grtExpectedStdOut = "12\n"
      , grtPendingReason = Just "C++ codegen does not yet escape Go identifiers that collide with C++ keywords"
      }
  , GoRuntimeTest
      { grtName = "compiles go factorial recursion"
      , grtSource =
          goProgram
            [ "func factorial(n int) int {"
            , "    if n <= 1 {"
            , "        return 1"
            , "    }"
            , "    return n * factorial(n-1)"
            , "}"
            ]
            [ "fmt.Println(factorial(5))"
            ]
      , grtExpectedStdOut = "120\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go fibonacci recursion"
      , grtSource =
          goProgram
            [ "func fib(n int) int {"
            , "    if n <= 1 {"
            , "        return n"
            , "    }"
            , "    return fib(n-1) + fib(n-2)"
            , "}"
            ]
            [ "fmt.Println(fib(6))"
            ]
      , grtExpectedStdOut = "8\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go while-style loop"
      , grtSource =
          goProgram
            []
            [ "i := 0"
            , "for i < 3 {"
            , "    fmt.Println(i)"
            , "    i = i + 1"
            , "}"
            ]
      , grtExpectedStdOut = unlines ["0", "1", "2"]
      , grtPendingReason = Just "Go while-style loop translation is not yet supported in the C++ backend"
      }
  , GoRuntimeTest
      { grtName = "compiles go for loop summation"
      , grtSource =
          goProgram
            []
            [ "sum := 0"
            , "for i := 1; i <= 5; i = i + 1 {"
            , "    sum = sum + i"
            , "}"
            , "fmt.Println(sum)"
            ]
      , grtExpectedStdOut = "15\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go for loop with increment operator"
      , grtSource =
          goProgram
            []
            [ "total := 0"
            , "for i := 0; i < 4; i++ {"
            , "    total = total + i"
            , "}"
            , "fmt.Println(total)"
            ]
      , grtExpectedStdOut = "6\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go if else true branch"
      , grtSource =
          goProgram
            []
            [ "value := 10"
            , "if value > 5 {"
            , "    fmt.Println(\"greater\")"
            , "} else {"
            , "    fmt.Println(\"smaller\")"
            , "}"
            ]
      , grtExpectedStdOut = "greater\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go if else false branch"
      , grtSource =
          goProgram
            []
            [ "value := 1"
            , "if value > 5 {"
            , "    fmt.Println(\"greater\")"
            , "} else {"
            , "    fmt.Println(\"smaller\")"
            , "}"
            ]
      , grtExpectedStdOut = "smaller\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go boolean logic"
      , grtSource =
          goProgram
            []
            [ "a := true"
            , "b := false"
            , "if a && !b {"
            , "    fmt.Println(\"pass\")"
            , "} else {"
            , "    fmt.Println(\"fail\")"
            , "}"
            ]
      , grtExpectedStdOut = "pass\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go printf formatting"
      , grtSource =
          goProgram
            []
            [ "fmt.Printf(\"value: %d\\n\", 7)"
            ]
      , grtExpectedStdOut = "value: 7\n"
      , grtPendingReason = Just "Printf formatting support is not yet implemented in the C++ backend"
      }
  , GoRuntimeTest
      { grtName = "compiles go print without newline"
      , grtSource =
          goProgram
            []
            [ "fmt.Print(\"hello\")"
            ]
      , grtExpectedStdOut = "hello"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go println multiple arguments"
      , grtSource =
          goProgram
            []
            [ "value := 7"
            , "fmt.Println(\"value:\", value)"
            ]
      , grtExpectedStdOut = "value: 7\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go string concatenation"
      , grtSource =
          goProgram
            []
            [ "prefix := \"flux\""
            , "suffix := \"us\""
            , "fmt.Println(prefix + suffix)"
            ]
      , grtExpectedStdOut = "fluxus\n"
      , grtPendingReason = Just "String concatenation between Go strings is not yet mapped to std::string operations"
      }
  , GoRuntimeTest
      { grtName = "compiles go subtraction chain"
      , grtSource =
          goProgram
            []
            [ "value := 30"
            , "value = value - 12"
            , "value = value - 4"
            , "fmt.Println(value)"
            ]
      , grtExpectedStdOut = "14\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go nested loops accumulation"
      , grtSource =
          goProgram
            []
            [ "count := 0"
            , "for i := 0; i < 2; i = i + 1 {"
            , "    for j := 0; j < 3; j = j + 1 {"
            , "        count = count + 1"
            , "    }"
            , "}"
            , "fmt.Println(count)"
            ]
      , grtExpectedStdOut = "6\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go boolean function invocation"
      , grtSource =
          goProgram
            [ "func isPositive(n int) bool {"
            , "    if n > 0 {"
            , "        return true"
            , "    }"
            , "    return false"
            , "}"
            ]
            [ "if isPositive(5) {"
            , "    fmt.Println(\"positive\")"
            , "} else {"
            , "    fmt.Println(\"non-positive\")"
            , "}"
            ]
      , grtExpectedStdOut = "positive\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go string function"
      , grtSource =
          goProgram
            [ "func greet(name string) string {"
            , "    return \"Hello \" + name"
            , "}"
            ]
            [ "fmt.Println(greet(\"Fluxus\"))"
            ]
      , grtExpectedStdOut = "Hello Fluxus\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go helper compute function"
      , grtSource =
          goProgram
            [ "func compute() int {"
            , "    result := 0"
            , "    for i := 1; i <= 3; i = i + 1 {"
            , "        result = result + i"
            , "    }"
            , "    return result"
            , "}"
            ]
            [ "fmt.Println(compute())"
            ]
      , grtExpectedStdOut = "6\n"
      , grtPendingReason = Just "Go helper functions relying on loops are not yet emitted correctly by the C++ backend"
      }
  , GoRuntimeTest
      { grtName = "compiles go modulo operation"
      , grtSource =
          goProgram
            []
            [ "fmt.Println(41 % 6)"
            ]
      , grtExpectedStdOut = "5\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go integer division"
      , grtSource =
          goProgram
            []
            [ "fmt.Println(41 / 5)"
            ]
      , grtExpectedStdOut = "8\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go decrementing for loop"
      , grtSource =
          goProgram
            []
            [ "for i := 3; i > 0; i = i - 1 {"
            , "    fmt.Println(i)"
            , "}"
            ]
      , grtExpectedStdOut = unlines ["3", "2", "1"]
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go if else-if chain"
      , grtSource =
          goProgram
            []
            [ "value := 0"
            , "if value > 0 {"
            , "    fmt.Println(\"positive\")"
            , "} else if value == 0 {"
            , "    fmt.Println(\"zero\")"
            , "} else {"
            , "    fmt.Println(\"negative\")"
            , "}"
            ]
      , grtExpectedStdOut = "zero\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go nested while loops count"
      , grtSource =
          goProgram
            []
            [ "outer := 0"
            , "count := 0"
            , "for outer < 2 {"
            , "    inner := 0"
            , "    for inner < 2 {"
            , "        count = count + 1"
            , "        inner = inner + 1"
            , "    }"
            , "    outer = outer + 1"
            , "}"
            , "fmt.Println(count)"
            ]
      , grtExpectedStdOut = "4\n"
      , grtPendingReason = Just "Go nested while-loop translation is not yet supported in the C++ backend"
      }
  , GoRuntimeTest
      { grtName = "compiles go iterative factorial function"
      , grtSource =
          goProgram
            [ "func factorialIterative(n int) int {"
            , "    result := 1"
            , "    for i := 2; i <= n; i = i + 1 {"
            , "        result = result * i"
            , "    }"
            , "    return result"
            , "}"
            ]
            [ "fmt.Println(factorialIterative(5))"
            ]
      , grtExpectedStdOut = "120\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go even counter function"
      , grtSource =
          goProgram
            [ "func countEven(limit int) int {"
            , "    total := 0"
            , "    for i := 0; i <= limit; i = i + 1 {"
            , "        if i % 2 == 0 {"
            , "            total = total + 1"
            , "        }"
            , "    }"
            , "    return total"
            , "}"
            ]
            [ "fmt.Println(countEven(5))"
            ]
      , grtExpectedStdOut = "3\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go max of three function"
      , grtSource =
          goProgram
            [ "func maxOfThree(a int, b int, c int) int {"
            , "    max := a"
            , "    if b > max {"
            , "        max = b"
            , "    }"
            , "    if c > max {"
            , "        max = c"
            , "    }"
            , "    return max"
            , "}"
            ]
            [ "fmt.Println(maxOfThree(7, 3, 9))"
            ]
      , grtExpectedStdOut = "9\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go string repetition function"
      , grtSource =
          goProgram
            [ "func repeat(phrase string, count int) string {"
            , "    result := \"\""
            , "    for i := 0; i < count; i = i + 1 {"
            , "        result = result + phrase"
            , "    }"
            , "    return result"
            , "}"
            ]
            [ "fmt.Println(repeat(\"ha\", 3))"
            ]
      , grtExpectedStdOut = "hahaha\n"
      , grtPendingReason = Just "String accumulation in loops is not yet implemented with std::string in the C++ backend"
      }
  , GoRuntimeTest
      { grtName = "compiles go difference compute function"
      , grtSource =
          goProgram
            [ "func computeDifference() int {"
            , "    value := 50"
            , "    for i := 0; i < 3; i = i + 1 {"
            , "        value = value - 5"
            , "    }"
            , "    return value"
            , "}"
            ]
            [ "fmt.Println(computeDifference())"
            ]
      , grtExpectedStdOut = "35\n"
      , grtPendingReason = Just "Go for-loop state mutation translation is not yet implemented in the C++ backend"
      }
  , GoRuntimeTest
      { grtName = "compiles go arithmetic precedence"
      , grtSource =
          goProgram
            []
            [ "fmt.Println((2 + 3) * 4)"
            ]
      , grtExpectedStdOut = "20\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go combined arithmetic updates"
      , grtSource =
          goProgram
            []
            [ "value := 10"
            , "value = value * 3"
            , "value = value - 7"
            , "fmt.Println(value)"
            ]
      , grtExpectedStdOut = "23\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go chained integer division"
      , grtSource =
          goProgram
            []
            [ "fmt.Println(100 / 5 / 2)"
            ]
      , grtExpectedStdOut = "10\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go subtraction expression"
      , grtSource =
          goProgram
            []
            [ "fmt.Println(13 - 8)"
            ]
      , grtExpectedStdOut = "5\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go even branch"
      , grtSource =
          goProgram
            []
            [ "n := 4"
            , "if n % 2 == 0 {"
            , "    fmt.Println(\"even\")"
            , "} else {"
            , "    fmt.Println(\"odd\")"
            , "}"
            ]
      , grtExpectedStdOut = "even\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go odd branch"
      , grtSource =
          goProgram
            []
            [ "n := 5"
            , "if n % 2 == 0 {"
            , "    fmt.Println(\"even\")"
            , "} else {"
            , "    fmt.Println(\"odd\")"
            , "}"
            ]
      , grtExpectedStdOut = "odd\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go factorial loop in main"
      , grtSource =
          goProgram
            []
            [ "result := 1"
            , "for i := 2; i <= 5; i = i + 1 {"
            , "    result = result * i"
            , "}"
            , "fmt.Println(result)"
            ]
      , grtExpectedStdOut = "120\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go sum of squares loop"
      , grtSource =
          goProgram
            []
            [ "sum := 0"
            , "for i := 0; i < 5; i = i + 1 {"
            , "    sum = sum + (i * i)"
            , "}"
            , "fmt.Println(sum)"
            ]
      , grtExpectedStdOut = "30\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go nested loops product accumulation"
      , grtSource =
          goProgram
            []
            [ "total := 0"
            , "for i := 1; i <= 3; i = i + 1 {"
            , "    for j := 1; j <= 2; j = j + 1 {"
            , "        total = total + (i * j)"
            , "    }"
            , "}"
            , "fmt.Println(total)"
            ]
      , grtExpectedStdOut = "18\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go decrementing sum loop"
      , grtSource =
          goProgram
            []
            [ "sum := 0"
            , "for i := 5; i > 0; i = i - 1 {"
            , "    sum = sum + i"
            , "}"
            , "fmt.Println(sum)"
            ]
      , grtExpectedStdOut = "15\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go nested conditional selection"
      , grtSource =
          goProgram
            []
            [ "value := 7"
            , "if value > 10 {"
            , "    fmt.Println(\"large\")"
            , "} else {"
            , "    if value > 5 {"
            , "        fmt.Println(\"medium\")"
            , "    } else {"
            , "        fmt.Println(\"small\")"
            , "    }"
            , "}"
            ]
      , grtExpectedStdOut = "medium\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go recursive summation function"
      , grtSource =
          goProgram
            [ "func sumDown(n int) int {"
            , "    if n == 0 {"
            , "        return 0"
            , "    }"
            , "    return n + sumDown(n-1)"
            , "}"
            ]
            [ "fmt.Println(sumDown(4))"
            ]
      , grtExpectedStdOut = "10\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go recursive power function"
      , grtSource =
          goProgram
            [ "func power(base int, exp int) int {"
            , "    if exp == 0 {"
            , "        return 1"
            , "    }"
            , "    return base * power(base, exp-1)"
            , "}"
            ]
            [ "fmt.Println(power(3, 3))"
            ]
      , grtExpectedStdOut = "27\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go boolean helper branch"
      , grtSource =
          goProgram
            [ "func isAdult(age int) bool {"
            , "    if age >= 18 {"
            , "        return true"
            , "    }"
            , "    return false"
            , "}"
            ]
            [ "if isAdult(21) {"
            , "    fmt.Println(\"adult\")"
            , "} else {"
            , "    fmt.Println(\"minor\")"
            , "}"
            ]
      , grtExpectedStdOut = "adult\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go describe function"
      , grtSource =
          goProgram
            [ "func describe(n int) string {"
            , "    if n < 0 {"
            , "        return \"negative\""
            , "    } else if n == 0 {"
            , "        return \"zero\""
            , "    }"
            , "    return \"positive\""
            , "}"
            ]
            [ "fmt.Println(describe(0))"
            ]
      , grtExpectedStdOut = "zero\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go print and println sequence"
      , grtSource =
          goProgram
            []
            [ "fmt.Print(\"go\")"
            , "fmt.Println(\"lang\")"
            ]
      , grtExpectedStdOut = "golang\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go unconditional branch assignment"
      , grtSource =
          goProgram
            []
            [ "value := 1"
            , "if true {"
            , "    value = value + 5"
            , "}"
            , "fmt.Println(value)"
            ]
      , grtExpectedStdOut = "6\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go sequential accumulation loop"
      , grtSource =
          goProgram
            []
            [ "total := 0"
            , "for i := 0; i < 4; i = i + 1 {"
            , "    total = total + (i + 1)"
            , "}"
            , "fmt.Println(total)"
            ]
      , grtExpectedStdOut = "10\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go clamp function"
      , grtSource =
          goProgram
            [ "func clamp(n int) int {"
            , "    if n < 0 {"
            , "        return 0"
            , "    } else if n > 10 {"
            , "        return 10"
            , "    }"
            , "    return n"
            , "}"
            ]
            [ "fmt.Println(clamp(12))"
            ]
      , grtExpectedStdOut = "10\n"
      , grtPendingReason = Nothing
      }
  , GoRuntimeTest
      { grtName = "compiles go multiply helper"
      , grtSource =
          goProgram
            [ "func multiply(a int, b int) int {"
            , "    return a * b"
            , "}"
            ]
            [ "fmt.Println(multiply(6, 7))"
            ]
      , grtExpectedStdOut = "42\n"
      , grtPendingReason = Nothing
      }
  ]

runGoRuntimeTest :: FilePath -> GoRuntimeTest -> Expectation
runGoRuntimeTest compiler GoRuntimeTest { grtName = name, grtSource = sourceLines, grtExpectedStdOut = expectedStdOut } =
  withSystemTempDirectory ("fluxus-go-cpp-" ++ sanitizeName name) $ \tmpDir -> do
    let sourcePath = tmpDir </> "input.go"
        outputBinary = tmpDir </> "program"
        goSource = unlines sourceLines
        config =
          defaultConfig
            { ccSourceLanguage = Go
            , ccCppCompiler = T.pack compiler
            , ccOutputPath = Just outputBinary
            , ccVerboseLevel = 0
            , ccWorkDirectory = Just tmpDir
            , ccKeepIntermediates = True
            }
    writeFile sourcePath goSource
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

goProgram :: [String] -> [String] -> [String]
goProgram helperDecls mainBody =
  let header = ["package main", "", "import \"fmt\""]
      helpers = if null helperDecls then [] else [""] ++ helperDecls
      mainBlock =
        ["", "func main() {"] ++ indentLines mainBody ++ ["}"]
  in header ++ helpers ++ mainBlock

indentLines :: [String] -> [String]
indentLines = map ("    " ++)

pythonEndToEndSpec :: Spec
pythonEndToEndSpec = describe "Python end-to-end compilation" $ do
  maybeCompiler <- runIO findCppCompiler
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
      { prtName = "compiles integer multiplication"
      , prtSource =
          [ "result = 6 * 7"
          , "print(result)"
          ]
      , prtExpectedStdOut = "42\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles float division"
      , prtSource =
          [ "print(7 / 2)"
          ]
      , prtExpectedStdOut = "3.5\n"
      , prtPendingReason = Just "Floating-point division semantics are not yet implemented"
      }
  , PythonRuntimeTest
      { prtName = "compiles power operation"
      , prtSource =
          [ "print(2 ** 5)"
          ]
      , prtExpectedStdOut = "32\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles function definition and call"
      , prtSource =
          [ "def triple(x):"
          , "    return x * 3"
          , ""
          , "print(triple(7))"
          ]
      , prtExpectedStdOut = "21\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles nested function calls"
      , prtSource =
          [ "def double(x):"
          , "    return x * 2"
          , ""
          , "def quad(x):"
          , "    return double(double(x))"
          , ""
          , "print(quad(3))"
          ]
      , prtExpectedStdOut = "12\n"
      , prtPendingReason = Just "C++ code generation does not yet rename Python functions that collide with C++ keywords"
      }
  , PythonRuntimeTest
      { prtName = "compiles if else true branch"
      , prtSource =
          [ "value = 10"
          , "if value > 5:"
          , "    print(\"greater\")"
          , "else:"
          , "    print(\"smaller\")"
          ]
      , prtExpectedStdOut = "greater\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles if else false branch"
      , prtSource =
          [ "value = 1"
          , "if value > 5:"
          , "    print(\"greater\")"
          , "else:"
          , "    print(\"smaller\")"
          ]
      , prtExpectedStdOut = "smaller\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles while loop iteration"
      , prtSource =
          [ "i = 0"
          , "while i < 3:"
          , "    print(i)"
          , "    i = i + 1"
          ]
      , prtExpectedStdOut = unlines ["0", "1", "2"]
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles range simple"
      , prtSource =
          [ "for i in range(3):"
          , "    print(i)"
          ]
      , prtExpectedStdOut = unlines ["0", "1", "2"]
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles range with start"
      , prtSource =
          [ "for i in range(2, 5):"
          , "    print(i)"
          ]
      , prtExpectedStdOut = unlines ["2", "3", "4"]
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles range with positive step"
      , prtSource =
          [ "for i in range(1, 6, 2):"
          , "    print(i)"
          ]
      , prtExpectedStdOut = unlines ["1", "3", "5"]
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles range with negative step"
      , prtSource =
          [ "for i in range(5, 0, -2):"
          , "    print(i)"
          ]
      , prtExpectedStdOut = unlines ["5", "3", "1"]
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles len builtin"
      , prtSource =
          [ "values = [10, 20, 30, 40]"
          , "print(len(values))"
          ]
      , prtExpectedStdOut = "4\n"
      , prtPendingReason = Just "List literal parsing is not yet implemented in the Python frontend"
      }
  , PythonRuntimeTest
      { prtName = "compiles list indexing"
      , prtSource =
          [ "values = [4, 9, 16]"
          , "print(values[2])"
          ]
      , prtExpectedStdOut = "16\n"
      , prtPendingReason = Just "List literal parsing is not yet implemented in the Python frontend"
      }
  , PythonRuntimeTest
      { prtName = "compiles f-string formatting"
      , prtSource =
          [ "name = \"Fluxus\""
          , "count = 3"
          , "print(f\"{name}-{count}\")"
          ]
      , prtExpectedStdOut = "Fluxus-3\n"
      , prtPendingReason = Just "Python f-string expression evaluation is not yet supported in the C++ backend"
      }
  , PythonRuntimeTest
      { prtName = "compiles boolean logic"
      , prtSource =
          [ "a = True"
          , "b = False"
          , "if a and not b:"
          , "    print(\"passes\")"
          , "else:"
          , "    print(\"fails\")"
          ]
      , prtExpectedStdOut = "passes\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles chained comparison"
      , prtSource =
          [ "x = 7"
          , "if 1 < x < 10:"
          , "    print(\"inside\")"
          , "else:"
          , "    print(\"outside\")"
          ]
      , prtExpectedStdOut = "inside\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles factorial recursion"
      , prtSource =
          [ "def factorial(n):"
          , "    if n <= 1:"
          , "        return 1"
          , "    return n * factorial(n - 1)"
          , ""
          , "print(factorial(5))"
          ]
      , prtExpectedStdOut = "120\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles multiple print arguments"
      , prtSource =
          [ "value = 7"
          , "print(\"value:\", value)"
          ]
      , prtExpectedStdOut = "value: 7\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles string concatenation"
      , prtSource =
          [ "prefix = \"flux\""
          , "suffix = \"us\""
          , "print(prefix + suffix)"
          ]
      , prtExpectedStdOut = "fluxus\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles subtraction chain"
      , prtSource =
          [ "value = 30"
          , "value = value - 12"
          , "value = value - 4"
          , "print(value)"
          ]
      , prtExpectedStdOut = "14\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles factorial while loop"
      , prtSource =
          [ "i = 1"
          , "total = 1"
          , "while i <= 4:"
          , "    total = total * i"
          , "    i = i + 1"
          , "print(total)"
          ]
      , prtExpectedStdOut = "24\n"
      , prtPendingReason = Nothing
      }
  , PythonRuntimeTest
      { prtName = "compiles range summation"
      , prtSource =
          [ "total = 0"
          , "for i in range(1, 6):"
          , "    total = total + i"
          , "print(total)"
          ]
      , prtExpectedStdOut = "15\n"
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
      { prtName = "compiles modulo operation"
      , prtSource =
          [ "value = 41 % 6"
          , "print(value)"
          ]
      , prtExpectedStdOut = "5\n"
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

runPythonRuntimeTest :: FilePath -> PythonRuntimeTest -> Expectation
runPythonRuntimeTest compiler PythonRuntimeTest { prtName = name, prtSource = sourceLines, prtExpectedStdOut = expectedStdOut } =
  withSystemTempDirectory ("fluxus-python-cpp-" ++ sanitizeName name) $ \tmpDir -> do
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

sanitizeName :: String -> String
sanitizeName = map (\c -> if isAlphaNum c then c else '-')

-- Helpers for Go fmt translation tests

testCppConfig :: CppGenConfig
testCppConfig = CppGenConfig
  { cgcOptimizationLevel = 0
  , cgcEnableInterop = False
  , cgcTargetCppStd = "c++20"
  , cgcUseSmartPointers = False
  , cgcEnableParallel = False
  , cgcEnableCoroutines = False
  , cgcNamespace = "test"
  , cgcHeaderGuard = "TEST"
  }

goPrintlnAst :: GoAST
goPrintlnAst = goAstWithMain [fmtPrintlnStmt]

goPrintfAst :: GoAST
goPrintfAst = goAstWithMain [fmtPrintfStmt]

goAstWithMain :: [Located GoStmt] -> GoAST
goAstWithMain stmts =
  let mainFunc = GoFunction
        { goFuncName = Just (Identifier "main")
        , goFuncParams = []
        , goFuncResults = []
        , goFuncBody = Just (noLoc (GoBlock stmts))
        }
      file = GoFile
        { goFileName = "main.go"
        , goFilePackage = Identifier "main"
        , goFileImports = []
        , goFileDecls = [noLoc (GoFuncDecl mainFunc)]
        }
      pkg = GoPackage
        { goPackageName = Identifier "main"
        , goPackageFiles = [file]
        }
  in GoAST pkg

fmtPrintlnStmt :: Located GoStmt
fmtPrintlnStmt = noLoc $ GoExprStmt $ fmtCall "Println"
  [noLoc (GoLiteral (GoString "hello"))]

fmtPrintfStmt :: Located GoStmt
fmtPrintfStmt = noLoc $ GoExprStmt $ fmtCall "Printf"
  [ noLoc (GoLiteral (GoString "value: %d"))
  , noLoc (GoLiteral (GoInt 42))
  ]

fmtCall :: Text -> [Located GoExpr] -> Located GoExpr
fmtCall name args = noLoc $ GoCall (fmtSelector name) args
  where
    fmtSelector :: Text -> Located GoExpr
    fmtSelector selectorName =
      noLoc $ GoSelector (noLoc (GoIdent (Identifier "fmt"))) (Identifier selectorName)

isFooFunction :: CppDecl -> Bool
isFooFunction (CppFunction "foo" _ _ _) = True
isFooFunction _ = False

isMainFunction :: CppDecl -> Bool
isMainFunction (CppFunction "main" _ _ _) = True
isMainFunction _ = False

isStreamFromCout :: CppExpr -> Bool
isStreamFromCout (CppBinary "<<" lhs _) =
  case lhs of
    CppVar "std::cout" -> True
    _ -> isStreamFromCout lhs
isStreamFromCout _ = False

containsStdEndl :: CppExpr -> Bool
containsStdEndl (CppVar "std::endl") = True
containsStdEndl (CppBinary _ lhs rhs) = containsStdEndl lhs || containsStdEndl rhs
containsStdEndl _ = False

containsCoutCall :: CppExpr -> Bool
containsCoutCall (CppCall (CppVar "std::cout") _) = True
containsCoutCall (CppBinary _ lhs rhs) = containsCoutCall lhs || containsCoutCall rhs
containsCoutCall (CppCall func args) = containsCoutCall func || any containsCoutCall args
containsCoutCall _ = False

isStdPrintfCall :: CppExpr -> Bool
isStdPrintfCall (CppCall (CppVar "std::printf") _) = True
isStdPrintfCall _ = False

findCppCompiler :: IO (Maybe FilePath)
findCppCompiler = go ["g++", "clang++", "c++"]
  where
    go [] = pure Nothing
    go (candidate:rest) = do
      found <- findExecutable candidate
      case found of
        Just path -> pure (Just path)
        Nothing -> go rest
