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
  it "generates literal expressions" $ do
    let intLit = CppLiteral (CppIntLit 42)
    let stringLit = CppLiteral (CppStringLit "hello")
    let boolLit = CppLiteral (CppBoolLit True)
    
    intLit `shouldBe` CppLiteral (CppIntLit 42)
    stringLit `shouldBe` CppLiteral (CppStringLit "hello")
    boolLit `shouldBe` CppLiteral (CppBoolLit True)
  
  it "generates binary expressions" $ do
    let left = CppVar "x"
    let right = CppLiteral (CppIntLit 10)
    let addExpr = CppBinary "+" left right
    
    addExpr `shouldBe` CppBinary "+" (CppVar "x") (CppLiteral (CppIntLit 10))
  
  it "generates function calls" $ do
    let args = [CppLiteral (CppStringLit "Hello %s"), CppVar "name"]
    let funcCall = CppCall (CppVar "printf") args
    
    funcCall `shouldBe` CppCall (CppVar "printf") [CppLiteral (CppStringLit "Hello %s"), CppVar "name"]

statementGenerationSpec :: Spec
statementGenerationSpec = describe "Statement Generation" $ do
  it "generates expression statements" $ do
    let expr = CppCall (CppVar "printf") [CppLiteral (CppStringLit "Hello")]
    let exprStmt = CppExprStmt expr
    
    exprStmt `shouldBe` CppExprStmt (CppCall (CppVar "printf") [CppLiteral (CppStringLit "Hello")])
  
  it "generates return statements" $ do
    let returnStmt = CppReturn (Just (CppLiteral (CppIntLit 0)))
    returnStmt `shouldBe` CppReturn (Just (CppLiteral (CppIntLit 0)))

declarationGenerationSpec :: Spec
declarationGenerationSpec = describe "Declaration Generation" $ do
  it "generates variable declarations" $ do
    let varDecl = CppVariable "counter" CppInt (Just (CppLiteral (CppIntLit 0)))
    varDecl `shouldBe` CppVariable "counter" CppInt (Just (CppLiteral (CppIntLit 0)))
  
  it "generates function declarations" $ do
    let params = [CppParam "x" CppInt Nothing, CppParam "y" CppInt Nothing]
    let body = [CppReturn (Just (CppBinary "+" (CppVar "x") (CppVar "y")))]
    let funcDecl = CppFunction "add" CppInt params body
    
    funcDecl `shouldBe` CppFunction "add" CppInt params body
  
  it "generates class declarations" $ do
    let methods = [CppMethod "getValue" CppInt [] [CppReturn (Just (CppVar "value"))] False]
    let members = ["public", "private"]  -- Base classes as Text list
    let classDecl = CppClass "MyClass" members methods
    
    classDecl `shouldBe` CppClass "MyClass" members methods
  
  it "generates program units" $ do
    let decls = [CppVariable "global_var" CppInt (Just (CppLiteral (CppIntLit 42)))]
    let program = CppUnit [] [] decls  -- includes, namespaces, declarations
    
    program `shouldBe` CppUnit [] [] decls

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
          runPythonRuntimeTest compiler testCase

data PythonRuntimeTest = PythonRuntimeTest
  { prtName :: String
  , prtSource :: [String]
  , prtExpectedStdOut :: String
  }

pythonRuntimeTests :: [PythonRuntimeTest]
pythonRuntimeTests =
  [ PythonRuntimeTest
      { prtName = "compiles simple print"
      , prtSource =
          [ "print(\"hello fluxus\")"
          ]
      , prtExpectedStdOut = "hello fluxus\n"
      }
  , PythonRuntimeTest
      { prtName = "compiles integer addition"
      , prtSource =
          [ "result = 21 + 21"
          , "print(result)"
          ]
      , prtExpectedStdOut = "42\n"
      }
  , PythonRuntimeTest
      { prtName = "compiles integer multiplication"
      , prtSource =
          [ "result = 6 * 7"
          , "print(result)"
          ]
      , prtExpectedStdOut = "42\n"
      }
  , PythonRuntimeTest
      { prtName = "compiles float division"
      , prtSource =
          [ "print(7 / 2)"
          ]
      , prtExpectedStdOut = "3.5\n"
      }
  , PythonRuntimeTest
      { prtName = "compiles power operation"
      , prtSource =
          [ "print(2 ** 5)"
          ]
      , prtExpectedStdOut = "32\n"
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
      }
  , PythonRuntimeTest
      { prtName = "compiles range simple"
      , prtSource =
          [ "for i in range(3):"
          , "    print(i)"
          ]
      , prtExpectedStdOut = unlines ["0", "1", "2"]
      }
  , PythonRuntimeTest
      { prtName = "compiles range with start"
      , prtSource =
          [ "for i in range(2, 5):"
          , "    print(i)"
          ]
      , prtExpectedStdOut = unlines ["2", "3", "4"]
      }
  , PythonRuntimeTest
      { prtName = "compiles range with positive step"
      , prtSource =
          [ "for i in range(1, 6, 2):"
          , "    print(i)"
          ]
      , prtExpectedStdOut = unlines ["1", "3", "5"]
      }
  , PythonRuntimeTest
      { prtName = "compiles range with negative step"
      , prtSource =
          [ "for i in range(5, 0, -2):"
          , "    print(i)"
          ]
      , prtExpectedStdOut = unlines ["5", "3", "1"]
      }
  , PythonRuntimeTest
      { prtName = "compiles len builtin"
      , prtSource =
          [ "values = [10, 20, 30, 40]"
          , "print(len(values))"
          ]
      , prtExpectedStdOut = "4\n"
      }
  , PythonRuntimeTest
      { prtName = "compiles list indexing"
      , prtSource =
          [ "values = [4, 9, 16]"
          , "print(values[2])"
          ]
      , prtExpectedStdOut = "16\n"
      }
  , PythonRuntimeTest
      { prtName = "compiles f-string formatting"
      , prtSource =
          [ "name = \"Fluxus\""
          , "count = 3"
          , "print(f\"{name}-{count}\")"
          ]
      , prtExpectedStdOut = "Fluxus-3\n"
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
      }
  , PythonRuntimeTest
      { prtName = "compiles multiple print arguments"
      , prtSource =
          [ "value = 7"
          , "print(\"value:\", value)"
          ]
      , prtExpectedStdOut = "value: 7\n"
      }
  , PythonRuntimeTest
      { prtName = "compiles string concatenation"
      , prtSource =
          [ "prefix = \"flux\""
          , "suffix = \"us\""
          , "print(prefix + suffix)"
          ]
      , prtExpectedStdOut = "fluxus\n"
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
      }
  ]

runPythonRuntimeTest :: FilePath -> PythonRuntimeTest -> Expectation
runPythonRuntimeTest compiler (PythonRuntimeTest name sourceLines expectedStdOut) =
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
