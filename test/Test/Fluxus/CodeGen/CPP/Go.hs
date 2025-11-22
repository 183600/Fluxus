{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.CodeGen.CPP.Go (spec) where

import Data.Foldable (for_)
import Data.List (find)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Fluxus.AST.Common
import Fluxus.AST.Go
import Fluxus.CodeGen.CPP
import Fluxus.CodeGen.CPP.Diagnostics (diagMessage)
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
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import qualified Test.Fluxus.CodeGen.CPP.Shared as Shared

spec :: Spec
spec = describe "Go code generation" $ do
  goPrintingSpec
  goLiteralSpec
  goControlFlowSpec
  goConcurrencySpec
  goErrorReportingSpec
  goIdentifierSpec
  goRuntimeSpec
  helperSpec

goPrintingSpec :: Spec
goPrintingSpec = describe "fmt translation" $ do
  it "translates fmt.Println into std::cout stream chain" $ do
    case generateCpp Shared.testCppConfig (Right Shared.goPrintlnAst) of
      Right res -> do
        let unit = cgrUnit res
            includes = cppIncludes unit
        ("<iostream>" `elem` includes) `shouldBe` True
        case find Shared.isMainFunction (cppDeclarations unit) of
          Just (CppFunction _ _ _ body) ->
            case listToMaybe body of
              Just (CppExprStmt expr) -> do
                Shared.isStreamFromCout expr `shouldBe` True
                Shared.containsStdEndl expr `shouldBe` True
                Shared.containsCoutCall expr `shouldBe` False
              _ -> expectationFailure "expected fmt.Println to produce expression statement"
          _ -> expectationFailure "expected generated main function"
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

  it "translates fmt.Printf into std::printf call" $ do
    case generateCpp Shared.testCppConfig (Right Shared.goPrintfAst) of
      Right res -> do
        let unit = cgrUnit res
            includes = cppIncludes unit
        ("<cstdio>" `elem` includes) `shouldBe` True
        case find Shared.isMainFunction (cppDeclarations unit) of
          Just (CppFunction _ _ _ body) ->
            case listToMaybe body of
              Just (CppExprStmt expr) -> do
                Shared.isStdPrintfCall expr `shouldBe` True
                Shared.containsCoutCall expr `shouldBe` False
              _ -> expectationFailure "expected fmt.Printf to produce expression statement"
          _ -> expectationFailure "expected generated main function"
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

goLiteralSpec :: Spec
goLiteralSpec = describe "literal lowering" $ do
  it "translates Go array composite literals" $ do
    let intType = noLoc (GoBasicType (Identifier "int"))
        arrayType = noLoc (GoArrayType (noLoc (GoLiteral (GoInt 3))) intType)
        arrayLiteral = noLoc (GoCompositeLit (Just arrayType) (map (noLoc . GoLiteral . GoInt) [1, 2, 3]))
        stmt = noLoc (GoDefine [Identifier "values"] [arrayLiteral])
        ast = Shared.goAstWithMain [stmt]
    case generateCpp Shared.testCppConfig (Right ast) of
      Right res -> do
        body <- extractMainBody (cgrUnit res)
        let hasArrayInit = any isArrayInitializer (flattenStmts body)
        hasArrayInit `shouldBe` True
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

  it "translates Go slice composite literals" $ do
    let intType = noLoc (GoBasicType (Identifier "int"))
        sliceType = noLoc (GoSliceType intType)
        sliceLiteral = noLoc (GoCompositeLit (Just sliceType) (map (noLoc . GoLiteral . GoInt) [4, 5, 6]))
        stmt = noLoc (GoDefine [Identifier "items"] [sliceLiteral])
        ast = Shared.goAstWithMain [stmt]
    case generateCpp Shared.testCppConfig (Right ast) of
      Right res -> do
        body <- extractMainBody (cgrUnit res)
        let hasSliceInit = any isSliceInitializer (flattenStmts body)
        hasSliceInit `shouldBe` True
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

  it "translates Go map literals" $ do
    let intType = noLoc (GoBasicType (Identifier "int"))
        mapType = noLoc (GoMapType intType intType)
        entries =
          [ (noLoc (GoLiteral (GoInt 1)), noLoc (GoLiteral (GoInt 10)))
          , (noLoc (GoLiteral (GoInt 2)), noLoc (GoLiteral (GoInt 20)))
          ]
        mapLiteral = noLoc (GoMapLit mapType entries)
        stmt = noLoc (GoDefine [Identifier "lookup"] [mapLiteral])
        ast = Shared.goAstWithMain [stmt]
    case generateCpp Shared.testCppConfig (Right ast) of
      Right res -> do
        body <- extractMainBody (cgrUnit res)
        let hasMapInit = any isMapInitializer (flattenStmts body)
        hasMapInit `shouldBe` True
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

goControlFlowSpec :: Spec
goControlFlowSpec = describe "control flow lowering" $
  it "emits C++ break and continue statements" $ do
    let loopBody = noLoc (GoBlock [noLoc (GoContinue Nothing), noLoc (GoBreak Nothing)])
        stmt = noLoc (GoFor Nothing loopBody)
        ast = Shared.goAstWithMain [stmt]
    case generateCpp Shared.testCppConfig (Right ast) of
      Right res -> do
        body <- extractMainBody (cgrUnit res)
        let lowered = flattenStmts body
        (CppContinue `elem` lowered) `shouldBe` True
        (CppBreak `elem` lowered) `shouldBe` True
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

extractMainBody :: CppUnit -> IO [CppStmt]
extractMainBody unit =
  case find Shared.isMainFunction (cppDeclarations unit) of
    Just (CppFunction _ _ _ body) -> pure body
    _ -> do
      expectationFailure "expected generated main function"
      pure []

flattenStmts :: [CppStmt] -> [CppStmt]
flattenStmts = concatMap flattenStmt
  where
    flattenStmt stmt =
      stmt : case stmt of
        CppStmtSeq ss -> concatMap flattenStmt ss
        CppBlock ss -> concatMap flattenStmt ss
        CppIf _ thenSs elseSs -> concatMap flattenStmt (thenSs ++ elseSs)
        CppWhile _ body -> concatMap flattenStmt body
        CppFor mInit _ _ body -> maybe [] flattenStmt mInit ++ concatMap flattenStmt body
        CppForRange _ _ body -> concatMap flattenStmt body
        _ -> []

isArrayInitializer :: CppStmt -> Bool
isArrayInitializer stmt = case stmt of
  CppDecl (CppVariable "values" _ (Just (CppBracedInit (CppStdArray CppInt 3) exprs))) ->
    exprs == map (CppLiteral . CppIntLit) [1, 2, 3]
  _ -> False

isSliceInitializer :: CppStmt -> Bool
isSliceInitializer stmt = case stmt of
  CppDecl (CppVariable "items" _ (Just (CppBracedInit (CppVector CppInt) exprs))) ->
    exprs == map (CppLiteral . CppIntLit) [4, 5, 6]
  _ -> False

isMapInitializer :: CppStmt -> Bool
isMapInitializer stmt = case stmt of
  CppDecl (CppVariable "lookup" _ (Just (CppBracedInit (CppUnorderedMap CppInt CppInt) pairs))) ->
    length pairs == 2 && all isExpectedPair (zip pairs [ (1,10), (2,20) ])
  _ -> False
  where
    isExpectedPair (expr, (keyVal, valueVal)) = case expr of
      CppCall (CppVar "std::make_pair") [CppLiteral (CppIntLit key), CppLiteral (CppIntLit value)] ->
        key == keyVal && value == valueVal
      _ -> False

goConcurrencySpec :: Spec
goConcurrencySpec = describe "concurrency runtime selection" $ do
  it "does not include channel runtime when the program has no concurrency constructs" $ do
    case generateCpp Shared.testCppConfig (Right Shared.goPrintlnAst) of
      Right res -> do
        let unit = cgrUnit res
            includes = cppIncludes unit
            heavyIncludes = map T.pack ["<thread>", "<mutex>", "<condition_variable>", "<queue>"]
        any (`elem` includes) heavyIncludes `shouldBe` False
        any isChannelTemplateDecl (cppDeclarations unit) `shouldBe` False
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

  it "injects channel runtime only when channel types are present" $ do
    case generateCpp Shared.testCppConfig (Right channelSupportAst) of
      Right res -> do
        let unit = cgrUnit res
            includes = cppIncludes unit
            required = map T.pack ["<mutex>", "<condition_variable>", "<queue>", "<cstddef>"]
        all (`elem` includes) required `shouldBe` True
        any isChannelTemplateDecl (cppDeclarations unit) `shouldBe` True
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

goErrorReportingSpec :: Spec
goErrorReportingSpec = describe "error reporting" $
  it "reports unsupported select statements with source location" $ do
    let selectStmt = withSpan "select.go" 4 3 4 9 (GoSelect [])
        ast = Shared.goAstWithMain [selectStmt]
    case generateCpp Shared.testCppConfig (Right ast) of
      Right res -> do
        let messages = map diagMessage (cgrDiagnostics res)
            locationSnippet = T.pack "select.go:4:3"
            descriptionSnippet = T.pack "select statement"
            hasLocation = any (T.isInfixOf locationSnippet) messages
            hasDescription = any (T.isInfixOf descriptionSnippet) messages
        hasLocation `shouldBe` True
        hasDescription `shouldBe` True
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

goIdentifierSpec :: Spec
goIdentifierSpec = describe "Identifier sanitization" $
  it "renames Go identifiers that collide with C++ keywords" $ do
    let doubleFunc = GoFunction
          { goFuncName = Just (Identifier "double")
          , goFuncParams =
              [ GoField
                  { goFieldNames = [Identifier "x"]
                  , goFieldType = noLoc (GoBasicType (Identifier "int"))
                  , goFieldTag = Nothing
                  }
              ]
          , goFuncResults =
              [ GoField
                  { goFieldNames = []
                  , goFieldType = noLoc (GoBasicType (Identifier "int"))
                  , goFieldTag = Nothing
                  }
              ]
          , goFuncBody = Just (noLoc (GoBlock [noLoc (GoReturn [noLoc (GoBinaryOp OpAdd xExpr xExpr)])]))
          }
        mainFunc = GoFunction
          { goFuncName = Just (Identifier "main")
          , goFuncParams = []
          , goFuncResults = []
          , goFuncBody = Just (noLoc (GoBlock [noLoc (GoExprStmt callExpr)]))
          }
        xExpr = noLoc (GoIdent (Identifier "x"))
        callExpr = noLoc (GoCall (noLoc (GoIdent (Identifier "double"))) [noLoc (GoLiteral (GoInt 21))])
        ast = goAstWithDecls [noLoc (GoFuncDecl doubleFunc), noLoc (GoFuncDecl mainFunc)]
        sanitizedName = "double_fluxus"
        isSanitized decl = case decl of
          CppFunction name _ _ _ -> name == sanitizedName
          _ -> False
    case generateCpp Shared.testCppConfig (Right ast) of
      Right res -> do
        let unit = cgrUnit res
            decls = cppDeclarations unit
        any isSanitized decls `shouldBe` True
        case find Shared.isMainFunction decls of
          Just (CppFunction _ _ _ body) -> do
            let callTargets =
                  [ func
                  | CppExprStmt (CppCall func _) <- body
                  ]
            (CppVar sanitizedName `elem` callTargets) `shouldBe` True
          _ -> expectationFailure "expected generated main function"
      Left failure -> expectationFailure $ "Code generation failed: " <> show failure

goAstWithDecls :: [Located GoDecl] -> GoAST
goAstWithDecls decls =
  let file = GoFile
        { goFileName = "main.go"
        , goFilePackage = Identifier "main"
        , goFileImports = []
        , goFileDecls = decls
        }
      pkg = GoPackage
        { goPackageName = Identifier "main"
        , goPackageFiles = [file]
        }
  in GoAST pkg

channelSupportAst :: GoAST
channelSupportAst =
  goAstWithDecls
    [ noLoc (GoTypeDecl (Identifier "IntChan") channelType)
    , noLoc (GoFuncDecl mainFunction)
    ]
  where
    channelType = noLoc (GoChanType GoChanBidi (noLoc (GoBasicType (Identifier "int"))))
    mainFunction =
      GoFunction
        { goFuncName = Just (Identifier "main")
        , goFuncParams = []
        , goFuncResults = []
        , goFuncBody = Just (noLoc (GoBlock [Shared.fmtPrintlnStmt]))
        }

isChannelTemplateDecl :: CppDecl -> Bool
isChannelTemplateDecl (CppTemplate _ (CppClass "Channel" _ _)) = True
isChannelTemplateDecl _ = False

withSpan :: String -> Int -> Int -> Int -> Int -> a -> Located a
withSpan file sl sc el ec value =
  Located
    (SourceSpan (T.pack file) (SourcePos sl sc) (SourcePos el ec))
    value

goRuntimeSpec :: Spec
goRuntimeSpec = describe "Go end-to-end compilation" $ do
  maybeCompiler <- runIO Shared.findCppCompiler
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

helperSpec :: Spec
helperSpec = describe "helper utilities" $
  prop "indentLines adds four spaces to every line" $ \xs ->
    let actual = Shared.indentLines xs
        prefixes = map (take 4) actual
        stripped = map (drop 4) actual
        expectedPrefixes = replicate (length xs) "    "
    in (prefixes, stripped) === (expectedPrefixes, xs)

runGoRuntimeTest :: FilePath -> GoRuntimeTest -> Expectation
runGoRuntimeTest compiler GoRuntimeTest { grtName = name, grtSource = sourceLines, grtExpectedStdOut = expectedStdOut } =
  withSystemTempDirectory ("fluxus-go-cpp-" ++ Shared.sanitizeName name) $ \tmpDir -> do
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
        ["", "func main() {"] ++ Shared.indentLines mainBody ++ ["}"]
  in header ++ helpers ++ mainBlock

-------------------------------------------------------------------------------
-- Runtime test definitions ----------------------------------------------------
-------------------------------------------------------------------------------

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
      , grtPendingReason = Nothing
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
      , grtPendingReason = Nothing
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
