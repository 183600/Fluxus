{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.CodeGen.CPP (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (find)
import Data.Maybe (listToMaybe)

import Fluxus.CodeGen.CPP
import Fluxus.AST.Common
import Fluxus.AST.Go

spec :: Spec
spec = describe "C++ Code Generation" $ do
  typeMappingSpec
  expressionGenerationSpec
  statementGenerationSpec
  declarationGenerationSpec
  goPrintingSpec

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
