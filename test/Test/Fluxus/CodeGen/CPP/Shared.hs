{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.CodeGen.CPP.Shared
  ( testCppConfig
  , goPrintlnAst
  , goPrintfAst
  , goAstWithMain
  , fmtPrintlnStmt
  , fmtPrintfStmt
  , fmtCall
  , isFooFunction
  , isMainFunction
  , isStreamFromCout
  , containsStdEndl
  , containsCoutCall
  , isStdPrintfCall
  , findCppCompiler
  , sanitizeName
  , indentLines
  ) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import Fluxus.AST.Common
import Fluxus.AST.Go
import Fluxus.CodeGen.CPP
import System.Directory (findExecutable)

-- | Default configuration used in unit tests
-- Ensures predictable output without enabling optional features.
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
  , cgcStrictMode = False
  }

-- | Construct a Go AST with the provided statements inside main
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

goPrintlnAst :: GoAST
goPrintlnAst = goAstWithMain [fmtPrintlnStmt]

goPrintfAst :: GoAST
goPrintfAst = goAstWithMain [fmtPrintfStmt]

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

sanitizeName :: String -> String
sanitizeName = map (\c -> if isAlphaNum c then c else '-')

indentLines :: [String] -> [String]
indentLines = map ("    " ++)
