{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.CodeGen.CPP.Rendering (spec) where

import qualified Data.Text as T
import Test.Hspec

import Fluxus.CodeGen.CPP.AST
  ( CppUnit(..)
  , CppDecl(..)
  , CppStmt(..)
  , CppExpr(..)
  , CppType(..)
  , CppLiteral(..)
  , CppCatch(..)
  , renderCppUnit
  )

spec :: Spec
spec = describe "renderCppUnit" $ do
  it "renders try/catch/finally blocks with break and continue" $ do
    let loopBody =
          [ CppTry
              [ CppExprStmt (CppCall (CppVar "maybeFail") [])
              , CppBreak
              ]
              [ CppCatch CppInt "err"
                  [ CppExprStmt (CppCall (CppVar "logError") [CppVar "err"])
                  , CppContinue
                  ]
              ]
              [ CppExprStmt (CppCall (CppVar "cleanup") []) ]
          ]
        runFunction = CppFunction "run" CppVoid []
          [ CppWhile (CppLiteral (CppBoolLit True)) loopBody ]
        unit = CppUnit
          { cppIncludes = ["<vector>"]
          , cppNamespaces = ["hyperstatic"]
          , cppDeclarations = [runFunction]
          }
        rendered = renderCppUnit unit
    T.isInfixOf "#include <vector>" rendered `shouldBe` True
    T.isInfixOf "namespace hyperstatic" rendered `shouldBe` True
    T.isInfixOf "try" rendered `shouldBe` True
    T.isInfixOf "catch (int err)" rendered `shouldBe` True
    T.isInfixOf "/* finally */" rendered `shouldBe` True
    T.isInfixOf "break;" rendered `shouldBe` True
    T.isInfixOf "continue;" rendered `shouldBe` True
    T.isInfixOf "// TODO: Render other statement types" rendered `shouldBe` False
