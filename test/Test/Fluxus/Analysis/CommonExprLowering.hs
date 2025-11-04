module Test.Fluxus.Analysis.CommonExprLowering (spec) where

import Test.Hspec

import Fluxus.AST.Common
import Fluxus.AST.Python
import Fluxus.AST.Go
import Fluxus.Analysis.CommonExprLowering

spec :: Spec
spec = describe "CommonExpr lowering" $ do
  it "lowers Python list literals into CEList" $ do
    let expr = noLoc $ PyList [noLoc (PyLiteral (PyInt 1)), noLoc (PyLiteral (PyInt 2))]
    case pythonExprToCommon expr of
      Right (CEList elems) -> length elems `shouldBe` 2
      other -> expectationFailure $ "expected CEList, got " <> show other

  it "lowers simple Python list comprehensions" $ do
    let target = noLoc (PatVar (Identifier "x"))
        comp = PythonComprehension
          { pyCompTarget = target
          , pyCompIter = noLoc (PyVar (Identifier "xs"))
          , pyCompFilters = []
          , pyCompAsync = False
          }
        expr = noLoc (PyListComp (noLoc (PyVar (Identifier "x"))) [comp])
    case pythonExprToCommon expr of
      Right (CEListComp value clauses) -> do
        locValue value `shouldBe` CEVar (Identifier "x")
        map cccBindings clauses `shouldBe` [[Identifier "x"]]
      other -> expectationFailure $ "expected CEListComp, got " <> show other

  it "lowers Go map literals into CEDict" $ do
    let goType = noLoc (GoMapType (noLoc (GoBasicType (Identifier "int"))) (noLoc (GoBasicType (Identifier "int"))))
        pair = (noLoc (GoLiteral (GoInt 1)), noLoc (GoLiteral (GoInt 2)))
        expr = noLoc (GoMapLit goType [pair])
    case goExprToCommon expr of
      Right (CEDict entries) -> length entries `shouldBe` 1
      other -> expectationFailure $ "expected CEDict, got " <> show other

  it "reports unsupported Python lambdas as lowering issues" $ do
    let expr = noLoc (PyLambda [] (noLoc (PyVar (Identifier "x"))))
    case pythonExprToCommon expr of
      Left issue -> isUnsupportedIssue issue `shouldBe` True
      Right _ -> expectationFailure "expected lowering to report unsupported lambda"
