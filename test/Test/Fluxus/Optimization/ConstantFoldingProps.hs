{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Optimization.ConstantFoldingProps (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T

import Fluxus.Optimization.ConstantFolding

smallInt :: Gen Integer
smallInt = choose (-1000000, 1000000)

spec :: Spec
spec = describe "Constant Folding Properties" $ do
  it "addition is commutative for small integers" $ 
    forAll smallInt $ \a -> forAll smallInt $ \b ->
      constantFoldingPython (PyBinaryOp Add (PyLiteral (PyInt a)) (PyLiteral (PyInt b)))
        == PyLiteral (PyInt (a + b))

  it "multiplication is commutative for small integers" $ 
    forAll smallInt $ \a -> forAll smallInt $ \b ->
      constantFoldingPython (PyBinaryOp Mul (PyLiteral (PyInt a)) (PyLiteral (PyInt b)))
        == PyLiteral (PyInt (a * b))

  it "folding is idempotent on folded results (small integers)" $ 
    forAll smallInt $ \a -> forAll smallInt $ \b -> forAll smallInt $ \c ->
      let expr = PyBinaryOp Add (PyBinaryOp Add (PyLiteral (PyInt a)) (PyLiteral (PyInt b))) (PyLiteral (PyInt c))
          once = constantFoldingPython expr
          twice = constantFoldingPython once
      in once == twice && once == PyLiteral (PyInt (a + b + c))

  it "string concatenation equals runtime result" $ 
    forAll (listOf (elements ['a'..'z'])) $ \s1 ->
      forAll (listOf (elements ['a'..'z'])) $ \s2 ->
        constantFoldingPython (PyBinaryOp Add (PyLiteral (PyString (T.pack s1))) (PyLiteral (PyString (T.pack s2))))
          == PyLiteral (PyString (T.pack (s1 ++ s2)))
