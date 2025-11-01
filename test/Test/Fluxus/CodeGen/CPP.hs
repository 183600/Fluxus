module Test.Fluxus.CodeGen.CPP (spec) where

import Test.Hspec

import qualified Test.Fluxus.CodeGen.CPP.TypeMapping as TypeMapping
import qualified Test.Fluxus.CodeGen.CPP.Python as Python
import qualified Test.Fluxus.CodeGen.CPP.Go as Go

spec :: Spec
spec =
  describe "C++ Code Generation" $ do
    TypeMapping.spec
    Python.spec
    Go.spec
