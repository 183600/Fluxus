module Test.Fluxus.CodeGen.CPP (spec) where

import Test.Hspec

import qualified Test.Fluxus.CodeGen.CPP.TypeMapping as TypeMapping
import qualified Test.Fluxus.CodeGen.CPP.Rendering as Rendering
import qualified Test.Fluxus.CodeGen.CPP.Python as Python
import qualified Test.Fluxus.CodeGen.CPP.Go as Go
import qualified Test.Fluxus.CodeGen.CPP.UnsupportedConstructs as Unsupported

spec :: Spec
spec =
  describe "C++ Code Generation" $ do
    TypeMapping.spec
    Rendering.spec
    Python.spec
    Go.spec
    Unsupported.spec
