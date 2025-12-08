import Fluxus.AST.Python
import Fluxus.CodeGen.CPP
import Fluxus.CodeGen.CPP.Shared

main :: IO ()
main = do
  let listComp = PyListComp (noLoc (PyVar (Identifier "x"))) []
  print $ "PyListComp created: " ++ show listComp