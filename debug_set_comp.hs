import Fluxus.AST.Python
import Data.Text (Text)

main :: IO ()
main = do
  let setComp = PySetComp (noLoc (PyVar (Identifier "x"))) []
  print $ "PySetComp created: " ++ show setComp