import Fluxus.Analysis.ShapeAnalysis
import Fluxus.AST.Common
import qualified Data.Text as T

main :: IO ()
main = do
  let code = T.unlines [T.pack "def func():", T.pack "    x = 42", T.pack "    return x"]
  print $ "Code: " ++ show code
  result <- analyzeShapeFromText code
  print $ "Result: " ++ show result