import qualified Data.Text as T
import Fluxus.Parser.Python.Lexer

main :: IO ()
main = do
  let input = "x()"
  case lexPython "test" input of
    Left err -> putStrLn $ "Error: " ++ show err
    Right tokens -> do
      putStrLn "Tokens:"
      mapM_ print tokens