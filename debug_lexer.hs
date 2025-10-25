import Fluxus.Parser.Python.Lexer
import Data.Text as T

main = do
  let input = "x()"
  case lexPython "debug" input of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> mapM_ print tokens