import qualified Data.Text as T
import Fluxus.Parser.Python.Lexer
import Fluxus.AST.Common

main :: IO ()
main = do
  let exprText = T.pack "name"
      filename = T.pack "test.py"
  case runPythonLexer filename exprText of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Number of tokens: " ++ show (length tokens)
      mapM_ (putStrLn . show) tokens
