import qualified Data.Text as T
import Fluxus.Parser.Python.Lexer as Lexer

main :: IO ()
main = do
  let code = T.pack "x = [1]"
  putStrLn $ "Input: " ++ T.unpack code
  case Lexer.runPythonLexer "test.py" code of
    Left err -> putStrLn $ "Lexing failed: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show tokens
      putStrLn $ "Number of tokens: " ++ show (length tokens)