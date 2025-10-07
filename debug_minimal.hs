import qualified Data.Text as T
import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import Fluxus.AST.Python

main :: IO ()
main = do
  let code = T.pack "[1]"
  putStrLn $ "Testing: " ++ T.unpack code
  
  -- Test lexing
  case runPythonLexer "test.py" code of
    Left err -> putStrLn $ "Lexing failed: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show tokens
      
      -- Test parsing
      case runPythonParser "test.py" tokens of
        Left err -> putStrLn $ "Parsing failed: " ++ show err
        Right ast -> putStrLn $ "AST: " ++ show ast