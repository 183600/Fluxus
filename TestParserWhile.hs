import Fluxus.Parser.Python.Parser
import Fluxus.Parser.Python.Lexer
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main = do
  content <- TIO.readFile "test_parser_while.py"
  case runPythonLexer (T.pack "test_parser_while.py") content of
    Left err -> putStrLn ("Lexer error: " ++ show err)
    Right tokens -> do
      putStrLn "Lexer success!"
      case runPythonParser (T.pack "test_parser_while.py") tokens of
        Left err -> putStrLn ("Parser error: " ++ show err)
        Right ast -> putStrLn "Parser success!"