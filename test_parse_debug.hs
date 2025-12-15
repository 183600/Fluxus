import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser

main :: IO ()
main = do
  let source = T.unlines
        [ "def greet(name):"
        , "    return f\"Hello {name}\""
        , ""
        , "print(greet(\"Fluxus\"))"
        ]
  putStrLn "Source:"
  TIO.putStrLn source
  putStrLn "\nLexing..."
  case runPythonLexer "test.py" source of
    Left err -> putStrLn $ "Lex error: " ++ show err
    Right tokens -> do
      putStrLn $ "Got " ++ show (length tokens) ++ " tokens"
      putStrLn "\nParsing..."
      case runPythonParser "test.py" tokens of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right ast -> putStrLn $ "Success: " ++ show ast
