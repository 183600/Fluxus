import Fluxus.Parser.Python.Lexer as Lexer
import Fluxus.Parser.Python.Parser as Parser
import Data.Text (pack)

main :: IO ()
main = do
  let code = pack "x = 1"
  case Lexer.tokenizePython code of
    Left err -> putStrLn $ "Lexing failed: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show tokens
      case Parser.parsePython code of
        Left err -> putStrLn $ "Parsing failed: " ++ show err
        Right ast -> putStrLn $ "AST: " ++ show ast