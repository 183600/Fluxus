
import Fluxus.Parser.Python.Lexer
import qualified Data.Text as T

main :: IO ()
main = do
    let content = T.pack "def fibonacci(n):\n    if n <= 1:\n        return n\n    return fibonacci(n-1) + fibonacci(n-2)"
    case Fluxus.Parser.Python.Lexer.runPythonLexer "test.py" content of
        Left err -> print $ "Error: " ++ show err
        Right tokens -> do
            putStrLn $ "Successfully tokenized, got " ++ show (length tokens) ++ " tokens"
            mapM_ print tokens
