import Fluxus.Parser.Python.Lexer
import qualified Data.Text as T

main = do
    let code = T.pack "x = 10\nif x > 5:\n    print(\"greater\")\nelse:\n    print(\"less\")"
    case runPythonLexer (T.pack "test") code of
        Left err -> print err
        Right tokens -> mapM_ print tokens