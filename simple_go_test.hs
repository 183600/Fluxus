
import Fluxus.Parser.Go.Lexer
import qualified Data.Text as T

main :: IO ()
main = do
    let content = T.pack "package main\n\nfunc fibonacci(n int) int {\n    if n <= 1 {\n        return n\n    }\n    return fibonacci(n-1) + fibonacci(n-2)\n}"
    case Fluxus.Parser.Go.Lexer.runGoLexer "test.go" content of
        Left err -> print $ "Error: " ++ show err
        Right tokens -> do
            putStrLn $ "Successfully tokenized, got " ++ show (length tokens) ++ " tokens"
            mapM_ print tokens
