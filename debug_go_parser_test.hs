import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser

main :: IO ()
main = do
    content <- TIO.readFile "simple_test.go"
    putStrLn "=== Original Go Content ==="
    TIO.putStrLn content
    putStrLn "=== Lexer Output ==="
    case Fluxus.Parser.Go.Lexer.runGoLexer "simple_test.go" content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn $ "Tokens: " ++ show tokens
            putStrLn "=== Parser Output ==="
            case runGoParser "simple_test.go" tokens of
                Left err -> putStrLn $ "Parser error: " ++ show err
                Right ast -> putStrLn $ "AST: " ++ show ast