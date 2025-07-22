{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Fluxus.Parser.Python.Lexer as PyLexer
import qualified Fluxus.Parser.Python.Parser as PyParser
import qualified Fluxus.Parser.Go.Lexer as GoLexer
import qualified Fluxus.Parser.Go.Parser as GoParser

main :: IO ()
main = do
    putStrLn "=== Testing Python fibonacci.py ==="
    pyContent <- TIO.readFile "examples/python/fibonacci.py"
    
    putStrLn "\n--- Python Lexing ---"
    case PyLexer.runPythonLexer "fibonacci.py" pyContent of
        Left err -> putStrLn $ "Python lexer failed: " ++ show err
        Right tokens -> do
            putStrLn $ "Python lexer success: " ++ show (length tokens) ++ " tokens"
            putStrLn $ "First 10 tokens: " ++ show (take 10 tokens)
            
            putStrLn "\n--- Python Parsing ---"
            case PyParser.runPythonParser "fibonacci.py" tokens of
                Left err -> putStrLn $ "Python parser failed: " ++ show err
                Right ast -> putStrLn "Python parser SUCCESS!"
    
    putStrLn "\n=== Testing Go fibonacci.go ==="
    goContent <- TIO.readFile "examples/go/fibonacci.go"
    
    putStrLn "\n--- Go Lexing ---"
    case GoLexer.runGoLexer "fibonacci.go" goContent of
        Left err -> putStrLn $ "Go lexer failed: " ++ show err
        Right tokens -> do
            putStrLn $ "Go lexer success: " ++ show (length tokens) ++ " tokens"
            putStrLn $ "First 10 tokens: " ++ show (take 10 tokens)
            
            putStrLn "\n--- Go Parsing ---"
            case GoParser.runGoParser "fibonacci.go" tokens of
                Left err -> putStrLn $ "Go parser failed: " ++ show err
                Right ast -> putStrLn "Go parser SUCCESS!"