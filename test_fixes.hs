{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Python.Lexer (runPythonLexer)
import Fluxus.Parser.Python.Parser (runPythonParser)
import Fluxus.Parser.Go.Lexer (runGoLexer)
import Fluxus.Parser.Go.Parser (runGoParser)

-- Test with simplified versions of the files
main :: IO ()
main = do
    putStrLn "=== Testing simple Python ==="
    let simplePy = T.unlines [
        "def fibonacci(n):",
        "    if n <= 1:",
        "        return n",
        "    else:",
        "        return fibonacci(n - 1) + fibonacci(n - 2)",
        "",
        "def main():",
        "    for i in range(10):",
        "        result = fibonacci(i)",
        "        print(f\"fib({i}) = {result}\")",
        "",
        "if __name__ == \"__main__\":",
        "    main()"
        ]
    
    case runPythonLexer "test.py" simplePy of
        Left err -> putStrLn $ "Python lexer failed: " ++ show err
        Right tokens -> do
            putStrLn $ "Python lexer success: " ++ show (length tokens) ++ " tokens"
            case runPythonParser "test.py" tokens of
                Left err -> putStrLn $ "Python parser failed: " ++ show err
                Right _ -> putStrLn "Python parser success!"
    
    putStrLn "\n=== Testing simple Go ==="
    let simpleGo = T.unlines [
        "package main",
        "",
        "import \"fmt\"",
        "",
        "func fibonacci(n int) int {",
        "    if n <= 1 {",
        "        return n",
        "    }",
        "    return fibonacci(n-1) + fibonacci(n-2)",
        "}",
        "",
        "func main() {",
        "    fmt.Println(\"Sequential Fibonacci:\")",
        "    for i := 0; i < 10; i++ {",
        "        result := fibonacci(i)",
        "        fmt.Printf(\"fib(%d) = %d\\n\", i, result)",
        "    }",
        "}"
        ]
    
    case runGoLexer "test.go" simpleGo of
        Left err -> putStrLn $ "Go lexer failed: " ++ show err
        Right tokens -> do
            putStrLn $ "Go lexer success: " ++ show (length tokens) ++ " tokens"
            case runGoParser "test.go" tokens of
                Left err -> putStrLn $ "Go parser failed: " ++ show err
                Right _ -> putStrLn "Go parser success!"