
import Fluxus.Parser.Python.Lexer
import qualified Data.Text as T
main = do
    let content = T.pack """# Simple Python program to test the compiler
def fibonacci(n):
    """Calculate the nth Fibonacci number"""
    if n <= 1:
        return n
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)

def main():
    # Calculate first 10 Fibonacci numbers
    for i in range(10):
        result = fibonacci(i)
        print(f"fib({i}) = {result}")

if __name__ == "__main__":
    main()"""
    case Fluxus.Parser.Python.Lexer.runPythonLexer "fibonacci.py" content of
        Left err -> print err
        Right tokens -> do
            print ("Successfully tokenized, got " ++ show (length tokens) ++ " tokens")
            mapM_ print (take 10 tokens)
