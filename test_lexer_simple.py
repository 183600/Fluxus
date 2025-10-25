#!/usr/bin/env python3
"""Simple lexer test for Python and Go."""

import subprocess

def test_lexer_with_simple_content():
    """Test lexer with simple Python content"""
    
    # Test simple Python content
    python_test = '''
import Fluxus.Parser.Python.Lexer
import qualified Data.Text as T

main :: IO ()
main = do
    let content = T.pack "def fibonacci(n):\\n    if n <= 1:\\n        return n\\n    return fibonacci(n-1) + fibonacci(n-2)"
    case Fluxus.Parser.Python.Lexer.runPythonLexer "test.py" content of
        Left err -> print $ "Error: " ++ show err
        Right tokens -> do
            putStrLn $ "Successfully tokenized, got " ++ show (length tokens) ++ " tokens"
            mapM_ print tokens
'''
    
    with open('simple_python_test.hs', 'w') as f:
        f.write(python_test)
    
    result = subprocess.run(['runhaskell', 'simple_python_test.hs'], 
                          capture_output=True, text=True, timeout=30)
    print("Python lexer test:")
    print("Return code:", result.returncode)
    print("STDOUT:", result.stdout)
    print("STDERR:", result.stderr)
    
    # Test simple Go content  
    go_test = '''
import Fluxus.Parser.Go.Lexer
import qualified Data.Text as T

main :: IO ()
main = do
    let content = T.pack "package main\\n\\nfunc fibonacci(n int) int {\\n    if n <= 1 {\\n        return n\\n    }\\n    return fibonacci(n-1) + fibonacci(n-2)\\n}"
    case Fluxus.Parser.Go.Lexer.runGoLexer "test.go" content of
        Left err -> print $ "Error: " ++ show err
        Right tokens -> do
            putStrLn $ "Successfully tokenized, got " ++ show (length tokens) ++ " tokens"
            mapM_ print tokens
'''
    
    with open('simple_go_test.hs', 'w') as f:
        f.write(go_test)
    
    result = subprocess.run(['runhaskell', 'simple_go_test.hs'], 
                          capture_output=True, text=True, timeout=30)
    print("\nGo lexer test:")
    print("Return code:", result.returncode)
    print("STDOUT:", result.stdout)
    print("STDERR:", result.stderr)

if __name__ == "__main__":
    test_lexer_with_simple_content()