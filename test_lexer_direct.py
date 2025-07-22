#!/usr/bin/env python3
"""Direct lexer test for Python and Go."""

import sys
import os

def test_python_lexer_direct():
    """Test Python lexer directly"""
    print("=== Testing Python lexer directly ===")
    try:
        with open('examples/python/fibonacci.py', 'r') as f:
            content = f.read()
        
        # Test lexer directly via GHCi
        import subprocess
        test_code = f'''
import Fluxus.Parser.Python.Lexer
import qualified Data.Text as T
main = do
    let content = T.pack """{content}"""
    case Fluxus.Parser.Python.Lexer.runPythonLexer "fibonacci.py" content of
        Left err -> print err
        Right tokens -> do
            print ("Successfully tokenized, got " ++ show (length tokens) ++ " tokens")
            mapM_ print (take 10 tokens)
'''
        
        with open('test_python_lexer.hs', 'w') as f:
            f.write(test_code)
            
        result = subprocess.run(['runhaskell', 'test_python_lexer.hs'], 
                              capture_output=True, text=True, timeout=30)
        print("Return code:", result.returncode)
        print("STDOUT:", result.stdout)
        print("STDERR:", result.stderr)
        
    except Exception as e:
        print(f"Error: {e}")

def test_go_lexer_direct():
    """Test Go lexer directly"""
    print("\n=== Testing Go lexer directly ===")
    try:
        with open('examples/go/fibonacci.go', 'r') as f:
            content = f.read()
        
        # Test lexer directly via GHCi
        import subprocess
        test_code = f'''
import Fluxus.Parser.Go.Lexer
import qualified Data.Text as T
main = do
    let content = T.pack """{content}"""
    case Fluxus.Parser.Go.Lexer.runGoLexer "fibonacci.go" content of
        Left err -> print err
        Right tokens -> do
            print ("Successfully tokenized, got " ++ show (length tokens) ++ " tokens")
            mapM_ print (take 10 tokens)
'''
        
        with open('test_go_lexer.hs', 'w') as f:
            f.write(test_code)
            
        result = subprocess.run(['runhaskell', 'test_go_lexer.hs'], 
                              capture_output=True, text=True, timeout=30)
        print("Return code:", result.returncode)
        print("STDOUT:", result.stdout)
        print("STDERR:", result.stderr)
        
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    test_python_lexer_direct()
    test_go_lexer_direct()