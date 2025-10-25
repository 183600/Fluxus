#!/usr/bin/env python3
"""Debug script to test Python and Go lexers on example files."""

import sys
import os

def test_python_lexer():
    """Test Python lexer on fibonacci.py"""
    try:
        with open('examples/python/fibonacci.py', 'r') as f:
            content = f.read()
        print("=== Python Fibonacci Content ===")
        print(content)
        print("\n=== Testing Python lexer ===")
        
        # Try to run the lexer
        import subprocess
        result = subprocess.run(['cabal', 'run', 'fluxus', '--', 'examples/python/fibonacci.py'], 
                              capture_output=True, text=True, timeout=10)
        print("Return code:", result.returncode)
        print("STDOUT:", result.stdout)
        print("STDERR:", result.stderr)
        
    except Exception as e:
        print(f"Error testing Python lexer: {e}")

def test_go_lexer():
    """Test Go lexer on fibonacci.go"""
    try:
        with open('examples/go/fibonacci.go', 'r') as f:
            content = f.read()
        print("\n=== Go Fibonacci Content ===")
        print(content)
        print("\n=== Testing Go lexer ===")
        
        # Try to run the lexer
        import subprocess
        result = subprocess.run(['cabal', 'run', 'fluxus', '--', 'examples/go/fibonacci.go'], 
                              capture_output=True, text=True, timeout=10)
        print("Return code:", result.returncode)
        print("STDOUT:", result.stdout)
        print("STDERR:", result.stderr)
        
    except Exception as e:
        print(f"Error testing Go lexer: {e}")

if __name__ == "__main__":
    test_python_lexer()
    test_go_lexer()