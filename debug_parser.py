#!/usr/bin/env python3

import subprocess
import sys
import os

# Test the Python parser with the fibonacci example
test_code = '''def fibonacci(n):
    """Calculate the nth Fibonacci number"""
    if n <= 1:
        return n
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)
'''

# Write test code to file
with open('/tmp/test_fib.py', 'w') as f:
    f.write(test_code)

# Try to run the parser
try:
    result = subprocess.run([
        'stack', 'exec', 'fluxus', '--', 
        'python', '/tmp/test_fib.py'
    ], capture_output=True, text=True, cwd='/home/qwe12345678/hyperstatic2')
    
    print("STDOUT:")
    print(result.stdout)
    print("STDERR:")
    print(result.stderr)
    print("Return code:", result.returncode)
    
except Exception as e:
    print(f"Error running parser: {e}")