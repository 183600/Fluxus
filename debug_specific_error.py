#!/usr/bin/env python3

import subprocess
import sys
import os

# Test cases that might trigger the specific errors

test_cases = [
    # Case 1: Docstring handling
    '''def fibonacci(n):
    """Calculate the nth Fibonacci number"""
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)
''',
    
    # Case 2: Underscore in function parameter
    '''def test_func(_):
    return _ + 1
''',
    
    # Case 3: Complex expression
    '''def complex():
    x = [1, 2, 3]
    return x[0]
''',
    
    # Case 4: If statement with string comparison
    '''def check(s):
    if s == "test":
        return True
    return False
''',
]

for i, test_code in enumerate(test_cases):
    filename = f'/tmp/test_case_{i+1}.py'
    with open(filename, 'w') as f:
        f.write(test_code)
    
    print(f"\n=== Test Case {i+1} ===")
    print(test_code)
    
    try:
        result = subprocess.run([
            'stack', 'exec', 'fluxus', '--', 
            'python', filename
        ], capture_output=True, text=True, cwd='/home/qwe12345678/hyperstatic2')
        
        if result.returncode != 0:
            print("FAILED:")
            print("STDERR:", result.stderr)
        else:
            print("PASSED")
            
    except Exception as e:
        print(f"Error running parser: {e}")