#!/usr/bin/env python3
import subprocess
import os

test_cases = [
    ('print(14)', '14'),
    ('print(42)', '42'),
    ('print("hello")', 'hello'),
    ('x = 100\nprint(x)', '100'),
]

for i, (code, expected) in enumerate(test_cases):
    test_file = f'test_case_{i}.py'
    output_file = f'test_case_{i}'
    result_file = f'result_{i}.txt'
    
    # Write test
    with open(test_file, 'w') as f:
        f.write(code)
    
    # Compile
    subprocess.run(['stack', 'exec', 'fluxus', '--', '--python', '-O2', test_file, '-o', output_file],
                  capture_output=True)
    
    # Run
    if os.path.exists(output_file):
        with open(result_file, 'w') as f:
            subprocess.run([f'./{output_file}'], stdout=f, stderr=subprocess.STDOUT)
        
        # Read result
        with open(result_file, 'r') as f:
            actual = f.read().strip()
        
        match = "✓" if actual == expected else "✗"
        print(f"Test {i}: {match} Expected '{expected}', Got '{actual}'")
    else:
        print(f"Test {i}: ✗ Output file not found")
