#!/usr/bin/env python3
import subprocess
import os

# Test case 1: Simple print
test1 = """print("Hello World")"""

# Test case 2: Print number
test2 = """print(42)"""

# Test case 3: Variable and print
test3 = """x = 100
print(x)"""

tests = [
    ("test1.py", test1, "Hello World"),
    ("test2.py", test2, "42"),
    ("test3.py", test3, "100"),
]

for filename, code, expected in tests:
    # Write test file
    with open(filename, 'w') as f:
        f.write(code)
    
    # Compile
    output_name = filename.replace('.py', '.out')
    compile_result = subprocess.run(
        ['stack', 'exec', 'fluxus', '--', '--python', '-O2', filename, '-o', output_name],
        capture_output=True,
        text=True
    )
    
    if compile_result.returncode != 0:
        print(f"{filename}: Compilation FAILED")
        print(f"  stderr: {compile_result.stderr}")
        continue
    
    # Run
    if os.path.exists(output_name):
        run_result = subprocess.run(
            [f'./{output_name}'],
            capture_output=True,
            text=True
        )
        
        actual = run_result.stdout.strip()
        print(f"{filename}: Expected '{expected}', Got '{actual}', Match: {actual == expected}")
        if actual != expected:
            print(f"  Exit code: {run_result.returncode}")
            print(f"  Stderr: {run_result.stderr}")
    else:
        print(f"{filename}: Output file not found")
