#!/usr/bin/env python3
import subprocess
import os

# Create a simple Python test
test_code = 'print(5)'
with open('temp_test.py', 'w') as f:
    f.write(test_code)

# Compile
subprocess.run(['stack', 'exec', 'fluxus', '--', '--python', '-O2', 'temp_test.py', '-o', 'temp_test'])

# Run and capture output to file
with open('temp_output.txt', 'w') as f:
    subprocess.run(['./temp_test'], stdout=f, stderr=subprocess.STDOUT)

# Read and print the output
with open('temp_output.txt', 'r') as f:
    output = f.read()
    print(f"Output length: {len(output)}")
    print(f"Output repr: {repr(output)}")
    print(f"Output: {output}")
