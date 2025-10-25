#!/usr/bin/env python3
import subprocess
import sys

# Compile
result = subprocess.run(['clang++', '-std=c++20', 'test_simple_output.cpp', '-o', 'test_simple_output_manual'], 
                       capture_output=True, text=True)
print(f"Compilation exit code: {result.returncode}")
if result.returncode != 0:
    print(f"Compilation stderr: {result.stderr}")
    sys.exit(1)

# Run
result = subprocess.run(['./test_simple_output_manual'], capture_output=True, text=True)
print(f"Program output: '{result.stdout}'")
print(f"Program stderr: '{result.stderr}'")
print(f"Program exit code: {result.returncode}")
