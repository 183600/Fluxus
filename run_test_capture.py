#!/usr/bin/env python3
import subprocess
import sys

try:
    result = subprocess.run(
        ['stack', 'test'],
        capture_output=True,
        text=True,
        timeout=300
    )
    
    print("=== STDOUT ===")
    print(result.stdout)
    print("\n=== STDERR ===")
    print(result.stderr)
    print(f"\n=== EXIT CODE: {result.returncode} ===")
    
    # Save to file
    with open('test_output_captured.txt', 'w') as f:
        f.write("=== STDOUT ===\n")
        f.write(result.stdout)
        f.write("\n\n=== STDERR ===\n")
        f.write(result.stderr)
        f.write(f"\n\n=== EXIT CODE: {result.returncode} ===\n")
    
    sys.exit(result.returncode)
except Exception as e:
    print(f"Error: {e}")
    sys.exit(1)
