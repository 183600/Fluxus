#!/usr/bin/env python3

import subprocess
import tempfile
import os

# Create test files
with tempfile.TemporaryDirectory() as tmpdir:
    input_file = os.path.join(tmpdir, "memory.py")
    output_file = os.path.join(tmpdir, "memory.cpp")
    
    # Write the test code
    with open(input_file, 'w') as f:
        f.write("""result = 0
i = 0
while i < 1000000:
    result = result + (i % 1000)
    i = i + 1
print(f"Processed {result} elements")
""")
    
    print(f"Input file: {input_file}")
    print(f"Output file: {output_file}")
    
    # Run the fluxus compiler
    cmd = ["cabal", "run", "fluxus", "--", input_file, "-o", output_file]
    print(f"Running: {' '.join(cmd)}")
    
    result = subprocess.run(cmd, capture_output=True, text=True, cwd="/home/qwe12345678/hyperstatic2")
    
    print(f"Return code: {result.returncode}")
    print(f"Stdout: {result.stdout}")
    print(f"Stderr: {result.stderr}")
    
    # Check if output file exists
    if os.path.exists(output_file):
        print(f"Output file exists: {output_file}")
        print(f"File type: {subprocess.run(['file', output_file], capture_output=True, text=True).stdout.strip()}")
        
        # Try to run it
        run_result = subprocess.run([output_file], capture_output=True, text=True)
        print(f"Execution result: {run_result.returncode}")
        print(f"Execution stdout: {run_result.stdout}")
        print(f"Execution stderr: {run_result.stderr}")
    else:
        print(f"Output file does not exist: {output_file}")
        # Check what files were created
        print(f"Files in {tmpdir}:")
        for f in os.listdir(tmpdir):
            print(f"  {f}")