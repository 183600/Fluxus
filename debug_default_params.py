#!/usr/bin/env python3
"""
Debug script to test default parameter handling in the compilation pipeline.
This script will help identify exactly where default values are being lost.
"""

import sys
import os
import subprocess
import tempfile

def test_compilation_pipeline():
    """Test the entire compilation pipeline with debug output."""
    
    # Test Python code with default parameters
    test_code = '''def test_func(x, y=42):
    return x + y

def another_func(a, b="default", c=100):
    return a + b + str(c)

print("Testing default parameters...")
result1 = test_func(10)
result2 = test_func(10, 20)
print(f"test_func(10) = {result1}")
print(f"test_func(10, 20) = {result2}")
'''
    
    # Create temporary files
    with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
        f.write(test_code)
        py_file = f.name
    
    try:
        print(f"Testing file: {py_file}")
        print("Content:")
        print(test_code)
        print("\n" + "="*50)
        
        # Test 1: Run with Python to verify expected behavior
        print("1. Running with Python interpreter:")
        try:
            result = subprocess.run([sys.executable, py_file], 
                                  capture_output=True, text=True, timeout=10)
            print(f"Exit code: {result.returncode}")
            print(f"Stdout: {result.stdout}")
            if result.stderr:
                print(f"Stderr: {result.stderr}")
        except Exception as e:
            print(f"Error running Python: {e}")
        
        print("\n" + "="*50)
        
        # Test 2: Compile with hyperstatic to C++ only
        print("2. Compiling with hyperstatic (C++ generation only):")
        try:
            # Find the fluxus executable
            fluxus_path = os.path.join(os.getcwd(), 'bin/fluxus')
            if not os.path.exists(fluxus_path):
                fluxus_path = os.path.join(os.getcwd(), '.stack-work/install/x86_64-linux/ce68271cca8b1e81a648e79d0146716a82bd870664ce6cb94d80e6456537ae03/9.8.4/bin/fluxus')
            if not os.path.exists(fluxus_path):
                print("Fluxus executable not found. Trying stack build...")
                # Try to build with stack
                result = subprocess.run(['stack', 'build'], 
                                      capture_output=True, text=True, timeout=300)
                print(f"Stack build exit code: {result.returncode}")
                if result.returncode != 0:
                    print("Build failed:")
                    print(result.stderr)
                    return
                else:
                    # Try to find it again
                    fluxus_path = os.path.join(os.getcwd(), '.stack-work/install/x86_64-linux/ce68271cca8b1e81a648e79d0146716a82bd870664ce6cb94d80e6456537ae03/9.8.4/bin/fluxus')
            
            # Run fluxus with verbose output
            cpp_file = py_file.replace('.py', '.cpp')
            cmd = [fluxus_path, '--verbose', '--verbose', '--verbose', py_file]
            print(f"Running: {' '.join(cmd)}")
            
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=60)
            print(f"Exit code: {result.returncode}")
            print(f"Stdout: {result.stdout}")
            if result.stderr:
                print(f"Stderr: {result.stderr}")
            
            # Check if C++ file was generated
            if os.path.exists(cpp_file):
                print(f"\nGenerated C++ file: {cpp_file}")
                with open(cpp_file, 'r') as f:
                    cpp_content = f.read()
                    print("C++ content:")
                    print(cpp_content)
                    
                    # Check if default parameters are present
                    if '= 42' in cpp_content:
                        print("✅ Default parameters found in C++ output")
                    else:
                        print("❌ Default parameters NOT found in C++ output")
            else:
                print("❌ C++ file not generated")
                
        except Exception as e:
            print(f"Error running hyperstatic: {e}")
        
        print("\n" + "="*50)
        
        # Test 3: Test with simpler case
        print("3. Testing with minimal default parameter case:")
        simple_code = "def simple(x, y=42):\n    pass\n"
        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
            f.write(simple_code)
            simple_py = f.name
        
        try:
            simple_cpp = simple_py.replace('.py', '.cpp')
            cmd = [fluxus_path, '--verbose', '--verbose', '--verbose', simple_py]
            print(f"Running: {' '.join(cmd)}")
            
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=60)
            print(f"Exit code: {result.returncode}")
            print(f"Stdout: {result.stdout}")
            if result.stderr:
                print(f"Stderr: {result.stderr}")
            
            if os.path.exists(simple_cpp):
                with open(simple_cpp, 'r') as f:
                    simple_cpp_content = f.read()
                    print("Simple C++ content:")
                    print(simple_cpp_content)
                    
                    if '= 42' in simple_cpp_content:
                        print("✅ Default parameters found in simple case")
                    else:
                        print("❌ Default parameters NOT found in simple case")
            else:
                print("❌ Simple C++ file not generated")
                
        except Exception as e:
            print(f"Error testing simple case: {e}")
        finally:
            # Clean up simple test files
            if os.path.exists(simple_py):
                os.unlink(simple_py)
            if os.path.exists(simple_cpp):
                os.unlink(simple_cpp)
    
    finally:
        # Clean up main test file
        if os.path.exists(py_file):
            os.unlink(py_file)
        cpp_file = py_file.replace('.py', '.cpp')
        if os.path.exists(cpp_file):
            os.unlink(cpp_file)

if __name__ == "__main__":
    test_compilation_pipeline()