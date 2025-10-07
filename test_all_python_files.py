#!/usr/bin/env python3
"""
Script to test all .py files in the project with fluxus.
Shows error messages only when fluxus fails to compile a file.
"""

import os
import subprocess
import sys
from pathlib import Path

def find_python_files(root_dir):
    """Find all .py files in the given directory recursively."""
    python_files = []
    for root, dirs, files in os.walk(root_dir):
        for file in files:
            if file.endswith('.py'):
                python_files.append(os.path.join(root, file))
    return python_files

def test_python_file_with_fluxus(py_file):
    """Test a single python file with fluxus compiler."""
    try:
        cmd = ['fluxus', '--python', '-O2', py_file, '-o', 'fibonacci']
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=30  # 30 second timeout per file
        )
        return result.returncode, result.stdout, result.stderr
    except subprocess.TimeoutExpired:
        return -1, "", "Timeout after 30 seconds"
    except Exception as e:
        return -1, "", f"Error running fluxus: {str(e)}"

def main():
    # Get the project root directory (current directory)
    project_root = Path.cwd()

    print(f"Testing all .py files in: {project_root}")
    print("=" * 50)

    python_files = find_python_files(project_root)

    if not python_files:
        print("No .py files found.")
        return 0

    error_count = 0
    total_files = len(python_files)

    for py_file in sorted(python_files):
        returncode, stdout, stderr = test_python_file_with_fluxus(py_file)

        if returncode != 0:
            print(f"\n❌ Error in: {py_file}")
            print(f"   Return code: {returncode}")
            if stdout.strip():
                print(f"   STDOUT:\n{stdout}")
            if stderr.strip():
                print(f"   STDERR:\n{stderr}")
            print("-" * 40)
            error_count += 1

    print(f"\nTesting complete.")
    print(f"Total files tested: {total_files}")
    print(f"Files with errors: {error_count}")
    print(f"Files without errors: {total_files - error_count}")

    return error_count

if __name__ == "__main__":
    sys.exit(main())