#!/bin/bash

# Cleanup script for test files
# Removes all temporary test files and executables

echo "Cleaning up test files..."

# Remove test files
rm -f test_*.go
rm -f test_*.py
rm -f *_compiled
rm -f *_exe

echo "Test files cleaned up successfully."