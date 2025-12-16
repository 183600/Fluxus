#!/bin/bash

# Simple script to count warnings
echo "Analyzing project for warnings..."

# Create a temp directory for analysis
mkdir -p temp_analysis

# Try to compile with warnings and capture output
cabal clean > /dev/null 2>&1

# Build with verbose output
cabal build --flags="-fast production" -v 2>&1 | tee temp_analysis/build.log

# Extract warnings (excluding locale warnings)
grep -i "warning" temp_analysis/build.log | grep -v "LC_ALL" > temp_analysis/warnings.txt

# Count warnings
warning_count=$(wc -l < temp_analysis/warnings.txt)

echo "Found $warning_count warnings"

if [ $warning_count -gt 0 ]; then
    echo "Warnings:"
    cat temp_analysis/warnings.txt
else
    echo "No warnings found (excluding locale warnings)"
fi

# Clean up
rm -rf temp_analysis