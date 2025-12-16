#!/bin/bash

# Script to check for warnings in individual Haskell files
echo "Checking for warnings in Haskell source files..."

# Get list of all Haskell source files
src_files=$(find src -name "*.hs" | head -10)

# Check each file individually
for file in $src_files; do
    echo "Checking $file..."
    cabal repl --repl-options="-Wall" --repl-options="-fno-code" "$file" < /dev/null 2>&1 | grep -i "warning" | grep -v "LC_ALL"
done

echo "Done checking files."