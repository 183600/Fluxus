#!/bin/bash

# Check for incomplete pattern matches and other warnings
echo "Checking for pattern match warnings..."

# Use GHC to check for warnings in a specific file
echo "Checking specific files for warnings..."

# Check a few key files
files=(
    "src/Fluxus/CodeGen/CPP/Python.hs"
    "src/Fluxus/Analysis/TypeInference.hs"
    "src/Fluxus/Parser/Python/Parser.hs"
)

for file in "${files[@]}"; do
    echo "Checking $file..."
    # Use cabal repl to check individual files
    cabal repl --repl-options="-Wall" --repl-options="-fno-code" "$file" < /dev/null 2>&1 | grep -i "warning" | grep -v "LC_ALL" || echo "No warnings found in $file"
done

echo "Done checking files."