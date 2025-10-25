#!/bin/bash

echo "Checking compilation progress..."
echo "================================"

# Try to build and capture warnings
stack build --test --no-run-tests 2>&1 | tee build_progress.log

# Count warnings
echo ""
echo "Counting warnings..."
grep -c "warning:" build_progress.log || echo "0"

echo ""
echo "Build complete. Check build_progress.log for details."
