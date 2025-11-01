#!/bin/bash

echo "Checking compilation progress..."
echo "================================"

LOG_DIR="dist/logs"
LOG_FILE="$LOG_DIR/build_progress.log"

mkdir -p "$LOG_DIR"

# Try to build and capture warnings
stack build --test --no-run-tests 2>&1 | tee "$LOG_FILE"

# Count warnings
echo ""
echo "Counting warnings..."
grep -c "warning:" "$LOG_FILE" || echo "0"

echo ""
echo "Build complete. Check $LOG_FILE for details."
