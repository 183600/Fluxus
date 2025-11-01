#!/bin/bash

LOG_DIR="dist/logs"
LOG_FILE="$LOG_DIR/build_output.txt"

mkdir -p "$LOG_DIR"

stack clean
stack build 2>&1 | tee "$LOG_FILE"

echo "=== WARNINGS SUMMARY ==="
grep -n "warning:" "$LOG_FILE" || echo "No warnings found."
