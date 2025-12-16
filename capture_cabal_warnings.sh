#!/bin/bash

LOG_DIR="dist/logs"
LOG_FILE="$LOG_DIR/cabal_build_output.txt"

mkdir -p "$LOG_DIR"

cabal clean 2>&1 | tee "$LOG_FILE"
echo "=== BUILDING WITH CABAL ===" | tee -a "$LOG_FILE"
cabal build --flags="-fast production" -v2 2>&1 | tee -a "$LOG_FILE"

echo "=== WARNINGS SUMMARY ==="
grep -n -E "(warning|error|Warning|Error)" "$LOG_FILE" || echo "No warnings or errors found."

echo "=== PATTERN MATCHING WARNINGS ==="
grep -n -E "(Pattern match.*are non-exhaustive|Pattern match.*overlapped)" "$LOG_FILE" || echo "No pattern matching warnings found."

echo "=== UNUSED IMPORTS ==="
grep -n -E "(Defined but not used|Imported but not used)" "$LOG_FILE" || echo "No unused import warnings found."