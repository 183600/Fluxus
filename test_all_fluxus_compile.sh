#!/bin/bash

# Test compilation of all .py files in the project
# Usage: ./test_all_fluxus_compile.sh

LOGFILE="compile_test_results.txt"
ERRORFILE="compile_errors.txt"
SUCCESS_COUNT=0
FAIL_COUNT=0

> "$LOGFILE"
> "$ERRORFILE"

echo "Testing fluxus compilation on all .py files..." | tee -a "$LOGFILE"
echo "=======================================" | tee -a "$LOGFILE"
echo "" | tee -a "$LOGFILE"

# Find all .py files excluding __pycache__ and dist-newstyle
find . -name "*.py" -type f ! -path "*/__pycache__/*" ! -path "*/dist-newstyle/*" | while read pyfile; do
    echo "Testing: $pyfile" | tee -a "$LOGFILE"
    
    # Try to compile
    if stack exec fluxus -- --python -O2 "$pyfile" -o fibonacci 2>&1 | tee -a "$LOGFILE" | grep -q "error\|Error\|ERROR\|failed\|Failed\|FAILED"; then
        echo "  ❌ FAILED" | tee -a "$LOGFILE"
        echo "=== FAILED: $pyfile ===" >> "$ERRORFILE"
        stack exec fluxus -- --python -O2 "$pyfile" -o fibonacci 2>&1 >> "$ERRORFILE"
        echo "" >> "$ERRORFILE"
        FAIL_COUNT=$((FAIL_COUNT + 1))
    else
        echo "  ✓ SUCCESS" | tee -a "$LOGFILE"
        SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
    fi
    echo "" | tee -a "$LOGFILE"
done

echo "=======================================" | tee -a "$LOGFILE"
echo "Summary:" | tee -a "$LOGFILE"
echo "  Success: $SUCCESS_COUNT" | tee -a "$LOGFILE"
echo "  Failed: $FAIL_COUNT" | tee -a "$LOGFILE"
echo "" | tee -a "$LOGFILE"
echo "Full log: $LOGFILE" | tee -a "$LOGFILE"
echo "Errors: $ERRORFILE" | tee -a "$LOGFILE"
