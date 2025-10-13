#!/bin/bash

# Script to test compilation of all .py files
FAILED_FILES=()
SUCCESS_COUNT=0
FAIL_COUNT=0

echo "Testing compilation of all .py files..."
echo "========================================"
echo ""

# Find all .py files
while IFS= read -r py_file; do
    echo "Testing: $py_file"
    
    # Try to compile the file
    if stack exec fluxus -- --python -O2 "$py_file" -o fibonacci 2>&1 | tee /tmp/fluxus_test_output.txt | grep -q "error\|Error\|ERROR\|fatal\|Fatal"; then
        echo "  ❌ FAILED"
        FAILED_FILES+=("$py_file")
        ((FAIL_COUNT++))
        # Save the error for this file
        mkdir -p /tmp/fluxus_errors
        cp /tmp/fluxus_test_output.txt "/tmp/fluxus_errors/$(basename "$py_file").log"
    else
        # Check if the command actually succeeded (exit code 0)
        if stack exec fluxus -- --python -O2 "$py_file" -o fibonacci > /dev/null 2>&1; then
            echo "  ✓ SUCCESS"
            ((SUCCESS_COUNT++))
        else
            echo "  ❌ FAILED"
            FAILED_FILES+=("$py_file")
            ((FAIL_COUNT++))
            # Save the error for this file
            mkdir -p /tmp/fluxus_errors
            stack exec fluxus -- --python -O2 "$py_file" -o fibonacci 2>&1 > "/tmp/fluxus_errors/$(basename "$py_file").log"
        fi
    fi
    
    # Clean up output file
    rm -f fibonacci
    
    echo ""
done < <(find . -name "*.py" -not -path "./__pycache__/*" -not -path "./dist-newstyle/*" -not -path "./.stack-work/*")

echo "========================================"
echo "Summary:"
echo "  Success: $SUCCESS_COUNT"
echo "  Failed: $FAIL_COUNT"
echo ""

if [ ${#FAILED_FILES[@]} -gt 0 ]; then
    echo "Failed files:"
    for file in "${FAILED_FILES[@]}"; do
        echo "  - $file"
    done
    echo ""
    echo "Error logs saved in /tmp/fluxus_errors/"
fi
