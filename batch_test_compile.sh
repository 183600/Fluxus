#!/bin/bash

# Batch test compilation of .py files
# Tests a sample of files to identify common errors

ERROR_LOG="batch_compile_errors.txt"
> "$ERROR_LOG"

echo "Testing sample of .py files for compilation errors..."
echo "======================================================"

test_file() {
    local file="$1"
    echo -n "Testing $file ... "
    if stack exec fluxus -- --python -O2 "$file" -o fibonacci 2>&1 | grep -q "\[ERROR\]"; then
        echo "FAILED"
        echo "=== FAILED: $file ===" >> "$ERROR_LOG"
        stack exec fluxus -- --python -O2 "$file" -o fibonacci 2>&1 >> "$ERROR_LOG"
        echo "" >> "$ERROR_LOG"
        return 1
    else
        echo "OK"
        return 0
    fi
}

# Test a variety of files
test_file "test_simple.py"
test_file "test_print.py"
test_file "test_async_simple.py"
test_file "test/python-tests/test_comprehensions.py"
test_file "test/python-tests/test_decorators.py"
test_file "test/python-tests/test_generators.py"
test_file "test/python-tests/test_classes.py"
test_file "test/python-tests/test_exceptions.py"
test_file "test/python-tests/feature_with.py"
test_file "test/python-tests/feature_decorator.py"
test_file "test/python-tests/test_advanced_async.py"

echo ""
echo "======================================================"
echo "Sample testing complete. Check $ERROR_LOG for details."
