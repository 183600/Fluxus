#!/bin/bash

# Simple Python Test Suite for Fluxus Compiler
echo "=== Simple Python Test Suite ==="

FLUXUS="dist-newstyle/build/x86_64-linux/ghc-9.6.7/fluxus-0.1.0.0/x/fluxus/build/fluxus/fluxus"
PASSED=0
FAILED=0
TOTAL=0

run_test() {
    local name="$1"
    local expected="$2"
    local description="$3"
    
    echo "--- Test: $name ---"
    echo "Description: $description"
    
    TOTAL=$((TOTAL + 1))
    
    # Compile
    if ! $FLUXUS "${name}.py" -o "${name}_exe" 2>/dev/null; then
        echo "❌ COMPILATION FAILED"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    # Run
    if [ -x "${name}_exe" ]; then
        actual=$(./"${name}_exe" 2>&1)
        if [ "$expected" = "$actual" ]; then
            echo "✅ PASSED"
            PASSED=$((PASSED + 1))
        else
            echo "❌ OUTPUT MISMATCH"
            echo "Expected: '$expected'"
            echo "Actual: '$actual'"
            FAILED=$((FAILED + 1))
        fi
    else
        echo "❌ EXECUTABLE NOT CREATED"
        FAILED=$((FAILED + 1))
    fi
    echo
}

# Test 1: Basic print
echo 'print(42)' > test1.py
run_test "test1" "42" "Simple print"

# Test 2: String print
echo 'print("Hello")' > test2.py
run_test "test2" "Hello" "String print"

# Test 3: Math
echo 'x = 5
y = 3
print(x + y)' > test3.py
run_test "test3" "8" "Simple arithmetic"

# Test 4: Function
echo 'def add(a, b):
    return a + b

print(add(5, 3))' > test4.py
run_test "test4" "8" "Function call"

# Test 5: If statement
echo 'x = 10
if x > 5:
    print("big")
else:
    print("small")' > test5.py
run_test "test5" "big" "If statement"

# Clean up
rm -f test*.py test*_exe

echo "=== Python Test Results ==="
echo "Total: $TOTAL, Passed: $PASSED, Failed: $FAILED"
echo "Success rate: $((PASSED * 100 / TOTAL))%"

if [ $FAILED -eq 0 ]; then
    echo "🎉 All tests passed!"
    exit 0
else
    echo "⚠️  Some tests failed"
    exit 1
fi