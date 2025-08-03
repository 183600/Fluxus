#!/bin/bash

# Simple Go Test Suite for Fluxus Compiler
echo "=== Simple Go Test Suite ==="

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
    if ! $FLUXUS --go "${name}.go" -o "${name}_exe" 2>/dev/null; then
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

# Test 1: Empty main
echo 'package main

func main() {
}' > test1.go

run_test "test1" "" "Empty Go main function"

# Test 2: Simple print
echo 'package main

func main() {
    println("Hello")
}' > test2.go

run_test "test2" "Hello" "Simple println"

# Test 3: Math
echo 'package main

func main() {
    x := 5
    y := 3
    println(x + y)
}' > test3.go

run_test "test3" "8" "Simple arithmetic"

# Test 4: Function
echo 'package main

func add(a int, b int) int {
    return a + b
}

func main() {
    println(add(5, 3))
}' > test4.go

run_test "test4" "8" "Function call"

# Clean up
rm -f test*.go test*_exe

echo "=== Go Test Results ==="
echo "Total: $TOTAL, Passed: $PASSED, Failed: $FAILED"
echo "Success rate: $((PASSED * 100 / TOTAL))%"

if [ $FAILED -eq 0 ]; then
    echo "🎉 All tests passed!"
    exit 0
else
    echo "⚠️  Some tests failed"
    exit 1
fi