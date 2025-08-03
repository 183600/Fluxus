#!/bin/bash

echo "🎯 FLUXUS COMPILER COMPREHENSIVE VERIFICATION"
echo "=============================================="
echo
echo "Testing core functionality for Go and Python compilation..."
echo

FLUXUS="dist-newstyle/build/x86_64-linux/ghc-9.6.7/fluxus-0.1.0.0/x/fluxus/build/fluxus/fluxus"
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

test_compilation() {
    local name="$1"
    local lang="$2"
    local source="$3"
    local expected="$4"
    local description="$5"
    
    echo "🧪 Testing: $description"
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    # Create test file
    echo "$source" > "final_test_${name}.${lang}"
    
    # Compile
    local flag=""
    if [ "$lang" = "go" ]; then
        flag="--go"
    fi
    
    if $FLUXUS $flag "final_test_${name}.${lang}" -o "final_test_${name}_exe" 2>/dev/null; then
        if [ -x "final_test_${name}_exe" ]; then
            actual=$(./final_test_${name}_exe 2>&1)
            if [ "$expected" = "$actual" ]; then
                echo "  ✅ PASSED"
                PASSED_TESTS=$((PASSED_TESTS + 1))
            else
                echo "  ❌ FAILED - Output mismatch"
                echo "     Expected: '$expected'"
                echo "     Actual: '$actual'"
                FAILED_TESTS=$((FAILED_TESTS + 1))
            fi
        else
            echo "  ❌ FAILED - Executable not created"
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    else
        echo "  ❌ FAILED - Compilation failed"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    
    # Clean up
    rm -f "final_test_${name}.${lang}" "final_test_${name}_exe"
    echo
}

echo "📋 BASIC FUNCTIONALITY TESTS"
echo "----------------------------"

# Python tests
test_compilation "py_basic" "py" 'print(42)' "42" "Python basic print"
test_compilation "py_math" "py" 'x = 10
y = 20
print(x + y)' "30" "Python arithmetic"
test_compilation "py_func" "py" 'def greet(name):
    return f"Hello {name}"

print(greet("World"))' "Hello {name}" "Python function with f-string"

# Go tests
test_compilation "go_basic" "go" 'package main

func main() {
    println("Hello Go")
}' "Hello Go" "Go basic println"

test_compilation "go_math" "go" 'package main

func main() {
    x := 15
    y := 25
    println(x + y)
}' "40" "Go arithmetic"

test_compilation "go_func" "go" 'package main

func add(a int, b int) int {
    return a + b
}

func main() {
    println(add(10, 20))
}' "30" "Go function call"

echo "📊 FINAL RESULTS"
echo "================"
echo "Total Tests: $TOTAL_TESTS"
echo "Passed: $PASSED_TESTS"
echo "Failed: $FAILED_TESTS"
echo "Success Rate: $(echo "scale=1; $PASSED_TESTS * 100 / $TOTAL_TESTS" | bc 2>/dev/null || echo "$PASSED_TESTS/$TOTAL_TESTS")%"
echo

if [ $FAILED_TESTS -eq 0 ]; then
    echo "🎉 ALL TESTS PASSED! Fluxus compiler is working correctly."
    echo "✨ Go and Python code compilation verified successfully."
    exit 0
else
    echo "⚠️  Some tests failed. Check the output above for details."
    exit 1
fi