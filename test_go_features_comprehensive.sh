#!/bin/bash

# Comprehensive Go Feature Support Test
# This script tests all major Go features supported by Fluxus

echo "=== Fluxus Go Feature Support Test ==="
echo "Testing comprehensive Go language support..."
echo ""

FLUXUS="dist-newstyle/build/x86_64-linux/ghc-9.6.7/fluxus-0.1.0.0/x/fluxus/build/fluxus/fluxus"

# Test results
passed=0
failed=0

# Test a Go file
test_go_file() {
    local file="$1"
    local name="$2"
    
    echo "Testing: $name"
    echo "File: $file"
    
    # Check if file exists
    if [ ! -f "$file" ]; then
        echo "✗ File not found: $file"
        failed=$((failed + 1))
        return
    fi
    
    # Try to compile
    if timeout 30s $FLUXUS --go -O2 "$file" > /dev/null 2>&1; then
        echo "✓ Compilation successful"
        passed=$((passed + 1))
    else
        echo "✗ Compilation failed"
        failed=$((failed + 1))
    fi
    echo ""
}

# Test 1: Basic Go features
echo "=== Testing Basic Go Features ==="
test_go_file "examples/go/very_basic.go" "very_basic"
test_go_file "examples/go/hello_simple.go" "hello_simple"
test_go_file "examples/go/simple_print.go" "simple_print"

# Test 2: Go functions and methods
echo "=== Testing Go Functions and Methods ==="
test_go_file "examples/go/basic_functions.go" "basic_functions"
test_go_file "examples/go/methods.go" "methods"

# Test 3: Go control structures
echo "=== Testing Go Control Structures ==="
test_go_file "examples/go/control_structures.go" "control_structures"
test_go_file "examples/go/loops.go" "loops"

# Test 4: Go data structures
echo "=== Testing Go Data Structures ==="
test_go_file "examples/go/structs.go" "structs"
test_go_file "examples/go/interfaces.go" "interfaces"
test_go_file "examples/go/maps.go" "maps"
test_go_file "examples/go/slices.go" "slices"

# Test 5: Go concurrency
echo "=== Testing Go Concurrency ==="
test_go_file "examples/go/goroutines.go" "goroutines"
test_go_file "examples/go/channels.go" "channels"
test_go_file "examples/go/select.go" "select"

# Test 6: Go error handling
echo "=== Testing Go Error Handling ==="
test_go_file "examples/go/error_handling.go" "error_handling"

# Test 7: Go generics
echo "=== Testing Go Generics ==="
test_go_file "examples/go/generics.go" "generics"
test_go_file "examples/go/generic_functions.go" "generic_functions"
test_go_file "examples/go/generic_types.go" "generic_types"

# Test 8: Go advanced features
echo "=== Testing Go Advanced Features ==="
test_go_file "examples/go/reflection.go" "reflection"
test_go_file "examples/go/unsafe.go" "unsafe"
test_go_file "examples/go/cgo.go" "cgo"

# Test 9: Go standard library
echo "=== Testing Go Standard Library ==="
test_go_file "examples/go/stdlib_fmt.go" "stdlib_fmt"
test_go_file "examples/go/stdlib_math.go" "stdlib_math"
test_go_file "examples/go/stdlib_io.go" "stdlib_io"
test_go_file "examples/go/stdlib_net.go" "stdlib_net"

# Test 10: Go approximation constraints (new feature)
echo "=== Testing Go Approximation Constraints ==="
test_go_file "examples/go/approximation_constraints.go" "approximation_constraints"

# Summary
echo "=== Test Results ==="
echo "Total tests: $((passed + failed))"
echo "Passed: $passed"
echo "Failed: $failed"
echo ""

if [ $failed -eq 0 ]; then
    echo "🎉 All tests passed! Go feature support is excellent."
    exit 0
else
    echo "⚠️  $failed tests failed. Go feature support needs improvement."
    exit 1
fi