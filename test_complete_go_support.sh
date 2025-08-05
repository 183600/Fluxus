#!/bin/bash

# Comprehensive Go Feature Support Test
# This script tests all major Go features supported by Fluxus

echo "=== Fluxus Complete Go Feature Support Test ==="
echo "Testing comprehensive Go language support including new features..."
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
    
    # Test compilation
    if timeout 30 "$FLUXUS" --go "$file" 2>/dev/null; then
        echo "✓ PASSED"
        passed=$((passed + 1))
    else
        echo "✗ FAILED"
        failed=$((failed + 1))
    fi
    
    echo ""
}

# Test approximation constraints
echo "=== Testing Approximation Constraints ==="
test_go_file "test_comprehensive_approximation_constraints.go" "Comprehensive Approximation Constraints"
test_go_file "test_approximation_constraints.go" "Basic Approximation Constraints"
test_go_file "test_enhanced_constraints.go" "Enhanced Generic Constraints"

# Test Go 1.20+ features
echo "=== Testing Go 1.20+ Features ==="
test_go_file "test_go_120_plus_features.go" "Go 1.20+ Features"

# Test standard library packages
echo "=== Testing Standard Library Packages ==="
test_go_file "test_standard_library_packages.go" "Standard Library Packages"

# Test advanced Go features
echo "=== Testing Advanced Go Features ==="
test_go_file "examples/go/advanced_go_features.go" "Advanced Go Features"
test_go_file "examples/go/advanced_go_features_compatible.go" "Advanced Go Features Compatible"

# Test comprehensive Go features
echo "=== Testing Comprehensive Go Features ==="
test_go_file "examples/go/comprehensive_go_features.go" "Comprehensive Go Features"

# Test specific edge cases
echo "=== Testing Edge Cases ==="
test_go_file "test_interface_embedding.go" "Interface Embedding"
test_go_file "test_simple_interface.go" "Simple Interface"
test_go_file "test_builtin_functions.go" "Built-in Functions"
test_go_file "test_enhanced_package_features.go" "Enhanced Package Features"
test_go_file "test_enhanced_stdlib.go" "Enhanced Standard Library"

# Test basic functionality
echo "=== Testing Basic Functionality ==="
test_go_file "test_basic.go" "Basic Functionality"
test_go_file "test_go_simple.go" "Simple Go Test"
test_go_file "test_simple_tilde.go" "Simple Tilde Test"

# Print summary
echo "=== Test Summary ==="
echo "Total tests: $((passed + failed))"
echo "Passed: $passed"
echo "Failed: $failed"
echo "Success rate: $(( passed * 100 / (passed + failed) ))%"

if [ $failed -eq 0 ]; then
    echo "🎉 All tests passed! Go language support is complete."
    exit 0
else
    echo "⚠️  Some tests failed. Go language support needs improvement."
    exit 1
fi