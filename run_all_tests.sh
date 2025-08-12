#!/bin/bash

# Master Test Runner for Fluxus Compiler
# Executes all test suites in order: basic, advanced, stress

echo "========================================"
echo "     Fluxus Compiler Master Test Suite    "
echo "========================================"
echo "Date: $(date)"
echo

# Check if Fluxus compiler exists
FLUXUS="dist-newstyle/build/x86_64-linux/ghc-9.6.7/fluxus-0.1.0.0/x/fluxus/build/fluxus/fluxus"

if [ ! -x "$FLUXUS" ]; then
    echo "Error: Fluxus compiler not found at $FLUXUS"
    echo "Please build the project first with 'cabal build'"
    exit 1
fi

echo "Found Fluxus compiler at: $FLUXUS"
echo

# Initialize counters
TOTAL_SUITES=0
PASSED_SUITES=0

# Function to run a test suite
run_suite() {
    local suite_name="$1"
    local suite_script="$2"
    
    TOTAL_SUITES=$((TOTAL_SUITES + 1))
    
    echo "========================================"
    echo "Running $suite_name"
    echo "========================================"
    
    if [ -x "$suite_script" ]; then
        if "$suite_script" > /dev/null 2>&1; then
            PASSED_SUITES=$((PASSED_SUITES + 1))
        else
            echo -e "\n❌ $suite_name FAILED"
            "$suite_script"
        fi
    else
        echo "Error: Test script $suite_script not found or not executable"
        echo "❌ $suite_name FAILED"
    fi
    
    echo
    echo
}

# Run all test suites
run_suite "Complete Test Suite" "./complete_test_suite.sh"
run_suite "Advanced Feature Test Suite" "./advanced_test_suite.sh"
run_suite "Stress Test Suite" "./stress_test_suite.sh"

# Final summary
echo "========================================"
echo "          MASTER TEST SUMMARY           "
echo "========================================"
echo "Total test suites: $TOTAL_SUITES"
echo "Passed suites: $PASSED_SUITES"
echo "Failed suites: $((TOTAL_SUITES - PASSED_SUITES))"

if [ $PASSED_SUITES -eq $TOTAL_SUITES ]; then
    echo
    echo "🎉🎉🎉 ALL TEST SUITES PASSED! 🎉🎉🎉"
    echo "The Fluxus compiler is working correctly."
    exit 0
else
    echo
    echo "⚠️  SOME TEST SUITES FAILED"
    echo "Please review the output above for details."
    exit 1
fi
