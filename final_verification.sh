#!/bin/bash

echo "========================================="
echo "Final Verification of Warning Fixes"
echo "========================================="
echo ""

echo "Step 1: Building project..."
echo "----------------------------"
stack build 2>&1 | tee final_build.log

echo ""
echo "Step 2: Running tests..."
echo "------------------------"
stack test 2>&1 | tee final_test.log

echo ""
echo "Step 3: Analyzing results..."
echo "----------------------------"

# Count warnings in build
BUILD_WARNINGS=$(grep -c "warning:" final_build.log 2>/dev/null || echo "0")
echo "Build warnings: $BUILD_WARNINGS"

# Count test failures
TEST_FAILURES=$(grep -c "FAIL" final_test.log 2>/dev/null || echo "0")
echo "Test failures: $TEST_FAILURES"

# Count test successes
TEST_SUCCESSES=$(grep -c "PASS" final_test.log 2>/dev/null || echo "0")
echo "Test successes: $TEST_SUCCESSES"

echo ""
echo "========================================="
echo "Verification Complete"
echo "========================================="
echo ""
echo "Summary:"
echo "  - Build warnings: $BUILD_WARNINGS"
echo "  - Test failures: $TEST_FAILURES"
echo "  - Test successes: $TEST_SUCCESSES"
echo ""
echo "Check final_build.log and final_test.log for details."
