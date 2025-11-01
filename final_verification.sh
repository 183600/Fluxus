#!/bin/bash

echo "========================================="
echo "Final Verification of Warning Fixes"
echo "========================================="
echo ""

LOG_DIR="dist/logs"
BUILD_LOG="$LOG_DIR/final_build.log"
TEST_LOG="$LOG_DIR/final_test.log"

mkdir -p "$LOG_DIR"

echo "Step 1: Building project..."
echo "----------------------------"
stack build 2>&1 | tee "$BUILD_LOG"

echo ""
echo "Step 2: Running tests..."
echo "------------------------"
stack test 2>&1 | tee "$TEST_LOG"

echo ""
echo "Step 3: Analyzing results..."
echo "----------------------------"

# Count warnings in build
BUILD_WARNINGS=$(grep -c "warning:" "$BUILD_LOG" 2>/dev/null || echo "0")
echo "Build warnings: $BUILD_WARNINGS"

# Count test failures
TEST_FAILURES=$(grep -c "FAIL" "$TEST_LOG" 2>/dev/null || echo "0")
echo "Test failures: $TEST_FAILURES"

# Count test successes
TEST_SUCCESSES=$(grep -c "PASS" "$TEST_LOG" 2>/dev/null || echo "0")
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
echo "Check $BUILD_LOG and $TEST_LOG for details."
