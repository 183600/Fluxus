#!/bin/bash

# Debug test script for Fluxus
# Usage: ./debug_test.sh [debug_level] [test_pattern]

set -e

DEBUG_LEVEL=${1:-"info"}
TEST_PATTERN=${2:-""}

echo "Running Fluxus tests with debug level: $DEBUG_LEVEL"
echo "Test pattern: $TEST_PATTERN"

# Set debug environment variable
export FLUXUS_DEBUG=$DEBUG_LEVEL

# Build the project
echo "Building project..."
stack build

# Run tests with debug output
if [ -n "$TEST_PATTERN" ]; then
    echo "Running tests matching pattern: $TEST_PATTERN"
    stack test --test-arguments "--match $TEST_PATTERN"
else
    echo "Running all tests"
    stack test
fi

echo "Test run completed"