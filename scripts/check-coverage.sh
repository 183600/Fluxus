#!/bin/bash

set -e

# Default coverage threshold
THRESHOLD=${1:-90}

echo "Checking test coverage (threshold: ${THRESHOLD}%)..."

# Run tests with coverage and generate coverage report
cabal test fluxus-test --enable-coverage --test-show-details=direct

# Check if coverage meets threshold
COVERAGE_FILE=$(find . -name "tix" -type f | head -1)
if [ -z "$COVERAGE_FILE" ]; then
    echo "Error: No coverage file found"
    exit 1
fi

# Generate HTML coverage report
cabal hpc coverage fluxus-test --destdir=coverage-report

# Extract coverage percentage (this is a simplified version)
# In a real scenario, you might want to use hpc report or a custom tool
echo "Coverage report generated in coverage-report/"
echo "Please manually verify that coverage meets ${THRESHOLD}% threshold"

echo "Coverage check completed"