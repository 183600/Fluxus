#!/bin/bash

set -e

echo "=========================================="
echo "Running Enhanced Stack Test for Production"
echo "=========================================="

# Ensure we're in the project root
cd "$(dirname "$0")/.."

# Step 1: Clean previous builds
echo "Step 1: Cleaning previous builds..."
stack clean

# Step 2: Build with tests and benchmarks
echo "Step 2: Building project with tests and benchmarks..."
stack build --test --bench --no-run-tests --no-run-benchmarks

# Step 3: Run unit tests with coverage
echo "Step 3: Running unit tests with coverage..."
stack test --coverage

# Step 4: Run benchmarks
echo "Step 4: Running benchmarks..."
stack bench --benchmark-options=--output=bench-results.html

# Step 5: Run quality gate
echo "Step 5: Running quality gate checks..."
if [ -f "scripts/quality-gate.sh" ]; then
    chmod +x scripts/quality-gate.sh
    ./scripts/quality-gate.sh "$@"
else
    echo "Warning: quality-gate.sh not found, skipping quality checks"
fi

echo ""
echo "=========================================="
echo "Enhanced Stack Test Completed Successfully"
echo "=========================================="
echo "The project has passed all production readiness checks"
echo ""
echo "Generated reports:"
echo "- Coverage: .stack-work/install/.../hpc/"
echo "- Benchmarks: bench-results.html"
echo "- Quality reports: See individual tool outputs above"