#!/bin/bash

# Build first
echo "Building project..."
stack build 2>&1

# Run tests
echo "Running tests..."
stack exec -- fluxus-test 2>&1

echo "Done"
