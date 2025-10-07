#!/bin/bash

set -e

echo "Running Ormolu format check..."

# Check formatting for source files
echo "Checking source files..."
find src/ -name "*.hs" -exec ormolu --mode check {} \;

# Check formatting for test files
echo "Checking test files..."
find test/ -name "*.hs" -exec ormolu --mode check {} \;

# Check formatting for app files
echo "Checking app files..."
find app/ -name "*.hs" -exec ormolu --mode check {} \;

# Check formatting for benchmark files
echo "Checking benchmark files..."
find bench/ -name "*.hs" -exec ormolu --mode check {} \;

echo "All files are properly formatted"