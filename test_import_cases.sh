#!/bin/bash

echo "Testing Go Import Declaration Parser..."
echo "========================================"
echo ""

# Run the specific import tests
stack test --test-arguments "--match 'Import Declaration'" 2>&1

echo ""
echo "Test completed!"
