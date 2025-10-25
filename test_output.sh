#!/bin/bash
echo "Testing fluxus output..."
./print_test_new > output.txt 2>&1
echo "Output written to output.txt"
cat output.txt
echo "---"
echo "Exit code: $?"
