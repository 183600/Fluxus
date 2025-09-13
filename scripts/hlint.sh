#!/bin/bash

set -e

echo "Running HLint..."

# Run HLint on source code
hlint src/ --report=hlint-report.html

# Run HLint on test code
hlint test/ --report=hlint-test-report.html

echo "HLint completed successfully"
echo "Reports generated: hlint-report.html, hlint-test-report.html"