#!/bin/bash

set -e

echo "Running Weeder dead code analysis..."

# Run weeder to detect dead code
weeder --type-class-defaults

echo "Weeder analysis completed successfully"