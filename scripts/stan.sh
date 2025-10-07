#!/bin/bash

set -e

echo "Running Stan static analysis..."

# Run Stan analysis on the project
stan --include-dir=src/ --include-dir=test/ --include-dir=app/

echo "Stan analysis completed successfully"