#!/bin/bash

set -e

echo "Running Doctest..."

# Run doctest on all modules with documentation
doctest src/

# Also run doctest on test modules if they have documentation
doctest test/

echo "Doctest completed successfully"