# Fluxus Compiler Test Suite

This directory contains comprehensive test scripts for the Fluxus compiler, which tests both Go and Python language feature support.

## Test Scripts Overview

### 1. Complete Test Suite (`complete_test_suite.sh`)
- Tests basic language features for both Go and Python
- Covers variables, functions, control flow, data structures
- Good for initial verification of compiler functionality

### 2. Advanced Feature Test Suite (`advanced_test_suite.sh`)
- Tests advanced language features like pointers, interfaces, decorators, etc.
- Includes edge cases and some expected failure cases
- Useful for verifying advanced compiler capabilities

### 3. Stress Test Suite (`stress_test_suite.sh`)
- Tests performance and stability with large programs
- Tests memory handling, recursive functions, complex data structures
- Useful for identifying performance bottlenecks

### 4. Master Test Runner (`run_all_tests.sh`)
- Runs all test suites in sequence
- Provides a comprehensive verification of the compiler

## Usage

Make sure the Fluxus compiler is built before running tests:

```bash
# Build the compiler
cabal build

# Run individual test suite
./complete_test_suite.sh
./advanced_test_suite.sh
./stress_test_suite.sh

# Or run all tests
./run_all_tests.sh
```

## Test Results

Each test script will output:
- Detailed information about each test case
- Color-coded pass/fail indicators
- Summary of passed/failed tests
- Performance timing for stress tests

## Adding New Tests

To add new tests:
1. Create a new test file in the appropriate test script
2. Follow the existing patterns for the `run_test` function
3. Make sure to clean up any temporary files
4. Update the test counters and summaries as needed

## Troubleshooting

If tests fail:
1. Check that the Fluxus compiler is properly built
2. Verify the FLUXUS path in each script matches your build output
3. Review the specific test output for error details
4. Check that all dependencies are installed