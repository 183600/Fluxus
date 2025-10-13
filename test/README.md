# Fluxus Test Suite

This directory contains the comprehensive test suite for the Fluxus compiler.

## Directory Structure

```
test/
├── Spec.hs                          # Main test entry point
├── README.md                        # This file
├── Test/
│   └── Fluxus/
│       ├── Analysis/                # Analysis phase tests
│       │   ├── TypeInference.hs
│       │   ├── EscapeAnalysis.hs
│       │   ├── OwnershipInference.hs
│       │   ├── ShapeAnalysis.hs
│       │   └── SmartFallback.hs
│       ├── Parser/                  # Parser tests
│       │   ├── Python.hs
│       │   └── Go.hs
│       ├── CodeGen/                 # Code generation tests
│       │   ├── CPP.hs
│       │   └── Go.hs
│       ├── Optimization/            # Optimization pass tests
│       │   ├── ConstantFolding.hs
│       │   ├── DeadCodeElimination.hs
│       │   ├── Inlining.hs
│       │   └── ...
│       ├── Compiler/                # Compiler infrastructure tests
│       │   ├── Config.hs
│       │   └── Driver.hs
│       ├── Utils/                   # Utility module tests
│       │   ├── Pretty.hs
│       │   └── Graph.hs
│       ├── Runtime/                 # Runtime integration tests
│       │   ├── Python.hs
│       │   └── Go.hs
│       ├── Integration.hs           # Integration tests
│       ├── EndToEnd.hs              # End-to-end tests
│       ├── PythonGolden.hs          # Golden file tests
│       ├── Performance.hs           # Performance tests
│       ├── Debug.hs                 # Debug & logging tests
│       └── ConvertCommand.hs        # CLI command tests
├── python-tests/                    # Python test cases
│   ├── basic_arithmetic.py
│   ├── test_functions.py
│   ├── test_classes.py
│   └── ...
└── go-tests/                        # Go test cases
    └── ...
```

## Running Tests

### Quick Start

```bash
# Run all tests
stack test

# Run tests with verbose output
stack test --test-arguments="-v"

# Run specific test suite
stack test --test-arguments="-m Parser"
```

### Using the Test Script

```bash
# Run all tests
./scripts/run-tests.sh

# Run with coverage
./scripts/run-tests.sh --coverage

# Run fast (skip performance tests)
./scripts/run-tests.sh --fast

# Run specific tests
./scripts/run-tests.sh --test "Unit Tests"
```

## Test Categories

### 1. Unit Tests
- **Analysis**: Type inference, escape analysis, ownership inference
- **Parsers**: Python and Go lexer/parser tests
- **Code Generation**: C++ code generation tests
- **Optimization**: Individual optimization pass tests
- **Compiler**: Config and driver tests
- **Utils**: Pretty printer and graph utility tests
- **Debug**: Logging and error reporting tests

### 2. Integration Tests
- Multi-module compilation
- Import resolution
- Runtime integration

### 3. End-to-End Tests
- Complete compilation workflows
- Executable generation
- Real-world application compilation

### 4. Golden Tests
- Known-good output verification
- Regression detection

### 5. Performance Tests
- Compilation speed benchmarks
- Generated code performance
- Memory usage validation

## Writing New Tests

### Test Structure

```haskell
module Test.Fluxus.MyModule (spec) where

import Test.Hspec
import Test.QuickCheck  -- For property-based tests

spec :: Spec
spec = describe "My Module" $ do
  unitTestsSpec
  propertyTestsSpec

unitTestsSpec :: Spec
unitTestsSpec = describe "Unit Tests" $ do
  it "does something correctly" $ do
    result <- myFunction input
    result `shouldBe` expected
  
  it "handles edge cases" $ do
    result <- myFunction edgeCase
    result `shouldSatisfy` isValid

propertyTestsSpec :: Spec
propertyTestsSpec = describe "Property-Based Tests" $ do
  it "maintains invariant" $ property $ \input ->
    let result = myFunction input
    in invariantHolds result
```

### Adding a New Test Module

1. Create test file in appropriate directory: `test/Test/Fluxus/MyModule.hs`
2. Add module to `package.yaml` under `tests.fluxus-test.other-modules`
3. Import and add to `test/Spec.hs`:
   ```haskell
   import qualified Test.Fluxus.MyModule as MyModuleTests
   -- In main:
   MyModuleTests.spec
   ```
4. Run tests to verify: `stack test`

## Test Best Practices

### Do:
- ✓ Write descriptive test names
- ✓ Test both success and failure cases
- ✓ Include edge cases
- ✓ Use property-based testing for invariants
- ✓ Keep tests isolated and independent
- ✓ Use temporary directories for file operations
- ✓ Clean up resources in tests

### Don't:
- ✗ Depend on test execution order
- ✗ Use hardcoded absolute paths
- ✗ Leave test files in the filesystem
- ✗ Make tests depend on external services
- ✗ Write tests that take too long (> 30s)
- ✗ Commit generated test artifacts

## Coverage Requirements

- **Overall**: ≥ 80%
- **Critical paths**: ≥ 95%
- **Branch coverage**: ≥ 75%

Check coverage:
```bash
./scripts/run-tests.sh --coverage
```

## Debugging Failed Tests

### Run single test with verbose output
```bash
stack test --test-arguments="-m \"specific test name\" -v"
```

### Run tests with GHCi for debugging
```bash
stack ghci fluxus:test:fluxus-test
> :main -m "specific test"
```

### Common Issues

1. **Test Timeout**: Increase timeout or optimize test
2. **File Not Found**: Check paths are absolute or relative to test directory
3. **Import Errors**: Verify module is listed in package.yaml
4. **Type Errors**: Ensure test data matches expected types

## Continuous Integration

Tests run automatically on:
- Every push to main branch
- Pull request creation/update
- Nightly builds (with performance tests)

CI must pass before merging:
- All tests pass
- Coverage requirements met
- No compiler warnings

## Performance Benchmarks

Performance tests are separate from regular tests:

```bash
# Run benchmarks
stack bench

# Compare with baseline
stack bench --benchmark-arguments="--output benchmark-results.html"
```

## Related Documentation

- [TEST_STRATEGY.md](../TEST_STRATEGY.md) - Comprehensive testing strategy
- [CONTRIBUTING.md](../CONTRIBUTING.md) - Contribution guidelines
- [README.md](../README.md) - Project overview

## Questions?

- Check existing tests for examples
- Review test strategy document
- Ask in team chat or open an issue
- Consult Hspec documentation: https://hspec.github.io/
