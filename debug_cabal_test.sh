#!/bin/bash

# Fluxus Cabal Test Debug Script
# This script helps debug cabal test issues with enhanced logging and breakpoints

echo "=== Fluxus Cabal Test Debugger ==="
echo "Starting debug session..."

# Create debug log directory
mkdir -p debug_logs

# Set timestamp for this debug session
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
DEBUG_LOG="debug_logs/cabal_test_${TIMESTAMP}.log"

echo "Debug log will be written to: $DEBUG_LOG"

# Function to run cabal test with different verbosity levels
run_cabal_test_debug() {
    local verbosity=$1
    local extra_args=$2
    
    echo "Running cabal test with verbosity: $verbosity"
    echo "Extra args: $extra_args"
    
    # Run cabal test and capture output
    cabal test --verbose=$verbosity $extra_args 2>&1 | tee "$DEBUG_LOG"
    
    return ${PIPESTATUS[0]}
}

# Function to analyze test failures
analyze_test_failures() {
    local log_file=$1
    
    echo "=== Analyzing Test Failures ==="
    
    # Look for common failure patterns
    echo "Looking for failed tests..."
    grep -n "FAILED\|✘\|expectationFailure" "$log_file" || echo "No explicit failures found"
    
    echo "Looking for warnings..."
    grep -n "warning:\|Warning" "$log_file" || echo "No warnings found"
    
    echo "Looking for errors..."
    grep -n "error:\|Error" "$log_file" || echo "No errors found"
    
    echo "Looking for timeouts..."
    grep -n "timeout\|TIMEOUT" "$log_file" || echo "No timeouts found"
}

# Function to run specific test modules
run_specific_tests() {
    echo "=== Running Specific Test Modules ==="
    
    # Test modules that showed issues
    test_modules=(
        "Test.Fluxus.Parser.Go"
        "Test.Fluxus.Analysis.EscapeAnalysis"
        "Test.Fluxus.Analysis.ShapeAnalysis"
    )
    
    for module in "${test_modules[@]}"; do
        echo "Testing module: $module"
        cabal test --test-options="--match=\"$module\"" 2>&1 | tee -a "$DEBUG_LOG"
        echo "---"
    done
}

# Function to check build warnings
check_build_warnings() {
    echo "=== Checking Build Warnings ==="
    
    # Build with warnings enabled
    cabal build --ghc-options="-Wall -Wextra" 2>&1 | grep -i warning | tee -a "$DEBUG_LOG"
}

# Function to run tests with memory profiling
run_memory_debug() {
    echo "=== Running Tests with Memory Profiling ==="
    
    # Run with RTS options for memory profiling
    cabal test --test-options="+RTS -s -h -RTS" 2>&1 | tee -a "$DEBUG_LOG"
    
    # Generate heap profile if available
    if [ -f fluxus-test.hp ]; then
        echo "Heap profile generated: fluxus-test.hp"
    fi
}

# Function to create minimal test case
create_minimal_test() {
    echo "=== Creating Minimal Test Case ==="
    
    cat > debug_minimal_test.hs << 'EOF'
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser

main :: IO ()
main = hspec $ do
  describe "Minimal Go Parser Test" $ do
    it "should tokenize simple expression" $ do
      let input = "x + 42"
      case runGoLexer "test.go" input of
        Left err -> expectationFailure $ "Lexer failed: " ++ show err
        Right tokens -> do
          length tokens `shouldBe` 3
          
    it "should parse package declaration" $ do
      let tokens = []  -- Mock tokens for testing
      case runGoParser "test.go" tokens of
        Left err -> expectationFailure $ "Parser failed: " ++ show err  
        Right _ast -> return ()
EOF

    echo "Minimal test created: debug_minimal_test.hs"
    echo "Run with: runhaskell debug_minimal_test.hs"
}

# Function to add debug logging to modules
add_debug_logging() {
    echo "=== Adding Debug Logging ==="
    
    # Find modules that need debugging
    find src -name "*.hs" -exec grep -l "TODO\|FIXME\|undefined" {} \; > debug_modules.txt
    
    echo "Modules that may need attention:"
    cat debug_modules.txt
}

# Main debug workflow
main() {
    case "${1:-full}" in
        "quick")
            echo "Running quick debug..."
            run_cabal_test_debug 1 "--test-options='--quickcheck-tests=10'"
            ;;
        "verbose")
            echo "Running verbose debug..."
            run_cabal_test_debug 3 ""
            analyze_test_failures "$DEBUG_LOG"
            ;;
        "specific")
            echo "Running specific tests..."
            run_specific_tests
            ;;
        "memory")
            echo "Running memory debug..."
            run_memory_debug
            ;;
        "warnings")
            echo "Checking build warnings..."
            check_build_warnings
            ;;
        "minimal")
            echo "Creating minimal test case..."
            create_minimal_test
            ;;
        "analyze")
            if [ -f "$2" ]; then
                analyze_test_failures "$2"
            else
                analyze_test_failures "$DEBUG_LOG"
            fi
            ;;
        "full"|*)
            echo "Running full debug workflow..."
            
            # Step 1: Check build warnings
            check_build_warnings
            
            # Step 2: Run quick test to see basic issues
            echo "Step 2: Quick test run..."
            run_cabal_test_debug 1 "--test-options='--quickcheck-tests=5'" || true
            
            # Step 3: Analyze failures
            analyze_test_failures "$DEBUG_LOG"
            
            # Step 4: Run specific problematic tests
            echo "Step 4: Testing specific modules..."
            run_specific_tests
            
            # Step 5: Create minimal test cases
            echo "Step 5: Creating minimal test cases..."
            create_minimal_test
            
            echo "=== Debug Session Complete ==="
            echo "Log file: $DEBUG_LOG"
            echo "Next steps:"
            echo "1. Review the log file for specific errors"
            echo "2. Run 'bash debug_cabal_test.sh minimal' to create minimal tests"
            echo "3. Fix the identified issues in the source code"
            echo "4. Run 'bash debug_cabal_test.sh specific' to test individual modules"
            ;;
    esac
}

# Run the main function with provided arguments
main "$@"
