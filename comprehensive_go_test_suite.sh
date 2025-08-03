#!/bin/bash

# Comprehensive Go Test Suite for Fluxus Compiler
set -e

FLUXUS="dist-newstyle/build/x86_64-linux/ghc-9.6.7/fluxus-0.1.0.0/x/fluxus/build/fluxus/fluxus"
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

echo "=== Comprehensive Go Test Suite for Fluxus Compiler ==="
echo "Using Fluxus: $FLUXUS"
echo ""

# Function to run a single test
run_go_test() {
    local test_name="$1"
    local source_file="$2"
    local expected_output="$3"
    local test_description="$4"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo "Test $TOTAL_TESTS: $test_name - $test_description"
    echo "Source: $source_file"
    
    # Create output filename
    local output_file="${test_name}_compiled"
    
    # Remove old output file if exists
    rm -f "$output_file"
    
    # Compile
    if ./"$FLUXUS" --go "$source_file" -o "$output_file" 2>/dev/null; then
        echo "✓ Compilation successful"
        
        # Check if executable was created
        if [ -f "$output_file" ]; then
            echo "✓ Executable created"
            
            # Run and capture output
            if actual_output=$(./"$output_file" 2>&1); then
                echo "✓ Execution successful"
                echo "Expected: $expected_output"
                echo "Actual: $actual_output"
                
                # Compare output
                if [ "$actual_output" = "$expected_output" ]; then
                    echo "✓ Output matches expected"
                    echo "✅ TEST PASSED: $test_name"
                    PASSED_TESTS=$((PASSED_TESTS + 1))
                else
                    echo "❌ Output mismatch"
                    echo "❌ TEST FAILED: $test_name"
                    FAILED_TESTS=$((FAILED_TESTS + 1))
                fi
            else
                echo "❌ Execution failed"
                echo "❌ TEST FAILED: $test_name"
                FAILED_TESTS=$((FAILED_TESTS + 1))
            fi
            
            # Clean up
            rm -f "$output_file"
        else
            echo "❌ Executable not created"
            echo "❌ TEST FAILED: $test_name"
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    else
        echo "❌ Compilation failed"
        echo "❌ TEST FAILED: $test_name"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    
    echo ""
}

# Create test files
echo "Creating test files..."

# Test 1: Very Basic Go - Empty main
cat > test_go_empty.go << 'EOF'
package main

func main() {
}
EOF

# Test 2: Simple print
cat > test_go_print.go << 'EOF'
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}
EOF

# Test 3: Basic arithmetic
cat > test_go_arithmetic.go << 'EOF'
package main

import "fmt"

func main() {
    a := 5
    b := 3
    fmt.Println(a + b)
}
EOF

# Test 4: Function definition and call
cat > test_go_function.go << 'EOF'
package main

import "fmt"

func add(x int, y int) int {
    return x + y
}

func main() {
    result := add(5, 3)
    fmt.Println(result)
}
EOF

# Test 5: For loop
cat > test_go_loop.go << 'EOF'
package main

import "fmt"

func main() {
    for i := 1; i <= 3; i++ {
        fmt.Println(i)
    }
}
EOF

# Test 6: If statement
cat > test_go_if.go << 'EOF'
package main

import "fmt"

func main() {
    x := 5
    if x > 3 {
        fmt.Println("greater")
    } else {
        fmt.Println("not greater")
    }
}
EOF

# Test 7: Multiple variable assignments
cat > test_go_variables.go << 'EOF'
package main

import "fmt"

func main() {
    x := 10
    y := 20
    z := x + y
    fmt.Println(z)
}
EOF

# Test 8: String operations
cat > test_go_strings.go << 'EOF'
package main

import "fmt"

func main() {
    name := "Go"
    fmt.Println("Hello " + name)
}
EOF

# Test 9: Fibonacci function
cat > test_go_fibonacci.go << 'EOF'
package main

import "fmt"

func fibonacci(n int) int {
    if n <= 1 {
        return n
    }
    return fibonacci(n-1) + fibonacci(n-2)
}

func main() {
    fmt.Println(fibonacci(5))
}
EOF

# Test 10: Printf formatting
cat > test_go_printf.go << 'EOF'
package main

import "fmt"

func main() {
    x := 42
    fmt.Printf("The answer is %d\n", x)
}
EOF

echo "Running Go tests..."
echo ""

# Run all tests
run_go_test "go_empty" "test_go_empty.go" "" "Empty main function"
run_go_test "go_print" "test_go_print.go" "Hello, World!" "Simple print statement"
run_go_test "go_arithmetic" "test_go_arithmetic.go" "8" "Basic arithmetic operations"
run_go_test "go_function" "test_go_function.go" "8" "Function definition and call"
run_go_test "go_loop" "test_go_loop.go" "1
2
3" "For loop iteration"
run_go_test "go_if" "test_go_if.go" "greater" "Conditional statement"
run_go_test "go_variables" "test_go_variables.go" "30" "Multiple variable assignments"
run_go_test "go_strings" "test_go_strings.go" "Hello Go" "String concatenation"
run_go_test "go_fibonacci" "test_go_fibonacci.go" "5" "Recursive fibonacci function"
run_go_test "go_printf" "test_go_printf.go" "The answer is 42" "Printf formatting"

# Clean up test files
echo "Cleaning up test files..."
rm -f test_go_*.go

# Print summary
echo "=== GO TEST SUMMARY ==="
echo "Total tests: $TOTAL_TESTS"
echo "Passed: $PASSED_TESTS"
echo "Failed: $FAILED_TESTS"
echo "Success rate: $(( PASSED_TESTS * 100 / TOTAL_TESTS ))%"

if [ $FAILED_TESTS -eq 0 ]; then
    echo "🎉 All Go tests passed!"
    exit 0
else
    echo "❌ Some Go tests failed"
    exit 1
fi