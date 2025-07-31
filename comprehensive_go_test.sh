#!/bin/bash

# Comprehensive test suite for Fluxus Go compilation
# This script tests that Go code compiled through Fluxus produces the correct output

set -e

echo "=== Fluxus Go Compilation Test Suite ==="
echo

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test counter
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Function to run a test
run_test() {
    local test_name="$1"
    local source_file="$2"
    local expected_output="$3"
    local description="$4"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo -e "${YELLOW}Test $TOTAL_TESTS: $test_name${NC}"
    echo "Description: $description"
    echo "Source file: $source_file"
    
    # Compile with Fluxus
    echo "Compiling with Fluxus..."
    if cabal run fluxus -- --go -o test_output "$source_file" 2>/dev/null; then
        echo "✓ Fluxus compilation successful"
        
        # Run the generated executable and capture output
        echo "Running generated executable..."
        local actual_output=$(./test_output 2>&1)
        
        # Compare with expected output
        if [[ "$actual_output" == "$expected_output" ]]; then
            echo -e "${GREEN}✓ Test PASSED${NC}"
            echo "Expected: '$expected_output'"
            echo "Actual:   '$actual_output'"
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            echo -e "${RED}✗ Test FAILED${NC}"
            echo "Expected: '$expected_output'"
            echo "Actual:   '$actual_output'"
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    else
        echo -e "${RED}✗ Fluxus compilation failed${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    
    # Cleanup
    rm -f test_output
    echo
}

# Test 1: Simple Hello World
run_test "Hello World" \
    "examples/go/simple_hello.go" \
    "Hello, Fluxus!
Count: 0
Count: 1
Count: 2
Count: 3
Count: 4" \
    "Basic hello world with loop"

# Test 2: Simple Math Operations
run_test "Simple Math" \
    "examples/go/simple_math.go" \
    "Simple Math Operations
5 + 3 = 8
5 * 3 = 15
5! = 120
Numbers 1 to 5:
1 2 3 4 5" \
    "Basic arithmetic operations and factorial"

# Test 3: Fibonacci Working
run_test "Fibonacci Working" \
    "examples/go/fibonacci_working.go" \
    "Fibonacci sequence:
fib(0) = 0
fib(1) = 1
fib(2) = 1
fib(3) = 2
fib(4) = 3
fib(5) = 5
fib(6) = 8
fib(7) = 13
fib(8) = 21
fib(9) = 34" \
    "Basic fibonacci sequence generation"

# Test 4: Variable Operations
cat > test_variable_ops.go << 'EOF'
package main

import "fmt"

func main() {
    x := 10
    y := 20
    sum := x + y
    fmt.Printf("Sum: %d\n", sum)
    
    x = x + 5
    fmt.Printf("Updated x: %d\n", x)
    
    isGreater := x > y
    fmt.Printf("x > y: %t\n", isGreater)
}
EOF

run_test "Variable Operations" \
    "test_variable_ops.go" \
    "Sum: 30
Updated x: 15
x > y: false" \
    "Variable assignment and comparison"

# Test 5: Simple Function Calls
cat > test_functions.go << 'EOF'
package main

import "fmt"

func greet(name string) string {
    return "Hello, " + name + "!"
}

func add(a int, b int) int {
    return a + b
}

func main() {
    message := greet("World")
    fmt.Println(message)
    
    result := add(10, 20)
    fmt.Printf("10 + 20 = %d\n", result)
}
EOF

run_test "Function Calls" \
    "test_functions.go" \
    "Hello, World!
10 + 20 = 30" \
    "Function calls with different parameter types"

# Test 6: Control Flow
cat > test_control_flow.go << 'EOF'
package main

import "fmt"

func main() {
    // If-else
    x := 15
    if x > 10 {
        fmt.Println("x is greater than 10")
    } else {
        fmt.Println("x is not greater than 10")
    }
    
    // For loop
    fmt.Print("Even numbers: ")
    for i := 0; i <= 10; i += 2 {
        fmt.Printf("%d ", i)
    }
    fmt.Println()
}
EOF

run_test "Control Flow" \
    "test_control_flow.go" \
    "x is greater than 10
Even numbers: 0 2 4 6 8 10 " \
    "If-else statements and for loops"

# Print summary
echo "=== Test Summary ==="
echo "Total tests: $TOTAL_TESTS"
echo -e "${GREEN}Passed: $PASSED_TESTS${NC}"
echo -e "${RED}Failed: $FAILED_TESTS${NC}"

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed.${NC}"
    exit 1
fi