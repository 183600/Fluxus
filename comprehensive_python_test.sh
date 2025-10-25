#!/bin/bash

# Comprehensive test suite for Fluxus Python compilation
# This script tests that Python code compiled through Fluxus produces the correct output

set -e

echo "=== Fluxus Python Compilation Test Suite ==="
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
    if cabal run fluxus -- --python -o test_output "$source_file" 2>/dev/null; then
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

# Test 1: Very Basic Print
run_test "Very Basic" \
    "examples/python/very_basic.py" \
    "42" \
    "Simple number printing"

# Test 2: Simple Print Multiple
run_test "Simple Print Multiple" \
    "examples/python/simple_print.py" \
    "5
8
13" \
    "Multiple print statements"

# Test 3: Fibonacci Basic
run_test "Fibonacci Basic" \
    "examples/python/fibonacci_basic.py" \
    "1
1
2
3
5" \
    "Basic fibonacci sequence generation"

# Test 4: Variable Operations
cat > test_variable_ops.py << 'EOF'
x = 10
y = 20
sum_result = x + y
print(f"Sum: {sum_result}")

x = x + 5
print(f"Updated x: {x}")

is_greater = x > y
print(f"x > y: {is_greater}")
EOF

run_test "Variable Operations" \
    "test_variable_ops.py" \
    "Sum: 30
Updated x: 15
x > y: False" \
    "Variable assignment and comparison"

# Test 5: Function Definitions
cat > test_functions.py << 'EOF'
def greet(name):
    return f"Hello, {name}!"

def add(a, b):
    return a + b

print(greet("World"))
result = add(10, 20)
print(f"10 + 20 = {result}")
EOF

run_test "Function Definitions" \
    "test_functions.py" \
    "Hello, World!
10 + 20 = 30" \
    "Function definitions and calls"

# Test 6: Control Flow
cat > test_control_flow.py << 'EOF'
x = 15
if x > 10:
    print("x is greater than 10")
else:
    print("x is not greater than 10")

print("Even numbers:", end=" ")
for i in range(0, 11, 2):
    print(i, end=" ")
print()
EOF

run_test "Control Flow" \
    "test_control_flow.py" \
    "x is greater than 10
Even numbers: 0 2 4 6 8 10 " \
    "If-else statements and for loops"

# Test 7: Mathematical Operations
cat > test_math_ops.py << 'EOF'
import math

# Basic arithmetic
a = 10
b = 3
print(f"Addition: {a + b}")
print(f"Subtraction: {a - b}")
print(f"Multiplication: {a * b}")
print(f"Division: {a / b}")
print(f"Modulo: {a % b}")

# Power and square root
print(f"Power: {a ** 2}")
print(f"Square root: {math.sqrt(a)}")

# Built-in functions
numbers = [1, 2, 3, 4, 5]
print(f"Sum: {sum(numbers)}")
print(f"Max: {max(numbers)}")
print(f"Min: {min(numbers)}")
EOF

run_test "Mathematical Operations" \
    "test_math_ops.py" \
    "Addition: 13
Subtraction: 7
Multiplication: 30
Division: 3.3333333333333335
Modulo: 1
Power: 100
Square root: 3.1622776601683795
Sum: 15
Max: 5
Min: 1" \
    "Mathematical operations and built-in functions"

# Test 8: String Operations
cat > test_strings.py << 'EOF'
name = "Fluxus"
version = "1.0"

# String concatenation
full_name = name + " " + version
print(full_name)

# String methods
print(f"Upper: {name.upper()}")
print(f"Lower: {name.lower()}")
print(f"Length: {len(name)}")

# String formatting
print(f"Welcome to {name} v{version}")
EOF

run_test "String Operations" \
    "test_strings.py" \
    "Fluxus 1.0
Upper: FLUXUS
Lower: fluxus
Length: 6
Welcome to Fluxus v1.0" \
    "String operations and formatting"

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