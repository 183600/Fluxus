#!/bin/bash

# Comprehensive Test Suite for Fluxus Compiler
# Tests both Go and Python code compilation and execution

FLUXUS="./dist-newstyle/build/x86_64-linux/ghc-9.6.7/fluxus-0.1.0.0/x/fluxus/build/fluxus/fluxus"
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
TEST_RESULTS=()

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test function
run_test() {
    local test_name="$1"
    local source_file="$2"
    local expected_output="$3"
    local test_type="$4"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    echo -e "${BLUE}Testing: $test_name${NC}"
    
    # Compile
    if ! $FLUXUS "$source_file" -o "test_${test_name}" 2>/dev/null; then
        echo -e "${RED}FAIL: Compilation failed for $test_name${NC}"
        TEST_RESULTS+=("FAIL: $test_name - Compilation failed")
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
    
    # Execute and capture output  
    if [ -f "test_${test_name}" ]; then
        actual_output=$(./test_${test_name} 2>&1)
        exit_code=$?
        
        if [ $exit_code -eq 0 ]; then
            if [ "$actual_output" = "$expected_output" ]; then
                echo -e "${GREEN}PASS: $test_name${NC}"
                TEST_RESULTS+=("PASS: $test_name")
                PASSED_TESTS=$((PASSED_TESTS + 1))
                return 0
            else
                echo -e "${RED}FAIL: $test_name - Output mismatch${NC}"
                echo -e "${YELLOW}Expected: '$expected_output'${NC}"
                echo -e "${YELLOW}Got: '$actual_output'${NC}"
                TEST_RESULTS+=("FAIL: $test_name - Output mismatch")
                FAILED_TESTS=$((FAILED_TESTS + 1))
                return 1
            fi
        else
            echo -e "${RED}FAIL: $test_name - Runtime error (exit code: $exit_code)${NC}"
            echo -e "${YELLOW}Output: '$actual_output'${NC}"
            TEST_RESULTS+=("FAIL: $test_name - Runtime error")
            FAILED_TESTS=$((FAILED_TESTS + 1))
            return 1
        fi
    else
        echo -e "${RED}FAIL: $test_name - Executable not created${NC}"
        TEST_RESULTS+=("FAIL: $test_name - Executable not created")
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
}

# Clean up function
cleanup() {
    rm -f test_* temp_test_*
}

echo "=== Comprehensive Fluxus Test Suite ==="
echo "Cleaning up previous test files..."
cleanup

# Create temporary test files
mkdir -p temp_tests

echo "=== BASIC TESTS ==="

# Test 1: Go Empty Main
cat > temp_tests/go_empty.go << 'EOF'
package main

func main() {
}
EOF
run_test "go_empty" "temp_tests/go_empty.go" "" "go"

# Test 2: Python Simple Print
cat > temp_tests/py_simple_print.py << 'EOF'
print(42)
EOF
run_test "py_simple_print" "temp_tests/py_simple_print.py" "42" "python"

# Test 3: Go Simple Print
cat > temp_tests/go_simple_print.go << 'EOF'
package main

import "fmt"

func main() {
    fmt.Println(42)
}
EOF
run_test "go_simple_print" "temp_tests/go_simple_print.go" "42" "go"

# Test 4: Python Simple Math
cat > temp_tests/py_simple_math.py << 'EOF'
result = 5 + 3
print(result)
EOF
run_test "py_simple_math" "temp_tests/py_simple_math.py" "8" "python"

# Test 5: Go Simple Math
cat > temp_tests/go_simple_math.go << 'EOF'
package main

import "fmt"

func main() {
    result := 5 + 3
    fmt.Println(result)
}
EOF
run_test "go_simple_math" "temp_tests/go_simple_math.go" "8" "go"

echo ""
echo "=== FUNCTION TESTS ==="

# Test 6: Python Simple Function
cat > temp_tests/py_simple_func.py << 'EOF'
def add(a, b):
    return a + b

result = add(3, 4)
print(result)
EOF
run_test "py_simple_func" "temp_tests/py_simple_func.py" "7" "python"

# Test 7: Go Simple Function
cat > temp_tests/go_simple_func.go << 'EOF'
package main

import "fmt"

func add(a int, b int) int {
    return a + b
}

func main() {
    result := add(3, 4)
    fmt.Println(result)
}
EOF
run_test "go_simple_func" "temp_tests/go_simple_func.go" "7" "go"

echo ""
echo "=== CONTROL FLOW TESTS ==="

# Test 8: Python If Statement
cat > temp_tests/py_if.py << 'EOF'
x = 10
if x > 5:
    print("greater")
else:
    print("smaller")
EOF
run_test "py_if" "temp_tests/py_if.py" "greater" "python"

# Test 9: Go If Statement  
cat > temp_tests/go_if.go << 'EOF'
package main

import "fmt"

func main() {
    x := 10
    if x > 5 {
        fmt.Println("greater")
    } else {
        fmt.Println("smaller")
    }
}
EOF
run_test "go_if" "temp_tests/go_if.go" "greater" "go"

# Test 10: Python For Loop
cat > temp_tests/py_for.py << 'EOF'
for i in range(3):
    print(i)
EOF
run_test "py_for" "temp_tests/py_for.py" "0
1
2" "python"

# Test 11: Go For Loop
cat > temp_tests/go_for.go << 'EOF'
package main

import "fmt"

func main() {
    for i := 0; i < 3; i++ {
        fmt.Println(i)
    }
}
EOF
run_test "go_for" "temp_tests/go_for.go" "0
1
2" "go"

echo ""
echo "=== FIBONACCI TESTS ==="

# Test 12: Python Fibonacci (Simple)
cat > temp_tests/py_fib_simple.py << 'EOF'
def fib(n):
    if n <= 1:
        return n
    return fib(n-1) + fib(n-2)

print(fib(5))
EOF
run_test "py_fib_simple" "temp_tests/py_fib_simple.py" "5" "python"

# Test 13: Go Fibonacci (Simple)
cat > temp_tests/go_fib_simple.go << 'EOF'
package main

import "fmt"

func fib(n int) int {
    if n <= 1 {
        return n
    }
    return fib(n-1) + fib(n-2)
}

func main() {
    fmt.Println(fib(5))
}
EOF
run_test "go_fib_simple" "temp_tests/go_fib_simple.go" "5" "go"

echo ""
echo "=== STRING TESTS ==="

# Test 14: Python String Operations
cat > temp_tests/py_strings.py << 'EOF'
name = "World"
message = "Hello " + name
print(message)
EOF
run_test "py_strings" "temp_tests/py_strings.py" "Hello World" "python"

# Test 15: Go String Operations
cat > temp_tests/go_strings.go << 'EOF'
package main

import "fmt"

func main() {
    name := "World"
    message := "Hello " + name
    fmt.Println(message)
}
EOF
run_test "go_strings" "temp_tests/go_strings.go" "Hello World" "go"

echo ""
echo "=== VARIABLE TESTS ==="

# Test 16: Python Multiple Variables
cat > temp_tests/py_vars.py << 'EOF'
a = 5
b = 10
c = a * b
print(c)
EOF
run_test "py_vars" "temp_tests/py_vars.py" "50" "python"

# Test 17: Go Multiple Variables
cat > temp_tests/go_vars.go << 'EOF'
package main

import "fmt"

func main() {
    a := 5
    b := 10
    c := a * b
    fmt.Println(c)
}
EOF
run_test "go_vars" "temp_tests/go_vars.go" "50" "go"

echo ""
echo "=== COMPREHENSIVE RESULTS ==="
echo "Total Tests: $TOTAL_TESTS"
echo "Passed: $PASSED_TESTS"
echo "Failed: $FAILED_TESTS"

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "${GREEN}All tests passed! 🎉${NC}"
else
    echo -e "${RED}Some tests failed. Details:${NC}"
    for result in "${TEST_RESULTS[@]}"; do
        if [[ $result == FAIL* ]]; then
            echo -e "${RED}$result${NC}"
        fi
    done
fi

echo ""
echo "Pass rate: $(( PASSED_TESTS * 100 / TOTAL_TESTS ))%"

# Cleanup
cleanup
rm -rf temp_tests

exit $FAILED_TESTS