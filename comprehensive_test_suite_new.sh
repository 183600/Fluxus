#!/bin/bash

# Comprehensive Test Suite for Fluxus Compiler
# Tests Go and Python code compilation and execution

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Fluxus compiler path
FLUXUS="dist-newstyle/build/x86_64-linux/ghc-9.6.7/fluxus-0.1.0.0/x/fluxus/build/fluxus/fluxus"

# Check if compiler exists
if [ ! -f "$FLUXUS" ]; then
    echo -e "${RED}Error: Fluxus compiler not found at $FLUXUS${NC}"
    echo "Please build the compiler first with: cabal build"
    exit 1
fi

echo -e "${BLUE}=== Fluxus Compiler Comprehensive Test Suite ===${NC}"
echo "Testing both Go and Python code compilation and execution"
echo

# Function to run a test
run_test() {
    local test_name="$1"
    local source_file="$2"
    local expected_output="$3"
    local language="$4"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo -e "${YELLOW}Running test: $test_name${NC}"
    
    # Determine output filename
    local output_file="${source_file%.*}_compiled"
    
    # Compile the source file
    if [ "$language" = "go" ]; then
        if $FLUXUS --go "$source_file" -o "$output_file" 2>/dev/null; then
            if [ -f "$output_file" ]; then
                # Run the compiled program and capture output
                if actual_output=$(timeout 10s "./$output_file" 2>/dev/null); then
                    if [ "$actual_output" = "$expected_output" ]; then
                        echo -e "${GREEN}✓ PASS: $test_name${NC}"
                        PASSED_TESTS=$((PASSED_TESTS + 1))
                        rm -f "$output_file"
                        return 0
                    else
                        echo -e "${RED}✗ FAIL: $test_name - Output mismatch${NC}"
                        echo "  Expected: '$expected_output'"
                        echo "  Actual:   '$actual_output'"
                    fi
                else
                    echo -e "${RED}✗ FAIL: $test_name - Runtime error${NC}"
                fi
                rm -f "$output_file"
            else
                echo -e "${RED}✗ FAIL: $test_name - No executable generated${NC}"
            fi
        else
            echo -e "${RED}✗ FAIL: $test_name - Compilation failed${NC}"
        fi
    else # Python
        if $FLUXUS "$source_file" -o "$output_file" 2>/dev/null; then
            if [ -f "$output_file" ]; then
                # Run the compiled program and capture output
                if actual_output=$(timeout 10s "./$output_file" 2>/dev/null); then
                    if [ "$actual_output" = "$expected_output" ]; then
                        echo -e "${GREEN}✓ PASS: $test_name${NC}"
                        PASSED_TESTS=$((PASSED_TESTS + 1))
                        rm -f "$output_file"
                        return 0
                    else
                        echo -e "${RED}✗ FAIL: $test_name - Output mismatch${NC}"
                        echo "  Expected: '$expected_output'"
                        echo "  Actual:   '$actual_output'"
                    fi
                else
                    echo -e "${RED}✗ FAIL: $test_name - Runtime error${NC}"
                fi
                rm -f "$output_file"
            else
                echo -e "${RED}✗ FAIL: $test_name - No executable generated${NC}"
            fi
        else
            echo -e "${RED}✗ FAIL: $test_name - Compilation failed${NC}"
        fi
    fi
    
    FAILED_TESTS=$((FAILED_TESTS + 1))
    return 1
}

# Create test directories
mkdir -p test_cases_comprehensive/{go,python}

echo -e "${BLUE}=== Creating Go Test Cases ===${NC}"

# Go Test 1: Basic Hello World
cat > test_cases_comprehensive/go/hello.go << 'EOF'
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}
EOF

# Go Test 2: Simple Variables
cat > test_cases_comprehensive/go/variables.go << 'EOF'
package main

import "fmt"

func main() {
    x := 42
    y := 3.14
    fmt.Println(x)
    fmt.Println(y)
}
EOF

# Go Test 3: Basic Arithmetic
cat > test_cases_comprehensive/go/arithmetic.go << 'EOF'
package main

import "fmt"

func main() {
    a := 10
    b := 5
    fmt.Println(a + b)
    fmt.Println(a - b)
    fmt.Println(a * b)
    fmt.Println(a / b)
}
EOF

# Go Test 4: Simple Function
cat > test_cases_comprehensive/go/function.go << 'EOF'
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

# Go Test 5: For Loop
cat > test_cases_comprehensive/go/loop.go << 'EOF'
package main

import "fmt"

func main() {
    for i := 1; i <= 3; i++ {
        fmt.Println(i)
    }
}
EOF

# Go Test 6: If Statement
cat > test_cases_comprehensive/go/conditional.go << 'EOF'
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

echo -e "${BLUE}=== Creating Python Test Cases ===${NC}"

# Python Test 1: Basic Hello World
cat > test_cases_comprehensive/python/hello.py << 'EOF'
print("Hello, World!")
EOF

# Python Test 2: Simple Variables
cat > test_cases_comprehensive/python/variables.py << 'EOF'
x = 42
y = 3.14
print(x)
print(y)
EOF

# Python Test 3: Basic Arithmetic
cat > test_cases_comprehensive/python/arithmetic.py << 'EOF'
a = 10
b = 5
print(a + b)
print(a - b)
print(a * b)
print(a // b)
EOF

# Python Test 4: Simple Function
cat > test_cases_comprehensive/python/function.py << 'EOF'
def add(a, b):
    return a + b

result = add(3, 4)
print(result)
EOF

# Python Test 5: For Loop
cat > test_cases_comprehensive/python/loop.py << 'EOF'
for i in range(1, 4):
    print(i)
EOF

# Python Test 6: If Statement
cat > test_cases_comprehensive/python/conditional.py << 'EOF'
x = 10
if x > 5:
    print("greater")
else:
    print("smaller")
EOF

echo -e "${BLUE}=== Running Go Tests ===${NC}"

# Run Go tests
run_test "Go Hello World" "test_cases_comprehensive/go/hello.go" "Hello, World!" "go"
run_test "Go Variables" "test_cases_comprehensive/go/variables.go" "42
3.14" "go"
run_test "Go Arithmetic" "test_cases_comprehensive/go/arithmetic.go" "15
5
50
2" "go"
run_test "Go Function" "test_cases_comprehensive/go/function.go" "7" "go"
run_test "Go For Loop" "test_cases_comprehensive/go/loop.go" "1
2
3" "go"
run_test "Go Conditional" "test_cases_comprehensive/go/conditional.go" "greater" "go"

echo
echo -e "${BLUE}=== Running Python Tests ===${NC}"

# Run Python tests
run_test "Python Hello World" "test_cases_comprehensive/python/hello.py" "Hello, World!" "python"
run_test "Python Variables" "test_cases_comprehensive/python/variables.py" "42
3.14" "python"
run_test "Python Arithmetic" "test_cases_comprehensive/python/arithmetic.py" "15
5
50
2" "python"
run_test "Python Function" "test_cases_comprehensive/python/function.py" "7" "python"
run_test "Python For Loop" "test_cases_comprehensive/python/loop.py" "1
2
3" "python"
run_test "Python Conditional" "test_cases_comprehensive/python/conditional.py" "greater" "python"

echo
echo -e "${BLUE}=== Test Summary ===${NC}"
echo "Total tests: $TOTAL_TESTS"
echo -e "Passed: ${GREEN}$PASSED_TESTS${NC}"
echo -e "Failed: ${RED}$FAILED_TESTS${NC}"

PASS_RATE=$((PASSED_TESTS * 100 / TOTAL_TESTS))
echo "Pass rate: $PASS_RATE%"

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "${GREEN}🎉 All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}❌ Some tests failed. Need to fix compiler issues.${NC}"
    exit 1
fi