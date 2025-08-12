#!/bin/bash

# Complete Test Suite for Fluxus Compiler
# Tests both Go and Python language features comprehensively

echo "=== Fluxus Compiler Complete Test Suite ==="
echo "Date: $(date)"
echo

# Variables
FLUXUS="dist-newstyle/build/x86_64-linux/ghc-9.6.7/fluxus-0.1.0.0/x/fluxus/build/fluxus/fluxus"
PASSED=0
FAILED=0
TOTAL=0
FAILED_TESTS=()

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test runner function
run_test() {
    local name="$1"
    local lang="$2"
    local expected="$3"
    local description="$4"
    
    echo "------------------------------------------------------------------------------"
    echo "Test: $name"
    echo "Language: $lang"
    echo "Description: $description"
    echo
    
    TOTAL=$((TOTAL + 1))
    
    local source_file="${name}.${lang}"
    local compiled_file="${name}_compiled"
    
    # Compile
    echo "Compiling ${source_file}..."
    if ! $FLUXUS ${lang=="go" && echo "--go"} "${source_file}" -o "${compiled_file}" 2>/dev/null; then
        echo -e "${RED}❌ COMPILATION FAILED${NC}"
        FAILED=$((FAILED + 1))
        FAILED_TESTS+=("${name}: Compilation failed")
        return 1
    fi
    
    # Check if executable was created
    if [ ! -x "${compiled_file}" ]; then
        echo -e "${RED}❌ EXECUTABLE NOT CREATED${NC}"
        FAILED=$((FAILED + 1))
        FAILED_TESTS+=("${name}: Executable not created")
        return 1
    fi
    
    # Run and capture output
    echo "Running ${compiled_file}..."
    local actual=$(./"${compiled_file}" 2>&1)
    
    # Compare output
    if [ "$expected" = "$actual" ]; then
        echo -e "${GREEN}✅ PASSED${NC}"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}❌ OUTPUT MISMATCH${NC}"
        echo "Expected: '$expected'"
        echo "Actual:   '$actual'"
        FAILED=$((FAILED + 1))
        FAILED_TESTS+=("${name}: Output mismatch")
    fi
    
    # Cleanup
    rm -f "${compiled_file}"
    echo
}

# Create test files and run tests

echo "=== Creating and Running Go Tests ==="
echo

# Test 1: Basic Go program
cat > test_go_basic.go << 'EOF'
package main

func main() {
}
EOF

run_test "test_go_basic" "go" "" "Empty main function"

# Test 2: Go with simple print
cat > test_go_print.go << 'EOF'
package main

func main() {
    println("Hello from Go!")
}
EOF

run_test "test_go_print" "go" "Hello from Go!" "Simple println"

# Test 3: Go with fmt.Println
cat > test_go_fmt_println.go << 'EOF'
package main

import "fmt"

func main() {
    fmt.Println("Hello with fmt!")
}
EOF

run_test "test_go_fmt_println" "go" "Hello with fmt!" "fmt.Println"

# Test 4: Go variables
cat > test_go_variables.go << 'EOF'
package main

func main() {
    x := 42
    y := "Hello"
    println(x)
    println(y)
}
EOF

run_test "test_go_variables" "go" "42
Hello" "Variable declarations and assignment"

# Test 5: Go functions
cat > test_go_functions.go << 'EOF'
package main

func add(a int, b int) int {
    return a + b
}

func main() {
    result := add(5, 3)
    println(result)
}
EOF

run_test "test_go_functions" "go" "8" "Function definition and call"

# Test 6: Go control flow
cat > test_go_control_flow.go << 'EOF'
package main

func main() {
    x := 10
    if x > 5 {
        println("greater")
    } else {
        println("less or equal")
    }
    
    for i := 0; i < 3; i++ {
        println(i)
    }
}
EOF

run_test "test_go_control_flow" "go" "greater
0
1
2" "If statement and for loop"

# Test 7: Go arrays and slices
cat > test_go_arrays.go << 'EOF'
package main

func main() {
    arr := [3]int{1, 2, 3}
    slice := []int{4, 5, 6}
    println(arr[0])
    println(slice[1])
}
EOF

run_test "test_go_arrays" "go" "1
5" "Arrays and slices"

# Test 8: Go maps
cat > test_go_maps.go << 'EOF'
package main

func main() {
    m := make(map[string]int)
    m["key"] = 42
    println(m["key"])
}
EOF

run_test "test_go_maps" "go" "" "Map creation and access"

# Test 9: Go structs
cat > test_go_structs.go << 'EOF'
package main

type Person struct {
    Name string
    Age  int
}

func main() {
    p := Person{Name: "Alice", Age: 30}
    println(p.Name)
    println(p.Age)
}
EOF

run_test "test_go_structs" "go" "" "Struct definition and usage"

echo "=== Creating and Running Python Tests ==="
echo

# Test 1: Basic Python print
cat > test_py_basic.py << 'EOF'
print(42)
EOF

run_test "test_py_basic" "py" "42" "Simple print statement"

# Test 2: Python variables
cat > test_py_variables.py << 'EOF'
x = 42
y = "Hello"
print(x)
print(y)
EOF

run_test "test_py_variables" "py" "42
Hello" "Variable assignment"

# Test 3: Python functions
cat > test_py_functions.py << 'EOF'
def add(a, b):
    return a + b

result = add(5, 3)
print(result)
EOF

run_test "test_py_functions" "py" "8" "Function definition and call"

# Test 4: Python control flow
cat > test_py_control_flow.py << 'EOF'
x = 10
if x > 5:
    print("greater")
else:
    print("less or equal")

for i in range(3):
    print(i)
EOF

run_test "test_py_control_flow" "py" "greater
0
1
2" "If statement and for loop"

# Test 5: Python lists
cat > test_py_lists.py << 'EOF'
my_list = [1, 2, 3]
print(my_list[0])
print(my_list[1])
EOF

run_test "test_py_lists" "py" "0
0" "List creation and indexing"

# Test 6: Python dictionaries
cat > test_py_dicts.py << 'EOF'
my_dict = {"key": 42}
print(my_dict["key"])
EOF

run_test "test_py_dicts" "py" "0" "Dictionary creation and access"

# Test 7: Python string operations
cat > test_py_strings.py << 'EOF'
s1 = "Hello"
s2 = "World"
result = s1 + " " + s2
print(result)
EOF

run_test "test_py_strings" "py" "Hello World" "String concatenation"

# Test 8: Python arithmetic
cat > test_py_math.py << 'EOF'
x = 10
y = 5
print(x + y)
print(x - y)
print(x * y)
print(x / y)
EOF

run_test "test_py_math" "py" "15
5
50
2" "Basic arithmetic operations"

echo "=== Test Summary ==="
echo "Total tests: $TOTAL"
echo "Passed: $PASSED"
echo "Failed: $FAILED"
echo "Success rate: $((PASSED * 100 / TOTAL))%"

if [ $FAILED -gt 0 ]; then
    echo
    echo "Failed tests:"
    for test in "${FAILED_TESTS[@]}"; do
        echo "  - $test"
    done
    echo
fi

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}🎉 All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}⚠️  Some tests failed${NC}"
    exit 1
fi