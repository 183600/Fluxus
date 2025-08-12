#!/bin/bash

# Comprehensive Test Script for Fluxus Compiler
# Tests both Go and Python language features

set -e

echo "=== Comprehensive Fluxus Compiler Test Script ==="

# Set up variables
FLUXUS="dist-newstyle/build/x86_64-linux/ghc-9.6.7/fluxus-0.1.0.0/x/fluxus/build/fluxus/fluxus"
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Create a temporary directory for test files
TEST_DIR="fluxus_test_tmp"
mkdir -p "$TEST_DIR"
cd "$TEST_DIR"

# Function to run a test
run_test() {
    local test_name="$1"
    local source_file="$2"
    local expected_output="$3"
    local test_description="$4"
    local language_flag="$5"  # --go for Go, empty for Python
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo "Test $TOTAL_TESTS: $test_name - $test_description"
    echo "Source: $source_file"
    
    # Create output filename
    local output_file="${test_name}_compiled"
    
    # Remove old output file if exists
    rm -f "$output_file"
    
    # Compile
    if ../"$FLUXUS" $language_flag "$source_file" -o "$output_file" 2>/dev/null; then
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

# === GO TESTS ===

# Test 1: Basic Go types
cat > test_go_basic_types.go << 'EOF'
package main

import "fmt"

func main() {
    var intVar int = 42
    var stringVar string = "Hello"
    var boolVar bool = true
    inferred := "inferred"
    
    fmt.Printf("int: %d, string: %s, bool: %t, inferred: %s\n", intVar, stringVar, boolVar, inferred)
}
EOF

# Test 2: Go control flow
cat > test_go_control_flow.go << 'EOF'
package main

import "fmt"

func main() {
    x := 5
    
    // If statement
    if x > 3 {
        fmt.Print("greater ")
    } else {
        fmt.Print("not greater ")
    }
    
    // For loop
    for i := 1; i <= 3; i++ {
        fmt.Printf("%d ", i)
    }
    
    fmt.Println()
}
EOF

# Test 3: Go functions
cat > test_go_functions.go << 'EOF'
package main

import "fmt"

func add(a, b int) int {
    return a + b
}

func main() {
    result := add(3, 4)
    fmt.Println(result)
}
EOF

# Test 4: Go arrays and slices
cat > test_go_arrays.go << 'EOF'
package main

import "fmt"

func main() {
    arr := [3]int{1, 2, 3}
    slice := []int{4, 5, 6}
    
    fmt.Printf("arr[0]: %d, slice[0]: %d, slice len: %d\n", arr[0], slice[0], len(slice))
}
EOF

# Test 5: Go maps
cat > test_go_maps.go << 'EOF'
package main

import "fmt"

func main() {
    m := map[string]int{
        "one": 1,
        "two": 2,
    }
    
    fmt.Printf("m[one]: %d, m[two]: %d\n", m["one"], m["two"])
}
EOF

# Test 6: Go pointers
cat > test_go_pointers.go << 'EOF'
package main

import "fmt"

func main() {
    x := 42
    p := &x
    *p = 43
    
    fmt.Printf("x: %d, *p: %d\n", x, *p)
}
EOF

# Test 7: Go structs
cat > test_go_structs.go << 'EOF'
package main

import "fmt"

type Person struct {
    Name string
    Age  int
}

func main() {
    p := Person{Name: "Alice", Age: 30}
    fmt.Printf("Person: %s, Age: %d\n", p.Name, p.Age)
}
EOF

# === PYTHON TESTS ===

# Test 1: Basic Python types
cat > test_py_basic_types.py << 'EOF'
x = 42
s = "Hello"
b = True
inferred = "inferred"

print(f"int: {x}, string: {s}, bool: {b}, inferred: {inferred}")
EOF

# Test 2: Python control flow
cat > test_py_control_flow.py << 'EOF'
x = 5

# If statement
if x > 3:
    print("greater", end=" ")
else:
    print("not greater", end=" ")

# For loop
for i in range(1, 4):
    print(i, end=" ")

print()
EOF

# Test 3: Python functions
cat > test_py_functions.py << 'EOF'
def add(a, b):
    return a + b

result = add(3, 4)
print(result)
EOF

# Test 4: Python lists
cat > test_py_lists.py << 'EOF'
arr = [1, 2, 3]
lst = [4, 5, 6]

print(f"arr[0]: {arr[0]}, lst[0]: {lst[0]}, lst len: {len(lst)}")
EOF

# Test 5: Python dictionaries
cat > test_py_dicts.py << 'EOF'
d = {
    "one": 1,
    "two": 2,
}

print(f"d[one]: {d['one']}, d[two]: {d['two']}")
EOF

# Test 6: Python classes
cat > test_py_classes.py << 'EOF'
class Person:
    def __init__(self, name, age):
        self.name = name
        self.age = age

p = Person("Alice", 30)
print(f"Person: {p.name}, Age: {p.age}")
EOF

# Test 7: Python list comprehensions
cat > test_py_comprehensions.py << 'EOF'
numbers = [1, 2, 3]
squares = [x**2 for x in numbers]
print(squares)
EOF

# Run Go tests
echo ""
echo "=== Running Go Tests ==="
echo ""

run_test "go_basic_types" "test_go_basic_types.go" "int: 42, string: Hello, bool: true, inferred: inferred" "Basic types and variable declarations" "--go"
run_test "go_control_flow" "test_go_control_flow.go" "greater 1 2 3 " "If statements and for loops" "--go"
run_test "go_functions" "test_go_functions.go" "7" "Function definitions and calls" "--go"
run_test "go_arrays" "test_go_arrays.go" "arr[0]: 1, slice[0]: 4, slice len: 3" "Arrays and slices" "--go"
run_test "go_maps" "test_go_maps.go" "m[one]: 1, m[two]: 2" "Maps" "--go"
run_test "go_pointers" "test_go_pointers.go" "x: 43, *p: 43" "Pointers" "--go"
run_test "go_structs" "test_go_structs.go" "Person: Alice, Age: 30" "Structs" "--go"

# Run Python tests
echo ""
echo "=== Running Python Tests ==="
echo ""

run_test "py_basic_types" "test_py_basic_types.py" "int: 42, string: Hello, bool: True, inferred: inferred" "Basic types and variable declarations" ""
run_test "py_control_flow" "test_py_control_flow.py" "greater 1 2 3 " "If statements and for loops" ""
run_test "py_functions" "test_py_functions.py" "7" "Function definitions and calls" ""
run_test "py_lists" "test_py_lists.py" "arr[0]: 1, lst[0]: 4, lst len: 3" "Lists" ""
run_test "py_dicts" "test_py_dicts.py" "d[one]: 1, d[two]: 2" "Dictionaries" ""
run_test "py_classes" "test_py_classes.py" "Person: Alice, Age: 30" "Classes" ""
run_test "py_comprehensions" "test_py_comprehensions.py" "[1, 4, 9]" "List comprehensions" ""

# Clean up test files
echo "Cleaning up test files..."
cd ..
rm -rf "$TEST_DIR"

# Print summary
echo ""
echo "=== COMPREHENSIVE TEST SUMMARY ==="
echo "Total tests: $TOTAL_TESTS"
echo "Passed: $PASSED_TESTS"
echo "Failed: $FAILED_TESTS"
echo "Success rate: $(( PASSED_TESTS * 100 / TOTAL_TESTS ))%"

if [ $FAILED_TESTS -eq 0 ]; then
    echo "🎉 All tests passed!"
    exit 0
else
    echo "❌ Some tests failed"
    exit 1
fi