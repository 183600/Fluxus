#\!/bin/bash

# Comprehensive test suite for Fluxus compiler
echo "=== Fluxus Compiler Test Suite ==="

FLUXUS="dist-newstyle/build/x86_64-linux/ghc-9.6.7/fluxus-0.1.0.0/x/fluxus/build/fluxus/fluxus"
PASSED=0
TOTAL=0

test_compilation() {
    local test_name="$1"
    local source_file="$2"
    local expected_output="$3"
    
    echo "Testing: $test_name"
    ((TOTAL++))
    
    # Clean up previous files
    rm -f "${source_file%.*}_compiled" "${source_file%.*}.cpp"
    
    # Compile
    if $FLUXUS "$source_file" -o "${source_file%.*}_compiled" 2>/dev/null; then
        # Run and check output
        if [ -x "${source_file%.*}_compiled" ]; then
            actual_output=$(./"${source_file%.*}_compiled" 2>&1)
            if [ "$actual_output" = "$expected_output" ]; then
                echo "✅ PASS: $test_name"
                ((PASSED++))
            else
                echo "❌ FAIL: $test_name (wrong output)"
                echo "Expected: '$expected_output'"
                echo "Actual:   '$actual_output'"
            fi
        else
            echo "❌ FAIL: $test_name (not executable)"
        fi
    else
        echo "❌ FAIL: $test_name (compilation failed)"
    fi
    echo
}

# Create test files

# Test 1: Basic Python - simple print
cat > test_py_basic.py << 'PYEOF'
print(42)
PYEOF

# Test 2: Python fibonacci
cat > test_py_fib.py << 'PYEOF'
def fibonacci(n):
    if n <= 1:
        return n
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)

def main():
    for i in range(5):
        result = fibonacci(i)
        print(f"fib({i}) = {result}")

if __name__ == "__main__":
    main()
PYEOF

# Test 3: Basic Go - empty main
cat > test_go_basic.go << 'GOEOF'
package main

func main() {
}
GOEOF

# Test 4: Go with simple print
cat > test_go_print.go << 'GOEOF'
package main

import "fmt"

func main() {
    fmt.Println("Hello from Go\!")
}
GOEOF

# Test 5: Go with function calls
cat > test_go_func.go << 'GOEOF'
package main

import "fmt"

func add(a int, b int) int {
    return a + b
}

func main() {
    result := add(5, 3)
    fmt.Println(result)
}
GOEOF

# Test 6: Go simple arithmetic
cat > test_go_math.go << 'GOEOF'
package main

import "fmt"

func main() {
    x := 10
    y := 5
    fmt.Printf("x + y = %d\n", x + y)
}
GOEOF

# Run tests
echo "Building Fluxus..."
cabal build -v0

echo "Running tests..."
echo

test_compilation "Python basic print" "test_py_basic.py" "42"

test_compilation "Python fibonacci" "test_py_fib.py" "fib(0) = 0
fib(1) = 1
fib(2) = 1
fib(3) = 2
fib(4) = 3"

test_compilation "Go basic empty main" "test_go_basic.go" ""

test_compilation "Go simple print" "test_go_print.go" "Hello from Go\!"

test_compilation "Go function calls" "test_go_func.go" "8"

test_compilation "Go simple math" "test_go_math.go" "x + y = 15"

# Summary
echo "=== Test Results ==="
echo "Passed: $PASSED/$TOTAL tests"

if [ $PASSED -eq $TOTAL ]; then
    echo "🎉 All tests passed\!"
    exit 0
else
    echo "⚠️ Some tests failed"
    exit 1
fi
EOF < /dev/null
