#\!/bin/bash

# Comprehensive Go Test Suite for Fluxus Compiler
echo "=== Comprehensive Go Test Suite ==="
echo "Testing Go code compilation and execution through Fluxus..."
echo

FLUXUS="dist-newstyle/build/x86_64-linux/ghc-9.6.7/fluxus-0.1.0.0/x/fluxus/build/fluxus/fluxus"
PASSED=0
FAILED=0
TOTAL=0

# Function to run a test
run_test() {
    local test_name="$1"
    local source_file="$2"
    local expected_output="$3"
    local description="$4"
    
    echo "--- Test: $test_name ---"
    echo "Description: $description"
    echo "Source: $source_file"
    
    TOTAL=$((TOTAL + 1))
    
    # Clean up previous executable
    rm -f "test_${test_name}"
    
    # Compile
    echo "Compiling..."
    if \! $FLUXUS --go "$source_file" -o "test_${test_name}" 2>/dev/null; then
        echo "❌ COMPILATION FAILED"
        FAILED=$((FAILED + 1))
        echo
        return 1
    fi
    
    # Run and capture output
    echo "Running..."
    if [ -x "test_${test_name}" ]; then
        actual_output=$(./test_${test_name} 2>&1)
        exit_code=$?
        
        if [ "$exit_code" -eq 0 ]; then
            if [ "$expected_output" = "$actual_output" ]; then
                echo "✅ PASSED"
                echo "Expected: $expected_output"
                echo "Actual: $actual_output"
                PASSED=$((PASSED + 1))
            else
                echo "❌ OUTPUT MISMATCH"
                echo "Expected: '$expected_output'"
                echo "Actual: '$actual_output'"
                FAILED=$((FAILED + 1))
            fi
        else
            echo "❌ RUNTIME ERROR (exit code: $exit_code)"
            echo "Output: $actual_output"
            FAILED=$((FAILED + 1))
        fi
    else
        echo "❌ EXECUTABLE NOT CREATED"
        FAILED=$((FAILED + 1))
    fi
    
    echo
}

# Test 1: Very basic Go program (empty main)
cat > test_go_empty.go << 'TESTEOF'
package main

func main() {
}
TESTEOF

run_test "go_empty" "test_go_empty.go" "" "Empty Go main function"

# Test 2: Simple variable assignment
cat > test_go_var.go << 'INNER_EOF'
package main

func main() {
    x := 42
}
INNER_EOF

run_test "go_var" "test_go_var.go" "" "Simple variable assignment"

# Test 3: Basic print without import
cat > test_go_print_basic.go << 'INNER_EOF'
package main

func main() {
    println("Hello World")
}
INNER_EOF

run_test "go_print_basic" "test_go_print_basic.go" "Hello World" "Basic println without import"

# Test 4: Simple arithmetic
cat > test_go_math.go << 'INNER_EOF'
package main

func main() {
    x := 5
    y := 3
    z := x + y
    println(z)
}
INNER_EOF

run_test "go_math" "test_go_math.go" "8" "Simple arithmetic operations"

# Test 5: Simple function call
cat > test_go_func.go << 'INNER_EOF'
package main

func add(a int, b int) int {
    return a + b
}

func main() {
    result := add(5, 3)
    println(result)
}
INNER_EOF

run_test "go_func" "test_go_func.go" "8" "Simple function call"

# Test 6: If statement
cat > test_go_if.go << 'INNER_EOF'
package main

func main() {
    x := 10
    if x > 5 {
        println("big")
    } else {
        println("small")
    }
}
INNER_EOF

run_test "go_if" "test_go_if.go" "big" "If-else statement"

# Test 7: For loop
cat > test_go_for.go << 'INNER_EOF'
package main

func main() {
    for i := 0; i < 3; i++ {
        println(i)
    }
}
INNER_EOF

run_test "go_for" "test_go_for.go" "0
1
2" "For loop"

# Test 8: Fibonacci function
cat > test_go_fibonacci.go << 'INNER_EOF'
package main

func fibonacci(n int) int {
    if n <= 1 {
        return n
    }
    return fibonacci(n-1) + fibonacci(n-2)
}

func main() {
    println(fibonacci(5))
}
INNER_EOF

run_test "go_fibonacci" "test_go_fibonacci.go" "5" "Recursive fibonacci function"

# Test 9: Multiple variable declarations
cat > test_go_multi_var.go << 'INNER_EOF'
package main

func main() {
    var a int = 10
    var b int = 20
    c := a + b
    println(c)
}
INNER_EOF

run_test "go_multi_var" "test_go_multi_var.go" "30" "Multiple variable declarations"

# Test 10: String operations
cat > test_go_string.go << 'INNER_EOF'
package main

func main() {
    s := "Hello"
    println(s)
}
INNER_EOF

run_test "go_string" "test_go_string.go" "Hello" "String operations"

# Clean up test files
rm -f test_go_*.go test_go_*

# Summary
echo "=== Go Test Results ==="
echo "Total tests: $TOTAL"
echo "Passed: $PASSED"
echo "Failed: $FAILED"
if [ $TOTAL -gt 0 ]; then
    success_rate=$(echo "scale=2; $PASSED * 100 / $TOTAL" | bc 2>/dev/null || echo "$PASSED/$TOTAL")
    echo "Success rate: $success_rate%"
else
    echo "Success rate: 0%"
fi
echo

if [ $FAILED -eq 0 ]; then
    echo "🎉 All Go tests passed\!"
    exit 0
else
    echo "⚠️  Some Go tests failed. Check the output above for details."
    exit 1
fi
EOF < /dev/null
