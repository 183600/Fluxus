#\!/bin/bash

# Comprehensive Python Test Suite for Fluxus Compiler
echo "=== Comprehensive Python Test Suite ==="
echo "Testing Python code compilation and execution through Fluxus..."
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
    if \! $FLUXUS "$source_file" -o "test_${test_name}" 2>/dev/null; then
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

# Test 1: Simple print statement
cat > test_py_basic.py << 'INNER_EOF'
print(42)
INNER_EOF

run_test "py_basic" "test_py_basic.py" "42" "Simple print statement"

# Test 2: String print
cat > test_py_string.py << 'INNER_EOF'
print("Hello World")
INNER_EOF

run_test "py_string" "test_py_string.py" "Hello World" "String print statement"

# Test 3: Variable assignment and print
cat > test_py_var.py << 'INNER_EOF'
x = 42
print(x)
INNER_EOF

run_test "py_var" "test_py_var.py" "42" "Variable assignment and print"

# Test 4: Arithmetic operations
cat > test_py_math.py << 'INNER_EOF'
x = 5
y = 3
z = x + y
print(z)
INNER_EOF

run_test "py_math" "test_py_math.py" "8" "Arithmetic operations"

# Test 5: Simple function
cat > test_py_func.py << 'INNER_EOF'
def add(a, b):
    return a + b

result = add(5, 3)
print(result)
INNER_EOF

run_test "py_func" "test_py_func.py" "8" "Simple function definition and call"

# Test 6: If statement
cat > test_py_if.py << 'INNER_EOF'
x = 10
if x > 5:
    print("big")
else:
    print("small")
INNER_EOF

run_test "py_if" "test_py_if.py" "big" "If-else statement"

# Test 7: For loop
cat > test_py_for.py << 'INNER_EOF'
for i in range(3):
    print(i)
INNER_EOF

run_test "py_for" "test_py_for.py" "0
1
2" "For loop with range"

# Test 8: Fibonacci function
cat > test_py_fibonacci.py << 'INNER_EOF'
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

print(fibonacci(5))
INNER_EOF

run_test "py_fibonacci" "test_py_fibonacci.py" "5" "Recursive fibonacci function"

# Test 9: List operations
cat > test_py_list.py << 'INNER_EOF'
numbers = [1, 2, 3, 4, 5]
print(sum(numbers))
INNER_EOF

run_test "py_list" "test_py_list.py" "15" "List operations"

# Test 10: Multiple print statements
cat > test_py_multi_print.py << 'INNER_EOF'
print("Line 1")
print("Line 2")
print("Line 3")
INNER_EOF

run_test "py_multi_print" "test_py_multi_print.py" "Line 1
Line 2
Line 3" "Multiple print statements"

# Clean up test files
rm -f test_py_*.py test_py_*

# Summary
echo "=== Python Test Results ==="
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
    echo "🎉 All Python tests passed\!"
    exit 0
else
    echo "⚠️  Some Python tests failed. Check the output above for details."
    exit 1
fi
EOF < /dev/null
