#!/bin/bash

# Comprehensive Python Test Suite for Fluxus Compiler
set -e

FLUXUS="dist-newstyle/build/x86_64-linux/ghc-9.6.7/fluxus-0.1.0.0/x/fluxus/build/fluxus/fluxus"
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

echo "=== Comprehensive Python Test Suite for Fluxus Compiler ==="
echo "Using Fluxus: $FLUXUS"
echo ""

# Function to run a single test
run_python_test() {
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
    if ./"$FLUXUS" "$source_file" -o "$output_file" 2>/dev/null; then
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

# Test 1: Very Basic Python - Simple print
cat > test_py_basic.py << 'EOF'
print(42)
EOF

# Test 2: String print
cat > test_py_string.py << 'EOF'
print("Hello, World!")
EOF

# Test 3: Basic arithmetic
cat > test_py_arithmetic.py << 'EOF'
a = 5
b = 3
print(a + b)
EOF

# Test 4: Function definition and call
cat > test_py_function.py << 'EOF'
def add(x, y):
    return x + y

result = add(5, 3)
print(result)
EOF

# Test 5: For loop
cat > test_py_loop.py << 'EOF'
for i in range(1, 4):
    print(i)
EOF

# Test 6: If statement
cat > test_py_if.py << 'EOF'
x = 5
if x > 3:
    print("greater")
else:
    print("not greater")
EOF

# Test 7: Multiple variable assignments
cat > test_py_variables.py << 'EOF'
x = 10
y = 20
z = x + y
print(z)
EOF

# Test 8: String operations
cat > test_py_strings.py << 'EOF'
name = "Python"
print("Hello " + name)
EOF

# Test 9: Fibonacci function
cat > test_py_fibonacci.py << 'EOF'
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

print(fibonacci(5))
EOF

# Test 10: List operations
cat > test_py_list.py << 'EOF'
numbers = [1, 2, 3]
total = 0
for num in numbers:
    total += num
print(total)
EOF

# Test 11: Main function pattern
cat > test_py_main.py << 'EOF'
def main():
    print("Hello from main")

if __name__ == "__main__":
    main()
EOF

# Test 12: Multiple functions
cat > test_py_multi_func.py << 'EOF'
def multiply(a, b):
    return a * b

def square(x):
    return multiply(x, x)

print(square(4))
EOF

echo "Running Python tests..."
echo ""

# Run all tests
run_python_test "py_basic" "test_py_basic.py" "42" "Simple integer print"
run_python_test "py_string" "test_py_string.py" "Hello, World!" "Simple string print"
run_python_test "py_arithmetic" "test_py_arithmetic.py" "8" "Basic arithmetic operations"
run_python_test "py_function" "test_py_function.py" "8" "Function definition and call"
run_python_test "py_loop" "test_py_loop.py" "1
2
3" "For loop iteration"
run_python_test "py_if" "test_py_if.py" "greater" "Conditional statement"
run_python_test "py_variables" "test_py_variables.py" "30" "Multiple variable assignments"
run_python_test "py_strings" "test_py_strings.py" "Hello Python" "String concatenation"
run_python_test "py_fibonacci" "test_py_fibonacci.py" "5" "Recursive fibonacci function"
run_python_test "py_list" "test_py_list.py" "6" "List operations and iteration"
run_python_test "py_main" "test_py_main.py" "Hello from main" "Main function pattern"
run_python_test "py_multi_func" "test_py_multi_func.py" "16" "Multiple function definitions"

# Clean up test files
echo "Cleaning up test files..."
rm -f test_py_*.py

# Print summary
echo "=== PYTHON TEST SUMMARY ==="
echo "Total tests: $TOTAL_TESTS"
echo "Passed: $PASSED_TESTS"
echo "Failed: $FAILED_TESTS"
echo "Success rate: $(( PASSED_TESTS * 100 / TOTAL_TESTS ))%"

if [ $FAILED_TESTS -eq 0 ]; then
    echo "🎉 All Python tests passed!"
    exit 0
else
    echo "❌ Some Python tests failed"
    exit 1
fi