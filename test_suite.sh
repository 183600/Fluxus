#!/bin/bash

# Comprehensive test script for Fluxus
echo "=== Fluxus Comprehensive Test Suite ==="
echo

# Create test directory
mkdir -p tests
cd tests

# Test 1: Simple Python arithmetic
echo "Test 1: Python Arithmetic"
cat > test_arithmetic.py << 'EOF'
def add(a, b):
    return a + b

def main():
    result = add(5, 3)
    print("5 + 3 =")
    print(result)

if __name__ == "__main__":
    main()
EOF

../bin/fluxus --python test_arithmetic.py -o test_arithmetic_compiled
if [ $? -eq 0 ]; then
    echo "✓ Compilation successful"
    ./test_arithmetic_compiled
    echo
else
    echo "✗ Compilation failed"
    echo
fi

# Test 2: Simple Go arithmetic
echo "Test 2: Go Arithmetic"
cat > test_arithmetic.go << 'EOF'
package main

import "fmt"

func add(a int, b int) int {
    return a + b
}

func main() {
    result := add(5, 3)
    fmt.Printf("5 + 3 = %d\n", result)
}
EOF

../bin/fluxus --go test_arithmetic.go -o test_arithmetic_go_compiled
if [ $? -eq 0 ]; then
    echo "✓ Compilation successful"
    ./test_arithmetic_go_compiled
    echo
else
    echo "✗ Compilation failed"
    echo
fi

# Test 3: Simple Python function
echo "Test 3: Python Function"
cat > test_simple_func.py << 'EOF'
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

def main():
    result = fibonacci(5)
    print("fibonacci(5) =")
    print(result)

if __name__ == "__main__":
    main()
EOF

../bin/fluxus --python test_simple_func.py -o test_simple_func_compiled
if [ $? -eq 0 ]; then
    echo "✓ Compilation successful"
    ./test_simple_func_compiled
    echo
else
    echo "✗ Compilation failed"
    echo
fi

# Test 4: Simple Go function
echo "Test 4: Go Function"
cat > test_simple_func.go << 'EOF'
package main

import "fmt"

func fibonacci(n int) int {
    if n <= 1 {
        return n
    }
    return fibonacci(n-1) + fibonacci(n-2)
}

func main() {
    result := fibonacci(5)
    fmt.Printf("fibonacci(5) = %d\n", result)
}
EOF

../bin/fluxus --go test_simple_func.go -o test_simple_func_go_compiled
if [ $? -eq 0 ]; then
    echo "✓ Compilation successful"
    ./test_simple_func_go_compiled
    echo
else
    echo "✗ Compilation failed"
    echo
fi

echo "=== Test Suite Complete ==="