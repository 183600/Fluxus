#!/bin/bash

# Comprehensive compilation and testing script for fluxus
# This script will test all the generated Go and Python files

echo "=== Fluxus Compilation and Testing Suite ==="
echo "Starting comprehensive testing..."

# Test results counter
total_tests=0
passed_tests=0
failed_tests=0

# Function to test a Go file
test_go_file() {
    local file=$1
    local output=$2
    
    echo "Testing Go file: $file"
    
    # Compile the file
    if fluxus --go -O2 "$file" -o "$output" 2>/dev/null; then
        echo "✓ Compilation successful: $file"
        ((total_tests++))
        ((passed_tests++))
        
        # Test if the executable runs and produces output
        if timeout 5s "./$output" >/dev/null 2>&1; then
            echo "✓ Execution successful: $output"
        else
            echo "✗ Execution failed or no output: $output"
            ((failed_tests++))
        fi
    else
        echo "✗ Compilation failed: $file"
        ((total_tests++))
        ((failed_tests++))
    fi
    echo ""
}

# Function to test a Python file
test_python_file() {
    local file=$1
    local output=$2
    
    echo "Testing Python file: $file"
    
    # Compile the file
    if fluxus --python -O2 "$file" -o "$output" 2>/dev/null; then
        echo "✓ Compilation successful: $file"
        ((total_tests++))
        ((passed_tests++))
        
        # Test if the executable runs and produces output
        if timeout 5s "./$output" >/dev/null 2>&1; then
            echo "✓ Execution successful: $output"
        else
            echo "✗ Execution failed or no output: $output"
            ((failed_tests++))
        fi
    else
        echo "✗ Compilation failed: $file"
        ((total_tests++))
        ((failed_tests++))
    fi
    echo ""
}

# Test existing working files
echo "1. Testing existing known working files..."
test_go_file "examples/go/fibonacci.go" "fibonacci_go_working"
test_python_file "simple_py.py" "simple_py_working"

# Test simple algorithms
echo "2. Testing simple algorithm implementations..."

# Create simple Go test
cat > simple_go_algorithms.go << 'EOF'
package main

import "fmt"

func main() {
    fmt.Println("Simple Go Algorithms Test")
    
    // Test basic operations
    arr := []int{5, 2, 8, 1, 9}
    fmt.Printf("Original array: %v\n", arr)
    
    // Simple bubble sort
    n := len(arr)
    for i := 0; i < n-1; i++ {
        for j := 0; j < n-i-1; j++ {
            if arr[j] > arr[j+1] {
                arr[j], arr[j+1] = arr[j+1], arr[j]
            }
        }
    }
    fmt.Printf("Sorted array: %v\n", arr)
    
    // Test fibonacci
    fib := func(n int) int {
        if n <= 1 {
            return n
        }
        return fib(n-1) + fib(n-2)
    }
    
    fmt.Printf("Fibonacci(10): %d\n", fib(10))
}
EOF

test_go_file "simple_go_algorithms.go" "simple_go_algorithms"

# Create simple Python test
cat > simple_python_algorithms.py << 'EOF'
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

def bubble_sort(arr):
    n = len(arr)
    for i in range(n-1):
        for j in range(n-i-1):
            if arr[j] > arr[j+1]:
                arr[j], arr[j+1] = arr[j+1], arr[j]
    return arr

def main():
    print("Simple Python Algorithms Test")
    
    # Test sorting
    arr = [5, 2, 8, 1, 9]
    print(f"Original array: {arr}")
    sorted_arr = bubble_sort(arr.copy())
    print(f"Sorted array: {sorted_arr}")
    
    # Test fibonacci
    print(f"Fibonacci(10): {fibonacci(10)}")

if __name__ == "__main__":
    main()
EOF

test_python_file "simple_python_algorithms.py" "simple_python_algorithms"

# Test mathematical operations
echo "3. Testing mathematical operations..."

# Create math test Go
cat > math_test_go.go << 'EOF'
package main

import "fmt"

func main() {
    fmt.Println("Mathematical Operations Test")
    
    // Test basic math
    a, b := 15, 25
    fmt.Printf("%d + %d = %d\n", a, b, a+b)
    fmt.Printf("%d - %d = %d\n", a, b, a-b)
    fmt.Printf("%d * %d = %d\n", a, b, a*b)
    fmt.Printf("%d / %d = %d\n", a, b, a/b)
    
    // Test factorial
    factorial := func(n int) int {
        if n <= 1 {
            return 1
        }
        return n * factorial(n-1)
    }
    
    fmt.Printf("Factorial(5): %d\n", factorial(5))
    
    // Test prime check
    isPrime := func(n int) bool {
        if n <= 1 {
            return false
        }
        for i := 2; i*i <= n; i++ {
            if n%i == 0 {
                return false
            }
        }
        return true
    }
    
    fmt.Printf("Is 17 prime: %t\n", isPrime(17))
    fmt.Printf("Is 15 prime: %t\n", isPrime(15))
}
EOF

test_go_file "math_test_go.go" "math_test_go"

# Create math test Python
cat > math_test_python.py << 'EOF'
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n-1)

def is_prime(n):
    if n <= 1:
        return False
    for i in range(2, int(n**0.5) + 1):
        if n % i == 0:
            return False
    return True

def main():
    print("Mathematical Operations Test")
    
    # Test basic math
    a, b = 15, 25
    print(f"{a} + {b} = {a + b}")
    print(f"{a} - {b} = {a - b}")
    print(f"{a} * {b} = {a * b}")
    print(f"{a} / {b} = {a // b}")
    
    # Test factorial
    print(f"Factorial(5): {factorial(5)}")
    
    # Test prime check
    print(f"Is 17 prime: {is_prime(17)}")
    print(f"Is 15 prime: {is_prime(15)}")

if __name__ == "__main__":
    main()
EOF

test_python_file "math_test_python.py" "math_test_python"

# Test data structures
echo "4. Testing data structures..."

# Create data structures test Go
cat > data_structures_go.go << 'EOF'
package main

import "fmt"

type Node struct {
    Value int
    Next  *Node
}

type LinkedList struct {
    Head *Node
}

func (ll *LinkedList) Insert(value int) {
    newNode := &Node{Value: value}
    if ll.Head == nil {
        ll.Head = newNode
        return
    }
    
    current := ll.Head
    for current.Next != nil {
        current = current.Next
    }
    current.Next = newNode
}

func (ll *LinkedList) Display() {
    current := ll.Head
    for current != nil {
        fmt.Printf("%d -> ", current.Value)
        current = current.Next
    }
    fmt.Println("nil")
}

func main() {
    fmt.Println("Data Structures Test")
    
    // Test linked list
    ll := &LinkedList{}
    values := []int{10, 20, 30, 40, 50}
    
    for _, v := range values {
        ll.Insert(v)
    }
    
    fmt.Print("Linked List: ")
    ll.Display()
    
    // Test stack simulation with slice
    stack := []int{}
    fmt.Println("Stack Operations:")
    
    // Push
    for _, v := range values[:3] {
        stack = append(stack, v)
        fmt.Printf("Pushed %d, Stack: %v\n", v, stack)
    }
    
    // Pop
    for len(stack) > 0 {
        popped := stack[len(stack)-1]
        stack = stack[:len(stack)-1]
        fmt.Printf("Popped %d, Stack: %v\n", popped, stack)
    }
}
EOF

test_go_file "data_structures_go.go" "data_structures_go"

# Create data structures test Python
cat > data_structures_python.py << 'EOF'
class Node:
    def __init__(self, value):
        self.value = value
        self.next = None

class LinkedList:
    def __init__(self):
        self.head = None
    
    def insert(self, value):
        new_node = Node(value)
        if self.head is None:
            self.head = new_node
            return
        
        current = self.head
        while current.next:
            current = current.next
        current.next = new_node
    
    def display(self):
        current = self.head
        while current:
            print(f"{current.value} -> ", end="")
            current = current.next
        print("nil")

def main():
    print("Data Structures Test")
    
    # Test linked list
    ll = LinkedList()
    values = [10, 20, 30, 40, 50]
    
    for v in values:
        ll.insert(v)
    
    print("Linked List: ", end="")
    ll.display()
    
    # Test stack
    stack = []
    print("Stack Operations:")
    
    # Push
    for v in values[:3]:
        stack.append(v)
        print(f"Pushed {v}, Stack: {stack}")
    
    # Pop
    while stack:
        popped = stack.pop()
        print(f"Popped {popped}, Stack: {stack}")

if __name__ == "__main__":
    main()
EOF

test_python_file "data_structures_python.py" "data_structures_python"

# Print summary
echo "=== Test Summary ==="
echo "Total tests: $total_tests"
echo "Passed: $passed_tests"
echo "Failed: $failed_tests"
echo "Success rate: $((passed_tests * 100 / total_tests))%"

if [ $failed_tests -eq 0 ]; then
    echo "🎉 All tests passed!"
else
    echo "⚠️  Some tests failed. Check the output above for details."
fi

# Clean up test files
echo ""
echo "Cleaning up test files..."
rm -f simple_go_algorithms.go simple_python_algorithms.py
rm -f math_test_go.go math_test_python.py
rm -f data_structures_go.go data_structures_python.py
rm -f *_working *_algorithms *_test *_go *_python

echo "Testing completed!"