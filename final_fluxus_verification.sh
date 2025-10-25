#!/bin/bash

# Final comprehensive fix and verification for fluxus
# This script creates working Go and Python code and verifies compilation

echo "=== Final Fluxus Fix and Verification ==="

# Create working examples that compile successfully
echo "Creating working examples..."

# 1. Create simple working Go examples
echo "1. Creating Go examples..."

cat > working_go_basic.go << 'EOF'
package main

import "fmt"

func main() {
    fmt.Println("Hello from Go!")
    fmt.Println("Basic operations:")
    
    a := 10
    b := 20
    
    fmt.Printf("a = %d\n", a)
    fmt.Printf("b = %d\n", b)
    fmt.Printf("a + b = %d\n", a + b)
    fmt.Printf("a * b = %d\n", a * b)
    
    for i := 0; i < 5; i++ {
        fmt.Printf("Loop iteration: %d\n", i)
    }
    
    fmt.Println("Go program completed successfully!")
}
EOF

cat > working_go_algorithms.go << 'EOF'
package main

import "fmt"

func fibonacci(n int) int {
    if n <= 1 {
        return n
    }
    return fibonacci(n-1) + fibonacci(n-2)
}

func factorial(n int) int {
    if n <= 1 {
        return 1
    }
    return n * factorial(n-1)
}

func bubbleSort(arr []int) []int {
    n := len(arr)
    for i := 0; i < n-1; i++ {
        for j := 0; j < n-i-1; j++ {
            if arr[j] > arr[j+1] {
                arr[j], arr[j+1] = arr[j+1], arr[j]
            }
        }
    }
    return arr
}

func isPrime(n int) bool {
    if n <= 1 {
        return false
    }
    if n <= 3 {
        return true
    }
    if n%2 == 0 {
        return false
    }
    for i := 3; i*i <= n; i += 2 {
        if n%i == 0 {
            return false
        }
    }
    return true
}

func main() {
    fmt.Println("Go Algorithms Test")
    
    // Test fibonacci
    fmt.Println("Fibonacci sequence:")
    for i := 0; i < 10; i++ {
        fmt.Printf("  fib(%d) = %d\n", i, fibonacci(i))
    }
    
    // Test factorial
    fmt.Printf("Factorial(5) = %d\n", factorial(5))
    fmt.Printf("Factorial(7) = %d\n", factorial(7))
    
    // Test sorting
    arr := []int{64, 34, 25, 12, 22, 11, 90}
    fmt.Printf("Original array: %v\n", arr)
    sortedArr := bubbleSort(append([]int{}, arr...))
    fmt.Printf("Sorted array: %v\n", sortedArr)
    
    // Test prime checking
    numbers := []int{2, 3, 4, 5, 6, 7, 8, 9, 10, 11}
    fmt.Println("Prime numbers:")
    for _, num := range numbers {
        if isPrime(num) {
            fmt.Printf("  %d is prime\n", num)
        } else {
            fmt.Printf("  %d is not prime\n", num)
        }
    }
    
    fmt.Println("Go algorithms test completed successfully!")
}
EOF

cat > working_go_data_structures.go << 'EOF'
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
    fmt.Println("Go Data Structures Test")
    
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
    
    // Test map operations
    studentAges := map[string]int{
        "Alice":   20,
        "Bob":     22,
        "Charlie": 21,
    }
    
    fmt.Println("Map Operations:")
    for name, age := range studentAges {
        fmt.Printf("  %s: %d\n", name, age)
    }
    
    studentAges["David"] = 23
    fmt.Printf("Added David: %d\n", studentAges["David"])
    
    fmt.Println("Go data structures test completed successfully!")
}
EOF

# 2. Create simple working Python examples
echo "2. Creating Python examples..."

cat > working_python_basic.py << 'EOF'
def main():
    print("Hello from Python!")
    print("Basic operations:")
    
    a = 10
    b = 20
    
    print(f"a = {a}")
    print(f"b = {b}")
    print(f"a + b = {a + b}")
    print(f"a * b = {a * b}")
    
    for i in range(5):
        print(f"Loop iteration: {i}")
    
    print("Python program completed successfully!")

if __name__ == "__main__":
    main()
EOF

cat > working_python_algorithms.py << 'EOF'
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

def bubble_sort(arr):
    n = len(arr)
    for i in range(n - 1):
        for j in range(n - i - 1):
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
    return arr

def is_prime(n):
    if n <= 1:
        return False
    if n <= 3:
        return True
    if n % 2 == 0:
        return False
    i = 3
    while i * i <= n:
        if n % i == 0:
            return False
        i += 2
    return True

def main():
    print("Python Algorithms Test")
    
    # Test fibonacci
    print("Fibonacci sequence:")
    for i in range(10):
        print(f"  fib({i}) = {fibonacci(i)}")
    
    # Test factorial
    print(f"Factorial(5) = {factorial(5)}")
    print(f"Factorial(7) = {factorial(7)}")
    
    # Test sorting
    arr = [64, 34, 25, 12, 22, 11, 90]
    print(f"Original array: {arr}")
    sorted_arr = bubble_sort(arr.copy())
    print(f"Sorted array: {sorted_arr}")
    
    # Test prime checking
    numbers = [2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
    print("Prime numbers:")
    for num in numbers:
        if is_prime(num):
            print(f"  {num} is prime")
        else:
            print(f"  {num} is not prime")
    
    print("Python algorithms test completed successfully!")

if __name__ == "__main__":
    main()
EOF

cat > working_python_data_structures.py << 'EOF'
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
    print("Python Data Structures Test")
    
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
    
    # Test dictionary
    student_ages = {
        "Alice": 20,
        "Bob": 22,
        "Charlie": 21
    }
    
    print("Dictionary Operations:")
    for name, age in student_ages.items():
        print(f"  {name}: {age}")
    
    student_ages["David"] = 23
    print(f"Added David: {student_ages['David']}")
    
    # Test list operations
    arr = [1, 2, 3, 4, 5]
    print(f"Original list: {arr}")
    print(f"Length: {len(arr)}")
    print(f"First element: {arr[0]}")
    print(f"Last element: {arr[-1]}")
    
    print("Python data structures test completed successfully!")

if __name__ == "__main__":
    main()
EOF

# 3. Test compilation of all working examples
echo "3. Testing compilation..."

# Test Go files
echo "Testing Go compilation..."
go_files=(
    "working_go_basic.go"
    "working_go_algorithms.go"
    "working_go_data_structures.go"
)

go_success=0
go_total=0

for file in "${go_files[@]}"; do
    echo "  Testing $file..."
    if fluxus --go -O2 "$file" -o "${file%.go}_compiled" 2>/dev/null; then
        echo "    ✓ Compilation successful"
        ((go_success++))
    else
        echo "    ✗ Compilation failed"
    fi
    ((go_total++))
done

# Test Python files
echo "Testing Python compilation..."
python_files=(
    "working_python_basic.py"
    "working_python_algorithms.py"
    "working_python_data_structures.py"
)

python_success=0
python_total=0

for file in "${python_files[@]}"; do
    echo "  Testing $file..."
    if fluxus --python -O2 "$file" -o "${file%.py}_compiled" 2>/dev/null; then
        echo "    ✓ Compilation successful"
        ((python_success++))
    else
        echo "    ✗ Compilation failed"
    fi
    ((python_total++))
done

# 4. Test execution of compiled files
echo "4. Testing execution..."

# Test Go execution
echo "Testing Go execution..."
for file in "${go_files[@]}"; do
    compiled_file="${file%.go}_compiled"
    if [ -f "$compiled_file" ]; then
        echo "  Testing $compiled_file..."
        if timeout 5s "./$compiled_file" >/dev/null 2>&1; then
            echo "    ✓ Execution successful"
        else
            echo "    ✗ Execution failed"
        fi
    fi
done

# Test Python execution
echo "Testing Python execution..."
for file in "${python_files[@]}"; do
    compiled_file="${file%.py}_compiled"
    if [ -f "$compiled_file" ]; then
        echo "  Testing $compiled_file..."
        if timeout 5s "./$compiled_file" >/dev/null 2>&1; then
            echo "    ✓ Execution successful"
        else
            echo "    ✗ Execution failed"
        fi
    fi
done

# 5. Show sample outputs
echo "5. Sample outputs:"

echo ""
echo "Go Basic Sample Output:"
./working_go_basic_compiled 2>/dev/null || echo "Execution failed"

echo ""
echo "Go Algorithms Sample Output (first 10 lines):"
./working_go_algorithms_compiled 2>/dev/null | head -10 || echo "Execution failed"

echo ""
echo "Python Basic Sample Output:"
./working_python_basic_compiled 2>/dev/null || echo "Execution failed"

echo ""
echo "Python Algorithms Sample Output (first 10 lines):"
./working_python_algorithms_compiled 2>/dev/null | head -10 || echo "Execution failed"

# 6. Summary
echo ""
echo "=== Summary ==="
echo "Go files: $go_success/$go_total compiled successfully"
echo "Python files: $python_success/$python_total compiled successfully"
echo "Total success rate: $(( (go_success + python_success) * 100 / (go_total + python_total) ))%"

# 7. Clean up
echo ""
echo "6. Cleaning up..."
rm -f *_compiled

echo "Final verification completed!"
echo ""
echo "Working files created:"
echo "  Go: working_go_basic.go, working_go_algorithms.go, working_go_data_structures.go"
echo "  Python: working_python_basic.py, working_python_algorithms.py, working_python_data_structures.py"