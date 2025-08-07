package main

import "fmt"

// Test basic function definitions
func add(a, b int) int {
    return a + b
}

// Test multiple return values
func swap(a, b string) (string, string) {
    return b, a
}

// Test control flow - if/else
func max(a, b int) int {
    if a > b {
        return a
    } else {
        return b
    }
}

// Test for loop
func fibonacci(n int) int {
    if n <= 1 {
        return n
    }
    a, b := 0, 1
    for i := 2; i <= n; i++ {
        a, b = b, a+b
    }
    return b
}

// Test array and slice
func processArray() {
    arr := [3]int{1, 2, 3}
    slice := []int{4, 5, 6}
    fmt.Println("Array:", arr)
    fmt.Println("Slice:", slice)
}

// Test struct
type Point struct {
    X, Y int
}

func (p Point) String() string {
    return fmt.Sprintf("(%d, %d)", p.X, p.Y)
}

// Main function with comprehensive tests
func main() {
    // Test function calls
    sum := add(3, 4)
    fmt.Printf("3 + 4 = %d\n", sum)
    
    // Test multiple return values
    first, second := swap("hello", "world")
    fmt.Printf("After swap: %s, %s\n", first, second)
    
    // Test control flow
    maximum := max(10, 20)
    fmt.Printf("Max of 10 and 20: %d\n", maximum)
    
    // Test loop
    fib := fibonacci(10)
    fmt.Printf("Fibonacci(10) = %d\n", fib)
    
    // Test arrays and slices
    processArray()
    
    // Test struct
    p := Point{X: 3, Y: 4}
    fmt.Println("Point:", p)
}