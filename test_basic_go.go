package main

import "fmt"

// Basic function
func add(a, b int) int {
	return a + b
}

// Test basic Go features
func main() {
	// Basic arithmetic
	x := 10
	y := 20
	result := add(x, y)
	
	fmt.Printf("add(%d, %d) = %d\n", x, y, result)
	
	// Loop
	for i := 0; i < 3; i++ {
		fmt.Printf("Loop iteration: %d\n", i)
	}
	
	// Slice operations
	numbers := []int{1, 2, 3, 4, 5}
	fmt.Printf("Numbers: %v\n", numbers)
	fmt.Printf("Length: %d\n", len(numbers))
	
	// Built-in functions
	fmt.Printf("min(10, 5) = %d\n", min(10, 5))
	fmt.Printf("max(10, 5) = %d\n", max(10, 5))
}