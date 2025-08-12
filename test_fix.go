package main

import "fmt"

func main() {
	fmt.Println("Hello, World!")
	
	// Test variable declarations
	var x int = 42
	var y float64 = 3.14
	var s string = "test"
	
	fmt.Printf("x = %d, y = %f, s = %s\n", x, y, s)
	
	// Test short variable declarations
	z := 100
	fmt.Printf("z = %d\n", z)
	
	// Test arrays and slices
	arr := [3]int{1, 2, 3}
	slice := []int{4, 5, 6}
	
	fmt.Printf("arr = %v, slice = %v\n", arr, slice)
	
	// Test maps
	m := make(map[string]int)
	m["one"] = 1
	m["two"] = 2
	
	fmt.Printf("map = %v\n", m)
	
	// Test function calls
	result := add(5, 3)
	fmt.Printf("add(5, 3) = %d\n", result)
}

func add(a, b int) int {
	return a + b
}