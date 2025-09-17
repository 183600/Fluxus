package main

import "fmt"
import "math"
import "time"

func main() {
	fmt.Println("Hello, World!")
	fmt.Printf("2 + 2 = %d\n", 2+2)
	
	// Test basic arithmetic
	a := 10
	b := 3
	fmt.Printf("%d + %d = %d\n", a, b, a+b)
	fmt.Printf("%d - %d = %d\n", a, b, a-b)
	fmt.Printf("%d * %d = %d\n", a, b, a*b)
	fmt.Printf("%d / %d = %d\n", a, b, a/b)
	
	// Test math functions
	fmt.Printf("math.Pi = %.2f\n", math.Pi)
	fmt.Printf("math.Sqrt(16) = %.2f\n", math.Sqrt(16))
	
	// Test time
	now := time.Now()
	fmt.Printf("Current time: %s\n", now.Format("2006-01-02 15:04:05"))
	
	// Test loops
	fmt.Println("Counting to 5:")
	for i := 1; i <= 5; i++ {
		fmt.Printf("%d ", i)
	}
	fmt.Println()
	
	// Test conditionals
	if a > b {
		fmt.Printf("%d is greater than %d\n", a, b)
	} else {
		fmt.Printf("%d is less than or equal to %d\n", a, b)
	}
	
	// Test arrays
	arr := [3]int{1, 2, 3}
	fmt.Printf("Array: %v\n", arr)
	
	// Test slices
	slice := []int{10, 20, 30}
	fmt.Printf("Slice: %v\n", slice)
	
	// Test strings
	str := "Hello, Go!"
	fmt.Printf("String: %s, Length: %d\n", str, len(str))
}