package main

import "fmt"

// 1. Basic function
func add(a, b int) int {
	return a + b
}

// 2. Multiple return values
func divmod(a, b int) (int, int) {
	return a / b, a % b
}

// 3. Named return values
func calculate(a, b int) (sum, diff int) {
	sum = a + b
	diff = a - b
	return // naked return
}

// 4. Variadic function
func sum(numbers ...int) int {
	total := 0
	for _, n := range numbers {
		total += n
	}
	return total
}

// 5. Higher-order function
func applyFunc(f func(int) int, value int) int {
	return f(value)
}

// 6. Closures
func makeAdder(x int) func(int) int {
	return func(y int) int {
		return x + y
	}
}

// 7. Recursive function
func factorial(n int) int {
	if n <= 1 {
		return 1
	}
	return n * factorial(n-1)
}

// 8. Struct for methods
type Rectangle struct {
	width, height float64
}

// 9. Value receiver method
func (r Rectangle) Area() float64 {
	return r.width * r.height
}

// 10. Pointer receiver method
func (r *Rectangle) Scale(factor float64) {
	r.width *= factor
	r.height *= factor
}

// 11. Interface
type Shape interface {
	Area() float64
}

func printArea(s Shape) {
	fmt.Printf("Area: %.2f\n", s.Area())
}

func main() {
	// Test basic function
	result := add(3, 4)
	fmt.Printf("add(3, 4) = %d\n", result)

	// Test multiple return values
	div, mod := divmod(10, 3)
	fmt.Printf("divmod(10, 3) = %d, %d\n", div, mod)

	// Test named return values
	s, d := calculate(10, 3)
	fmt.Printf("calculate(10, 3) = sum:%d, diff:%d\n", s, d)

	// Test variadic function
	total := sum(1, 2, 3, 4, 5)
	fmt.Printf("sum(1,2,3,4,5) = %d\n", total)

	// Test higher-order function
	square := func(x int) int { return x * x }
	squared := applyFunc(square, 5)
	fmt.Printf("square(5) = %d\n", squared)

	// Test closures
	add10 := makeAdder(10)
	fmt.Printf("add10(5) = %d\n", add10(5))

	// Test recursion
	fact := factorial(5)
	fmt.Printf("factorial(5) = %d\n", fact)

	// Test methods
	rect := Rectangle{width: 3.0, height: 4.0}
	fmt.Printf("Rectangle area: %.2f\n", rect.Area())
	
	rect.Scale(2.0)
	fmt.Printf("Scaled rectangle area: %.2f\n", rect.Area())

	// Test interface
	printArea(&rect)
}