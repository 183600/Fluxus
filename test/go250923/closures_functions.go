package main

import "fmt"

func main() {
	fmt.Println("=== Go Closures and Function Types Tests ===")
	testBasicClosures()
	testFunctionTypes()
	testHigherOrderFunctions()
	testFunctionComposition()
	testCurrying()
	testMemoization()
	testFactoryFunctions()

	fmt.Println("\n=== All closure and function tests completed successfully! ===")
}

func testBasicClosures() {
	fmt.Println("\n--- Basic Closures ---")

	// Closure capturing variable
	x := 10
	closure := func() int {
		x++
		return x
	}

	fmt.Printf("First call: %d\n", closure())
	fmt.Printf("Second call: %d\n", closure())
	fmt.Printf("Third call: %d\n", closure())

	// Closure with multiple captures
	a, b := 1, 2
	add := func() int {
		a++
		b++
		return a + b
	}

	fmt.Printf("Add result: %d\n", add())
	fmt.Printf("Add result: %d\n", add())
}

func testFunctionTypes() {
	fmt.Println("\n--- Function Types ---")

	// Function type declaration
	type MathFunc func(int, int) int

	// Functions that match the type
	add := func(a, b int) int { return a + b }
	subtract := func(a, b int) int { return a - b }
	multiply := func(a, b int) int { return a * b }

	// Function slice
	operations := []MathFunc{add, subtract, multiply}

	x, y := 10, 5
	for _, op := range operations {
		result := op(x, y)
		fmt.Printf("Operation result: %d\n", result)
	}

	// Function as parameter
	compute := func(f MathFunc, a, b int) int {
		return f(a, b)
	}

	fmt.Printf("Compute add: %d\n", compute(add, x, y))
	fmt.Printf("Compute multiply: %d\n", compute(multiply, x, y))
}

func testHigherOrderFunctions() {
	fmt.Println("\n--- Higher-Order Functions ---")

	// Map function
	mapFunc := func(numbers []int, f func(int) int) []int {
		result := make([]int, len(numbers))
		for i, num := range numbers {
			result[i] = f(num)
		}
		return result
	}

	numbers := []int{1, 2, 3, 4, 5}
	squares := mapFunc(numbers, func(x int) int { return x * x })
	doubles := mapFunc(numbers, func(x int) int { return x * 2 })

	fmt.Printf("Original: %v\n", numbers)
	fmt.Printf("Squares: %v\n", squares)
	fmt.Printf("Doubles: %v\n", doubles)

	// Filter function
	filterFunc := func(numbers []int, predicate func(int) bool) []int {
		var result []int
		for _, num := range numbers {
			if predicate(num) {
				result = append(result, num)
			}
		}
		return result
	}

	evens := filterFunc(numbers, func(x int) bool { return x%2 == 0 })
	odds := filterFunc(numbers, func(x int) bool { return x%2 != 0 })

	fmt.Printf("Evens: %v\n", evens)
	fmt.Printf("Odds: %v\n", odds)
}

func testFunctionComposition() {
	fmt.Println("\n--- Function Composition ---")

	// Function composition
	compose := func(f, g func(int) int) func(int) int {
		return func(x int) int {
			return f(g(x))
		}
	}

	double := func(x int) int { return x * 2 }
	addOne := func(x int) int { return x + 1 }

	doubleThenAdd := compose(addOne, double)
	addThenDouble := compose(double, addOne)

	fmt.Printf("Double then add (5): %d\n", doubleThenAdd(5))
	fmt.Printf("Add then double (5): %d\n", addThenDouble(5))
}

func testCurrying() {
	fmt.Println("\n--- Currying ---")

	// Curried addition
	add := func(a int) func(int) int {
		return func(b int) int {
			return a + b
		}
	}

	add5 := add(5)
	add10 := add(10)

	fmt.Printf("Add 5 to 3: %d\n", add5(3))
	fmt.Printf("Add 10 to 3: %d\n", add10(3))

	// Multi-argument currying
	multiply := func(a, b int) func(int) int {
		return func(c int) int {
			return a * b * c
		}
	}

	multiply2_3 := multiply(2, 3)
	fmt.Printf("Multiply 2 * 3 * 4: %d\n", multiply2_3(4))
}

func testMemoization() {
	fmt.Println("\n--- Memoization ---")

	// Memoized fibonacci
	fib := func() func(int) int {
		cache := make(map[int]int)

		var fibFunc func(int) int
		fibFunc = func(n int) int {
			if n <= 1 {
				return n
			}

			if result, exists := cache[n]; exists {
				return result
			}

			result := fibFunc(n-1) + fibFunc(n-2)
			cache[n] = result
			return result
		}

		return fibFunc
	}()

	memoizedFib := fib()
	fmt.Printf("Fibonacci(10): %d\n", memoizedFib(10))
	fmt.Printf("Fibonacci(15): %d\n", memoizedFib(15))
	fmt.Printf("Fibonacci(20): %d\n", memoizedFib(20))
}

func testFactoryFunctions() {
	fmt.Println("\n--- Factory Functions ---")

	// Counter factory
	createCounter := func() func() int {
		count := 0
		return func() int {
			count++
			return count
		}
	}

	counter1 := createCounter()
	counter2 := createCounter()

	fmt.Printf("Counter 1: %d\n", counter1())
	fmt.Printf("Counter 1: %d\n", counter1())
	fmt.Printf("Counter 2: %d\n", counter2())
	fmt.Printf("Counter 1: %d\n", counter1())

	// Multiplier factory
	createMultiplier := func(factor int) func(int) int {
		return func(x int) int {
			return x * factor
		}
	}

	triple := createMultiplier(3)
	quadruple := createMultiplier(4)

	fmt.Printf("Triple 5: %d\n", triple(5))
	fmt.Printf("Quadruple 5: %d\n", quadruple(5))
}