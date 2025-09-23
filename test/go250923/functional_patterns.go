package main

import (
	"fmt"
	"strings"
)

func main() {
	fmt.Println("=== Functional Programming Patterns in Go ===")

	fmt.Println("\n--- Higher-Order Functions ---")
	higherOrderFunctionsExample()

	fmt.Println("\n--- Function Composition ---")
	functionCompositionExample()

	fmt.Println("\n--- Map, Filter, Reduce ---")
	mapFilterReduceExample()

	fmt.Println("\n--- Currying ---")
	curryingExample()

	fmt.Println("\n--- Closures ---")
	closuresExample()

	fmt.Println("\n--- Functional Data Structures ---")
	functionalDataStructuresExample()

	fmt.Println("\n--- Partial Application ---")
	partialApplicationExample()
}

func higherOrderFunctionsExample() {
	add := func(a, b int) int { return a + b }
	multiply := func(a, b int) int { return a * b }

	fmt.Printf("Add: %d + %d = %d\n", 5, 3, add(5, 3))
	fmt.Printf("Multiply: %d * %d = %d\n", 5, 3, multiply(5, 3))

	operate := func(fn func(int, int) int, x, y int) int {
		return fn(x, y)
	}

	fmt.Printf("Operate with add: %d\n", operate(add, 10, 5))
	fmt.Printf("Operate with multiply: %d\n", operate(multiply, 10, 5))
}

func functionCompositionExample() {
	toUpper := func(s string) string { return strings.ToUpper(s) }
	addPrefix := func(s string) string { return "Prefix_" + s }
	addSuffix := func(s string) string { return s + "_Suffix" }

	compose := func(f, g func(string) string) func(string) string {
		return func(x string) string {
			return f(g(x))
		}
	}

	upperAndPrefix := compose(addPrefix, toUpper)
	prefixAndSuffix := compose(addSuffix, addPrefix)

	result1 := upperAndPrefix("hello")
	result2 := prefixAndSuffix("hello")

	fmt.Printf("Upper and Prefix: %s\n", result1)
	fmt.Printf("Prefix and Suffix: %s\n", result2)
}

func mapFilterReduceExample() {
	numbers := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

	mapInt := func(nums []int, fn func(int) int) []int {
		result := make([]int, len(nums))
		for i, num := range nums {
			result[i] = fn(num)
		}
		return result
	}

	filterInt := func(nums []int, fn func(int) bool) []int {
		result := []int{}
		for _, num := range nums {
			if fn(num) {
				result = append(result, num)
			}
		}
		return result
	}

	reduceInt := func(nums []int, fn func(int, int) int, initial int) int {
		result := initial
		for _, num := range nums {
			result = fn(result, num)
		}
		return result
	}

	doubled := mapInt(numbers, func(n int) int { return n * 2 })
	fmt.Printf("Doubled: %v\n", doubled)

	evens := filterInt(numbers, func(n int) bool { return n%2 == 0 })
	fmt.Printf("Even numbers: %v\n", evens)

	sum := reduceInt(numbers, func(acc, n int) int { return acc + n }, 0)
	fmt.Printf("Sum: %d\n", sum)

	product := reduceInt(numbers, func(acc, n int) int { return acc * n }, 1)
	fmt.Printf("Product: %d\n", product)
}

func curryingExample() {
	add := func(a int) func(int) int {
		return func(b int) int {
			return a + b
		}
	}

	add5 := add(5)
	add10 := add(10)

	fmt.Printf("Add 5 to 3: %d\n", add5(3))
	fmt.Printf("Add 10 to 3: %d\n", add10(3))

	multiply := func(a int) func(int) int {
		return func(b int) int {
			return a * b
		}
	}

	times2 := multiply(2)
	times5 := multiply(5)

	fmt.Printf("2 * 4 = %d\n", times2(4))
	fmt.Printf("5 * 4 = %d\n", times5(4))
}

func closuresExample() {
	sequence := func() func() int {
		i := 0
		return func() int {
			i++
			return i
		}
	}()

	next := sequence()
	fmt.Printf("Next: %d\n", next)
	fmt.Printf("Next: %d\n", next)
	fmt.Printf("Next: %d\n", next)

	counter := func(start int) func() int {
		count := start
		return func() int {
			count++
			return count
		}
	}

	counter1 := counter(0)
	counter2 := counter(100)

	fmt.Printf("Counter1: %d\n", counter1())
	fmt.Printf("Counter1: %d\n", counter1())
	fmt.Printf("Counter2: %d\n", counter2())
	fmt.Printf("Counter2: %d\n", counter2())

	multiplier := func(factor int) func(int) int {
		return func(x int) int {
			return x * factor
		}
	}

	double := multiplier(2)
	triple := multiplier(3)

	fmt.Printf("Double 5: %d\n", double(5))
	fmt.Printf("Triple 5: %d\n", triple(5))
}

func functionalDataStructuresExample() {
	type IntList struct {
		head int
		tail *IntList
	}

	cons := func(x int, xs *IntList) *IntList {
		return &IntList{head: x, tail: xs}
	}

	list := cons(1, cons(2, cons(3, cons(4, nil))))

	printList := func(lst *IntList) {
		for lst != nil {
			fmt.Printf("%d ", lst.head)
			lst = lst.tail
		}
		fmt.Println()
	}

	fmt.Print("Linked list: ")
	printList(list)

	var mapList func(lst *IntList, fn func(int) int) *IntList
	mapList = func(lst *IntList, fn func(int) int) *IntList {
		if lst == nil {
			return nil
		}
		return cons(fn(lst.head), mapList(lst.tail, fn))
	}

	squared := mapList(list, func(x int) int { return x * x })
	fmt.Print("Squared list: ")
	printList(squared)
}

func partialApplicationExample() {
	divide := func(a, b float64) float64 {
		return a / b
	}

	divideBy2 := func(a float64) float64 {
		return divide(a, 2)
	}

	divideBy10 := func(a float64) float64 {
		return divide(a, 10)
	}

	fmt.Printf("10 / 2 = %.2f\n", divideBy2(10))
	fmt.Printf("100 / 10 = %.2f\n", divideBy10(100))

	partial := func(fn func(float64, float64) float64, arg2 float64) func(float64) float64 {
		return func(arg1 float64) float64 {
			return fn(arg1, arg2)
		}
	}

	divideByPartial := partial(divide, 5)
	fmt.Printf("15 / 5 = %.2f\n", divideByPartial(15))
	fmt.Printf("25 / 5 = %.2f\n", divideByPartial(25))

	stringProcessor := func(prefix, suffix, s string) string {
		return prefix + s + suffix
	}

	withBrackets := partial2(stringProcessor, "[", "]")
	withQuotes := partial2(stringProcessor, "\"", "\"")

	fmt.Printf("With brackets: %s\n", withBrackets("hello"))
	fmt.Printf("With quotes: %s\n", withQuotes("hello"))
}

func partial2(fn func(string, string, string) string, arg1, arg2 string) func(string) string {
	return func(arg3 string) string {
		return fn(arg1, arg2, arg3)
	}
}