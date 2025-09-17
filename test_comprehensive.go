package main

import (
	"fmt"
	"math"
	"reflect"
	"runtime"
	"strings"
	"sync"
	"time"
)

// Basic types
var (
	intVar      int = 42
	int8Var     int8 = 8
	int16Var    int16 = 16
	int32Var    int32 = 32
	int64Var    int64 = 64
	uintVar     uint = 42
	uint8Var    uint8 = 8
	uint16Var   uint16 = 16
	uint32Var   uint32 = 32
	uint64Var   uint64 = 64
	float32Var  float32 = 32.5
	float64Var  float64 = 64.5
	boolVar     bool = true
	stringVar   string = "Hello, World!"
	runeVar     rune = 'A'
	byteVar     byte = 'B'
	complex64Var complex64 = 1 + 2i
	complex128Var complex128 = 3 + 4i
)

// Constants
const (
	Pi          = 3.14159
	MaxInt      = int(^uint(0) >> 1)
	HelloWorld  = "Hello"
)

// Enums using iota
type Direction int

const (
	North Direction = iota
	East
	South
	West
)

// Structs
type Person struct {
	Name    string
	Age     int
	Address Address
}

type Address struct {
	Street  string
	City    string
	Country string
}

// Interfaces
type Shape interface {
	Area() float64
	Perimeter() float64
}

type Circle struct {
	Radius float64
}

type Rectangle struct {
	Width  float64
	Height float64
}

func (c Circle) Area() float64 {
	return math.Pi * c.Radius * c.Radius
}

func (c Circle) Perimeter() float64 {
	return 2 * math.Pi * c.Radius
}

func (r Rectangle) Area() float64 {
	return r.Width * r.Height
}

func (r Rectangle) Perimeter() float64 {
	return 2 * (r.Width + r.Height)
}

// Methods with value and pointer receivers
func (p Person) Greet() string {
	return fmt.Sprintf("Hello, my name is %s", p.Name)
}

func (p *Person) SetAge(age int) {
	p.Age = age
}

// Functions with various parameters and return types
func add(a, b int) int {
	return a + b
}

func swap(a, b string) (string, string) {
	return b, a
}

func divide(a, b float64) (float64, error) {
	if b == 0 {
		return 0, fmt.Errorf("division by zero")
	}
	return a / b, nil
}

// Variadic function
func sum(nums ...int) int {
	total := 0
	for _, num := range nums {
		total += num
	}
	return total
}

// Higher-order function
func applyOperation(a, b int, op func(int, int) int) int {
	return op(a, b)
}

// Closure
func counter() func() int {
	count := 0
	return func() int {
		count++
		return count
	}
}

// Generic function (Go 1.18+)
func min[T int | float64](a, b T) T {
	if a < b {
		return a
	}
	return b
}

// Channels and goroutines
func worker(id int, jobs <-chan int, results chan<- int) {
	for j := range jobs {
		fmt.Printf("Worker %d processing job %d\n", id, j)
		time.Sleep(time.Millisecond * 100)
		results <- j * 2
	}
}

// Deferred function and panic/recover
func mayPanic(shouldPanic bool) {
	defer func() {
		if r := recover(); r != nil {
			fmt.Println("Recovered from panic:", r)
		}
	}()

	if shouldPanic {
		panic("intentional panic")
	}
	fmt.Println("No panic occurred")
}

// Maps and slices manipulation
func demonstrateCollections() {
	// Slice operations
	slice := []int{1, 2, 3, 4, 5}
	slice = append(slice, 6, 7, 8)
	subSlice := slice[2:5]
	fmt.Printf("Slice: %v, Sub-slice: %v\n", slice, subSlice)

	// Map operations
	mapVar := make(map[string]int)
	mapVar["one"] = 1
	mapVar["two"] = 2
	mapVar["three"] = 3

	if value, exists := mapVar["two"]; exists {
		fmt.Printf("Map value for 'two': %d\n", value)
	}

	// Delete from map
	delete(mapVar, "one")

	// Range over map
	for key, value := range mapVar {
		fmt.Printf("Map: %s -> %d\n", key, value)
	}
}

// Control structures
func demonstrateControlStructures() {
	// If-else
	x := 10
	if x > 5 {
		fmt.Println("x is greater than 5")
	} else if x == 5 {
		fmt.Println("x is equal to 5")
	} else {
		fmt.Println("x is less than 5")
	}

	// Switch statement
	switch day := "Wednesday"; day {
	case "Monday", "Tuesday":
		fmt.Println("Start of week")
	case "Wednesday", "Thursday":
		fmt.Println("Mid week")
	case "Friday":
		fmt.Println("End of week")
	default:
		fmt.Println("Weekend")
	}

	// Switch with no condition
	switch {
	case x > 10:
		fmt.Println("x > 10")
	case x < 10:
		fmt.Println("x < 10")
	default:
		fmt.Println("x == 10")
	}

	// For loops
	// Traditional for loop
	for i := 0; i < 5; i++ {
		fmt.Printf("For loop iteration: %d\n", i)
	}

	// While-style for loop
	count := 0
	for count < 3 {
		fmt.Printf("While loop iteration: %d\n", count)
		count++
	}

	// Infinite loop with break
	for {
		fmt.Println("Infinite loop")
		break
	}

	// For with range
	numbers := []int{1, 2, 3, 4, 5}
	for index, value := range numbers {
		fmt.Printf("Range: index=%d, value=%d\n", index, value)
	}

	// Continue statement
	for i := 0; i < 5; i++ {
		if i == 2 {
			continue
		}
		fmt.Printf("Continue example: %d\n", i)
	}
}

// Pointers
func demonstratePointers() {
	a := 42
	b := &a
	fmt.Printf("Value: %d, Pointer: %p, Dereferenced: %d\n", a, b, *b)

	*b = 100
	fmt.Printf("After modification: %d\n", a)
}

// Type assertions and type switches
func demonstrateTypeAssertions() {
	var i interface{} = "Hello"

	// Type assertion
	if str, ok := i.(string); ok {
		fmt.Printf("Type assertion successful: %s\n", str)
	}

	// Type switch
	switch v := i.(type) {
	case string:
		fmt.Printf("String value: %s\n", v)
	case int:
		fmt.Printf("Integer value: %d\n", v)
	default:
		fmt.Printf("Unknown type: %T\n", v)
	}
}

// Struct tags and reflection
func demonstrateReflection() {
	type User struct {
		Name string `json:"name"`
		Age  int    `json:"age"`
	}

	user := User{Name: "Alice", Age: 30}
	t := reflect.TypeOf(user)

	for i := 0; i < t.NumField(); i++ {
		field := t.Field(i)
		fmt.Printf("Field: %s, Tag: %s\n", field.Name, field.Tag.Get("json"))
	}
}

// Error handling
func demonstrateErrorHandling() {
	result, err := divide(10, 2)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("Result: %f\n", result)
	}

	_, err = divide(10, 0)
	if err != nil {
		fmt.Printf("Expected error: %v\n", err)
	}
}

// Goroutines and synchronization
func demonstrateConcurrency() {
	var wg sync.WaitGroup
	wg.Add(2)

	// Goroutine with anonymous function
	go func() {
		defer wg.Done()
		for i := 0; i < 3; i++ {
			fmt.Printf("Goroutine 1: %d\n", i)
			time.Sleep(time.Millisecond * 50)
		}
	}()

	// Goroutine with named function
	go func() {
		defer wg.Done()
		for i := 0; i < 3; i++ {
			fmt.Printf("Goroutine 2: %d\n", i)
			time.Sleep(time.Millisecond * 50)
		}
	}()

	wg.Wait()

	// Channel example
	ch := make(chan int)
	go func() {
		ch <- 42
	}()

	value := <-ch
	fmt.Printf("Channel value: %d\n", value)

	// Buffered channel
	buffered := make(chan int, 3)
	buffered <- 1
	buffered <- 2
	buffered <- 3

	for i := 0; i < 3; i++ {
		fmt.Printf("Buffered channel: %d\n", <-buffered)
	}
}

// Main function
func main() {
	fmt.Println("=== Go Comprehensive Syntax Demo ===")

	// Basic types
	fmt.Printf("Int: %d, Float: %f, Bool: %t, String: %s\n", intVar, float64Var, boolVar, stringVar)

	// Constants
	fmt.Printf("Pi: %f, MaxInt: %d\n", Pi, MaxInt)

	// Enums
	direction := North
	fmt.Printf("Direction: %d\n", direction)

	// Structs
	person := Person{
		Name: "John Doe",
		Age:  30,
		Address: Address{
			Street:  "123 Main St",
			City:    "New York",
			Country: "USA",
		},
	}

	fmt.Printf("Person: %s\n", person.Greet())
	person.SetAge(31)
	fmt.Printf("Updated age: %d\n", person.Age)

	// Interfaces
	var shape Shape = Circle{Radius: 5}
	fmt.Printf("Circle area: %f, perimeter: %f\n", shape.Area(), shape.Perimeter())

	shape = Rectangle{Width: 4, Height: 6}
	fmt.Printf("Rectangle area: %f, perimeter: %f\n", shape.Area(), shape.Perimeter())

	// Functions
	result := add(5, 3)
	fmt.Printf("Add result: %d\n", result)

	a, b := swap("hello", "world")
	fmt.Printf("Swapped: %s, %s\n", a, b)

	quotient, err := divide(10, 2)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("Division result: %f\n", quotient)
	}

	// Variadic function
	sumResult := sum(1, 2, 3, 4, 5)
	fmt.Printf("Sum: %d\n", sumResult)

	// Higher-order function
	multiply := func(a, b int) int {
		return a * b
	}
	product := applyOperation(4, 5, multiply)
	fmt.Printf("Product: %d\n", product)

	// Closure
	counterFunc := counter()
	fmt.Printf("Counter: %d, %d, %d\n", counterFunc(), counterFunc(), counterFunc())

	// Collections
	demonstrateCollections()

	// Control structures
	demonstrateControlStructures()

	// Pointers
	demonstratePointers()

	// Type assertions
	demonstrateTypeAssertions()

	// Reflection
	demonstrateReflection()

	// Error handling
	demonstrateErrorHandling()

	// Concurrency
	demonstrateConcurrency()

	// Panic and recover
	fmt.Println("=== Testing panic/recover ===")
	mayPanic(false)
	mayPanic(true)

	// Goroutine worker pool
	fmt.Println("=== Worker pool example ===")
	jobs := make(chan int, 5)
	results := make(chan int, 5)

	for w := 1; w <= 3; w++ {
		go worker(w, jobs, results)
	}

	for j := 1; j <= 5; j++ {
		jobs <- j
	}
	close(jobs)

	for a := 1; a <= 5; a++ {
		result := <-results
		fmt.Printf("Result: %d\n", result)
	}

	// Runtime info
	fmt.Printf("GOMAXPROCS: %d\n", runtime.GOMAXPROCS(0))
	fmt.Printf("NumGoroutine: %d\n", runtime.NumGoroutine())

	// String operations
	fmt.Printf("Uppercase: %s\n", strings.ToUpper("hello"))
	fmt.Printf("Contains: %t\n", strings.Contains("hello world", "world"))

	// Math operations
	fmt.Printf("Square root: %f\n", math.Sqrt(16))
	fmt.Printf("Power: %f\n", math.Pow(2, 3))

	fmt.Println("=== Demo completed successfully ===")
}