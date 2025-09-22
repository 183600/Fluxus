package main

import (
	"fmt"
	"math"
	"math/rand"
	"os"
	"reflect"
	"runtime"
	"strings"
	"sync"
	"time"
	"unsafe"
)

// Basic types and constants
const (
	Pi        = 3.14159265358979323846
	MaxInt    = int(^uint(0) >> 1)
	TrueConst = true
	FalseConst = false
)

// Variables with various types
var (
	globalInt       int = 42
	globalFloat     float64 = 3.14
	globalString    string = "hello"
	globalBool      bool = true
	globalSlice     []int = []int{1, 2, 3}
	globalMap       map[string]int = make(map[string]int)
	globalChannel   chan int = make(chan int)
	globalPointer   *int = &globalInt
	globalInterface interface{} = "interface value"
)

// Struct definitions
type Person struct {
	Name    string
	Age     int
	Address string
}

type Employee struct {
	Person
	Salary float64
	ID     int
}

// Interface definitions
type Speaker interface {
	Speak() string
}

type Walker interface {
	Walk() string
}

type Human struct {
	name string
}

func (h Human) Speak() string {
	return h.name + " says hello"
}

func (h Human) Walk() string {
	return h.name + " is walking"
}

// Generic functions and types
type Container[T any] struct {
	value T
}

func (c Container[T]) GetValue() T {
	return c.value
}

func Max[T int | float64](a, b T) T {
	if a > b {
		return a
	}
	return b
}

// Function with multiple return values and named returns
func divide(a, b float64) (result float64, err error) {
	if b == 0 {
		err = fmt.Errorf("division by zero")
		return
	}
	result = a / b
	return
}

// Function with variadic parameters
func sum(numbers ...int) int {
	total := 0
	for _, num := range numbers {
		total += num
	}
	return total
}

// Function with deferred calls and panic recovery
func safeOperation() {
	defer func() {
		if r := recover(); r != nil {
			fmt.Println("Recovered from panic:", r)
		}
	}()

	panic("intentional panic")
}

// Method with pointer receiver
type Counter struct {
	count int
}

func (c *Counter) Increment() {
	c.count++
}

func (c Counter) GetCount() int {
	return c.count
}

// Goroutine and channel demonstration
func worker(id int, jobs <-chan int, results chan<- int, wg *sync.WaitGroup) {
	defer wg.Done()
	for job := range jobs {
		fmt.Printf("Worker %d processing job %d\n", id, job)
		time.Sleep(time.Millisecond * 100)
		results <- job * 2
	}
}

// Closure and function as value
func getCalculator() func(int, int) int {
	return func(a, b int) int {
		return a + b
	}
}

// Select statement
func channelOperations() {
	ch1 := make(chan int)
	ch2 := make(chan int)

	go func() {
		time.Sleep(100 * time.Millisecond)
		ch1 <- 1
	}()

	go func() {
		time.Sleep(50 * time.Millisecond)
		ch2 <- 2
	}()

	select {
	case msg1 := <-ch1:
		fmt.Println("Received from ch1:", msg1)
	case msg2 := <-ch2:
		fmt.Println("Received from ch2:", msg2)
	case <-time.After(200 * time.Millisecond):
		fmt.Println("Timeout")
	}
}

// Array operations
func arrayOperations() {
	var arr1 [5]int = [5]int{1, 2, 3, 4, 5}
	var arr2 [5]int

	for i := 0; i < len(arr1); i++ {
		arr2[i] = arr1[i] * 2
	}

	fmt.Println("Array operations:", arr1, arr2)
}

// Slice operations with append and copy
func sliceOperations() {
	slice1 := []int{1, 2, 3}
	slice2 := make([]int, len(slice1))

	copy(slice2, slice1)
	slice1 = append(slice1, 4, 5, 6)

	subSlice := slice1[1:4]

	fmt.Println("Slice operations:", slice1, slice2, subSlice)
}

// Map operations
func mapOperations() {
	m := make(map[string]int)
	m["a"] = 1
	m["b"] = 2
	m["c"] = 3

	if val, ok := m["a"]; ok {
		fmt.Println("Found value:", val)
	}

	delete(m, "b")

	for key, value := range m {
		fmt.Println("Map:", key, value)
	}
}

// Type assertion and type switch
func typeAssertions(i interface{}) {
	switch v := i.(type) {
	case int:
		fmt.Println("Type is int:", v)
	case string:
		fmt.Println("Type is string:", v)
	case bool:
		fmt.Println("Type is bool:", v)
	default:
		fmt.Println("Unknown type:", v)
	}
}

// Reflection
func reflectionDemo() {
	var x float64 = 3.4
	v := reflect.ValueOf(x)
	fmt.Println("Type:", v.Type())
	fmt.Println("Kind:", v.Kind())
	fmt.Println("Value:", v.Float())
}

// Unsafe operations
func unsafeDemo() {
	var x int = 42
	p := unsafe.Pointer(&x)
	p2 := (*int)(p)
	fmt.Println("Unsafe pointer:", *p2)
}

// Error handling
type MyError struct {
	msg string
}

func (e *MyError) Error() string {
	return e.msg
}

func mayError() error {
	return &MyError{"something went wrong"}
}

func main() {
	// Basic printing
	fmt.Println("Hello, World!")
	fmt.Printf("Pi = %.2f\n", Pi)

	// Variables and basic operations
	a := 10
	b := 20
	fmt.Println("Sum:", a+b)
	fmt.Println("Max:", Max(a, b))

	// Control structures
	if a < b {
		fmt.Println("a is less than b")
	}

	switch a {
	case 10:
		fmt.Println("a is 10")
	default:
		fmt.Println("a is not 10")
	}

	for i := 0; i < 5; i++ {
		fmt.Print(i, " ")
	}
	fmt.Println()

	// Range loops
	slice := []string{"a", "b", "c"}
	for index, value := range slice {
		fmt.Printf("%d: %s ", index, value)
	}
	fmt.Println()

	// Function calls
	result, err := divide(10.0, 2.0)
	if err != nil {
		fmt.Println("Error:", err)
	} else {
		fmt.Println("Division result:", result)
	}

	// Variadic function
	fmt.Println("Sum:", sum(1, 2, 3, 4, 5))

	// Struct usage
	person := Person{Name: "Alice", Age: 30, Address: "123 Main St"}
	fmt.Println("Person:", person)

	employee := Employee{
		Person: Person{Name: "Bob", Age: 25, Address: "456 Oak Ave"},
		Salary: 50000.0,
		ID:     1001,
	}
	fmt.Println("Employee:", employee)

	// Interface usage
	var speaker Speaker = Human{name: "Charlie"}
	fmt.Println(speaker.Speak())

	// Methods
	counter := Counter{count: 0}
	counter.Increment()
	fmt.Println("Counter:", counter.GetCount())

	// Pointer operations
	ptr := &a
	*ptr = 15
	fmt.Println("Modified a:", a)

	// Array, slice, map operations
	arrayOperations()
	sliceOperations()
	mapOperations()

	// Type assertions
	typeAssertions(42)
	typeAssertions("hello")
	typeAssertions(true)

	// Goroutines and channels
	go func() {
		fmt.Println("Goroutine says hello")
	}()

	channelOperations()

	// Reflection
	reflectionDemo()

	// Unsafe
	unsafeDemo()

	// Error handling
	if err := mayError(); err != nil {
		fmt.Println("Custom error:", err)
	}

	// Sync and goroutines with wait group
	var wg sync.WaitGroup
	jobs := make(chan int, 10)
	results := make(chan int, 10)

	// Start workers
	for i := 1; i <= 3; i++ {
		wg.Add(1)
		go worker(i, jobs, results, &wg)
	}

	// Send jobs
	for j := 1; j <= 5; j++ {
		jobs <- j
	}
	close(jobs)

	// Wait for workers to finish
	wg.Wait()
	close(results)

	// Collect results
	for result := range results {
		fmt.Println("Result:", result)
	}

	// Container generic type
	stringContainer := Container[string]{value: "generic value"}
	fmt.Println("Generic container:", stringContainer.GetValue())

	// Function as value
	add := getCalculator()
	fmt.Println("Function as value:", add(5, 3))

	// Constants
	fmt.Println("Constants:", MaxInt, TrueConst, FalseConst)

	// Global variables
	fmt.Println("Global variables:", globalInt, globalFloat, globalString, globalBool)

	// Math operations
	fmt.Println("Math: sqrt(16) =", math.Sqrt(16))
	fmt.Println("Math: pow(2, 3) =", math.Pow(2, 3))

	// Random numbers
	rand.Seed(time.Now().UnixNano())
	fmt.Println("Random:", rand.Intn(100))

	// String operations
	text := "Hello, Go!"
	fmt.Println("String operations:")
	fmt.Println("  Length:", len(text))
	fmt.Println("  Contains Go:", strings.Contains(text, "Go"))
	fmt.Println("  To upper:", strings.ToUpper(text))
	fmt.Println("  Split:", strings.Split(text, ", "))

	// Runtime info
	fmt.Println("Runtime:")
	fmt.Println("  GOOS:", runtime.GOOS)
	fmt.Println("  GOARCH:", runtime.GOARCH)
	fmt.Println("  NumGoroutine:", runtime.NumGoroutine())

	// File operations
	content := []byte("File content example")
	err = os.WriteFile("test.txt", content, 0644)
	if err != nil {
		fmt.Println("Error writing file:", err)
	} else {
		fmt.Println("File written successfully")
	}

	// Safe operation with recovery
	fmt.Print("Safe operation: ")
	safeOperation()

	// Final message
	fmt.Println("All Go syntax demonstrated successfully!")
}