package main

import (
	"fmt"
	"sync"
	"time"
	"context"
	"errors"
	"io"
	"os"
	"reflect"
	"unsafe"
)

// ===== BASIC TYPES AND VARIABLES =====

// Basic types
var (
	intVar     int       = 42
	int8Var    int8      = 127
	int16Var   int16     = 32767
	int32Var   int32     = 2147483647
	int64Var   int64     = 9223372036854775807
	uintVar    uint      = 42
	uint8Var   uint8     = 255
	uint16Var  uint16    = 65535
	uint32Var  uint32    = 4294967295
	uint64Var  uint64    = 18446744073709551615
	float32Var float32   = 3.1415926535
	float64Var float64   = 3.141592653589793
	complex64Var complex64 = 1 + 2i
	complex128Var complex128 = 1 + 2i
	stringVar  string    = "Hello, World!"
	runeVar    rune      = 'A'
	boolVar    bool      = true
	byteVar    byte      = 255
)

// Type inference
var (
	inferredInt    = 42
	inferredFloat  = 3.14
	inferredString = "inferred"
	inferredBool   = true
)

// Constants
const (
	MaxInt = int(^uint(0) >> 1)
	MinInt = -MaxInt - 1
	Pi     = 3.141592653589793
)

// ===== FUNCTIONS =====

// Function with multiple return values
func divide(a, b int) (int, error) {
	if b == 0 {
		return 0, errors.New("division by zero")
	}
	return a / b, nil
}

// Function with named return values
func split(sum int) (x, y int) {
	x = sum * 4 / 9
	y = sum - x
	return
}

// Variadic function
func sum(numbers ...int) int {
	total := 0
	for _, num := range numbers {
		total += num
	}
	return total
}

// Function as value
type BinOp func(int, int) int

func add(a, b int) int { return a + b }
func sub(a, b int) int { return a - b }

func apply(op BinOp, a, b int) int {
	return op(a, b)
}

// Anonymous function
func getOperator(op string) BinOp {
	switch op {
	case "+":
		return func(a, b int) int { return a + b }
	case "-":
		return func(a, b int) int { return a - b }
	case "*":
		return func(a, b int) int { return a * b }
	case "/":
		return func(a, b int) int { return a / b }
	default:
		return func(a, b int) int { return 0 }
	}
}

// ===== CONTROL STRUCTURES =====

func testControlStructures() {
	// If statements
	if x := 42; x > 0 {
		fmt.Println("x is positive")
	} else if x < 0 {
		fmt.Println("x is negative")
	} else {
		fmt.Println("x is zero")
	}

	// Switch statement
	switch day := "Monday"; day {
	case "Monday", "Tuesday", "Wednesday", "Thursday", "Friday":
		fmt.Println("Weekday")
	case "Saturday", "Sunday":
		fmt.Println("Weekend")
	default:
		fmt.Println("Unknown day")
	}

	// Type switch
	var val interface{} = 42
	switch v := val.(type) {
	case int:
		fmt.Printf("Integer: %d\n", v)
	case string:
		fmt.Printf("String: %s\n", v)
	case bool:
		fmt.Printf("Boolean: %t\n", v)
	default:
		fmt.Printf("Unknown type: %T\n", v)
	}

	// For loop (classic)
	for i := 0; i < 5; i++ {
		fmt.Printf("Classic loop: %d\n", i)
	}

	// For loop (while-style)
	i := 0
	for i < 5 {
		fmt.Printf("While loop: %d\n", i)
		i++
	}

	// For loop (infinite)
	// for {
	//     fmt.Println("Infinite loop")
	// }

	// For loop with range
	numbers := []int{1, 2, 3, 4, 5}
	for idx, num := range numbers {
		fmt.Printf("Range loop: index=%d, value=%d\n", idx, num)
	}

	// Range over string
	for i, r := range "hello" {
		fmt.Printf("String range: index=%d, rune=%c\n", i, r)
	}

	// Range over map
	m := map[string]int{"a": 1, "b": 2, "c": 3}
	for k, v := range m {
		fmt.Printf("Map range: key=%s, value=%d\n", k, v)
	}

	// Range over channel
	ch := make(chan int)
	go func() {
		for i := 1; i <= 3; i++ {
			ch <- i
		}
		close(ch)
	}()
	for val := range ch {
		fmt.Printf("Channel range: %d\n", val)
	}

	// Defer, panic, recover
	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("Recovered from panic: %v\n", r)
		}
	}()

	// panic("This will be recovered")
}

// ===== DATA STRUCTURES =====

// Arrays
var fixedArray [5]int = [5]int{1, 2, 3, 4, 5}

// Slices
func testSlices() {
	// Slice creation
	var slice1 []int
	slice2 := []int{1, 2, 3, 4, 5}
	slice3 := make([]int, 5)
	slice4 := make([]int, 0, 5) // len=0, cap=5

	// Slice operations
	slice2 = append(slice2, 6, 7, 8)
	subSlice := slice2[1:4]
	slice2[0] = 100

	// Copy
	dest := make([]int, len(slice2))
	copy(dest, slice2)

	// Slice expressions with capacity
	slice5 := slice2[2:4:6] // low=2, high=4, max=6

	// Multi-dimensional slices
	matrix := [][]int{
		{1, 2, 3},
		{4, 5, 6},
		{7, 8, 9},
	}
}

// Maps
func testMaps() {
	// Map creation
	var map1 map[string]int
	map2 := make(map[string]int)
	map3 := map[string]int{
		"apple":  1,
		"banana": 2,
		"cherry": 3,
	}

	// Map operations
	map2["key1"] = 100
	value := map2["key1"]
	delete(map2, "key1")

	// Check if key exists
	if val, exists := map3["apple"]; exists {
		fmt.Printf("Apple value: %d\n", val)
	}

	// Map iteration
	for key, val := range map3 {
		fmt.Printf("Map: %s = %d\n", key, val)
	}
}

// Structs
type Person struct {
	Name    string
	Age     int
	Address *Address
}

type Address struct {
	Street string
	City   string
}

func testStructs() {
	// Struct creation
	person := Person{
		Name: "John Doe",
		Age:  30,
		Address: &Address{
			Street: "123 Main St",
			City:   "Anytown",
		},
	}

	// Struct methods
	fmt.Printf("Person: %s, age %d\n", person.Name, person.Age)
	person.Greet()
	person.Birthday()
}

// Struct methods
func (p *Person) Greet() {
	fmt.Printf("Hello, my name is %s\n", p.Name)
}

func (p *Person) Birthday() {
	p.Age++
	fmt.Printf("Happy birthday! Now %d years old\n", p.Age)
}

// Struct with tags
type User struct {
	ID        int    `json:"id" db:"user_id"`
	Username  string `json:"username" db:"username"`
	Email     string `json:"email" db:"email"`
	CreatedAt string `json:"created_at" db:"created_at"`
}

// Interfaces
type Writer interface {
	Write([]byte) (int, error)
}

type Reader interface {
	Read([]byte) (int, error)
}

type ReadWriter interface {
	Writer
	Reader
}

// Custom type implementing interface
type ConsoleWriter struct{}

func (cw ConsoleWriter) Write(data []byte) (int, error) {
	fmt.Print(string(data))
	return len(data), nil
}

func testInterfaces() {
	var w Writer = ConsoleWriter{}
	w.Write([]byte("Hello from interface!\n"))
}

// Type embedding
type Animal struct {
	Name string
}

func (a *Animal) Speak() {
	fmt.Printf("%s makes a sound\n", a.Name)
}

type Dog struct {
	Animal  // Embedded type
	Breed string
}

func (d *Dog) Speak() {
	fmt.Printf("%s barks\n", d.Name)
}

func testEmbedding() {
	dog := Dog{
		Animal: Animal{Name: "Buddy"},
		Breed: "Golden Retriever",
	}

	dog.Speak() // Calls Dog's Speak method
	dog.Animal.Speak() // Calls Animal's Speak method
}

// ===== POINTERS =====

func testPointers() {
	x := 42
	p := &x
	fmt.Printf("Value: %d, Pointer: %p, Dereferenced: %d\n", x, p, *p)

	*p = 100
	fmt.Printf("Modified value: %d\n", x)

	// Pointer to struct
	person := &Person{Name: "Alice", Age: 25}
	person.Name = "Alice Smith"
	person.Greet()
}

// ===== CONCURRENCY =====

func testConcurrency() {
	// Goroutines
	go func() {
		fmt.Println("Goroutine 1")
	}()

	go func(msg string) {
		fmt.Printf("Goroutine 2: %s\n", msg)
	}("Hello from goroutine 2")

	// Channels
	ch := make(chan int)
	go func() {
		ch <- 42
	}()
	value := <-ch
	fmt.Printf("Received from channel: %d\n", value)

	// Buffered channels
	bufCh := make(chan int, 3)
	bufCh <- 1
	bufCh <- 2
	bufCh <- 3

	// Select statement
	select {
	case msg1 := <-ch:
		fmt.Printf("Received: %d\n", msg1)
	case msg2 := <-bufCh:
		fmt.Printf("Received from buffer: %d\n", msg2)
	default:
		fmt.Println("No messages available")
	}

	// Channel directions
	sendOnly := make(chan<- int)
	recvOnly := make(<-chan int)

	// Mutex
	var mu sync.Mutex
	var counter int

	go func() {
		mu.Lock()
		counter++
		mu.Unlock()
	}()

	// WaitGroup
	var wg sync.WaitGroup
	for i := 0; i < 3; i++ {
		wg.Add(1)
		go func(id int) {
			defer wg.Done()
			fmt.Printf("Worker %d starting\n", id)
			time.Sleep(time.Millisecond * 100)
			fmt.Printf("Worker %d done\n", id)
		}(i)
	}
	wg.Wait()

	// Once
	var once sync.Once
	once.Do(func() {
		fmt.Println("This runs only once")
	})
	once.Do(func() {
		fmt.Println("This won't run")
	})

	// Context
	ctx, cancel := context.WithTimeout(context.Background(), time.Millisecond*500)
	defer cancel()

	go func() {
		select {
		case <-time.After(time.Second):
			fmt.Println("Operation completed")
		case <-ctx.Done():
			fmt.Println("Operation timed out")
		}
	}()

	// Atomic operations
	var atomicCounter int64
	go func() {
		for i := 0; i < 1000; i++ {
			// atomic.AddInt64(&atomicCounter, 1)
		}
	}()
}

// ===== ERROR HANDLING =====

func testErrorHandling() {
	// Basic error handling
	result, err := divide(10, 2)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("Result: %d\n", result)
	}

	// Custom error type
	type MyError struct {
		Message string
		Code    int
	}

	func (e *MyError) Error() string {
		return fmt.Sprintf("Error %d: %s", e.Code, e.Message)
	}

	// Error wrapping
	func process() error {
		if err := divide(10, 0); err != nil {
			return fmt.Errorf("process failed: %w", err)
		}
		return nil
	}

	// Multiple errors
	var errs []error
	errs = append(errs, errors.New("first error"))
	errs = append(errs, errors.New("second error"))

	// Combine errors
	combined := errors.Join(errs...)
	fmt.Printf("Combined error: %v\n", combined)
}

// ===== REFLECTION =====

func testReflection() {
	// Type information
	var x int = 42
	t := reflect.TypeOf(x)
	v := reflect.ValueOf(x)

	fmt.Printf("Type: %s, Kind: %s, Value: %v\n", t, t.Kind(), v)

	// Struct reflection
	person := Person{Name: "Bob", Age: 35}
	personType := reflect.TypeOf(person)
	personValue := reflect.ValueOf(person)

	for i := 0; i < personType.NumField(); i++ {
		field := personType.Field(i)
		value := personValue.Field(i)
		fmt.Printf("Field: %s, Type: %s, Value: %v\n", field.Name, field.Type, value)
	}

	// Create value through reflection
	newInt := reflect.New(reflect.TypeOf(42)).Elem()
	newInt.SetInt(100)
	fmt.Printf("Created value: %v\n", newInt.Int())

	// Call function through reflection
	fn := reflect.ValueOf(add)
	args := []reflect.Value{reflect.ValueOf(10), reflect.ValueOf(20)}
	result := fn.Call(args)
	fmt.Printf("Function call result: %v\n", result[0].Int())
}

// ===== UNSAFE OPERATIONS =====

func testUnsafe() {
	var x int64 = 42
	p := unsafe.Pointer(&x)
	p2 := (*int32)(p)

	fmt.Printf("Original: %d, Through pointer: %d\n", x, *p2)

	// Sizeof
	var y int
	fmt.Printf("Size of int: %d\n", unsafe.Sizeof(y))

	// Alignof
	fmt.Printf("Alignment of int: %d\n", unsafe.Alignof(y))

	// Offsetof
	type MyStruct struct {
		a int
		b int
		c int
	}
	var s MyStruct
	fmt.Printf("Offset of b: %d\n", unsafe.Offsetof(s.b))
}

// ===== GENERIC TYPES (Go 1.18+) =====

// Generic function
func PrintSlice[T any](s []T) {
	for _, v := range s {
		fmt.Print(v, " ")
	}
	fmt.Println()
}

// Generic struct
type Stack[T any] struct {
	items []T
}

func (s *Stack[T]) Push(item T) {
	s.items = append(s.items, item)
}

func (s *Stack[T]) Pop() (T, bool) {
	if len(s.items) == 0 {
		var zero T
		return zero, false
	}
	item := s.items[len(s.items)-1]
	s.items = s.items[:len(s.items)-1]
	return item, true
}

// Generic interface
type Number interface {
	int | int8 | int16 | int32 | int64 | uint | uint8 | uint16 | uint32 | uint64 | float32 | float64
}

func AddNumbers[T Number](a, b T) T {
	return a + b
}

func testGenerics() {
	// Generic function
	PrintSlice([]int{1, 2, 3})
	PrintSlice([]string{"a", "b", "c"})

	// Generic struct
	stack := Stack[string]{}
	stack.Push("hello")
	stack.Push("world")
	if item, ok := stack.Pop(); ok {
		fmt.Printf("Popped: %s\n", item)
	}

	// Generic with constraint
	fmt.Printf("Add int: %d\n", AddNumbers(10, 20))
	fmt.Printf("Add float: %f\n", AddNumbers(3.14, 2.86))
}

// ===== MAIN FUNCTION =====

func main() {
	fmt.Println("=== Comprehensive Go Feature Test ===")

	fmt.Println("\n1. Testing Variables and Types:")
	fmt.Printf("Int: %d, Float: %f, String: %s, Bool: %t\n", intVar, float64Var, stringVar, boolVar)

	fmt.Println("\n2. Testing Functions:")
	result, err := divide(10, 2)
	if err == nil {
		fmt.Printf("10 / 2 = %d\n", result)
	}
	fmt.Printf("Sum of 1,2,3,4,5: %d\n", sum(1, 2, 3, 4, 5))
	fmt.Printf("Apply add: %d\n", apply(add, 10, 20))
	fmt.Printf("Get operator '+': %d\n", getOperator("+")(10, 5))

	fmt.Println("\n3. Testing Control Structures:")
	testControlStructures()

	fmt.Println("\n4. Testing Data Structures:")
	testSlices()
	testMaps()
	testStructs()
	testInterfaces()
	testEmbedding()

	fmt.Println("\n5. Testing Pointers:")
	testPointers()

	fmt.Println("\n6. Testing Concurrency:")
	testConcurrency()

	fmt.Println("\n7. Testing Error Handling:")
	testErrorHandling()

	fmt.Println("\n8. Testing Reflection:")
	testReflection()

	fmt.Println("\n9. Testing Unsafe Operations:")
	testUnsafe()

	fmt.Println("\n10. Testing Generics:")
	testGenerics()

	fmt.Println("\n=== All Go features tested successfully! ===")
}