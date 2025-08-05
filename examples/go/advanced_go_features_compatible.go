package main

import (
	"fmt"
	"unsafe"
	"reflect"
	"sync"
	"context"
	"errors"
)

// ===== ADVANCED TYPES =====

// Function types
type Calculator func(int, int) int

func createCalculator(op string) Calculator {
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

// Channel types
type MessageChan chan string
type DataStream <-chan int
type CommandChan chan<- func()

// Method expressions
type Adder struct {
	value int
}

func (a *Adder) Add(b int) int {
	return a.value + b
}

func testMethodExpressions() {
	adder := Adder{value: 10}
	
	// Method value
	method := adder.Add
	result := method(5)
	fmt.Printf("Method value result: %d\n", result)
	
	// Method expression
	methodExpr := (*Adder).Add
	result2 := methodExpr(&adder, 15)
	fmt.Printf("Method expression result: %d\n", result2)
}

// ===== ADVANCED STRUCTURES =====

// Struct with function fields
type Handler struct {
	Process func(string) string
	Validate func(string) bool
}

func testStructWithFunctions() {
	handler := Handler{
		Process: func(s string) string {
			return "Processed: " + s
		},
		Validate: func(s string) bool {
			return len(s) > 0
		},
	}
	
	if handler.Validate("test") {
		result := handler.Process("test")
		fmt.Printf("Handler result: %s\n", result)
	}
}

// Anonymous struct
func testAnonymousStruct() {
	person := struct {
		Name string
		Age  int
	}{
		Name: "Anonymous",
		Age:  99,
	}
	
	fmt.Printf("Anonymous struct: %+v\n", person)
}

// Empty interface
func testEmptyInterface() {
	var anything interface{}
	
	anything = 42
	fmt.Printf("As int: %v\n", anything)
	
	anything = "hello"
	fmt.Printf("As string: %v\n", anything)
	
	anything = true
	fmt.Printf("As bool: %v\n", anything)
	
	// Type assertion
	if s, ok := anything.(string); ok {
		fmt.Printf("Type assertion successful: %s\n", s)
	}
	
	// Type switch
	switch v := anything.(type) {
	case int:
		fmt.Printf("It's an int: %d\n", v)
	case string:
		fmt.Printf("It's a string: %s\n", v)
	case bool:
		fmt.Printf("It's a bool: %t\n", v)
	default:
		fmt.Printf("Unknown type: %T\n", v)
	}
}

// ===== ADVANCED INTERFACES =====

// Interface embedding
type ReaderWriter interface {
	Read([]byte) (int, error)
	Write([]byte) (int, error)
}

type Closer interface {
	Close() error
}

type ReadWriteCloser interface {
	ReaderWriter
	Closer
}

// Concrete implementation
type File struct {
	name string
	data []byte
	pos  int
}

func (f *File) Read(p []byte) (int, error) {
	if f.pos >= len(f.data) {
		return 0, fmt.Errorf("EOF")
	}
	
	n := copy(p, f.data[f.pos:])
	f.pos += n
	return n, nil
}

func (f *File) Write(p []byte) (int, error) {
	if f.pos+len(p) > len(f.data) {
		newData := make([]byte, f.pos+len(p))
		copy(newData, f.data)
		f.data = newData
	}
	
	n := copy(f.data[f.pos:], p)
	f.pos += n
	return n, nil
}

func (f *File) Close() error {
	fmt.Printf("File %s closed\n", f.name)
	return nil
}

func testAdvancedInterfaces() {
	file := &File{name: "test.txt", data: []byte("Hello, World!")}
	
	var rw ReaderWriter = file
	data := make([]byte, 5)
	n, err := rw.Read(data)
	if err == nil {
		fmt.Printf("Read %d bytes: %s\n", n, string(data))
	}
	
	var rwc ReadWriteCloser = file
	rwc.Write([]byte(" Go!"))
	rwc.Close()
}

// ===== ADVANCED CONCURRENCY =====

// Buffered channel with select
func testBufferedChannelSelect() {
	ch := make(chan int, 3)
	
	// Producer
	go func() {
		for i := 1; i <= 5; i++ {
			ch <- i
			fmt.Printf("Sent: %d\n", i)
		}
		close(ch)
	}()
	
	// Consumer with timeout
	for i := 0; i < 5; i++ {
		select {
		case val, ok := <-ch:
			if !ok {
				fmt.Println("Channel closed")
				return
			}
			fmt.Printf("Received: %d\n", val)
		default:
			fmt.Println("No data available")
		}
	}
}

// Worker pool with context
type WorkerPool struct {
	tasks   chan func()
	workers int
	wg      sync.WaitGroup
	ctx     context.Context
	cancel  context.CancelFunc
}

func NewWorkerPool(workers int) *WorkerPool {
	ctx, cancel := context.WithCancel(context.Background())
	return &WorkerPool{
		tasks:   make(chan func(), workers*2),
		workers: workers,
		ctx:     ctx,
		cancel:  cancel,
	}
}

func (wp *WorkerPool) Start() {
	for i := 0; i < wp.workers; i++ {
		wp.wg.Add(1)
		go wp.worker(i)
	}
}

func (wp *WorkerPool) worker(id int) {
	defer wp.wg.Done()
	
	for {
		select {
		case task, ok := <-wp.tasks:
			if !ok {
				return
			}
			task()
		case <-wp.ctx.Done():
			return
		}
	}
}

func (wp *WorkerPool) Submit(task func()) bool {
	select {
	case wp.tasks <- task:
		return true
	case <-wp.ctx.Done():
		return false
	}
}

func (wp *WorkerPool) Stop() {
	wp.cancel()
	close(wp.tasks)
	wp.wg.Wait()
}

func testWorkerPool() {
	pool := NewWorkerPool(3)
	pool.Start()
	
	// Submit tasks
	for i := 1; i <= 10; i++ {
		taskID := i
		pool.Submit(func() {
			fmt.Printf("Processing task %d\n", taskID)
		})
	}
	
	pool.Stop()
}

// ===== ADVANCED ERROR HANDLING =====

// Custom error types with multiple fields
type DetailedError struct {
	Code    int
	Message string
	Details map[string]interface{}
}

func (e *DetailedError) Error() string {
	return fmt.Sprintf("Error %d: %s", e.Code, e.Message)
}

func (e *DetailedError) Is(target error) bool {
	if other, ok := target.(*DetailedError); ok {
		return e.Code == other.Code
	}
	return false
}

func testCustomErrors() {
	err := &DetailedError{
		Code:    404,
		Message: "Not found",
		Details: map[string]interface{}{"path": "/api/users"},
	}
	
	fmt.Printf("Custom error: %v\n", err)
	
	// Error wrapping
	wrapped := fmt.Errorf("request failed: %w", err)
	fmt.Printf("Wrapped error: %v\n", wrapped)
	
	// Error unwrapping
	if unwrapped := errors.Unwrap(wrapped); unwrapped != nil {
		fmt.Printf("Unwrapped: %v\n", unwrapped)
	}
	
	// Error checking
	if errors.Is(wrapped, err) {
		fmt.Println("Errors match")
	}
}

// Panic and recover with context
func safeOperation() {
	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("Recovered from panic: %v\n", r)
		}
	}()
	
	// This will panic
	panic("intentional panic")
}

func testPanicRecover() {
	fmt.Println("Before panic")
	safeOperation()
	fmt.Println("After panic recovery")
}

// ===== ADVANCED REFLECTION =====

// Deep reflection
func testDeepReflection() {
	// Create a struct through reflection
	typ := reflect.TypeOf(Person{})
	val := reflect.New(typ).Elem()
	
	// Set field values
	val.FieldByName("Name").SetString("Reflected Person")
	val.FieldByName("Age").SetInt(25)
	
	fmt.Printf("Created struct: %+v\n", val.Interface())
	
	// Call method through reflection
	method := val.MethodByName("Greet")
	if method.IsValid() {
		args := []reflect.Value{}
		method.Call(args)
	}
	
	// Create slice through reflection
	sliceType := reflect.TypeOf([]int{})
	sliceValue := reflect.MakeSlice(sliceType, 5, 10)
	
	for i := 0; i < 5; i++ {
		sliceValue.Index(i).SetInt(reflect.ValueOf(i).Int())
	}
	
	fmt.Printf("Created slice: %v\n", sliceValue.Interface())
}

// Struct tags reflection
func testStructTags() {
	user := User{
		ID:        1,
		Username:  "john_doe",
		Email:     "john@example.com",
		CreatedAt: "2023-01-01",
	}
	
	typ := reflect.TypeOf(user)
	
	for i := 0; i < typ.NumField(); i++ {
		field := typ.Field(i)
		jsonTag := field.Tag.Get("json")
		dbTag := field.Tag.Get("db")
		
		fmt.Printf("Field: %s, JSON: %s, DB: %s\n", 
			field.Name, jsonTag, dbTag)
	}
}

// ===== ADVANCED UNSAFE =====

// Pointer arithmetic simulation
func testUnsafePointerArithmetic() {
	arr := [5]int{10, 20, 30, 40, 50}
	
	// Get pointer to first element
	ptr := unsafe.Pointer(&arr[0])
	
	// "Pointer arithmetic" by converting to different pointer types
	intPtr := (*int)(ptr)
	fmt.Printf("First element: %d\n", *intPtr)
	
	// Move to next element by adding size of int
	nextPtr := unsafe.Pointer(uintptr(ptr) + unsafe.Sizeof(arr[0]))
	nextIntPtr := (*int)(nextPtr)
	fmt.Printf("Second element: %d\n", *nextIntPtr)
}

// Type punning with unsafe
func testTypePunning() {
	var f float32 = 3.14
	
	// Convert float32 to int32 bit pattern
	intPtr := (*int32)(unsafe.Pointer(&f))
	intValue := *intPtr
	
	fmt.Printf("Float32: %f, Int32 bits: %d\n", f, intValue)
	
	// Convert back
	floatPtr := (*float32)(unsafe.Pointer(&intValue))
	floatValue := *floatPtr
	
	fmt.Printf("Converted back: %f\n", floatValue)
}

// ===== SIMPLE GENERICS (without ~ constraint) =====

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

// Simple generic constraint using basic types
type Number interface {
	int | int8 | int16 | int32 | int64 | 
	uint | uint8 | uint16 | uint32 | uint64 | 
	float32 | float64
}

func AddNumbers[T Number](a, b T) T {
	return a + b
}

func testSimpleGenerics() {
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
	fmt.Println("=== Advanced Go Feature Test ===")
	
	fmt.Println("\n1. Testing Method Expressions:")
	testMethodExpressions()
	
	fmt.Println("\n2. Testing Struct with Functions:")
	testStructWithFunctions()
	
	fmt.Println("\n3. Testing Anonymous Struct:")
	testAnonymousStruct()
	
	fmt.Println("\n4. Testing Empty Interface:")
	testEmptyInterface()
	
	fmt.Println("\n5. Testing Advanced Interfaces:")
	testAdvancedInterfaces()
	
	fmt.Println("\n6. Testing Buffered Channel Select:")
	testBufferedChannelSelect()
	
	fmt.Println("\n7. Testing Worker Pool:")
	testWorkerPool()
	
	fmt.Println("\n8. Testing Custom Errors:")
	testCustomErrors()
	
	fmt.Println("\n9. Testing Panic and Recover:")
	testPanicRecover()
	
	fmt.Println("\n10. Testing Deep Reflection:")
	testDeepReflection()
	
	fmt.Println("\n11. Testing Struct Tags:")
	testStructTags()
	
	fmt.Println("\n12. Testing Unsafe Pointer Arithmetic:")
	testUnsafePointerArithmetic()
	
	fmt.Println("\n13. Testing Type Punning:")
	testTypePunning()
	
	fmt.Println("\n14. Testing Simple Generics:")
	testSimpleGenerics()
	
	fmt.Println("\n=== Advanced Go features tested successfully! ===")
}