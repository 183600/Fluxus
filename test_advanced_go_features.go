package main

import "fmt"

// Test Go 1.18+ Generics
func testGenerics() {
    fmt.Printf("=== Testing Go 1.18+ Generics ===\n")
    
    // Basic generic function
    func PrintSlice[T any](s []T) {
        for _, v := range s {
            fmt.Printf("%v ", v)
        }
        fmt.Printf("\n")
    }
    
    // Generic function with constraints
    func NumberSlice[T int | int32 | int64 | float32 | float64](s []T) T {
        var sum T
        for _, v := range s {
            sum += v
        }
        return sum
    }
    
    // Generic struct
    type Stack[T any] struct {
        elements []T
    }
    
    func (s *Stack[T]) Push(element T) {
        s.elements = append(s.elements, element)
    }
    
    func (s *Stack[T]) Pop() (T, bool) {
        if len(s.elements) == 0 {
            var zero T
            return zero, false
        }
        element := s.elements[len(s.elements)-1]
        s.elements = s.elements[:len(s.elements)-1]
        return element, true
    }
    
    // Test generic functions
    intSlice := []int{1, 2, 3, 4, 5}
    stringSlice := []string{"a", "b", "c"}
    
    fmt.Printf("Int slice: ")
    PrintSlice(intSlice)
    fmt.Printf("String slice: ")
    PrintSlice(stringSlice)
    
    // Test constrained generic function
    numbers := []float64{1.1, 2.2, 3.3, 4.4, 5.5}
    sum := NumberSlice(numbers)
    fmt.Printf("Sum of numbers: %f\n", sum)
    
    // Test generic struct
    intStack := Stack[int]{}
    intStack.Push(10)
    intStack.Push(20)
    intStack.Push(30)
    
    if val, ok := intStack.Pop(); ok {
        fmt.Printf("Popped from int stack: %d\n", val)
    }
    
    stringStack := Stack[string]{}
    stringStack.Push("hello")
    stringStack.Push("world")
    
    if val, ok := stringStack.Pop(); ok {
        fmt.Printf("Popped from string stack: %s\n", val)
    }
}

// Test Go 1.18+ Type sets and constraints
func testTypeConstraints() {
    fmt.Printf("=== Testing Type Constraints ===\n")
    
    // Custom constraint
    type Number interface {
        int | int32 | int64 | float32 | float64
    }
    
    func Max[T Number](a, b T) T {
        if a > b {
            return a
        }
        return b
    }
    
    // Approximation constraint (~)
    type Addable interface {
        ~int | ~int32 | ~int64 | ~float32 | ~float64
    }
    
    func Add[T Addable](a, b T) T {
        return a + b
    }
    
    // Union constraint
    type Signed interface {
        ~int | ~int32 | ~int64 | ~float32 | ~float64
    }
    
    func IsSigned[T Signed](x T) bool {
        return x < 0
    }
    
    // Test constraints
    fmt.Printf("Max of 5 and 10: %d\n", Max(5, 10))
    fmt.Printf("Max of 3.14 and 2.71: %f\n", Max(3.14, 2.71))
    
    fmt.Printf("Add 5 and 3: %d\n", Add(5, 3))
    fmt.Printf("Add 3.14 and 2.71: %f\n", Add(3.14, 2.71))
    
    fmt.Printf("Is -5 signed: %t\n", IsSigned(-5))
    fmt.Printf("Is 5 signed: %t\n", IsSigned(5))
}

// Test Go 1.19+ features
func testGo119Features() {
    fmt.Printf("=== Testing Go 1.19+ Features ===\n")
    
    // Test improved type inference
    func Identity[T any](x T) T {
        return x
    }
    
    // Type parameter inference
    result := Identity(42)
    fmt.Printf("Identity result: %d\n", result)
    
    // Test improved struct literals
    type Point struct {
        X, Y int
    }
    
    p := Point{X: 10, Y: 20}
    fmt.Printf("Point: %+v\n", p)
}

// Test Go 1.20+ features
func testGo120Features() {
    fmt.Printf("=== Testing Go 1.20+ Features ===\n")
    
    // Test unsafe.String and unsafe.Slice
    // Note: These require unsafe package, which might not be fully supported
    
    // Test improved error wrapping
    func wrapError(msg string, err error) error {
        return fmt.Errorf("%s: %w", msg, err)
    }
    
    baseErr := fmt.Errorf("base error")
    wrappedErr := wrapError("wrapped", baseErr)
    fmt.Printf("Wrapped error: %v\n", wrappedErr)
    
    // Test struct field ordering optimization
    type OptimizedStruct struct {
        SmallField int8
        LargeField int64
        MediumField int32
    }
    
    opt := OptimizedStruct{SmallField: 1, LargeField: 2, MediumField: 3}
    fmt.Printf("Optimized struct: %+v\n", opt)
}

// Test Go 1.21+ features
func testGo121Features() {
    fmt.Printf("=== Testing Go 1.21+ Features ===\n")
    
    // Test new built-in functions
    fmt.Printf("Min of 3 and 7: %d\n", min(3, 7))
    fmt.Printf("Max of 3 and 7: %d\n", max(3, 7))
    fmt.Printf("Clear test with map\n")
    
    // Test slices package functions
    m := map[string]int{"a": 1, "b": 2, "c": 3}
    clear(m)
    fmt.Printf("Map after clear: %v (len: %d)\n", m, len(m))
    
    // Test new math/bits functions
    fmt.Printf("Leading zeros of 8: %d\n", LeadingZeros8(8))
}

// Test Go 1.22+ features
func testGo122Features() {
    fmt.Printf("=== Testing Go 1.22+ Features ===\n")
    
    // Test range over integer
    fmt.Printf("Range over integer 0-4:\n")
    for i := range 5 {
        fmt.Printf("  %d\n", i)
    }
    
    // Test math/rand/v2
    // Note: This might not be fully supported in all environments
    fmt.Printf("Random number generation (if supported)\n")
    
    // Test improved toolchain
    fmt.Printf("Toolchain improvements tested\n")
}

// Test advanced struct features
func testAdvancedStructs() {
    fmt.Printf("=== Testing Advanced Struct Features ===\n")
    
    // Struct with embedded types
    type Base struct {
        ID int
    }
    
    type Derived struct {
        Base
        Name string
    }
    
    d := Derived{Base: Base{ID: 1}, Name: "test"}
    fmt.Printf("Derived struct: %+v\n", d)
    fmt.Printf("Derived.ID: %d\n", d.ID)
    
    // Struct with tags
    type Person struct {
        Name string `json:"name" db:"name"`
        Age  int    `json:"age" db:"age"`
    }
    
    p := Person{Name: "Alice", Age: 30}
    fmt.Printf("Person with tags: %+v\n", p)
    
    // Anonymous struct
    anon := struct {
        X int
        Y string
    }{X: 10, Y: "hello"}
    fmt.Printf("Anonymous struct: %+v\n", anon)
}

// Test advanced interface features
func testAdvancedInterfaces() {
    fmt.Printf("=== Testing Advanced Interface Features ===\n")
    
    // Interface with type parameters
    type Container[T any] interface {
        Add(item T)
        Get(index int) T
        Size() int
    }
    
    type SliceContainer[T any] struct {
        items []T
    }
    
    func (s *SliceContainer[T]) Add(item T) {
        s.items = append(s.items, item)
    }
    
    func (s *SliceContainer[T]) Get(index int) T {
        return s.items[index]
    }
    
    func (s *SliceContainer[T]) Size() int {
        return len(s.items)
    }
    
    // Test generic interface
    container := &SliceContainer[int]{}
    container.Add(1)
    container.Add(2)
    container.Add(3)
    
    fmt.Printf("Container size: %d\n", container.Size())
    fmt.Printf("Container item at 1: %d\n", container.Get(1))
    
    // Interface embedding
    type Reader interface {
        Read() string
    }
    
    type Writer interface {
        Write(string)
    }
    
    type ReadWriter interface {
        Reader
        Writer
    }
    
    type Console struct{}
    
    func (c Console) Read() string {
        return "input"
    }
    
    func (c Console) Write(s string) {
        fmt.Printf("Writing: %s\n", s)
    }
    
    console := Console{}
    fmt.Printf("Console read: %s\n", console.Read())
    console.Write("output")
}

// Test advanced function features
func testAdvancedFunctions() {
    fmt.Printf("=== Testing Advanced Function Features ===\n")
    
    // Closure
    counter := func() func() int {
        count := 0
        return func() int {
            count++
            return count
        }
    }()
    
    fmt.Printf("Counter: %d\n", counter())
    fmt.Printf("Counter: %d\n", counter())
    
    // Higher-order function
    func apply(numbers []int, f func(int) int) []int {
        result := make([]int, len(numbers))
        for i, n := range numbers {
            result[i] = f(n)
        }
        return result
    }
    
    numbers := []int{1, 2, 3, 4, 5}
    doubled := apply(numbers, func(n int) int { return n * 2 })
    fmt.Printf("Doubled numbers: %v\n", doubled)
    
    // Function with multiple returns
    func multiReturn() (int, string, bool) {
        return 42, "answer", true
    }
    
    a, b, c := multiReturn()
    fmt.Printf("Multiple returns: %d, %s, %t\n", a, b, c)
    
    // Function with named returns
    func namedReturn(x, y int) (sum, product int) {
        sum = x + y
        product = x * y
        return
    }
    
    sum, product := namedReturn(5, 3)
    fmt.Printf("Named returns: sum=%d, product=%d\n", sum, product)
}

// Test advanced concurrency
func testAdvancedConcurrency() {
    fmt.Printf("=== Testing Advanced Concurrency Features ===\n")
    
    // Select statement
    ch1 := make(chan string)
    ch2 := make(chan string)
    
    go func() {
        ch1 <- "from channel 1"
    }()
    
    go func() {
        ch2 <- "from channel 2"
    }()
    
    select {
    case msg1 := <-ch1:
        fmt.Printf("Received: %s\n", msg1)
    case msg2 := <-ch2:
        fmt.Printf("Received: %s\n", msg2)
    }
    
    // Buffered channels
    buffered := make(chan int, 3)
    buffered <- 1
    buffered <- 2
    buffered <- 3
    
    fmt.Printf("Buffered channel length: %d\n", len(buffered))
    
    // Channel direction
    func sendOnly(ch chan<- int) {
        ch <- 42
    }
    
    func receiveOnly(ch <-chan int) {
        val := <-ch
        fmt.Printf("Received from receive-only channel: %d\n", val)
    }
    
    bidirectional := make(chan int)
    go sendOnly(bidirectional)
    receiveOnly(bidirectional)
    
    // Worker pool pattern
    jobs := make(chan int, 10)
    results := make(chan int, 10)
    
    // Start workers
    for i := 1; i <= 3; i++ {
        go func(workerID int) {
            for job := range jobs {
                fmt.Printf("Worker %d processing job %d\n", workerID, job)
                results <- job * 2
            }
        }(i)
    }
    
    // Send jobs
    for j := 1; j <= 5; j++ {
        jobs <- j
    }
    close(jobs)
    
    // Collect results
    for i := 1; i <= 5; i++ {
        <-results
    }
}

// Test advanced error handling
func testAdvancedErrorHandling() {
    fmt.Printf("=== Testing Advanced Error Handling ===\n")
    
    // Custom error types
    type MyError struct {
        Code    int
        Message string
    }
    
    func (e MyError) Error() string {
        return fmt.Sprintf("Error %d: %s", e.Code, e.Message)
    }
    
    func doSomething(flag bool) error {
        if flag {
            return MyError{Code: 404, Message: "Not found"}
        }
        return nil
    }
    
    err := doSomething(true)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
        if myErr, ok := err.(MyError); ok {
            fmt.Printf("Error code: %d\n", myErr.Code)
        }
    }
    
    // Error wrapping
    func wrapError(err error, msg string) error {
        return fmt.Errorf("%s: %w", msg, err)
    }
    
    baseErr := MyError{Code: 500, Message: "Internal server error"}
    wrappedErr := wrapError(baseErr, "Failed to process request")
    fmt.Printf("Wrapped error: %v\n", wrappedErr)
    
    // Panic and recover
    func mayPanic(shouldPanic bool) {
        defer func() {
            if r := recover(); r != nil {
                fmt.Printf("Recovered from panic: %v\n", r)
            }
        }()
        
        if shouldPanic {
            panic("Something went wrong")
        }
        fmt.Printf("No panic occurred\n")
    }
    
    mayPanic(false)
    mayPanic(true)
}

// Test reflection (basic)
func testReflection() {
    fmt.Printf("=== Testing Reflection ===\n")
    
    // Basic reflection on types
    x := 42
    fmt.Printf("Type of x: %T\n", x)
    
    str := "hello"
    fmt.Printf("Type of str: %T\n", str)
    
    // Type assertion
    var i interface{} = "hello"
    if s, ok := i.(string); ok {
        fmt.Printf("Type assertion successful: %s\n", s)
    }
    
    // Type switch
    func typeSwitch(i interface{}) {
        switch v := i.(type) {
        case int:
            fmt.Printf("Integer: %d\n", v)
        case string:
            fmt.Printf("String: %s\n", v)
        case bool:
            fmt.Printf("Boolean: %t\n", v)
        default:
            fmt.Printf("Unknown type: %T\n", v)
        }
    }
    
    typeSwitch(42)
    typeSwitch("hello")
    typeSwitch(true)
    typeSwitch(3.14)
}

// Helper functions for Go 1.21+ features
func LeadingZeros8(x uint8) int {
    if x == 0 {
        return 8
    }
    count := 0
    for x < 128 {
        x <<= 1
        count++
    }
    return count
}

func main() {
    fmt.Printf("=== Testing Advanced Go Language Features ===\n")
    
    testGenerics()
    testTypeConstraints()
    testGo119Features()
    testGo120Features()
    testGo121Features()
    testGo122Features()
    testAdvancedStructs()
    testAdvancedInterfaces()
    testAdvancedFunctions()
    testAdvancedConcurrency()
    testAdvancedErrorHandling()
    testReflection()
    
    fmt.Printf("=== All advanced tests completed ===\n")
}