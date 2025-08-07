package main

import "fmt"

// Test variables and types
func testVariables() {
    fmt.Println("=== Testing Variables and Types ===")
    
    // Basic types
    var x int = 42
    var y float64 = 3.14
    var z string = "Hello"
    var b bool = true
    
    fmt.Printf("int: %d, float64: %f, string: %s, bool: %t\n", x, y, z, b)
    
    // Type inference
    a := 100
    c := "World"
    d := 2.718
    
    fmt.Printf("inferred int: %d, string: %s, float64: %f\n", a, c, d)
    
    // Multiple variables
    var i, j int = 1, 2
    k, l := 3, 4
    
    fmt.Printf("multiple: %d, %d, %d, %d\n", i, j, k, l)
}

// Test control flow
func testControlFlow() {
    fmt.Println("\n=== Testing Control Flow ===")
    
    // If-else
    x := 10
    if x > 5 {
        fmt.Println("x is greater than 5")
    } else if x < 5 {
        fmt.Println("x is less than 5")
    } else {
        fmt.Println("x is 5")
    }
    
    // For loops
    fmt.Println("For loop:")
    for i := 0; i < 5; i++ {
        fmt.Printf("%d ", i)
    }
    fmt.Println()
    
    // While-style for loop
    fmt.Println("While-style for loop:")
    j := 0
    for j < 3 {
        fmt.Printf("%d ", j)
        j++
    }
    fmt.Println()
    
    // Infinite loop with break
    fmt.Println("For loop with break:")
    k := 0
    for {
        if k >= 3 {
            break
        }
        fmt.Printf("%d ", k)
        k++
    }
    fmt.Println()
    
    // Switch statement
    fmt.Println("Switch statement:")
    day := "Monday"
    switch day {
    case "Monday":
        fmt.Println("Start of week")
    case "Friday":
        fmt.Println("End of week")
    default:
        fmt.Println("Middle of week")
    }
}

// Test functions
func testFunctions() {
    fmt.Println("\n=== Testing Functions ===")
    
    // Function with parameters and return value
    add := func(a, b int) int {
        return a + b
    }
    
    result := add(5, 3)
    fmt.Printf("5 + 3 = %d\n", result)
    
    // Multiple return values
    divide := func(a, b int) (int, int) {
        return a / b, a % b
    }
    
    quotient, remainder := divide(10, 3)
    fmt.Printf("10 / 3 = %d remainder %d\n", quotient, remainder)
    
    // Named return values
    calculate := func(x, y int) (sum int, product int) {
        sum = x + y
        product = x * y
        return
    }
    
    s, p := calculate(4, 5)
    fmt.Printf("4 + 5 = %d, 4 * 5 = %d\n", s, p)
}

// Test arrays and slices
func testArraysAndSlices() {
    fmt.Println("\n=== Testing Arrays and Slices ===")
    
    // Array
    var arr [5]int = [5]int{1, 2, 3, 4, 5}
    fmt.Printf("Array: %v\n", arr)
    
    // Slice
    slice := []int{10, 20, 30, 40, 50}
    fmt.Printf("Slice: %v\n", slice)
    
    // Slice operations
    fmt.Printf("Slice[1:3]: %v\n", slice[1:3])
    fmt.Printf("Slice length: %d, capacity: %d\n", len(slice), cap(slice))
    
    // Append to slice
    slice = append(slice, 60)
    fmt.Printf("After append: %v\n", slice)
}

// Test maps
func testMaps() {
    fmt.Println("\n=== Testing Maps ===")
    
    // Map creation
    m := make(map[string]int)
    m["one"] = 1
    m["two"] = 2
    m["three"] = 3
    
    fmt.Printf("Map: %v\n", m)
    
    // Map operations
    value, exists := m["two"]
    if exists {
        fmt.Printf("m[two] = %d\n", value)
    }
    
    // Delete from map
    delete(m, "two")
    fmt.Printf("After delete: %v\n", m)
    
    // Map iteration
    fmt.Println("Map iteration:")
    for key, value := range m {
        fmt.Printf("  %s: %d\n", key, value)
    }
}

// Test structs
func testStructs() {
    fmt.Println("\n=== Testing Structs ===")
    
    type Person struct {
        Name string
        Age  int
        City string
    }
    
    // Struct initialization
    p := Person{Name: "Alice", Age: 30, City: "New York"}
    fmt.Printf("Person: %+v\n", p)
    
    // Struct field access
    fmt.Printf("Name: %s, Age: %d\n", p.Name, p.Age)
    
    // Pointer to struct
    pPtr := &p
    pPtr.Age = 31
    fmt.Printf("Modified age: %d\n", p.Age)
}

// Test methods and interfaces
func testMethodsAndInterfaces() {
    fmt.Println("\n=== Testing Methods and Interfaces ===")
    
    type Shape interface {
        Area() float64
        Perimeter() float64
    }
    
    type Rectangle struct {
        Width, Height float64
    }
    
    type Circle struct {
        Radius float64
    }
    
    // Methods for Rectangle
    func (r Rectangle) Area() float64 {
        return r.Width * r.Height
    }
    
    func (r Rectangle) Perimeter() float64 {
        return 2 * (r.Width + r.Height)
    }
    
    // Methods for Circle
    func (c Circle) Area() float64 {
        return 3.14159 * c.Radius * c.Radius
    }
    
    func (c Circle) Perimeter() float64 {
        return 2 * 3.14159 * c.Radius
    }
    
    // Interface usage
    shapes := []Shape{
        Rectangle{Width: 3, Height: 4},
        Circle{Radius: 5},
    }
    
    for _, shape := range shapes {
        fmt.Printf("Shape area: %.2f, perimeter: %.2f\n", shape.Area(), shape.Perimeter())
    }
}

// Test error handling
func testErrorHandling() {
    fmt.Println("\n=== Testing Error Handling ===")
    
    // Simple function that returns an error
    divide := func(a, b int) (int, error) {
        if b == 0 {
            return 0, fmt.Errorf("division by zero")
        }
        return a / b, nil
    }
    
    result, err := divide(10, 2)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
    } else {
        fmt.Printf("10 / 2 = %d\n", result)
    }
    
    result, err = divide(10, 0)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
    } else {
        fmt.Printf("10 / 0 = %d\n", result)
    }
}

// Test goroutines and channels (basic)
func testConcurrency() {
    fmt.Println("\n=== Testing Concurrency ===")
    
    // Channel
    ch := make(chan int)
    
    // Goroutine
    go func() {
        ch <- 42
    }()
    
    // Receive from channel
    value := <-ch
    fmt.Printf("Received from channel: %d\n", value)
    
    // Buffered channel
    bufferedCh := make(chan int, 2)
    bufferedCh <- 1
    bufferedCh <- 2
    
    fmt.Printf("Buffered channel values: %d, %d\n", <-bufferedCh, <-bufferedCh)
}

func main() {
    fmt.Println("=== Go Language Feature Test Suite ===")
    
    testVariables()
    testControlFlow()
    testFunctions()
    testArraysAndSlices()
    testMaps()
    testStructs()
    testMethodsAndInterfaces()
    testErrorHandling()
    testConcurrency()
    
    fmt.Println("\n=== All tests completed successfully! ===")
}