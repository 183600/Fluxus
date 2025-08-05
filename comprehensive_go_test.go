package main

import "fmt"

// Basic types test
func testBasicTypes() {
    var intVar int = 42
    var floatVar float64 = 3.14
    var stringVar string = "Hello"
    var boolVar bool = true
    var runeVar rune = 'A'
    var byteVar byte = 'B'
    
    fmt.Printf("int: %d, float: %f, string: %s, bool: %t, rune: %c, byte: %c\n", 
        intVar, floatVar, stringVar, boolVar, runeVar, byteVar)
}

// Variable declarations
func testVariableDeclarations() {
    // Explicit type
    var x int = 10
    var y float64 = 20.5
    
    // Type inference
    z := 30
    name := "Go"
    
    // Multiple declarations
    var a, b, c int = 1, 2, 3
    
    // Short declaration
    d, e := 4, 5
    
    fmt.Printf("Variables: %d, %f, %d, %s, %d, %d, %d, %d\n", x, y, z, name, a, b, c, d, e)
}

// Arrays and slices
func testArraysAndSlices() {
    // Array
    var arr [5]int = [5]int{1, 2, 3, 4, 5}
    
    // Slice
    slice := []int{1, 2, 3, 4, 5}
    slice = append(slice, 6)
    
    // Slice operations
    subSlice := slice[1:4]
    
    fmt.Printf("Array: %v, Slice: %v, Sub-slice: %v\n", arr, slice, subSlice)
}

// Maps
func testMaps() {
    // Map declaration
    m := make(map[string]int)
    m["one"] = 1
    m["two"] = 2
    
    // Map literal
    m2 := map[string]int{"three": 3, "four": 4}
    
    // Accessing elements
    value, exists := m["one"]
    
    fmt.Printf("Map1: %v, Map2: %v, Value: %d, Exists: %t\n", m, m2, value, exists)
}

// Structs
type Person struct {
    Name string
    Age  int
    City string
}

func testStructs() {
    // Struct literal
    person := Person{Name: "Alice", Age: 30, City: "New York"}
    
    // Accessing fields
    fmt.Printf("Person: %s, %d, %s\n", person.Name, person.Age, person.City)
    
    // Struct with pointer
    p := &person
    p.Age = 31
    fmt.Printf("Updated age: %d\n", person.Age)
}

// Functions
func add(a, b int) int {
    return a + b
}

func multipleReturn() (int, string) {
    return 42, "answer"
}

func testFunctions() {
    result := add(5, 3)
    num, desc := multipleReturn()
    
    fmt.Printf("Add: %d, Multiple return: %d, %s\n", result, num, desc)
}

// Control flow
func testControlFlow() {
    // If-else
    x := 10
    if x > 5 {
        fmt.Println("x > 5")
    } else if x < 5 {
        fmt.Println("x < 5")
    } else {
        fmt.Println("x == 5")
    }
    
    // For loop
    for i := 0; i < 5; i++ {
        fmt.Printf("i: %d ", i)
    }
    fmt.Println()
    
    // While-like for loop
    j := 0
    for j < 3 {
        fmt.Printf("j: %d ", j)
        j++
    }
    fmt.Println()
    
    // Infinite loop with break
    k := 0
    for {
        if k >= 3 {
            break
        }
        fmt.Printf("k: %d ", k)
        k++
    }
    fmt.Println()
}

// Switch statement
func testSwitch() {
    day := "Monday"
    
    switch day {
    case "Monday":
        fmt.Println("It's Monday")
    case "Tuesday":
        fmt.Println("It's Tuesday")
    default:
        fmt.Println("It's another day")
    }
    
    // Switch with no condition
    hour := 14
    switch {
    case hour < 12:
        fmt.Println("Good morning")
    case hour < 18:
        fmt.Println("Good afternoon")
    default:
        fmt.Println("Good evening")
    }
}

// Pointers
func testPointers() {
    x := 42
    p := &x
    *p = 21
    
    fmt.Printf("x: %d, p: %p, *p: %d\n", x, p, *p)
}

// Methods and interfaces
type Shape interface {
    Area() float64
}

type Rectangle struct {
    Width, Height float64
}

func (r Rectangle) Area() float64 {
    return r.Width * r.Height
}

type Circle struct {
    Radius float64
}

func (c Circle) Area() float64 {
    return 3.14159 * c.Radius * c.Radius
}

func testInterfaces() {
    var s Shape
    s = Rectangle{Width: 3, Height: 4}
    fmt.Printf("Rectangle area: %f\n", s.Area())
    
    s = Circle{Radius: 5}
    fmt.Printf("Circle area: %f\n", s.Area())
}

// Error handling
func divide(a, b int) (int, error) {
    if b == 0 {
        return 0, fmt.Errorf("division by zero")
    }
    return a / b, nil
}

func testErrorHandling() {
    result, err := divide(10, 2)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
    } else {
        fmt.Printf("Result: %d\n", result)
    }
    
    result, err = divide(10, 0)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
    } else {
        fmt.Printf("Result: %d\n", result)
    }
}

// Goroutines and channels (basic test)
func testConcurrency() {
    ch := make(chan int)
    
    go func() {
        ch <- 42
    }()
    
    value := <-ch
    fmt.Printf("Received from channel: %d\n", value)
    
    // Buffered channel
    ch2 := make(chan int, 2)
    ch2 <- 1
    ch2 <- 2
    
    fmt.Printf("Buffered channel: %d, %d\n", <-ch2, <-ch2)
}

// Defer and panic
func testDeferAndPanic() {
    defer fmt.Println("Deferred function")
    
    fmt.Println("Before panic simulation")
    // Note: We won't actually panic to avoid breaking the test
    fmt.Println("After defer, before normal return")
}

// Range over various types
func testRange() {
    // Range over slice
    slice := []int{1, 2, 3}
    for i, v := range slice {
        fmt.Printf("slice[%d] = %d\n", i, v)
    }
    
    // Range over map
    m := map[string]int{"a": 1, "b": 2}
    for k, v := range m {
        fmt.Printf("map[%s] = %d\n", k, v)
    }
    
    // Range over string
    for i, c := range "hello" {
        fmt.Printf("string[%d] = %c\n", i, c)
    }
}

// Type assertions
func testTypeAssertions() {
    var i interface{} = "hello"
    
    s := i.(string)
    fmt.Printf("Type assertion: %s\n", s)
    
    // Safe type assertion
    if s, ok := i.(string); ok {
        fmt.Printf("Safe type assertion: %s\n", s)
    }
    
    // Type switch
    switch v := i.(type) {
    case string:
        fmt.Printf("Type switch: string - %s\n", v)
    case int:
        fmt.Printf("Type switch: int - %d\n", v)
    default:
        fmt.Printf("Type switch: unknown type\n")
    }
}

// Main function to run all tests
func main() {
    fmt.Println("=== Go Feature Test Suite ===")
    
    fmt.Println("\n1. Basic Types:")
    testBasicTypes()
    
    fmt.Println("\n2. Variable Declarations:")
    testVariableDeclarations()
    
    fmt.Println("\n3. Arrays and Slices:")
    testArraysAndSlices()
    
    fmt.Println("\n4. Maps:")
    testMaps()
    
    fmt.Println("\n5. Structs:")
    testStructs()
    
    fmt.Println("\n6. Functions:")
    testFunctions()
    
    fmt.Println("\n7. Control Flow:")
    testControlFlow()
    
    fmt.Println("\n8. Switch:")
    testSwitch()
    
    fmt.Println("\n9. Pointers:")
    testPointers()
    
    fmt.Println("\n10. Interfaces:")
    testInterfaces()
    
    fmt.Println("\n11. Error Handling:")
    testErrorHandling()
    
    fmt.Println("\n12. Concurrency:")
    testConcurrency()
    
    fmt.Println("\n13. Defer:")
    testDeferAndPanic()
    
    fmt.Println("\n14. Range:")
    testRange()
    
    fmt.Println("\n15. Type Assertions:")
    testTypeAssertions()
    
    fmt.Println("\n=== Test Suite Complete ===")
}