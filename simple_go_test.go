package main

import "fmt"

func main() {
    fmt.Println("=== Basic Go Test ===")
    
    // Test 1: Basic types
    var x int = 42
    var y float64 = 3.14
    var s string = "hello"
    var b bool = true
    
    fmt.Printf("Basic types: %d, %f, %s, %t\n", x, y, s, b)
    
    // Test 2: Variables
    z := 100
    fmt.Printf("Variable: %d\n", z)
    
    // Test 3: Arrays
    arr := [3]int{1, 2, 3}
    fmt.Printf("Array: %v\n", arr)
    
    // Test 4: Slices
    slice := []int{4, 5, 6}
    fmt.Printf("Slice: %v\n", slice)
    
    // Test 5: Maps
    m := make(map[string]int)
    m["one"] = 1
    fmt.Printf("Map: %v\n", m)
    
    // Test 6: Functions
    result := add(5, 3)
    fmt.Printf("Function result: %d\n", result)
    
    fmt.Println("=== Test Complete ===")
}

func add(a, b int) int {
    return a + b
}