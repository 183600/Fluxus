package main

import "fmt"

func main() {
    // Test basic types
    var x int = 42
    var y float64 = 3.14
    var z string = "Hello, World!"
    var b bool = true
    
    fmt.Printf("int: %d\n", x)
    fmt.Printf("float: %f\n", y)
    fmt.Printf("string: %s\n", z)
    fmt.Printf("bool: %t\n", b)
    
    // Test arrays
    arr := [5]int{1, 2, 3, 4, 5}
    fmt.Printf("Array: %v\n", arr)
    
    // Test slices
    slice := []int{1, 2, 3, 4, 5}
    fmt.Printf("Slice: %v\n", slice)
    
    // Test maps
    m := map[string]int{"one": 1, "two": 2}
    fmt.Printf("Map: %v\n", m)
}