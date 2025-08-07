package main

import "fmt"

func main() {
    // Test basic variable declarations
    var x int = 42
    var y float64 = 3.14
    
    fmt.Printf("x = %d, y = %f\n", x, y)
    
    // Test type inference
    a := 100
    b := "Hello"
    
    fmt.Printf("a = %d, b = %s\n", a, b)
    
    // Test multiple variables
    var i, j int = 1, 2
    fmt.Printf("i = %d, j = %d\n", i, j)
}