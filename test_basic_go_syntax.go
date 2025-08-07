package main

import "fmt"

func main() {
    // Basic variable declarations
    var x int = 10
    var y = 20
    z := 30
    
    // Basic arithmetic
    sum := x + y + z
    
    // Print statements
    fmt.Println("x:", x)
    fmt.Println("y:", y)
    fmt.Println("z:", z)
    fmt.Println("sum:", sum)
    
    // Basic if statement
    if sum > 50 {
        fmt.Println("Sum is greater than 50")
    }
    
    // Basic for loop
    for i := 0; i < 3; i++ {
        fmt.Println("Loop iteration:", i)
    }
    
    // Function call
    result := add(x, y)
    fmt.Println("add(x, y):", result)
}

func add(a int, b int) int {
    return a + b
}