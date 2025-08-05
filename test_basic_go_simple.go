package main

import "fmt"

func main() {
    // Test basic variable declarations
    var x int = 42
    y := 100
    var z float64 = 3.14
    
    fmt.Println("Testing variables:")
    fmt.Println("x =", x)
    fmt.Println("y =", y)
    fmt.Println("z =", z)
    
    // Test basic operations
    sum := x + y
    fmt.Println("x + y =", sum)
    
    // Test if statement
    if sum > 100 {
        fmt.Println("Sum is greater than 100")
    } else {
        fmt.Println("Sum is not greater than 100")
    }
    
    // Test for loop
    fmt.Println("For loop test:")
    for i := 0; i < 5; i++ {
        fmt.Printf("i = %d\n", i)
    }
    
    // Test function call
    result := multiply(5, 3)
    fmt.Println("5 * 3 =", result)
    
    fmt.Println("Basic test completed!")
}

func multiply(a int, b int) int {
    return a * b
}