package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
    
    // Test basic data types
    var x int = 42
    var y float64 = 3.14
    var message string = "Go compilation test"
    var isReady bool = true
    
    // Test arithmetic operations
    sum := x + 10
    product := y * 2.0
    
    // Test conditional
    if isReady {
        fmt.Printf("Message: %s\n", message)
        fmt.Printf("Sum: %d, Product: %.2f\n", sum, product)
    }
    
    // Test loop
    for i := 0; i < 3; i++ {
        fmt.Printf("Loop iteration: %d\n", i)
    }
    
    // Test array/slice
    numbers := []int{1, 2, 3, 4, 5}
    for _, num := range numbers {
        fmt.Printf("Number: %d\n", num)
    }
}