package main

import "fmt"

func main() {
    fmt.Println("Hello from optimized Go!")
    
    a := 10
    b := 20
    result := a + b
    fmt.Printf("Sum: %d\n", result)
    
    for i := 1; i <= 5; i++ {
        fmt.Printf("Count: %d\n", i)
    }
}