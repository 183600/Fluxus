package main

import "fmt"

func main() {
    fmt.Println("Hello from Go!")
    fmt.Println("Basic operations:")
    
    a := 10
    b := 20
    
    fmt.Printf("a = %d\n", a)
    fmt.Printf("b = %d\n", b)
    fmt.Printf("a + b = %d\n", a + b)
    fmt.Printf("a * b = %d\n", a * b)
    
    for i := 0; i < 5; i++ {
        fmt.Printf("Loop iteration: %d\n", i)
    }
    
    fmt.Println("Go program completed successfully!")
}
