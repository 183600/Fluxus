package main

import "fmt"

func main() {
    // If-else
    x := 15
    if x > 10 {
        fmt.Println("x is greater than 10")
    } else {
        fmt.Println("x is not greater than 10")
    }
    
    // For loop
    fmt.Print("Even numbers: ")
    for i := 0; i <= 10; i += 2 {
        fmt.Printf("%d ", i)
    }
    fmt.Println()
}
