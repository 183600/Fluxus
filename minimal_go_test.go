package main

import "fmt"

func add(a, b int) int {
    return a + b
}

func main() {
    result := add(5, 3)
    fmt.Println("test", result)
    
    for i := 0; i < 3; i++ {
        fmt.Printf("i = %d\n", i)
    }
}