package main

import "fmt"

func main() {
    fmt.Println("Hello from Go!")
    fmt.Println("Testing basic compilation")
    for i := 0; i < 5; i++ {
        fmt.Printf("i = %d\n", i)
    }
}