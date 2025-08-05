package main

import "fmt"

func simpleFunction() {
    fmt.Println("Hello from simple function")
}

func main() {
    fmt.Println("Testing parser")
    x := 42
    if x > 0 {
        fmt.Println("x is positive")
    }
    simpleFunction()
}