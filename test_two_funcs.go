package main

import "fmt"

func add(a, b int) int {
    return a + b
}

func multiply(a, b int) int {
    return a * b
}

func main() {
    fmt.Println(add(5, 3))
    fmt.Println(multiply(4, 7))
}