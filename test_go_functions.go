package main

import "fmt"

func add(a int, b int) int {
    return a + b
}

func multiply(a, b int) int {
    return a * b
}

func swap(a, b string) (string, string) {
    return b, a
}

func main() {
    x := add(10, 20)
    fmt.Println("add(10, 20):", x)
    
    y := multiply(5, 6)
    fmt.Println("multiply(5, 6):", y)
    
    first, second := swap("hello", "world")
    fmt.Println("swap result:", first, second)
}