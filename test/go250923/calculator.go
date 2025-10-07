package main

import "fmt"

func add(a, b int) int {
    return a + b
}

func main() {
    x, y := 10, 20
    result := add(x, y)
    fmt.Printf("%d + %d = %d\n", x, y, result)
}