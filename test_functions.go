package main

import "fmt"

var globalInt int = 42

func add(a int, b int) int {
    return a + b
}

func main() {
    result := add(10, 20)
    fmt.Println("Result:", result)
}