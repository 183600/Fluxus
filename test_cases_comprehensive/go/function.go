package main

import "fmt"

func add(a int, b int) int {
    return a + b
}

func main() {
    result := add(3, 4)
    fmt.Println(result)
}
