package main

import "fmt"

func simpleFunc() int {
    x := 5
    y := 10
    return x + y
}

func main() {
    result := simpleFunc()
    fmt.Printf("Result: %d\n", result)
}