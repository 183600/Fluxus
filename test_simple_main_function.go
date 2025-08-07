package main

import "fmt"

func testSimpleMain() {
    a := 10
    b := 20
    sum := a + b
    fmt.Printf("Sum: %d\n", sum)
}

func main() {
    testSimpleMain()
}