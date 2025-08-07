package main

import "fmt"

func testIfElse(x int) int {
    if x > 10 {
        return 100
    } else {
        return 0
    }
}

func main() {
    result := testIfElse(15)
    fmt.Println(result)
}