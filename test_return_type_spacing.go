package main

import "fmt"

func testIfElse(x int) string {
    if x > 10 {
        return "greater than 10"
    } else {
        return "less than or equal to 10"
    }
}

func main() {
    result := testIfElse(15)
    fmt.Println(result)
}