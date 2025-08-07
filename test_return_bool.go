package main

import "fmt"

func testBool(x bool) bool {
    return !x
}

func main() {
    result := testBool(true)
    fmt.Println(result)
}