package main

import "fmt"

func testBasic() int {
    x := 42
    fmt.Println(x)
    return x
}

func main() {
    result := testBasic()
    fmt.Printf("Result: %d\n", result)
}