package main

import "fmt"

func testCustom() MyType {
    return MyType{}
}

func main() {
    result := testCustom()
    fmt.Println(result)
}