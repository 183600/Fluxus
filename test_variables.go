package main

import "fmt"

func testVariables() {
    var x int = 42
    var y float64 = 3.14
    var s string = "Hello"
    
    fmt.Printf("x = %d\n", x)
    fmt.Printf("y = %f\n", y)
    fmt.Printf("s = %s\n", s)
}

func main() {
    testVariables()
    fmt.Println("Done!")
}