package main

import "fmt"

func main() {
    x := 10
    y := 20
    sum := x + y
    fmt.Printf("Sum: %d\n", sum)
    
    x = x + 5
    fmt.Printf("Updated x: %d\n", x)
    
    isGreater := x > y
    fmt.Printf("x > y: %t\n", isGreater)
}
