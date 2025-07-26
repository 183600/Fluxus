package main

import "fmt"

func add(a int, b int) int {
    return a + b
}

func multiply(a int, b int) int {
    return a * b
}

func main() {
    x := 5
    y := 3
    
    sum := add(x, y)
    product := multiply(x, y)
    
    fmt.Printf("Sum: %d + %d = %d\n", x, y, sum)
    fmt.Printf("Product: %d * %d = %d\n", x, y, product)
}