package main

import "fmt"

func add(a int, b int) int {
    return a + b
}

func multiply(a int, b int) int {
    return a * b
}

func factorial(n int) int {
    if n <= 1 {
        return 1
    }
    return n * factorial(n-1)
}

func main() {
    fmt.Println("Simple Math Operations")
    
    x := 5
    y := 3
    
    sum := add(x, y)
    fmt.Printf("%d + %d = %d\n", x, y, sum)
    
    product := multiply(x, y)
    fmt.Printf("%d * %d = %d\n", x, y, product)
    
    fact := factorial(5)
    fmt.Printf("5! = %d\n", fact)
    
    // Simple loop
    fmt.Println("Numbers 1 to 5:")
    for i := 1; i <= 5; i++ {
        fmt.Printf("%d ", i)
    }
    fmt.Println()
}