package main

import "fmt"

func fibonacci(n int) int {
    if n <= 1 {
        return n
    }
    a, b := 0, 1
    for i := 2; i <= n; i++ {
        a, b = b, a+b
    }
    return b
}

func main() {
    result := fibonacci(10)
    fmt.Printf("Fibonacci(10) = %d\n", result)
}