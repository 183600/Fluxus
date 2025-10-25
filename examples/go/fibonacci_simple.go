package main

import "fmt"

func fibonacci(n int) int {
    if n <= 1 {
        return n
    }
    return fibonacci(n-1) + fibonacci(n-2)
}

func main() {
    fmt.Printf("fib(5) = %d\n", fibonacci(5))
    fmt.Printf("fib(6) = %d\n", fibonacci(6))
    fmt.Printf("fib(7) = %d\n", fibonacci(7))
    fmt.Printf("fib(8) = %d\n", fibonacci(8))
}