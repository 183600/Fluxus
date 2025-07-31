package main

import "fmt"

func fibonacci(n int) int {
    if n <= 1 {
        return n
    }
    a := 0
    b := 1
    for i := 2; i <= n; i++ {
        c := a + b
        a = b
        b = c
    }
    return b
}

func main() {
    fmt.Printf("fib(5) = %d\n", fibonacci(5))
    fmt.Printf("fib(6) = %d\n", fibonacci(6))
    fmt.Printf("fib(7) = %d\n", fibonacci(7))
}