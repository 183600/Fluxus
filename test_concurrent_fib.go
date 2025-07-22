package main

import "fmt"

// fibonacci calculates the nth Fibonacci number
func fibonacci(n int) int {
    if n <= 1 {
        return n
    }
    return fibonacci(n-1) + fibonacci(n-2)
}

// concurrentFibonacci calculates Fibonacci numbers concurrently
func concurrentFibonacci(n int, ch chan int) {
    result := fibonacci(n)
    ch <- result
}

func main() {
    // Calculate first 10 Fibonacci numbers
    fmt.Println("Sequential Fibonacci:")
    for i := 0; i < 10; i++ {
        result := fibonacci(i)
        fmt.Printf("fib(%d) = %d\n", i, result)
    }

    // Calculate Fibonacci numbers concurrently
    fmt.Println("\nConcurrent Fibonacci:")
    ch := make(chan int, 5)
    
    for i := 5; i < 10; i++ {
        go concurrentFibonacci(i, ch)
    }
    
    for i := 0; i < 5; i++ {
        result := <-ch
        fmt.Printf("Concurrent result: %d\n", result)
    }
}