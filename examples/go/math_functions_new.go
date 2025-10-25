package main

import "fmt"

func factorial(n int) int {
    if n <= 1 {
        return 1
    }
    return n * factorial(n-1)
}

func isPrime(n int) bool {
    if n <= 1 {
        return false
    }
    if n <= 3 {
        return true
    }
    if n%2 == 0 || n%3 == 0 {
        return false
    }
    
    i := 5
    for i*i <= n {
        if n%i == 0 || n%(i+2) == 0 {
            return false
        }
        i += 6
    }
    return true
}

func gcd(a, b int) int {
    for b != 0 {
        a, b = b, a%b
    }
    return a
}

func main() {
    fmt.Println("Mathematical Functions Demo")
    
    // Test factorial
    n := 5
    fact := factorial(n)
    fmt.Printf("Factorial of %d is %d\n", n, fact)
    
    // Test prime checking
    numbers := []int{2, 3, 4, 5, 17, 25, 29}
    fmt.Println("Prime number check:")
    for _, num := range numbers {
        if isPrime(num) {
            fmt.Printf("%d is prime\n", num)
        } else {
            fmt.Printf("%d is not prime\n", num)
        }
    }
    
    // Test GCD
    a, b := 48, 18
    result := gcd(a, b)
    fmt.Printf("GCD of %d and %d is %d\n", a, b, result)
    
    // Test Fibonacci sequence
    fmt.Println("Fibonacci sequence (first 10 numbers):")
    fib_a, fib_b := 0, 1
    fmt.Printf("%d %d ", fib_a, fib_b)
    
    for i := 2; i < 10; i++ {
        next := fib_a + fib_b
        fmt.Printf("%d ", next)
        fib_a, fib_b = fib_b, next
    }
    fmt.Println()
}