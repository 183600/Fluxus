package main

import "fmt"

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
        i = i + 6
    }
    return true
}

func gcd(a int, b int) int {
    for b != 0 {
        temp := b
        b = a % b
        a = temp
    }
    return a
}

func main() {
    fmt.Println("Prime Numbers and GCD")
    
    // Test prime numbers
    fmt.Println("Prime numbers from 1 to 20:")
    for i := 1; i <= 20; i++ {
        if isPrime(i) {
            fmt.Printf("%d ", i)
        }
    }
    fmt.Println()
    
    // Test GCD
    a := 48
    b := 18
    result := gcd(a, b)
    fmt.Printf("GCD of %d and %d is %d\n", a, b, result)
    
    // Array processing
    numbers := [5]int{10, 20, 30, 40, 50}
    sum := 0
    
    fmt.Print("Array: ")
    for i := 0; i < 5; i++ {
        fmt.Printf("%d ", numbers[i])
        sum = sum + numbers[i]
    }
    fmt.Printf("\nSum: %d\n", sum)
}