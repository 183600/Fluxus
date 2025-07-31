package main

import "fmt"

func fibonacci(n int) int {
    if n <= 1 {
        return n
    }
    return fibonacci(n-1) + fibonacci(n-2)
}

func factorial(n int) int {
    if n <= 1 {
        return 1
    }
    return n * factorial(n-1)
}

func bubbleSort(arr []int) []int {
    n := len(arr)
    for i := 0; i < n-1; i++ {
        for j := 0; j < n-i-1; j++ {
            if arr[j] > arr[j+1] {
                arr[j], arr[j+1] = arr[j+1], arr[j]
            }
        }
    }
    return arr
}

func isPrime(n int) bool {
    if n <= 1 {
        return false
    }
    if n <= 3 {
        return true
    }
    if n%2 == 0 {
        return false
    }
    for i := 3; i*i <= n; i += 2 {
        if n%i == 0 {
            return false
        }
    }
    return true
}

func main() {
    fmt.Println("Go Algorithms Test")
    
    // Test fibonacci
    fmt.Println("Fibonacci sequence:")
    for i := 0; i < 10; i++ {
        fmt.Printf("  fib(%d) = %d\n", i, fibonacci(i))
    }
    
    // Test factorial
    fmt.Printf("Factorial(5) = %d\n", factorial(5))
    fmt.Printf("Factorial(7) = %d\n", factorial(7))
    
    // Test sorting
    arr := []int{64, 34, 25, 12, 22, 11, 90}
    fmt.Printf("Original array: %v\n", arr)
    sortedArr := bubbleSort(append([]int{}, arr...))
    fmt.Printf("Sorted array: %v\n", sortedArr)
    
    // Test prime checking
    numbers := []int{2, 3, 4, 5, 6, 7, 8, 9, 10, 11}
    fmt.Println("Prime numbers:")
    for _, num := range numbers {
        if isPrime(num) {
            fmt.Printf("  %d is prime\n", num)
        } else {
            fmt.Printf("  %d is not prime\n", num)
        }
    }
    
    fmt.Println("Go algorithms test completed successfully!")
}
