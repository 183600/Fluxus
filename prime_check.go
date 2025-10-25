package main

import "fmt"

func check_prime(n int) int {
    if n < 2 {
        return 0
    }
    i := 2
    for i*i <= n {
        if n%i == 0 {
            return 0
        }
        i = i + 1
    }
    return 1
}

func main() {
    num := 17
    result := check_prime(num)
    if result == 1 {
        fmt.Printf("prime\n")
    } else {
        fmt.Printf("not prime\n")
    }
}