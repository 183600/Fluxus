package main

import "fmt"

func fact(n int) int {
    if n <= 1 {
        return 1
    }
    return n * fact(n-1)
}

func main() {
    result := fact(4)
    fmt.Printf("%d\n", result)
}