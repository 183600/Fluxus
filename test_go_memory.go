package main

import "fmt"

func main() {
    // Create a large array
    size := 10000
    arr := make([]int, size)
    
    // Fill and sum
    sum := 0
    for i := 0; i < size; i++ {
        arr[i] = i % 100
        sum += arr[i]
    }
    
    fmt.Println(sum)
}
