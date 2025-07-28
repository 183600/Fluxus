package main

import "fmt"

func processSlice() {
    numbers := []int{1, 2, 3, 4, 5}
    fmt.Printf("Original slice: %v\n", numbers)
    
    // Create squares slice
    var squares []int
    for _, x := range numbers {
        squares = append(squares, x*x)
    }
    fmt.Printf("Squares: %v\n", squares)
    
    // Filter even numbers
    var evens []int
    for _, x := range numbers {
        if x%2 == 0 {
            evens = append(evens, x)
        }
    }
    fmt.Printf("Even numbers: %v\n", evens)
    
    // Sum and length
    total := 0
    for _, x := range numbers {
        total += x
    }
    length := len(numbers)
    fmt.Printf("Sum: %d, Length: %d\n", total, length)
}

func main() {
    processSlice()
}