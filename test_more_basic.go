package main

import "fmt"

func add(a, b int) int {
    return a + b
}

func main() {
    // Test multiple return values simulation
    result := add(5, 3)
    fmt.Println("5 + 3 =", result)
    
    // Test different variable declarations
    var name string = "Bob"
    age := 30
    fmt.Printf("Name: %s, Age: %d\n", name, age)
}