package main

import (
    "fmt"
    "time"
    "strings"
    "math"
)

func main() {
    // Multiple imports test
    fmt.Println("Hello from fmt")
    
    // Time package
    now := time.Now()
    fmt.Println("Current time:", now)
    
    // Strings package
    text := "Hello World"
    upper := strings.ToUpper(text)
    fmt.Println("Uppercase:", upper)
    
    // Math package
    pi := math.Pi
    fmt.Println("Pi value:", pi)
}