package main

import "fmt"

func main() {
    // Test various printf scenarios
    fmt.Printf("Hello, world!\n")
    fmt.Printf("The answer is %d\n", 42)
    fmt.Printf("Pi is approximately %.2f\n", 3.14159)
    fmt.Printf("Name: %s, Age: %d\n", "Alice", 25)
    fmt.Printf("Multiple values: %d + %d = %d\n", 5, 3, 8)
    
    // Test string with tabs and other escapes
    fmt.Printf("Column1\tColumn2\tColumn3\n")
    fmt.Printf("Quote test: \"Hello\"\n")
    fmt.Printf("Backslash test: \\n\n")
}