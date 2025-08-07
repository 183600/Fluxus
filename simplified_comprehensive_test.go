package main

import "fmt"

// Basic types test
func testBasicTypes() {
    var intVar int = 42
    var floatVar float64 = 3.14
    var stringVar string = "Hello"
    var boolVar bool = true
    
    fmt.Printf("int: %d, float: %f, string: %s, bool: %t\n", 
        intVar, floatVar, stringVar, boolVar)
}

// Main function to run all tests
func main() {
    fmt.Println("=== Comprehensive Go Feature Test Suite ===")
    
    fmt.Println("\n1. Basic Types:")
    testBasicTypes()
    
    fmt.Println("\n=== Test Suite Complete ===")
}
