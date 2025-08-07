package main

import "fmt"

func main() {
    // Test structs
    type Person struct {
        Name string
        Age  int
    }
    
    p := Person{Name: "Alice", Age: 25}
    fmt.Println("Person:", p.Name, p.Age)
}