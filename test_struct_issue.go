package main

import "fmt"

// Test struct support
type Person struct {
    Name string
    Age  int
}

func main() {
    // Test struct initialization
    person := Person{Name: "Bob", Age: 30}
    fmt.Println("Person:", person.Name, person.Age)
}