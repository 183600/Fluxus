package main

import "fmt"

// Test interface support
type Greeter interface {
    Greet() string
}

type Person struct {
    Name string
}

func (p Person) Greet() string {
    return "Hello, " + p.Name
}

func main() {
    person := Person{Name: "Alice"}
    fmt.Println(person.Greet())
}