package main

import "fmt"

// Struct definition
type Person struct {
    Name string
    Age  int
}

// Method with value receiver
func (p Person) Greet() string {
    return fmt.Sprintf("Hello, my name is %s and I'm %d years old.", p.Name, p.Age)
}

// Method with pointer receiver
func (p *Person) HaveBirthday() {
    p.Age++
}

// Interface definition
type Greeter interface {
    Greet() string
}

// Function that uses interface
func SayHello(g Greeter) {
    fmt.Println(g.Greet())
}

// Another struct
type Animal struct {
    Name string
    Species string
}

// Animal implements Greeter
func (a Animal) Greet() string {
    return fmt.Sprintf("Hi, I'm %s the %s.", a.Name, a.Species)
}

func main() {
    // Create struct instances
    person := Person{Name: "Alice", Age: 25}
    animal := Animal{Name: "Rex", Species: "dog"}
    
    // Use methods
    fmt.Println("Before birthday:", person.Greet())
    person.HaveBirthday()
    fmt.Println("After birthday:", person.Greet())
    
    // Use interface
    SayHello(person)
    SayHello(animal)
    
    // Test multiple assignment
    a, b := 10, 20
    fmt.Printf("a = %d, b = %d\n", a, b)
    
    // Test struct field access
    fmt.Printf("Person details: Name=%s, Age=%d\n", person.Name, person.Age)
}