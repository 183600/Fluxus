package main

import "fmt"

type Person struct {
    Name    string
    Age     int
    Email   string
}

func (p Person) introduce() {
    fmt.Printf("Hello, my name is %s and I'm %d years old.\n", p.Name, p.Age)
}

func main() {
    people := []Person{
        {"Alice", 25, "alice@example.com"},
        {"Bob", 30, "bob@example.com"},
        {"Charlie", 35, "charlie@example.com"},
    }

    fmt.Println("People:")
    for i, person := range people {
        fmt.Printf("%d. ", i+1)
        person.introduce()
    }
}