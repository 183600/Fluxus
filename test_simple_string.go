package main

import "fmt"

func greet(name string) string {
    return "Hello, " + name
}

func main() {
    result := greet("World")
    fmt.Println(result)
}