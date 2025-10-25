package main

import "fmt"

func greet(name string) string {
    return "Hello, " + name + "!"
}

func add(a int, b int) int {
    return a + b
}

func main() {
    message := greet("World")
    fmt.Println(message)
    
    result := add(10, 20)
    fmt.Printf("10 + 20 = %d\n", result)
}
