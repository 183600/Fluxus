package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
    fmt.Println("Testing basic Go compilation")

    // Variables
    a := 10
    b := 20
    fmt.Println("a =", a, "b =", b)

    // Basic operations
    sum := a + b
    fmt.Println("sum =", sum)

    // Slices
    numbers := []int{1, 2, 3, 4, 5}
    for i, num := range numbers {
        fmt.Printf("index %d: %d\n", i, num)
    }

    // Maps
    m := make(map[string]int)
    m["one"] = 1
    m["two"] = 2
    for key, value := range m {
        fmt.Println("map:", key, "=", value)
    }

    fmt.Println("Test completed successfully!")
}