package main

import "fmt"

func main() {
    fmt.Println("=== Final Verification Test ===")
    fmt.Println("1. Basic types:")
    x := 42
    y := 3.14
    s := "Hello"
    fmt.Printf("int: %d, float: %f, string: %s\n", x, y, s)
    
    fmt.Println("2. Control flow:")
    if x > 0 {
        fmt.Println("x is positive")
    }
    
    fmt.Println("3. Loops:")
    for i := 0; i < 3; i++ {
        fmt.Printf("i: %d\n", i)
    }
    
    fmt.Println("4. Functions:")
    add := func(a, b int) int {
        return a + b
    }
    result := add(5, 3)
    fmt.Printf("5 + 3 = %d\n", result)
    
    fmt.Println("5. Data structures:")
    arr := []int{1, 2, 3}
    m := make(map[string]int)
    m["one"] = 1
    
    fmt.Println("=== Test Complete ===")
}