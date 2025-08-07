package main

import "fmt"

func main() {
    // Arrays
    var arr [3]int = [3]int{1, 2, 3}
    fmt.Println("Array:", arr)
    
    // Slices
    slice := []int{4, 5, 6, 7}
    fmt.Println("Slice:", slice)
    fmt.Println("Slice length:", len(slice))
    
    // Maps
    m := make(map[string]int)
    m["apple"] = 5
    m["banana"] = 3
    fmt.Println("Map:", m)
    fmt.Println("Apple count:", m["apple"])
    
    // Structs
    type Person struct {
        Name string
        Age  int
    }
    
    p := Person{Name: "John", Age: 30}
    fmt.Println("Person:", p)
    fmt.Println("Name:", p.Name)
    fmt.Println("Age:", p.Age)
    
    // Pointers
    x := 42
    ptr := &x
    fmt.Println("Value of x:", x)
    fmt.Println("Address of x:", ptr)
    fmt.Println("Value at pointer:", *ptr)
}