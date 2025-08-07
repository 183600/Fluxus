package main

import "fmt"

func main() {
    // Test if-else
    x := 10
    if x > 5 {
        fmt.Println("x is greater than 5")
    } else {
        fmt.Println("x is less than or equal to 5")
    }
    
    // Test for loops
    for i := 0; i < 5; i++ {
        fmt.Printf("i = %d\n", i)
    }
    
    // Test while-like loop
    j := 0
    for j < 3 {
        fmt.Printf("j = %d\n", j)
        j++
    }
    
    // Test switch
    day := "Monday"
    switch day {
    case "Monday":
        fmt.Println("Start of the week")
    case "Friday":
        fmt.Println("End of the week")
    default:
        fmt.Println("Middle of the week")
    }
}