package main

import "fmt"

func main() {
    // If statements
    x := 10
    if x > 5 {
        fmt.Println("x is greater than 5")
    } else {
        fmt.Println("x is not greater than 5")
    }
    
    // For loops
    for i := 0; i < 5; i++ {
        fmt.Println("Loop:", i)
    }
    
    // While-style for loop
    j := 0
    for j < 3 {
        fmt.Println("While-style:", j)
        j++
    }
    
    // Switch statement
    day := 3
    switch day {
    case 1:
        fmt.Println("Monday")
    case 2:
        fmt.Println("Tuesday")
    case 3:
        fmt.Println("Wednesday")
    default:
        fmt.Println("Other day")
    }
}