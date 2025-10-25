package main

import "fmt"

func main() {
    fmt.Println("Simple Calculator")
    
    a := 10
    b := 5
    
    sum := a + b
    diff := a - b
    prod := a * b
    quot := a / b
    mod := a % b
    
    fmt.Printf("a = %d, b = %d\n", a, b)
    fmt.Printf("Addition: %d + %d = %d\n", a, b, sum)
    fmt.Printf("Subtraction: %d - %d = %d\n", a, b, diff)
    fmt.Printf("Multiplication: %d * %d = %d\n", a, b, prod)
    fmt.Printf("Division: %d / %d = %d\n", a, b, quot)
    fmt.Printf("Modulus: %d %% %d = %d\n", a, b, mod)
    
    // Test with floats
    x := 3.14
    y := 2.71
    
    fmt.Printf("Float operations:\n")
    fmt.Printf("%.2f + %.2f = %.2f\n", x, y, x+y)
    fmt.Printf("%.2f * %.2f = %.2f\n", x, y, x*y)
}