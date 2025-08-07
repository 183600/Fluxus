package main

import "fmt"

// Basic variable declarations
var globalInt int = 42
var globalString string = "Hello, World!"
var globalBool bool = true

func main() {
    // Local variable declarations
    var localInt int = 10
    var localString string = "Local variable"
    var localBool bool = false
    
    // Short variable declarations
    shortInt := 20
    shortString := "Short declaration"
    shortBool := true
    
    // Multiple variable declarations
    var a, b, c int = 1, 2, 3
    var x, y string = "hello", "world"
    
    // Type inference
    inferredInt := 100
    inferredString := "inferred"
    inferredFloat := 3.14
    
    // Print basic variables
    fmt.Println("=== Basic Variables Test ===")
    fmt.Println("Global variables:")
    fmt.Println("  globalInt:", globalInt)
    fmt.Println("  globalString:", globalString)
    fmt.Println("  globalBool:", globalBool)
    
    fmt.Println("Local variables:")
    fmt.Println("  localInt:", localInt)
    fmt.Println("  localString:", localString)
    fmt.Println("  localBool:", localBool)
    
    fmt.Println("Short declarations:")
    fmt.Println("  shortInt:", shortInt)
    fmt.Println("  shortString:", shortString)
    fmt.Println("  shortBool:", shortBool)
    
    fmt.Println("Multiple declarations:")
    fmt.Println("  a, b, c:", a, b, c)
    fmt.Println("  x, y:", x, y)
    
    fmt.Println("Type inference:")
    fmt.Println("  inferredInt:", inferredInt)
    fmt.Println("  inferredString:", inferredString)
    fmt.Println("  inferredFloat:", inferredFloat)
    
    // Constants
    const Pi = 3.14159
    const MaxInt = int(^uint(0) >> 1)
    
    fmt.Println("Constants:")
    fmt.Println("  Pi:", Pi)
    fmt.Println("  MaxInt:", MaxInt)
    
    // Basic arithmetic operations
    sum := a + b + c
    product := a * b * c
    difference := a - b
    quotient := a / b
    remainder := a % b
    
    fmt.Println("Arithmetic operations:")
    fmt.Println("  Sum:", sum)
    fmt.Println("  Product:", product)
    fmt.Println("  Difference:", difference)
    fmt.Println("  Quotient:", quotient)
    fmt.Println("  Remainder:", remainder)
    
    // Basic functions test
    result := add(5, 3)
    fmt.Println("Function call add(5, 3):", result)
    
    result2 := multiply(4, 7)
    fmt.Println("Function call multiply(4, 7):", result2)
    
    // Basic control flow
    if result > 10 {
        fmt.Println("Result is greater than 10")
    } else {
        fmt.Println("Result is less than or equal to 10")
    }
    
    // For loop
    fmt.Println("For loop test:")
    for i := 0; i < 5; i++ {
        fmt.Printf("  i = %d\n", i)
    }
    
    // While-like loop
    fmt.Println("While-like loop test:")
    j := 0
    for j < 3 {
        fmt.Printf("  j = %d\n", j)
        j++
    }
    
    // Infinite loop with break
    fmt.Println("Infinite loop with break test:")
    k := 0
    for {
        if k >= 2 {
            break
        }
        fmt.Printf("  k = %d\n", k)
        k++
    }
    
    fmt.Println("Basic test completed successfully!")
}

func add(a int, b int) int {
    return a + b
}

func multiply(a, b int) int {
    return a * b
}