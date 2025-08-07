package main

import "fmt"

// Test functions
func add(a, b int) int {
    return a + b
}

func multiply(a, b int) int {
    return a * b
}

// Test control flow
func testIfElse(x int) string {
    if x > 10 {
        return "greater than 10"
    } else if x < 5 {
        return "less than 5"
    } else {
        return "between 5 and 10"
    }
}

// Test for loops
func testForLoop() int {
    sum := 0
    for i := 1; i <= 10; i++ {
        sum += i
    }
    return sum
}

// Test while-like loops
func testWhileLoop() int {
    sum := 0
    i := 1
    for i <= 10 {
        sum += i
        i++
    }
    return sum
}

// Test infinite loops with break
func testInfiniteLoop() int {
    sum := 0
    i := 1
    for {
        if i > 10 {
            break
        }
        sum += i
        i++
    }
    return sum
}

// Test switch statements
func testSwitch(day string) string {
    switch day {
    case "Monday":
        return "Work day"
    case "Tuesday":
        return "Work day"
    case "Wednesday":
        return "Work day"
    case "Thursday":
        return "Work day"
    case "Friday":
        return "Work day"
    case "Saturday":
        return "Weekend"
    case "Sunday":
        return "Weekend"
    default:
        return "Invalid day"
    }
}

func main() {
    // Test functions
    fmt.Println("Testing functions:")
    fmt.Println("add(5, 3) =", add(5, 3))
    fmt.Println("multiply(4, 7) =", multiply(4, 7))
    
    // Test control flow
    fmt.Println("\nTesting control flow:")
    fmt.Println("testIfElse(15) =", testIfElse(15))
    fmt.Println("testIfElse(3) =", testIfElse(3))
    fmt.Println("testIfElse(7) =", testIfElse(7))
    
    // Test loops
    fmt.Println("\nTesting loops:")
    fmt.Println("testForLoop() =", testForLoop())
    fmt.Println("testWhileLoop() =", testWhileLoop())
    fmt.Println("testInfiniteLoop() =", testInfiniteLoop())
    
    // Test switch
    fmt.Println("\nTesting switch:")
    fmt.Println("testSwitch(\"Monday\") =", testSwitch("Monday"))
    fmt.Println("testSwitch(\"Saturday\") =", testSwitch("Saturday"))
    fmt.Println("testSwitch(\"Invalid\") =", testSwitch("Invalid"))
}