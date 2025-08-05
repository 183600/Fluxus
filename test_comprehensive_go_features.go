package main

import "fmt"

// Basic variable declarations
var globalInt int = 42
var globalString string = "Hello, World!"
var globalBool bool = true

// Function declarations
func add(a, b int) int {
    return a + b
}

func subtract(a, b int) int {
    return a - b
}

func multiply(a, b int) int {
    return a * b
}

func divide(a, b int) int {
    return a / b
}

// Multiple return values
func divideWithRemainder(a, b int) (int, int) {
    return a / b, a % b
}

// Named return values
func namedReturn(x, y int) (sum int, product int) {
    sum = x + y
    product = x * y
    return
}

// Variadic functions
func sum(numbers ...int) int {
    total := 0
    for _, num := range numbers {
        total += num
    }
    return total
}

// Function as value
func functionValue() {
    fmt.Println("Function as value called")
}

// Struct definitions
type Person struct {
    Name string
    Age  int
    City string
}

type Point struct {
    X, Y int
}

// Methods
func (p Person) Greet() string {
    return fmt.Sprintf("Hello, my name is %s and I'm %d years old", p.Name, p.Age)
}

func (p *Person) HaveBirthday() {
    p.Age++
}

// Interfaces
type Shape interface {
    Area() float64
    Perimeter() float64
}

type Rectangle struct {
    Width, Height float64
}

type Circle struct {
    Radius float64
}

func (r Rectangle) Area() float64 {
    return r.Width * r.Height
}

func (r Rectangle) Perimeter() float64 {
    return 2 * (r.Width + r.Height)
}

func (c Circle) Area() float64 {
    return 3.14159 * c.Radius * c.Radius
}

func (c Circle) Perimeter() float64 {
    return 2 * 3.14159 * c.Radius
}

// Slices and arrays
func sliceOperations() {
    // Array
    arr := [5]int{1, 2, 3, 4, 5}
    
    // Slice
    slice := []int{1, 2, 3, 4, 5}
    
    // Slice operations
    slice = append(slice, 6)
    slice = slice[1:4]
    
    // Make slice
    dynamicSlice := make([]int, 10)
    dynamicSlice[0] = 42
}

// Maps
func mapOperations() {
    // Map literal
    m := map[string]int{
        "apple":  1,
        "banana": 2,
        "orange": 3,
    }
    
    // Add to map
    m["grape"] = 4
    
    // Delete from map
    delete(m, "banana")
    
    // Check if key exists
    if value, exists := m["apple"]; exists {
        fmt.Printf("Apple exists: %d\n", value)
    }
}

// Control flow
func controlFlow() {
    // If-else
    x := 10
    if x > 5 {
        fmt.Println("x is greater than 5")
    } else if x < 5 {
        fmt.Println("x is less than 5")
    } else {
        fmt.Println("x is equal to 5")
    }
    
    // Switch
    switch x {
    case 1:
        fmt.Println("x is 1")
    case 10:
        fmt.Println("x is 10")
    default:
        fmt.Println("x is something else")
    }
    
    // For loops
    for i := 0; i < 5; i++ {
        fmt.Println(i)
    }
    
    // Range loop
    numbers := []int{1, 2, 3, 4, 5}
    for i, num := range numbers {
        fmt.Printf("Index %d: %d\n", i, num)
    }
    
    // While-like loop
    y := 0
    for y < 5 {
        fmt.Println(y)
        y++
    }
}

// Error handling
func divideWithError(a, b int) (int, error) {
    if b == 0 {
        return 0, fmt.Errorf("division by zero")
    }
    return a / b, nil
}

func errorHandling() {
    result, err := divideWithError(10, 2)
    if err != nil {
        fmt.Println("Error:", err)
    } else {
        fmt.Println("Result:", result)
    }
}

// Goroutines and channels
func worker(id int, jobs <-chan int, results chan<- int) {
    for j := range jobs {
        fmt.Printf("Worker %d processing job %d\n", id, j)
        results <- j * 2
    }
}

func concurrencyDemo() {
    jobs := make(chan int, 100)
    results := make(chan int, 100)
    
    // Start workers
    for w := 1; w <= 3; w++ {
        go worker(w, jobs, results)
    }
    
    // Send jobs
    for j := 1; j <= 5; j++ {
        jobs <- j
    }
    close(jobs)
    
    // Collect results
    for a := 1; a <= 5; a++ {
        <-results
    }
}

// Defer, panic, recover
func deferDemo() {
    defer fmt.Println("This will be printed last")
    fmt.Println("This will be printed first")
}

func panicDemo() {
    defer func() {
        if r := recover(); r != nil {
            fmt.Println("Recovered from panic:", r)
        }
    }()
    
    panic("This is a panic")
}

// Pointers
func pointerDemo() {
    x := 42
    p := &x
    *p = 21
    fmt.Println("x is now", x)
}

// Constants
const (
    Pi = 3.14159
    MaxInt = 2147483647
)

// Iota
const (
    Sunday = iota
    Monday
    Tuesday
    Wednesday
    Thursday
    Friday
    Saturday
)

func main() {
    // Test basic operations
    fmt.Println("Basic operations:")
    fmt.Printf("5 + 3 = %d\n", add(5, 3))
    fmt.Printf("10 - 4 = %d\n", subtract(10, 4))
    fmt.Printf("6 * 7 = %d\n", multiply(6, 7))
    fmt.Printf("15 / 3 = %d\n", divide(15, 3))
    
    // Multiple return values
    quotient, remainder := divideWithRemainder(17, 5)
    fmt.Printf("17 / 5 = %d remainder %d\n", quotient, remainder)
    
    // Named return values
    sum, product := namedReturn(8, 9)
    fmt.Printf("8 + 9 = %d, 8 * 9 = %d\n", sum, product)
    
    // Variadic functions
    total := sum(1, 2, 3, 4, 5)
    fmt.Printf("Sum of 1+2+3+4+5 = %d\n", total)
    
    // Structs and methods
    person := Person{Name: "Alice", Age: 30, City: "New York"}
    fmt.Println(person.Greet())
    person.HaveBirthday()
    fmt.Printf("After birthday: %s\n", person.Greet())
    
    // Interfaces
    rect := Rectangle{Width: 10, Height: 5}
    circle := Circle{Radius: 7}
    
    shapes := []Shape{rect, circle}
    for _, shape := range shapes {
        fmt.Printf("Shape area: %.2f, perimeter: %.2f\n", shape.Area(), shape.Perimeter())
    }
    
    // Control flow
    controlFlow()
    
    // Error handling
    errorHandling()
    
    // Concurrency
    concurrencyDemo()
    
    // Pointers
    pointerDemo()
    
    // Defer
    deferDemo()
    
    // Panic and recover
    panicDemo()
    
    // Constants
    fmt.Printf("Pi = %.5f, MaxInt = %d\n", Pi, MaxInt)
    
    // Iota
    days := []string{Sunday: "Sunday", Monday: "Monday", Tuesday: "Tuesday", Wednesday: "Wednesday", Thursday: "Thursday", Friday: "Friday", Saturday: "Saturday"}
    for i, day := range days {
        fmt.Printf("Day %d: %s\n", i, day)
    }
    
    fmt.Println("All tests completed successfully!")
}