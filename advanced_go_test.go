package main

import "fmt"

// Test Go 1.18+ Generics
type Number interface {
    int | int64 | float64
}

func Add[T Number](a, b T) T {
    return a + b
}

func Min[T Number](a, b T) T {
    if a < b {
        return a
    }
    return b
}

func testGenerics() {
    intResult := Add(5, 3)
    floatResult := Add(5.5, 3.3)
    
    minInt := Min(10, 20)
    minFloat := Min(10.5, 20.3)
    
    fmt.Printf("Generics - Add int: %d, Add float: %f, Min int: %d, Min float: %f\n", 
        intResult, floatResult, minInt, minFloat)
}

// Test advanced concurrency patterns
func worker(id int, jobs <-chan int, results chan<- int) {
    for j := range jobs {
        fmt.Printf("Worker %d processing job %d\n", id, j)
        results <- j * 2
    }
}

func testAdvancedConcurrency() {
    jobs := make(chan int, 100)
    results := make(chan int, 100)
    
    // Start 3 workers
    for w := 1; w <= 3; w++ {
        go worker(w, jobs, results)
    }
    
    // Send 5 jobs
    for j := 1; j <= 5; j++ {
        jobs <- j
    }
    close(jobs)
    
    // Collect results
    for a := 1; a <= 5; a++ {
        <-results
    }
    
    fmt.Println("Advanced concurrency test completed")
}

// Test select statement
func testSelect() {
    ch1 := make(chan string)
    ch2 := make(chan string)
    
    go func() {
        ch1 <- "Hello"
    }()
    
    go func() {
        ch2 <- "World"
    }()
    
    select {
    case msg1 := <-ch1:
        fmt.Printf("Received from ch1: %s\n", msg1)
    case msg2 := <-ch2:
        fmt.Printf("Received from ch2: %s\n", msg2)
    }
    
    // Select with timeout
    timeout := make(chan bool, 1)
    go func() {
        timeout <- true
    }()
    
    select {
    case <-ch1:
        fmt.Println("Received from ch1")
    case <-ch2:
        fmt.Println("Received from ch2")
    case <-timeout:
        fmt.Println("Timeout")
    }
}

// Test closures and higher-order functions
func testClosures() {
    // Function returning a function
    add := func(x int) func(int) int {
        return func(y int) int {
            return x + y
        }
    }
    
    add5 := add(5)
    result := add5(3)
    
    fmt.Printf("Closure result: %d\n", result)
    
    // Function as parameter
    apply := func(f func(int) int, x int) int {
        return f(x)
    }
    
    double := func(x int) int {
        return x * 2
    }
    
    doubled := apply(double, 10)
    fmt.Printf("Higher-order function result: %d\n", doubled)
}

// Test variadic functions
func sum(nums ...int) int {
    total := 0
    for _, num := range nums {
        total += num
    }
    return total
}

func testVariadic() {
    result1 := sum(1, 2, 3)
    result2 := sum(1, 2, 3, 4, 5)
    
    nums := []int{1, 2, 3, 4, 5, 6}
    result3 := sum(nums...)
    
    fmt.Printf("Variadic results: %d, %d, %d\n", result1, result2, result3)
}

// Test method sets and embedded types
type Animal struct {
    Name string
}

func (a Animal) Speak() string {
    return fmt.Sprintf("%s makes a sound", a.Name)
}

type Dog struct {
    Animal
    Breed string
}

func (d Dog) Speak() string {
    return fmt.Sprintf("%s the %s barks", d.Name, d.Breed)
}

func testEmbeddedTypes() {
    animal := Animal{Name: "Generic"}
    dog := Dog{
        Animal: Animal{Name: "Rex"},
        Breed:  "German Shepherd",
    }
    
    fmt.Printf("Animal: %s\n", animal.Speak())
    fmt.Printf("Dog: %s\n", dog.Speak())
    fmt.Printf("Dog embedded Animal: %s\n", dog.Animal.Speak())
}

// Test complex composite literals
func testCompositeLiterals() {
    // Struct with nested struct
    type Address struct {
        Street string
        City   string
    }
    
    type Person struct {
        Name    string
        Age     int
        Address Address
    }
    
    person := Person{
        Name: "John",
        Age:  30,
        Address: Address{
            Street: "123 Main St",
            City:   "New York",
        },
    }
    
    fmt.Printf("Composite literal: %+v\n", person)
    
    // Slice of structs
    people := []Person{
        {Name: "Alice", Age: 25, Address: Address{"456 Oak Ave", "Boston"}},
        {Name: "Bob", Age: 35, Address: Address{"789 Pine Rd", "Chicago"}},
    }
    
    fmt.Printf("Slice of structs: %+v\n", people)
}

// Test constant declarations
func testConstants() {
    const (
        Pi = 3.14159
        MaxInt = 1<<31 - 1
        MinInt = -1 << 31
        Greeting = "Hello"
    )
    
    fmt.Printf("Constants: Pi=%f, MaxInt=%d, MinInt=%d, Greeting=%s\n", 
        Pi, MaxInt, MinInt, Greeting)
}

// Test iota for enum-like constants
func testIota() {
    const (
        Monday = iota
        Tuesday
        Wednesday
        Thursday
        Friday
        Saturday
        Sunday
    )
    
    const (
        Read = 1 << iota
        Write
        Execute
    )
    
    fmt.Printf("Days: %d, %d, %d\n", Monday, Wednesday, Sunday)
    fmt.Printf("Permissions: Read=%d, Write=%d, Execute=%d\n", Read, Write, Execute)
}

// Test type aliases and definitions
func testTypeDefinitions() {
    type Integer int
    type Float float64
    type String string
    
    var i Integer = 42
    var f Float = 3.14
    var s String = "Hello"
    
    fmt.Printf("Type definitions: Integer=%d, Float=%f, String=%s\n", i, f, s)
}

// Test complex numbers
func testComplexNumbers() {
    var c1 complex128 = 3 + 4i
    var c2 complex64 = 1 + 2i
    
    fmt.Printf("Complex numbers: %v, %v\n", c1, c2)
    fmt.Printf("Real parts: %f, %f\n", real(c1), real(c2))
    fmt.Printf("Imaginary parts: %f, %f\n", imag(c1), imag(c2))
}

// Test rune literals and string operations
func testRunesAndStrings() {
    s := "Hello, 世界"
    
    fmt.Printf("String: %s\n", s)
    fmt.Printf("String length: %d\n", len(s))
    
    // Iterate over runes
    for i, r := range s {
        fmt.Printf("Rune %d: %c (Unicode: %U)\n", i, r, r)
    }
}

// Test bit operations
func testBitOperations() {
    a := 0b1010
    b := 0b1100
    
    fmt.Printf("Bit operations:\n")
    fmt.Printf("a & b = %b\n", a & b)
    fmt.Printf("a | b = %b\n", a | b)
    fmt.Printf("a ^ b = %b\n", a ^ b)
    fmt.Printf("a << 1 = %b\n", a << 1)
    fmt.Printf("a >> 1 = %b\n", a >> 1)
    fmt.Printf("^a = %b\n", ^a)
}

// Test empty interface and type switches
func testEmptyInterface() {
    var i interface{}
    
    i = 42
    processInterface(i)
    
    i = "hello"
    processInterface(i)
    
    i = 3.14
    processInterface(i)
    
    i = []int{1, 2, 3}
    processInterface(i)
}

func processInterface(i interface{}) {
    switch v := i.(type) {
    case int:
        fmt.Printf("Interface contains int: %d\n", v)
    case string:
        fmt.Printf("Interface contains string: %s\n", v)
    case float64:
        fmt.Printf("Interface contains float64: %f\n", v)
    case []int:
        fmt.Printf("Interface contains int slice: %v\n", v)
    default:
        fmt.Printf("Interface contains unknown type: %T\n", v)
    }
}

// Main function for advanced tests
func main() {
    fmt.Println("=== Advanced Go Feature Test Suite ===")
    
    fmt.Println("\n1. Generics:")
    testGenerics()
    
    fmt.Println("\n2. Advanced Concurrency:")
    testAdvancedConcurrency()
    
    fmt.Println("\n3. Select Statement:")
    testSelect()
    
    fmt.Println("\n4. Closures:")
    testClosures()
    
    fmt.Println("\n5. Variadic Functions:")
    testVariadic()
    
    fmt.Println("\n6. Embedded Types:")
    testEmbeddedTypes()
    
    fmt.Println("\n7. Composite Literals:")
    testCompositeLiterals()
    
    fmt.Println("\n8. Constants:")
    testConstants()
    
    fmt.Println("\n9. Iota:")
    testIota()
    
    fmt.Println("\n10. Type Definitions:")
    testTypeDefinitions()
    
    fmt.Println("\n11. Complex Numbers:")
    testComplexNumbers()
    
    fmt.Println("\n12. Runes and Strings:")
    testRunesAndStrings()
    
    fmt.Println("\n13. Bit Operations:")
    testBitOperations()
    
    fmt.Println("\n14. Empty Interface:")
    testEmptyInterface()
    
    fmt.Println("\n=== Advanced Test Suite Complete ===")
}