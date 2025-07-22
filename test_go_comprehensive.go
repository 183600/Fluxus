package main

import (
	"fmt"
	"math"
	"strconv"
	"strings"
	"sort"
	"time"
	"os"
	"encoding/json"
)

// Test basic data types and variable declarations
func testBasicTypes() {
	var intVar int = 42
	var floatVar float64 = 3.14159
	var stringVar string = "Hello, Go!"
	var boolVar bool = true
	
	fmt.Printf("Integer: %d\n", intVar)
	fmt.Printf("Float: %.2f\n", floatVar)
	fmt.Printf("String: %s\n", stringVar)
	fmt.Printf("Boolean: %t\n", boolVar)
}

// Test arrays and slices
func testArraysSlices() {
	// Array
	var arr [5]int = [5]int{1, 2, 3, 4, 5}
	fmt.Printf("Array: %v\n", arr)
	
	// Slice operations
	slice := []int{10, 20, 30, 40, 50}
	slice = append(slice, 60)
	fmt.Printf("Slice after append: %v\n", slice)
	
	// Slice manipulation
	subSlice := slice[1:4]
	fmt.Printf("Sub-slice [1:4]: %v\n", subSlice)
}

// Test maps
func testMaps() {
	m := make(map[string]int)
	m["apple"] = 5
	m["banana"] = 3
	m["cherry"] = 8
	
	fmt.Printf("Map: %v\n", m)
	
	// Check if key exists
	if val, exists := m["apple"]; exists {
		fmt.Printf("apple count: %d\n", val)
	}
	
	// Delete from map
	delete(m, "banana")
	fmt.Printf("Map after delete: %v\n", m)
}

// Test structs and methods
type Person struct {
	Name string
	Age  int
}

func (p Person) GetInfo() string {
	return fmt.Sprintf("Name: %s, Age: %d", p.Name, p.Age)
}

func (p *Person) Birthday() {
	p.Age++
}

func testStructs() {
	person := Person{Name: "Alice", Age: 30}
	fmt.Printf("Person info: %s\n", person.GetInfo())
	
	person.Birthday()
	fmt.Printf("After birthday: %s\n", person.GetInfo())
}

// Test functions with multiple return values
func divide(a, b float64) (float64, error) {
	if b == 0 {
		return 0, fmt.Errorf("division by zero")
	}
	return a / b, nil
}

func testMultipleReturns() {
	result, err := divide(10.0, 3.0)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("Division result: %.3f\n", result)
	}
	
	// Test error case
	_, err = divide(10.0, 0.0)
	if err != nil {
		fmt.Printf("Expected error: %v\n", err)
	}
}

// Test interfaces
type Shape interface {
	Area() float64
	Perimeter() float64
}

type Rectangle struct {
	Width, Height float64
}

func (r Rectangle) Area() float64 {
	return r.Width * r.Height
}

func (r Rectangle) Perimeter() float64 {
	return 2 * (r.Width + r.Height)
}

type Circle struct {
	Radius float64
}

func (c Circle) Area() float64 {
	return math.Pi * c.Radius * c.Radius
}

func (c Circle) Perimeter() float64 {
	return 2 * math.Pi * c.Radius
}

func testInterfaces() {
	shapes := []Shape{
		Rectangle{Width: 10, Height: 5},
		Circle{Radius: 7},
	}
	
	for i, shape := range shapes {
		fmt.Printf("Shape %d - Area: %.2f, Perimeter: %.2f\n", 
			i+1, shape.Area(), shape.Perimeter())
	}
}

// Test goroutines and channels
func worker(id int, jobs <-chan int, results chan<- int) {
	for j := range jobs {
		fmt.Printf("Worker %d processing job %d\n", id, j)
		time.Sleep(100 * time.Millisecond)
		results <- j * 2
	}
}

func testConcurrency() {
	const numJobs = 5
	jobs := make(chan int, numJobs)
	results := make(chan int, numJobs)
	
	// Start 3 workers
	for w := 1; w <= 3; w++ {
		go worker(w, jobs, results)
	}
	
	// Send jobs
	for j := 1; j <= numJobs; j++ {
		jobs <- j
	}
	close(jobs)
	
	// Collect results
	for a := 1; a <= numJobs; a++ {
		result := <-results
		fmt.Printf("Result: %d\n", result)
	}
}

// Test control flow
func testControlFlow() {
	// If-else
	x := 15
	if x > 10 {
		fmt.Printf("%d is greater than 10\n", x)
	} else if x == 10 {
		fmt.Printf("%d equals 10\n", x)
	} else {
		fmt.Printf("%d is less than 10\n", x)
	}
	
	// Switch
	day := time.Now().Weekday()
	switch day {
	case time.Monday:
		fmt.Println("It's Monday")
	case time.Tuesday:
		fmt.Println("It's Tuesday")
	default:
		fmt.Printf("It's %s\n", day)
	}
	
	// For loops
	fmt.Print("Numbers 1-5: ")
	for i := 1; i <= 5; i++ {
		fmt.Printf("%d ", i)
	}
	fmt.Println()
	
	// Range over slice
	fruits := []string{"apple", "banana", "cherry"}
	fmt.Print("Fruits: ")
	for _, fruit := range fruits {
		fmt.Printf("%s ", fruit)
	}
	fmt.Println()
}

// Test string operations
func testStringOperations() {
	str := "Hello, World!"
	
	fmt.Printf("Original: %s\n", str)
	fmt.Printf("Upper: %s\n", strings.ToUpper(str))
	fmt.Printf("Lower: %s\n", strings.ToLower(str))
	fmt.Printf("Contains 'World': %t\n", strings.Contains(str, "World"))
	fmt.Printf("Replace: %s\n", strings.Replace(str, "World", "Go", 1))
	
	parts := strings.Split("a,b,c,d", ",")
	fmt.Printf("Split result: %v\n", parts)
	
	joined := strings.Join(parts, "-")
	fmt.Printf("Joined: %s\n", joined)
}

// Test math operations
func testMathOperations() {
	fmt.Printf("Pi: %.6f\n", math.Pi)
	fmt.Printf("E: %.6f\n", math.E)
	fmt.Printf("Sqrt(16): %.1f\n", math.Sqrt(16))
	fmt.Printf("Pow(2, 8): %.0f\n", math.Pow(2, 8))
	fmt.Printf("Sin(Pi/2): %.3f\n", math.Sin(math.Pi/2))
	fmt.Printf("Cos(0): %.1f\n", math.Cos(0))
	fmt.Printf("Max(10, 20): %.0f\n", math.Max(10, 20))
	fmt.Printf("Min(10, 20): %.0f\n", math.Min(10, 20))
	fmt.Printf("Abs(-5.5): %.1f\n", math.Abs(-5.5))
	fmt.Printf("Ceil(4.2): %.0f\n", math.Ceil(4.2))
	fmt.Printf("Floor(4.8): %.0f\n", math.Floor(4.8))
}

// Test type conversions
func testTypeConversions() {
	// String to int
	strNum := "42"
	if num, err := strconv.Atoi(strNum); err == nil {
		fmt.Printf("String to int: %s -> %d\n", strNum, num)
	}
	
	// Int to string
	intNum := 123
	str := strconv.Itoa(intNum)
	fmt.Printf("Int to string: %d -> %s\n", intNum, str)
	
	// String to float
	strFloat := "3.14159"
	if f, err := strconv.ParseFloat(strFloat, 64); err == nil {
		fmt.Printf("String to float: %s -> %.3f\n", strFloat, f)
	}
	
	// Float to string
	floatVal := 2.71828
	floatStr := strconv.FormatFloat(floatVal, 'f', 3, 64)
	fmt.Printf("Float to string: %.5f -> %s\n", floatVal, floatStr)
}

// Test sorting
func testSorting() {
	// Sort integers
	ints := []int{64, 34, 25, 12, 22, 11, 90}
	fmt.Printf("Before sort: %v\n", ints)
	sort.Ints(ints)
	fmt.Printf("After sort: %v\n", ints)
	
	// Sort strings
	strs := []string{"banana", "apple", "cherry", "date"}
	fmt.Printf("Before sort: %v\n", strs)
	sort.Strings(strs)
	fmt.Printf("After sort: %v\n", strs)
	
	// Custom sort
	people := []Person{
		{"Alice", 30},
		{"Bob", 25},
		{"Charlie", 35},
	}
	fmt.Printf("Before custom sort: %v\n", people)
	sort.Slice(people, func(i, j int) bool {
		return people[i].Age < people[j].Age
	})
	fmt.Printf("After custom sort by age: %v\n", people)
}

// Test JSON operations
func testJSON() {
	data := map[string]interface{}{
		"name":    "John Doe",
		"age":     30,
		"city":    "New York",
		"hobbies": []string{"reading", "swimming", "coding"},
	}
	
	// Marshal to JSON
	jsonData, err := json.Marshal(data)
	if err != nil {
		fmt.Printf("JSON marshal error: %v\n", err)
		return
	}
	fmt.Printf("JSON: %s\n", string(jsonData))
	
	// Unmarshal from JSON
	var result map[string]interface{}
	err = json.Unmarshal(jsonData, &result)
	if err != nil {
		fmt.Printf("JSON unmarshal error: %v\n", err)
		return
	}
	fmt.Printf("Unmarshaled: %v\n", result)
}

// Test file operations
func testFileOperations() {
	filename := "test_file_go.txt"
	content := "Hello from Go file operations!"
	
	// Write to file
	err := os.WriteFile(filename, []byte(content), 0644)
	if err != nil {
		fmt.Printf("Write error: %v\n", err)
		return
	}
	fmt.Printf("Successfully wrote to %s\n", filename)
	
	// Read from file
	data, err := os.ReadFile(filename)
	if err != nil {
		fmt.Printf("Read error: %v\n", err)
		return
	}
	fmt.Printf("Read from file: %s\n", string(data))
	
	// Clean up
	err = os.Remove(filename)
	if err != nil {
		fmt.Printf("Remove error: %v\n", err)
	} else {
		fmt.Printf("Successfully cleaned up %s\n", filename)
	}
}

// Test error handling patterns
func testErrorHandling() {
	// Function that might return an error
	processValue := func(value int) error {
		if value < 0 {
			return fmt.Errorf("negative value not allowed: %d", value)
		}
		if value > 100 {
			return fmt.Errorf("value too large: %d", value)
		}
		fmt.Printf("Processing value: %d\n", value)
		return nil
	}
	
	testValues := []int{50, -10, 150, 75}
	for _, val := range testValues {
		if err := processValue(val); err != nil {
			fmt.Printf("Error processing %d: %v\n", val, err)
		}
	}
}

// Test pointers
func testPointers() {
	x := 42
	p := &x
	
	fmt.Printf("Value of x: %d\n", x)
	fmt.Printf("Address of x: %p\n", &x)
	fmt.Printf("Value of p: %p\n", p)
	fmt.Printf("Value pointed by p: %d\n", *p)
	
	// Modify through pointer
	*p = 100
	fmt.Printf("Value of x after modification through pointer: %d\n", x)
}

func main() {
	fmt.Println("=== Go Comprehensive Test Suite ===")
	
	fmt.Println("\n1. Basic Types Test:")
	testBasicTypes()
	
	fmt.Println("\n2. Arrays and Slices Test:")
	testArraysSlices()
	
	fmt.Println("\n3. Maps Test:")
	testMaps()
	
	fmt.Println("\n4. Structs Test:")
	testStructs()
	
	fmt.Println("\n5. Multiple Returns Test:")
	testMultipleReturns()
	
	fmt.Println("\n6. Interfaces Test:")
	testInterfaces()
	
	fmt.Println("\n7. Concurrency Test:")
	testConcurrency()
	
	fmt.Println("\n8. Control Flow Test:")
	testControlFlow()
	
	fmt.Println("\n9. String Operations Test:")
	testStringOperations()
	
	fmt.Println("\n10. Math Operations Test:")
	testMathOperations()
	
	fmt.Println("\n11. Type Conversions Test:")
	testTypeConversions()
	
	fmt.Println("\n12. Sorting Test:")
	testSorting()
	
	fmt.Println("\n13. JSON Test:")
	testJSON()
	
	fmt.Println("\n14. File Operations Test:")
	testFileOperations()
	
	fmt.Println("\n15. Error Handling Test:")
	testErrorHandling()
	
	fmt.Println("\n16. Pointers Test:")
	testPointers()
	
	fmt.Println("\n=== All tests completed ===")
}