package main

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"reflect"
	"strings"
	"sync"
	"time"
)

// Test interface implementations
type Animal interface {
	Speak() string
	Move() string
}

type Dog struct {
	Name string
	Breed string
}

func (d Dog) Speak() string {
	return "Woof!"
}

func (d Dog) Move() string {
	return "Running"
}

type Cat struct {
	Name string
	Color string
}

func (c Cat) Speak() string {
	return "Meow!"
}

func (c Cat) Move() string {
	return "Prowling"
}

// Test embedded structs
type Person struct {
	Name string
	Age  int
}

func (p Person) Greet() string {
	return fmt.Sprintf("Hello, I'm %s", p.Name)
}

type Employee struct {
	Person
	ID       int
	Position string
}

func (e Employee) Work() string {
	return fmt.Sprintf("%s is working as %s", e.Name, e.Position)
}

// Test methods with pointer receivers
type Counter struct {
	count int
	mutex sync.Mutex
}

func (c *Counter) Increment() {
	c.mutex.Lock()
	defer c.mutex.Unlock()
	c.count++
}

func (c *Counter) GetCount() int {
	c.mutex.Lock()
	defer c.mutex.Unlock()
	return c.count
}

func (c *Counter) Reset() {
	c.mutex.Lock()
	defer c.mutex.Unlock()
	c.count = 0
}

// Test goroutines and channels
func testConcurrency() map[string]interface{} {
	results := make(map[string]interface{})
	
	// Test basic goroutine
	done := make(chan bool)
	go func() {
		time.Sleep(100 * time.Millisecond)
		done <- true
	}()
	
	select {
	case <-done:
		results["goroutine_completed"] = true
	case <-time.After(200 * time.Millisecond):
		results["goroutine_completed"] = false
	}
	
	// Test worker pool pattern
	jobs := make(chan int, 100)
	results_chan := make(chan int, 100)
	
	// Start workers
	for w := 1; w <= 3; w++ {
		go func(id int) {
			for j := range jobs {
				result := j * j
				results_chan <- result
			}
		}(w)
	}
	
	// Send jobs
	for j := 1; j <= 5; j++ {
		jobs <- j
	}
	close(jobs)
	
	// Collect results
	var workerResults []int
	for a := 1; a <= 5; a++ {
		workerResults = append(workerResults, <-results_chan)
	}
	
	results["worker_pool_results"] = workerResults
	
	// Test buffered channel
	buffered := make(chan string, 3)
	buffered <- "first"
	buffered <- "second"
	buffered <- "third"
	
	var bufferedResults []string
	close(buffered)
	for val := range buffered {
		bufferedResults = append(bufferedResults, val)
	}
	results["buffered_channel"] = bufferedResults
	
	return results
}

// Test context usage
func testContext() map[string]interface{} {
	results := make(map[string]interface{})
	
	// Test context with timeout
	ctx, cancel := context.WithTimeout(context.Background(), 50*time.Millisecond)
	defer cancel()
	
	select {
	case <-time.After(100 * time.Millisecond):
		results["timeout_test"] = false
	case <-ctx.Done():
		results["timeout_test"] = true
		results["timeout_error"] = ctx.Err().Error()
	}
	
	// Test context with value
	ctx = context.WithValue(context.Background(), "key", "value")
	if val := ctx.Value("key"); val != nil {
		results["context_value"] = val.(string)
	}
	
	// Test context cancellation
	ctx, cancel = context.WithCancel(context.Background())
	go func() {
		time.Sleep(10 * time.Millisecond)
		cancel()
	}()
	
	select {
	case <-ctx.Done():
		results["cancellation_test"] = true
	case <-time.After(100 * time.Millisecond):
		results["cancellation_test"] = false
	}
	
	return results
}

// Test reflection
func testReflection() map[string]interface{} {
	results := make(map[string]interface{})
	
	// Test basic reflection
	var x interface{} = 42
	v := reflect.ValueOf(x)
	t := reflect.TypeOf(x)
	
	results["value_kind"] = v.Kind().String()
	results["type_name"] = t.Name()
	results["value_int"] = v.Int()
	
	// Test struct reflection
	person := Person{Name: "Alice", Age: 30}
	personValue := reflect.ValueOf(person)
	personType := reflect.TypeOf(person)
	
	results["struct_name"] = personType.Name()
	results["num_fields"] = personValue.NumField()
	
	// Get field values
	fieldValues := make(map[string]interface{})
	for i := 0; i < personValue.NumField(); i++ {
		field := personType.Field(i)
		value := personValue.Field(i)
		fieldValues[field.Name] = value.Interface()
	}
	results["field_values"] = fieldValues
	
	// Test method reflection
	results["num_methods"] = personValue.NumMethod()
	
	// Call method via reflection
	greetMethod := personValue.MethodByName("Greet")
	if greetMethod.IsValid() {
		greetResult := greetMethod.Call(nil)
		results["method_call_result"] = greetResult[0].String()
	}
	
	return results
}

// Test custom error types
type ValidationError struct {
	Field   string
	Message string
	Code    int
}

func (e ValidationError) Error() string {
	return fmt.Sprintf("Validation error in field '%s': %s (code: %d)", e.Field, e.Message, e.Code)
}

func validateAge(age int) error {
	if age < 0 {
		return ValidationError{
			Field:   "age",
			Message: "age cannot be negative",
			Code:    1001,
		}
	}
	if age > 150 {
		return ValidationError{
			Field:   "age",
			Message: "age seems unrealistic",
			Code:    1002,
		}
	}
	return nil
}

func testErrorHandling() map[string]interface{} {
	results := make(map[string]interface{})
	
	// Test custom error
	err := validateAge(-5)
	if err != nil {
		if validationErr, ok := err.(ValidationError); ok {
			results["custom_error_field"] = validationErr.Field
			results["custom_error_code"] = validationErr.Code
		}
		results["custom_error_message"] = err.Error()
	}
	
	// Test error wrapping
	originalErr := errors.New("original error")
	wrappedErr := fmt.Errorf("wrapped: %w", originalErr)
	
	results["wrapped_error"] = wrappedErr.Error()
	results["unwrap_success"] = errors.Unwrap(wrappedErr) == originalErr
	
	// Test error is
	results["error_is"] = errors.Is(wrappedErr, originalErr)
	
	return results
}

// Test generic-like patterns using interfaces
type Comparable interface {
	Compare(other Comparable) int
}

type IntComparable int

func (i IntComparable) Compare(other Comparable) int {
	if otherInt, ok := other.(IntComparable); ok {
		if i < otherInt {
			return -1
		} else if i > otherInt {
			return 1
		}
		return 0
	}
	return 0
}

func testGenericPatterns() map[string]interface{} {
	results := make(map[string]interface{})
	
	// Test comparable interface
	a := IntComparable(5)
	b := IntComparable(10)
	
	results["compare_result"] = a.Compare(b)
	results["reverse_compare"] = b.Compare(a)
	results["equal_compare"] = a.Compare(IntComparable(5))
	
	// Test empty interface usage
	var items []interface{}
	items = append(items, 42)
	items = append(items, "hello")
	items = append(items, true)
	items = append(items, []int{1, 2, 3})
	
	var itemTypes []string
	for _, item := range items {
		itemTypes = append(itemTypes, reflect.TypeOf(item).String())
	}
	results["interface_types"] = itemTypes
	
	return results
}

// Test panic and recover
func testPanicRecover() map[string]interface{} {
	results := make(map[string]interface{})
	
	// Test recover from panic
	func() {
		defer func() {
			if r := recover(); r != nil {
				results["panic_recovered"] = true
				results["panic_value"] = fmt.Sprintf("%v", r)
			}
		}()
		
		panic("test panic")
	}()
	
	// Test function that might panic
	results["division_result"] = func() (result interface{}) {
		defer func() {
			if r := recover(); r != nil {
				result = "panic: division by zero"
			}
		}()
		
		// This will cause a panic at runtime, not compile time
		var zero int = 0
		return 10 / zero
	}()
	
	return results
}

// Test defer statements
func testDefer() map[string]interface{} {
	results := make(map[string]interface{})
	
	// Test defer order (LIFO)
	var order []int
	func() {
		defer func() { order = append(order, 1) }()
		defer func() { order = append(order, 2) }()
		defer func() { order = append(order, 3) }()
	}()
	
	results["defer_order"] = order
	
	// Test defer with return value modification
	getValue := func() (result int) {
		defer func() {
			result *= 2
		}()
		return 5
	}
	
	results["defer_return_modification"] = getValue()
	
	return results
}

// Test select statement
func testSelect() map[string]interface{} {
	results := make(map[string]interface{})
	
	ch1 := make(chan string, 1)
	ch2 := make(chan string, 1)
	
	ch1 <- "from ch1"
	
	select {
	case msg1 := <-ch1:
		results["select_result"] = msg1
	case msg2 := <-ch2:
		results["select_result"] = msg2
	default:
		results["select_result"] = "default case"
	}
	
	// Test select with timeout
	timeout := make(chan bool, 1)
	go func() {
		time.Sleep(50 * time.Millisecond)
		timeout <- true
	}()
	
	select {
	case <-timeout:
		results["timeout_select"] = "timeout occurred"
	case <-time.After(10 * time.Millisecond):
		results["timeout_select"] = "time.After triggered"
	}
	
	return results
}

func main() {
	fmt.Println("Testing Advanced Go Features in Compiled Code")
	fmt.Println(strings.Repeat("=", 60))
	
	// Test interfaces
	var animals []Animal
	animals = append(animals, Dog{Name: "Buddy", Breed: "Golden Retriever"})
	animals = append(animals, Cat{Name: "Whiskers", Color: "Orange"})
	
	fmt.Println("Interface test:")
	for _, animal := range animals {
		fmt.Printf("Animal speaks: %s, moves: %s\n", animal.Speak(), animal.Move())
	}
	
	// Test embedded structs
	emp := Employee{
		Person:   Person{Name: "John", Age: 30},
		ID:       123,
		Position: "Developer",
	}
	fmt.Printf("Embedded struct: %s, %s\n", emp.Greet(), emp.Work())
	
	// Test pointer methods
	counter := &Counter{}
	counter.Increment()
	counter.Increment()
	fmt.Printf("Counter value: %d\n", counter.GetCount())
	
	// Run all test functions
	tests := map[string]func() map[string]interface{}{
		"Concurrency":       testConcurrency,
		"Context":           testContext,
		"Reflection":        testReflection,
		"Error Handling":    testErrorHandling,
		"Generic Patterns":  testGenericPatterns,
		"Panic/Recover":     testPanicRecover,
		"Defer":             testDefer,
		"Select":            testSelect,
	}
	
	allResults := make(map[string]interface{})
	successful := 0
	
	for testName, testFunc := range tests {
		fmt.Printf("\nTesting %s...\n", testName)
		
		func() {
			defer func() {
				if r := recover(); r != nil {
					fmt.Printf("✗ %s failed with panic: %v\n", testName, r)
					allResults[testName] = map[string]interface{}{"error": fmt.Sprintf("panic: %v", r)}
				}
			}()
			
			result := testFunc()
			allResults[testName] = result
			fmt.Printf("✓ %s completed successfully\n", testName)
			successful++
		}()
	}
	
	fmt.Println("\n" + strings.Repeat("=", 60))
	fmt.Println("Advanced Features Test Summary:")
	fmt.Printf("Total tests: %d\n", len(tests))
	fmt.Printf("Successful: %d\n", successful)
	fmt.Printf("Failed: %d\n", len(tests)-successful)
	
	// Save results to JSON file
	jsonResults, err := json.MarshalIndent(allResults, "", "  ")
	if err == nil {
		err = ioutil.WriteFile("go_advanced_test_results.json", jsonResults, 0644)
		if err == nil {
			fmt.Println("\nDetailed results saved to 'go_advanced_test_results.json'")
		}
	}
	
	if successful == len(tests) {
		fmt.Println("\n🎉 All advanced feature tests passed!")
	} else {
		fmt.Printf("\n❌ %d advanced feature tests failed\n", len(tests)-successful)
	}
}