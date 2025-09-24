package main

import (
	"fmt"
	"os"
	"time"
)

func main() {
	fmt.Println("=== Go Defer, Panic, and Recover Tests ===")
	testDeferBasics()
	testDeferOrder()
	testDeferWithParameters()
	testPanicBasics()
	testRecoverBasics()
	testDeferWithFiles()
	testRealWorldScenarios()

	fmt.Println("\n=== All defer, panic, and recover tests completed successfully! ===")
}

func testDeferBasics() {
	fmt.Println("\n--- Defer Basics ---")

	// Basic defer
	fmt.Println("Before defer")
	defer fmt.Println("Deferred call")
	fmt.Println("After defer")

	// Multiple defers
	fmt.Println("\nMultiple defers:")
	defer fmt.Println("Third defer")
	defer fmt.Println("Second defer")
	defer fmt.Println("First defer")

	// Defer with functions
	defer func() {
		fmt.Println("Deferred function call")
	}()

	fmt.Println("End of function")
}

func testDeferOrder() {
	fmt.Println("\n--- Defer Order ---")

	defer fmt.Println("1: This will be called last")
	defer fmt.Println("2: This will be called second")
	defer fmt.Println("3: This will be called first")

	fmt.Println("Function body executing...")
}

func testDeferWithParameters() {
	fmt.Println("\n--- Defer with Parameters ---")

	// Parameters are evaluated when defer is called
	x := 10
	defer fmt.Printf("x = %d\n", x)
	x = 20
	fmt.Printf("x changed to %d\n", x)

	// Defer with function that captures variable
	y := 5
	defer func() {
		fmt.Printf("y = %d\n", y)
	}()
	y = 15
	fmt.Printf("y changed to %d\n", y)

	// Defer with method calls
	counter := &Counter{count: 0}
	defer counter.increment()
	defer counter.increment()
	defer counter.increment()

	fmt.Printf("Final count: %d\n", counter.count)
}

type Counter struct {
	count int
}

func (c *Counter) increment() {
	c.count++
	fmt.Printf("Incremented count to %d\n", c.count)
}

func testPanicBasics() {
	fmt.Println("\n--- Panic Basics ---")

	// Simple panic
	fmt.Println("Before panic")
	// panic("This is a panic!")
	fmt.Println("This line won't be executed if panic is called")

	// Panic with different types
	fmt.Println("\nTesting panic with different types:")
	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("Recovered from panic: %v (type: %T)\n", r, r)
		}
	}()

	// Uncomment to test different panic types
	// panic("string panic")
	// panic(42)
	// panic(struct{ name string }{name: "anonymous"})
}

func testRecoverBasics() {
	fmt.Println("\n--- Recover Basics ---")

	// Recover from panic in same function
	fmt.Println("Testing recover in same function:")
	safeFunction()

	// Recover from panic in different function
	fmt.Println("\nTesting recover in different function:")
	callPanickingFunction()

	// Recover from panic in defer
	fmt.Println("\nTesting recover in defer:")
	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("Recovered from deferred recover: %v\n", r)
		}
	}()
	panicFunction("Deferred recover test")
}

func safeFunction() {
	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("Recovered in safeFunction: %v\n", r)
		}
	}()

	panic("This panic will be recovered")
	fmt.Println("This line won't be reached")
}

func callPanickingFunction() {
	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("Recovered in callPanickingFunction: %v\n", r)
		}
	}()

	panickingFunction()
	fmt.Println("This line won't be reached")
}

func panickingFunction() {
	panic("This panic will be recovered by caller")
	fmt.Println("This line won't be reached")
}

func panicFunction(message string) {
	panic(message)
}

func testDeferWithFiles() {
	fmt.Println("\n--- Defer with Files ---")

	filename := "test_defer_file.txt"

	// Create and write to file with defer for closing
	file, err := os.Create(filename)
	if err != nil {
		fmt.Printf("Error creating file: %v\n", err)
		return
	}
	defer file.Close()

	_, err = file.WriteString("Hello, World!\n")
	if err != nil {
		fmt.Printf("Error writing to file: %v\n", err)
		return
	}

	_, err = file.WriteString("This file will be closed automatically\n")
	if err != nil {
		fmt.Printf("Error writing to file: %v\n", err)
		return
	}

	fmt.Printf("File created and written: %s\n", filename)

	// Read file back with defer for closing
	readFile, err := os.Open(filename)
	if err != nil {
		fmt.Printf("Error opening file: %v\n", err)
		return
	}
	defer readFile.Close()

	buffer := make([]byte, 1024)
	n, err := readFile.Read(buffer)
	if err != nil {
		fmt.Printf("Error reading file: %v\n", err)
		return
	}

	fmt.Printf("File content:\n%s", buffer[:n])

	// Clean up
	os.Remove(filename)
}

func testRealWorldScenarios() {
	fmt.Println("\n--- Real-World Scenarios ---")

	// Database connection simulation
	fmt.Println("Database connection simulation:")
	db := &DatabaseConnection{connected: false}
	defer db.close()

	db.connect()
	db.query("SELECT * FROM users")

	// Resource cleanup simulation
	fmt.Println("\nResource cleanup simulation:")
	resources := []string{"resource1", "resource2", "resource3"}
	for _, res := range resources {
		defer fmt.Printf("Cleaned up %s\n", res)
	}
	fmt.Println("Allocated resources")

	// Performance timing
	fmt.Println("\nPerformance timing:")
	defer func() {
		fmt.Printf("Function execution completed\n")
	}()

	start := time.Now()
	defer func() {
		duration := time.Since(start)
		fmt.Printf("Execution time: %v\n", duration)
	}()

	// Simulate work
	time.Sleep(50 * time.Millisecond)
	fmt.Println("Work completed")
}

type DatabaseConnection struct {
	connected bool
}

func (db *DatabaseConnection) connect() {
	db.connected = true
	fmt.Println("Database connected")
}

func (db *DatabaseConnection) query(sql string) {
	if db.connected {
		fmt.Printf("Executing query: %s\n", sql)
	} else {
		fmt.Println("Database not connected")
	}
}

func (db *DatabaseConnection) close() {
	if db.connected {
		db.connected = false
		fmt.Println("Database disconnected")
	}
}

// Advanced defer patterns
func testAdvancedPatterns() {
	fmt.Println("\n--- Advanced Defer Patterns ---")

	// Defer with return values
	fmt.Println("Defer with return values:")
	result := functionWithDeferAndReturn()
	fmt.Printf("Result: %d\n", result)

	// Defer with named return values
	fmt.Println("\nDefer with named return values:")
	namedResult := functionWithNamedReturn()
	fmt.Printf("Named result: %d\n", namedResult)

	// Defer with recover stack trace
	fmt.Println("\nDefer with recover stack trace:")
	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("Recovered panic: %v\n", r)
			// In real code, you might log the stack trace here
		}
	}()

	panicWithStack()
}

func functionWithDeferAndReturn() int {
	defer func() {
		fmt.Println("Defer called before return")
	}()

	fmt.Println("Function body")
	return 42
}

func functionWithNamedReturn() (result int) {
	defer func() {
		fmt.Printf("Defer called, result is now %d\n", result)
	}()

	result = 100
	fmt.Println("Function body set result to 100")
	return result
}

func panicWithStack() {
	fmt.Println("About to panic with stack info")
	panic("Panic with stack information")
}