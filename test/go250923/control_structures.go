package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	fmt.Println("=== Go Control Structures Tests ===")
	testIfStatements()
	testForLoops()
	testRangeLoops()
	testSwitchStatements()
	testSelectStatements()
	testGotoAndLabels()
	testDeferAndPanic()
	fmt.Println("\n=== All control structures tests completed successfully! ===")
}

func testIfStatements() {
	fmt.Println("\n--- If Statements ---")

	// Basic if
	x := 10
	if x > 5 {
		fmt.Println("x is greater than 5")
	}

	// If with else
	if x > 15 {
		fmt.Println("x is greater than 15")
	} else {
		fmt.Println("x is not greater than 15")
	}

	// If with else if
	if x > 15 {
		fmt.Println("x is greater than 15")
	} else if x > 10 {
		fmt.Println("x is greater than 10")
	} else {
		fmt.Println("x is 10 or less")
	}

	// If with initialization
	if y := 20; y > x {
		fmt.Println("y is greater than x")
	}

	// If with multiple conditions
	age := 25
	if age >= 18 && age <= 65 {
		fmt.Println("Age is within working age")
	}

	if age < 18 || age > 65 {
		fmt.Println("Age is outside working age")
	}

	// Nested if
	score := 85
	if score >= 90 {
		fmt.Println("Grade: A")
	} else if score >= 80 {
		if score >= 85 {
			fmt.Println("Grade: B+")
		} else {
			fmt.Println("Grade: B")
		}
	} else {
		fmt.Println("Grade: C or lower")
	}

	// If with function calls
	if isPositive(10) {
		fmt.Println("Number is positive")
	}
}

func testForLoops() {
	fmt.Println("\n--- For Loops ---")

	// Basic for loop
	fmt.Println("Basic for loop:")
	for i := 0; i < 5; i++ {
		fmt.Printf("i = %d\n", i)
	}

	// For loop without initialization and increment
	fmt.Println("For loop without init and increment:")
	j := 0
	for j < 3 {
		fmt.Printf("j = %d\n", j)
		j++
	}

	// Infinite loop with break
	fmt.Println("Infinite loop with break:")
	k := 0
	for {
		fmt.Printf("k = %d\n", k)
		k++
		if k >= 3 {
			break
		}
	}

	// For loop with continue
	fmt.Println("For loop with continue:")
	for i := 0; i < 5; i++ {
		if i == 2 {
			continue
		}
		fmt.Printf("i = %d (skipped 2)\n", i)
	}

	// Nested for loops
	fmt.Println("Nested for loops:")
	for i := 0; i < 2; i++ {
		for j := 0; j < 2; j++ {
			fmt.Printf("i=%d, j=%d\n", i, j)
		}
	}

	// For loop with multiple conditions
	fmt.Println("For loop with multiple conditions:")
	for i, j := 0, 0; i < 3 && j < 3; i, j = i+1, j+1 {
		fmt.Printf("i=%d, j=%d\n", i, j)
	}

	// For loop with defer
	fmt.Println("For loop with defer:")
	for i := 0; i < 3; i++ {
		defer fmt.Printf("Deferred: i=%d\n", i)
		fmt.Printf("Immediate: i=%d\n", i)
	}
}

func testRangeLoops() {
	fmt.Println("\n--- Range Loops ---")

	// Range over slice
	fmt.Println("Range over slice:")
	numbers := []int{10, 20, 30, 40, 50}
	for index, value := range numbers {
		fmt.Printf("index=%d, value=%d\n", index, value)
	}

	// Range over slice (index only)
	fmt.Println("Range over slice (index only):")
	for index := range numbers {
		fmt.Printf("index=%d\n", index)
	}

	// Range over slice (value only)
	fmt.Println("Range over slice (value only):")
	for _, value := range numbers {
		fmt.Printf("value=%d\n", value)
	}

	// Range over map
	fmt.Println("Range over map:")
	persons := map[string]int{
		"Alice":   25,
		"Bob":     30,
		"Charlie": 35,
	}
	for key, value := range persons {
		fmt.Printf("key=%s, value=%d\n", key, value)
	}

	// Range over string
	fmt.Println("Range over string:")
	text := "Hello"
	for index, rune := range text {
		fmt.Printf("index=%d, rune=%c\n", index, rune)
	}

	// Range over channel
	fmt.Println("Range over channel:")
	ch := make(chan int)
	go func() {
		for i := 0; i < 3; i++ {
			ch <- i
		}
		close(ch)
	}()
	for value := range ch {
		fmt.Printf("channel value=%d\n", value)
	}

	// Range over array
	fmt.Println("Range over array:")
	arr := [3]string{"a", "b", "c"}
	for i, v := range arr {
		fmt.Printf("i=%d, v=%s\n", i, v)
	}

	// Range with pointer to slice
	fmt.Println("Range with pointer to slice:")
	slice := []int{1, 2, 3}
	pslice := &slice
	for i, v := range *pslice {
		fmt.Printf("i=%d, v=%d\n", i, v)
	}
}

func testSwitchStatements() {
	fmt.Println("\n--- Switch Statements ---")

	// Basic switch
	fmt.Println("Basic switch:")
	day := "Tuesday"
	switch day {
	case "Monday":
		fmt.Println("Start of week")
	case "Tuesday", "Wednesday", "Thursday":
		fmt.Println("Midweek")
	case "Friday":
		fmt.Println("End of week")
	default:
		fmt.Println("Weekend")
	}

	// Switch with no condition
	fmt.Println("Switch with no condition:")
	x := 10
	switch {
	case x < 0:
		fmt.Println("Negative")
	case x == 0:
		fmt.Println("Zero")
	case x > 0:
		fmt.Println("Positive")
	}

	// Switch with fallthrough
	fmt.Println("Switch with fallthrough:")
	switch x {
	case 10:
		fmt.Println("x is 10")
		fallthrough
	case 20:
		fmt.Println("This will also print")
		fallthrough
	default:
		fmt.Println("This will print too")
	}

	// Switch with type assertion
	fmt.Println("Switch with type assertion:")
	var i interface{} = "hello"
	switch v := i.(type) {
	case int:
		fmt.Printf("Integer: %d\n", v)
	case string:
		fmt.Printf("String: %s\n", v)
	case bool:
		fmt.Printf("Boolean: %t\n", v)
	default:
		fmt.Printf("Unknown type: %T\n", v)
	}

	// Switch with initialization
	fmt.Println("Switch with initialization:")
	switch y := 15; {
	case y < 10:
		fmt.Println("Less than 10")
	case y >= 10 && y < 20:
		fmt.Println("Between 10 and 20")
	default:
		fmt.Println("20 or more")
	}

	// Switch with multiple cases
	fmt.Println("Switch with multiple cases:")
	rand.Seed(time.Now().UnixNano())
	random := rand.Intn(3)
	switch random {
	case 0, 1:
		fmt.Println("Low number")
	case 2, 3:
		fmt.Println("Medium number")
	case 4, 5:
		fmt.Println("High number")
	default:
		fmt.Println("Very high number")
	}

	// Switch with functions
	fmt.Println("Switch with functions:")
	switch getGrade(85) {
	case "A":
		fmt.Println("Excellent")
	case "B":
		fmt.Println("Good")
	case "C":
		fmt.Println("Average")
	default:
		fmt.Println("Poor")
	}
}

func testSelectStatements() {
	fmt.Println("\n--- Select Statements ---")

	// Basic select
	fmt.Println("Basic select:")
	ch1 := make(chan string)
	ch2 := make(chan string)

	go func() {
		time.Sleep(100 * time.Millisecond)
		ch1 <- "from channel 1"
	}()

	go func() {
		time.Sleep(50 * time.Millisecond)
		ch2 <- "from channel 2"
	}()

	select {
	case msg1 := <-ch1:
		fmt.Printf("Received: %s\n", msg1)
	case msg2 := <-ch2:
		fmt.Printf("Received: %s\n", msg2)
	}

	// Select with default
	fmt.Println("Select with default:")
	ch3 := make(chan string)
	select {
	case msg := <-ch3:
		fmt.Printf("Received: %s\n", msg)
	default:
		fmt.Println("No messages available")
	}

	// Select with multiple cases
	fmt.Println("Select with multiple cases:")
	ch4 := make(chan string)
	ch5 := make(chan string)

	go func() {
		time.Sleep(150 * time.Millisecond)
		ch4 <- "channel 4"
	}()

	go func() {
		time.Sleep(100 * time.Millisecond)
		ch5 <- "channel 5"
	}()

	select {
	case msg1 := <-ch4:
		fmt.Printf("Fastest: %s\n", msg1)
	case msg2 := <-ch5:
		fmt.Printf("Fastest: %s\n", msg2)
	case <-time.After(200 * time.Millisecond):
		fmt.Println("Timeout")
	}

	// Select with send operations
	fmt.Println("Select with send operations:")
	ch6 := make(chan string, 1)
	ch7 := make(chan string, 1)

	select {
	case ch6 <- "send to ch6":
		fmt.Println("Sent to ch6")
	case ch7 <- "send to ch7":
		fmt.Println("Sent to ch7")
	default:
		fmt.Println("No channels ready for sending")
	}
}

func testGotoAndLabels() {
	fmt.Println("\n--- Goto and Labels ---")

	// Basic goto
	fmt.Println("Basic goto:")
	i := 0
start:
	fmt.Printf("i = %d\n", i)
	i++
	if i < 3 {
		goto start
	}

	// Goto with nested loops
	fmt.Println("Goto with nested loops:")
	for i := 0; i < 2; i++ {
		for j := 0; j < 2; j++ {
			if i == 1 && j == 1 {
				goto exit
			}
			fmt.Printf("i=%d, j=%d\n", i, j)
		}
	}
exit:
	fmt.Println("Exited nested loops")

	// Break with labels
	fmt.Println("Break with labels:")
outer:
	for i := 0; i < 3; i++ {
		for j := 0; j < 3; j++ {
			if i == 1 && j == 1 {
				break outer
			}
			fmt.Printf("i=%d, j=%d\n", i, j)
		}
	}

	// Continue with labels
	fmt.Println("Continue with labels:")
outer_continue:
	for i := 0; i < 3; i++ {
		for j := 0; j < 3; j++ {
			if i == 1 && j == 1 {
				continue outer_continue
			}
			fmt.Printf("i=%d, j=%d\n", i, j)
		}
	}
}

func testDeferAndPanic() {
	fmt.Println("\n--- Defer and Panic ---")

	// Defer in control structures
	fmt.Println("Defer in control structures:")
	for i := 0; i < 3; i++ {
		defer fmt.Printf("Deferred in loop: i=%d\n", i)
		fmt.Printf("Immediate in loop: i=%d\n", i)
	}

	// Panic and recover
	fmt.Println("Panic and recover:")
	func() {
		defer func() {
			if r := recover(); r != nil {
				fmt.Printf("Recovered from panic: %v\n", r)
			}
		}()
		panic("This is a controlled panic")
	}()

	// Select with panic recovery
	fmt.Println("Select with panic recovery:")
	func() {
		defer func() {
			if r := recover(); r != nil {
				fmt.Printf("Recovered from select panic: %v\n", r)
			}
		}()

		ch := make(chan int)
		select {
		case <-ch:
			fmt.Println("Received")
		case <-time.After(10 * time.Millisecond):
			panic("Timeout in select")
		}
	}()
}

// Helper functions
func isPositive(n int) bool {
	return n > 0
}

func getGrade(score int) string {
	if score >= 90 {
		return "A"
	}
	if score >= 80 {
		return "B"
	}
	if score >= 70 {
		return "C"
	}
	return "F"
}