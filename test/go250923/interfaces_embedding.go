package main

import (
	"context"
	"errors"
	"fmt"
	"reflect"
	"runtime"
	"time"
)

func main() {
	fmt.Println("=== Go Interfaces and Embedding Tests ===")
	testBasicInterfaces()
	testEmbeddedInterfaces()
	testInterfaceComposition()
	testTypeAssertions()
	testTypeSwitches()
	testEmptyInterface()
	testInterfaceValues()

	fmt.Println("\n=== All interface tests completed successfully! ===")
}

// Basic interface definition
type Speaker interface {
	Speak() string
}

// Another interface
type Mover interface {
	Move() string
}

// Embedded interface
type Walker interface {
	Speaker
	Mover
}

// Struct implementing Speaker
type Person struct {
	Name string
}

func (p Person) Speak() string {
	return fmt.Sprintf("Hello, I'm %s", p.Name)
}

// Struct implementing both Speaker and Mover
type Dog struct {
	Name string
	Breed string
}

func (d Dog) Speak() string {
	return fmt.Sprintf("Woof! I'm %s the %s", d.Name, d.Breed)
}

func (d Dog) Move() string {
	return fmt.Sprintf("%s is running", d.Name)
}

// Struct implementing Walker interface
type Robot struct {
	Model string
}

func (r Robot) Speak() string {
	return fmt.Sprintf("Beep boop! I'm %s", r.Model)
}

func (r Robot) Move() string {
	return fmt.Sprintf("%s is rolling", r.Model)
}

func testBasicInterfaces() {
	fmt.Println("\n--- Basic Interfaces ---")

	// Using Speaker interface
	var speaker Speaker = Person{Name: "Alice"}
	fmt.Printf("Speaker says: %s\n", speaker.Speak())

	// Same interface with different implementation
	speaker = Dog{Name: "Rex", Breed: "German Shepherd"}
	fmt.Printf("Speaker says: %s\n", speaker.Speak())
}

func testEmbeddedInterfaces() {
	fmt.Println("\n--- Embedded Interfaces ---")

	var walker Walker = Robot{Model: "T-800"}
	fmt.Printf("Walker speaks: %s\n", walker.Speak())
	fmt.Printf("Walker moves: %s\n", walker.Move())

	// Dog also implements Walker
	walker = Dog{Name: "Buddy", Breed: "Golden Retriever"}
	fmt.Printf("Walker speaks: %s\n", walker.Speak())
	fmt.Printf("Walker moves: %s\n", walker.Move())
}

func testInterfaceComposition() {
	fmt.Println("\n--- Interface Composition ---")

	// Function that takes Walker interface
	processWalker := func(w Walker) {
		fmt.Printf("Processing walker:\n")
		fmt.Printf("  Speech: %s\n", w.Speak())
		fmt.Printf("  Movement: %s\n", w.Move())
	}

	processWalker(Robot{Model: "R2-D2"})
	processWalker(Dog{Name: "Max", Breed: "Bulldog"})
}

func testTypeAssertions() {
	fmt.Println("\n--- Type Assertions ---")

	// Type assertion
	var speaker Speaker = Dog{Name: "Fido", Breed: "Poodle"}

	// Safe type assertion
	if dog, ok := speaker.(Dog); ok {
		fmt.Printf("Type assertion successful: %s is a %s\n", dog.Name, dog.Breed)
	} else {
		fmt.Println("Type assertion failed")
	}

	// Type assertion that will fail
	if person, ok := speaker.(Person); ok {
		fmt.Printf("This won't print: %s\n", person.Name)
	} else {
		fmt.Println("Type assertion correctly failed - speaker is not a Person")
	}

	// Unsafe type assertion (would panic)
	// person := speaker.(Person) // This would panic
}

func testTypeSwitches() {
	fmt.Println("\n--- Type Switches ---")

	// Type switch
	processInterface := func(i interface{}) {
		switch v := i.(type) {
		case Person:
			fmt.Printf("It's a Person: %s\n", v.Name)
		case Dog:
			fmt.Printf("It's a Dog: %s the %s\n", v.Name, v.Breed)
		case Robot:
			fmt.Printf("It's a Robot: %s\n", v.Model)
		case string:
			fmt.Printf("It's a string: %s\n", v)
		case int:
			fmt.Printf("It's an integer: %d\n", v)
		default:
			fmt.Printf("Unknown type: %T\n", v)
		}
	}

	processInterface(Person{Name: "Bob"})
	processInterface(Dog{Name: "Spot", Breed: "Dalmatian"})
	processInterface(Robot{Model: "C-3PO"})
	processInterface("hello world")
	processInterface(42)
	processInterface(3.14)
}

func testEmptyInterface() {
	fmt.Println("\n--- Empty Interface ---")

	// Empty interface can hold any value
	var anything interface{} = "hello"
	fmt.Printf("anything: %v (type: %T)\n", anything, anything)

	anything = 42
	fmt.Printf("anything: %v (type: %T)\n", anything, anything)

	anything = Person{Name: "Charlie"}
	fmt.Printf("anything: %v (type: %T)\n", anything, anything)

	anything = []string{"a", "b", "c"}
	fmt.Printf("anything: %v (type: %T)\n", anything, anything)

	// Slice of empty interface
	mixed := []interface{}{"string", 42, true, Person{Name: "Diana"}, []int{1, 2, 3}}
	for i, item := range mixed {
		fmt.Printf("mixed[%d]: %v (type: %T)\n", i, item, item)
	}
}

func testInterfaceValues() {
	fmt.Println("\n--- Interface Values ---")

	// Interface with nil concrete value
	var speaker Speaker
	fmt.Printf("Nil interface: %v (is nil: %t)\n", speaker, speaker == nil)

	// Interface with non-nil concrete value
	speaker = Person{Name: "Eve"}
	fmt.Printf("Non-nil interface: %v (is nil: %t)\n", speaker, speaker == nil)

	// Interface with nil pointer
	var person *Person
	speaker = person
	fmt.Printf("Interface with nil pointer: %v (is nil: %t)\n", speaker, speaker == nil)

	// Compare interfaces
	speaker1 := Person{Name: "Frank"}
	speaker2 := Person{Name: "Frank"}
	fmt.Printf("speaker1 == speaker2: %t\n", speaker1 == speaker2) // Different instances, not equal
}

// Additional interface patterns
func testInterfacePatterns() {
	fmt.Println("\n--- Interface Patterns ---")

	// Interface as constraint
	greet := func(s Speaker) {
		fmt.Printf("Greeting: %s says hello!\n", s.Speak())
	}

	greet(Person{Name: "Grace"})
	greet(Dog{Name: "Luna", Breed: "Siamese Cat"})

	// Interface embedding in structs
	type Greeter struct {
		speaker Speaker
	}

	greeter := Greeter{speaker: Robot{Model: "Bender"}}
	fmt.Printf("Greeter speaks: %s\n", greeter.speaker.Speak())

	// Method sets
	fmt.Printf("Robot method set includes Speak: %t\n", reflect.TypeOf(Robot{}).Implements(reflect.TypeOf((*Speaker)(nil)).Elem()))
	fmt.Printf("Robot method set includes Move: %t\n", reflect.TypeOf(Robot{}).Implements(reflect.TypeOf((*Mover)(nil)).Elem()))
	fmt.Printf("Robot method set includes Walker: %t\n", reflect.TypeOf(Robot{}).Implements(reflect.TypeOf((*Walker)(nil)).Elem()))
}