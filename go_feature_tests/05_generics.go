package main

import (
	"fmt"
	"golang.org/x/exp/constraints"
)

// 1. Basic generic function
func GenericMax[T constraints.Ordered](a, b T) T {
	if a > b {
		return a
	}
	return b
}

// 2. Generic function with multiple type parameters
func Pair[T, U any](first T, second U) (T, U) {
	return first, second
}

// 3. Generic struct
type Container[T any] struct {
	data []T
}

func (c *Container[T]) Add(item T) {
	c.data = append(c.data, item)
}

func (c *Container[T]) Get(index int) T {
	if index >= 0 && index < len(c.data) {
		return c.data[index]
	}
	var zero T
	return zero
}

func (c *Container[T]) Size() int {
	return len(c.data)
}

// 4. Generic interface
type Comparable[T any] interface {
	Compare(other T) int
}

// 5. Generic struct with constraints
type Number[T constraints.Integer | constraints.Float] struct {
	value T
}

func (n Number[T]) Add(other T) Number[T] {
	return Number[T]{value: n.value + other}
}

func (n Number[T]) Value() T {
	return n.value
}

// 6. Generic function with interface constraint
type Stringer interface {
	String() string
}

func PrintGeneric[T Stringer](items []T) {
	for i, item := range items {
		fmt.Printf("Item %d: %s\n", i, item.String())
	}
}

// 7. Type constraint with union
type Numeric interface {
	~int | ~int64 | ~float32 | ~float64
}

func Sum[T Numeric](values []T) T {
	var total T
	for _, v := range values {
		total += v
	}
	return total
}

// 8. Generic map operations
func MapKeys[K comparable, V any](m map[K]V) []K {
	keys := make([]K, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	return keys
}

func MapValues[K comparable, V any](m map[K]V) []V {
	values := make([]V, 0, len(m))
	for _, v := range m {
		values = append(values, v)
	}
	return values
}

// 9. Generic slice operations
func Filter[T any](slice []T, predicate func(T) bool) []T {
	var result []T
	for _, item := range slice {
		if predicate(item) {
			result = append(result, item)
		}
	}
	return result
}

func Map[T, U any](slice []T, transform func(T) U) []U {
	result := make([]U, len(slice))
	for i, item := range slice {
		result[i] = transform(item)
	}
	return result
}

// 10. Generic stack implementation
type Stack[T any] struct {
	items []T
}

func NewStack[T any]() *Stack[T] {
	return &Stack[T]{items: make([]T, 0)}
}

func (s *Stack[T]) Push(item T) {
	s.items = append(s.items, item)
}

func (s *Stack[T]) Pop() (T, bool) {
	if len(s.items) == 0 {
		var zero T
		return zero, false
	}
	index := len(s.items) - 1
	item := s.items[index]
	s.items = s.items[:index]
	return item, true
}

func (s *Stack[T]) IsEmpty() bool {
	return len(s.items) == 0
}

// Test implementations
type Person struct {
	Name string
	Age  int
}

func (p Person) String() string {
	return fmt.Sprintf("%s (age %d)", p.Name, p.Age)
}

func (p Person) Compare(other Person) int {
	if p.Age < other.Age {
		return -1
	} else if p.Age > other.Age {
		return 1
	}
	return 0
}

func main() {
	fmt.Println("=== Go 1.18+ Generics Test ===")

	// Test 1: Basic generic function
	fmt.Println("\n1. Basic Generic Function:")
	fmt.Printf("GenericMax(10, 20) = %d\n", GenericMax(10, 20))
	fmt.Printf("GenericMax(3.14, 2.71) = %.2f\n", GenericMax(3.14, 2.71))
	fmt.Printf("GenericMax(\"hello\", \"world\") = %s\n", GenericMax("hello", "world"))

	// Test 2: Multiple type parameters
	fmt.Println("\n2. Multiple Type Parameters:")
	str, num := Pair("answer", 42)
	fmt.Printf("Pair result: %s, %d\n", str, num)

	// Test 3: Generic container
	fmt.Println("\n3. Generic Container:")
	intContainer := Container[int]{}
	intContainer.Add(1)
	intContainer.Add(2)
	intContainer.Add(3)
	fmt.Printf("Int container size: %d, first item: %d\n", intContainer.Size(), intContainer.Get(0))

	stringContainer := Container[string]{}
	stringContainer.Add("hello")
	stringContainer.Add("world")
	fmt.Printf("String container size: %d, first item: %s\n", stringContainer.Size(), stringContainer.Get(0))

	// Test 4: Generic struct with constraints
	fmt.Println("\n4. Generic Struct with Constraints:")
	intNum := Number[int]{value: 10}
	floatNum := Number[float64]{value: 3.14}
	
	intResult := intNum.Add(5)
	floatResult := floatNum.Add(2.86)
	
	fmt.Printf("Int number: %d + 5 = %d\n", intNum.Value(), intResult.Value())
	fmt.Printf("Float number: %.2f + 2.86 = %.2f\n", floatNum.Value(), floatResult.Value())

	// Test 5: Generic interface
	fmt.Println("\n5. Generic with Interface Constraint:")
	people := []Person{
		{Name: "Alice", Age: 30},
		{Name: "Bob", Age: 25},
		{Name: "Charlie", Age: 35},
	}
	PrintGeneric(people)

	// Test 6: Type constraints with union
	fmt.Println("\n6. Type Constraints with Union:")
	intValues := []int{1, 2, 3, 4, 5}
	floatValues := []float64{1.1, 2.2, 3.3, 4.4, 5.5}
	
	intSum := Sum(intValues)
	floatSum := Sum(floatValues)
	
	fmt.Printf("Sum of ints: %d\n", intSum)
	fmt.Printf("Sum of floats: %.1f\n", floatSum)

	// Test 7: Generic map operations
	fmt.Println("\n7. Generic Map Operations:")
	testMap := map[string]int{"a": 1, "b": 2, "c": 3}
	keys := MapKeys(testMap)
	values := MapValues(testMap)
	
	fmt.Printf("Map keys: %v\n", keys)
	fmt.Printf("Map values: %v\n", values)

	// Test 8: Generic slice operations
	fmt.Println("\n8. Generic Slice Operations:")
	numbers := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
	
	evenNumbers := Filter(numbers, func(n int) bool { return n%2 == 0 })
	squared := Map(numbers, func(n int) int { return n * n })
	
	fmt.Printf("Even numbers: %v\n", evenNumbers)
	fmt.Printf("Squared numbers: %v\n", squared[:5]) // First 5 for brevity

	// Test 9: Generic stack
	fmt.Println("\n9. Generic Stack:")
	intStack := NewStack[int]()
	intStack.Push(10)
	intStack.Push(20)
	intStack.Push(30)
	
	for !intStack.IsEmpty() {
		if item, ok := intStack.Pop(); ok {
			fmt.Printf("Popped: %d\n", item)
		}
	}

	fmt.Println("\n=== Generics Tests Completed ===")
}