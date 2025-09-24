package main

import (
	"fmt"
	"reflect"
)

func main() {
	fmt.Println("=== Go Advanced Type System Features ===")
	testTypeAliases()
	testTypeConstraints()
	testGenericInterfaces()
	testGenericStructs()
	testMethodOverriding()
	testInterfaceComposition()
	testEmbeddingWithPromotion()
	testGenericTypeSets()
	testInterfaceSatisfaction()
	fmt.Println("\n=== All advanced type system tests completed successfully! ===")
}

// Type aliases
type UserID = int64
type Username = string
type Score = float64

func testTypeAliases() {
	fmt.Println("\n--- Type Aliases ---")
	
	var userID UserID = 12345
	var username Username = "john_doe"
	var score Score = 95.5
	
	fmt.Printf("UserID: %d (type: %T)\n", userID, userID)
	fmt.Printf("Username: %s (type: %T)\n", username, username)
	fmt.Printf("Score: %.2f (type: %T)\n", score, score)
	
	// Type aliases are interchangeable with underlying types
	var regularInt int64 = userID
	fmt.Printf("Regular int64 from UserID: %d\n", regularInt)
}

// Advanced type constraints
type Integer interface {
	~int | ~int8 | ~int16 | ~int32 | ~int64
}

type Unsigned interface {
	~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64
}

type Float interface {
	~float32 | ~float64
}

type Number interface {
	Integer | Unsigned | Float
}

type Ordered interface {
	Number | ~string
}
type Comparable interface {
	comparable
}

func testTypeConstraints() {
	fmt.Println("\n--- Advanced Type Constraints ---")
	
	// Using union type constraints
	numbers := []int{1, 2, 3, 4, 5}
	result := SumGeneric(numbers)
	fmt.Printf("Sum of integers: %d\n", result)
	
	floats := []float64{1.1, 2.2, 3.3}
	floatResult := SumGeneric(floats)
	fmt.Printf("Sum of floats: %.2f\n", floatResult)
	
	// Using ordered constraint
	maxInt := MaxOrdered([]int{10, 5, 8, 3, 15})
	fmt.Printf("Max int: %d\n", maxInt)
	
	maxString := MaxOrdered([]string{"apple", "banana", "cherry"})
	fmt.Printf("Max string: %s\n", maxString)
}

func SumGeneric[T Number](slice []T) T {
	var sum T
	for _, v := range slice {
		sum += v
	}
	return sum
}

func MaxOrdered[T Ordered](slice []T) T {
	if len(slice) == 0 {
		var zero T
		return zero
	}
	max := slice[0]
	for _, v := range slice[1:] {
		if v > max {
			max = v
		}
	}
	return max
}

// Generic interfaces
type Container[T any] interface {
	Add(item T)
	Remove() (T, bool)
	Size() int
	IsEmpty() bool
}

type ComparableContainer[T Comparable] interface {
	Container[T]
	Contains(item T) bool
	Find(item T) (T, bool)
}

type Transformable[T, U any] interface {
	Transform(func(T) U) Transformable[U, T]
}

func testGenericInterfaces() {
	fmt.Println("\n--- Generic Interfaces ---")
	
	// Basic generic interface usage
	stack := &GenericStack[int]{}
	stack.Push(1)
	stack.Push(2)
	stack.Push(3)
	
	fmt.Printf("Stack size: %d\n", stack.Size())
	fmt.Printf("Stack empty: %t\n", stack.IsEmpty())
	
	// Comparable container
	set := NewGenericSet[string]()
	set.Add("apple")
	set.Add("banana")
	set.Add("apple") // Duplicate
	
	fmt.Printf("Set contains 'apple': %t\n", set.Contains("apple"))
	fmt.Printf("Set contains 'orange': %t\n", set.Contains("orange"))
	fmt.Printf("Set size: %d\n", set.Size())
}

// Generic stack implementation
type GenericStack[T any] struct {
	items []T
}

func (s *GenericStack[T]) Push(item T) {
	s.items = append(s.items, item)
}

func (s *GenericStack[T]) Remove() (T, bool) {
	if len(s.items) == 0 {
		var zero T
		return zero, false
	}
	item := s.items[len(s.items)-1]
	s.items = s.items[:len(s.items)-1]
	return item, true
}

func (s *GenericStack[T]) Pop() (T, bool) {
	return s.Remove()
}

func (s *GenericStack[T]) Size() int {
	return len(s.items)
}

func (s *GenericStack[T]) IsEmpty() bool {
	return len(s.items) == 0
}

func (s *GenericStack[T]) Add(item T) {
	s.Push(item)
}

// Generic set implementation
type GenericSet[T comparable] struct {
	items map[T]bool
}

func NewGenericSet[T comparable]() *GenericSet[T] {
	return &GenericSet[T]{items: make(map[T]bool)}
}

func (s *GenericSet[T]) Add(item T) {
	s.items[item] = true
}

func (s *GenericSet[T]) Remove() (T, bool) {
	for item := range s.items {
		delete(s.items, item)
		return item, true
	}
	var zero T
	return zero, false
}

func (s *GenericSet[T]) Size() int {
	return len(s.items)
}

func (s *GenericSet[T]) IsEmpty() bool {
	return len(s.items) == 0
}

func (s *GenericSet[T]) Contains(item T) bool {
	return s.items[item]
}

func (s *GenericSet[T]) Find(item T) (T, bool) {
	if s.items[item] {
		return item, true
	}
	var zero T
	return zero, false
}

// Generic structs with multiple type parameters
type Result[T any, E any] struct {
	value T
	error E
	isOk  bool
}

type Either[L any, R any] struct {
	left  L
	right R
	isLeft bool
}

type Pair[T, U any] struct {
	First  T
	Second U
}

type Triple[A, B, C any] struct {
	First  A
	Second B
	Third  C
}

func testGenericStructs() {
	fmt.Println("\n--- Generic Structs ---")
	
	// Result type (like Rust's Result)
	okResult := Result[int, string]{value: 42, isOk: true}
	errResult := Result[int, string]{error: "something went wrong", isOk: false}
	
	fmt.Printf("OK result: %v\n", okResult)
	fmt.Printf("Error result: %v\n", errResult)
	
	// Either type
	leftEither := Either[string, int]{left: "error message", isLeft: true}
	rightEither := Either[string, int]{right: 123, isLeft: false}
	
	fmt.Printf("Left either: %v\n", leftEither)
	fmt.Printf("Right either: %v\n", rightEither)
	
	// Pair and Triple
	pair := Pair[string, int]{First: "age", Second: 25}
	triple := Triple[string, int, bool]{First: "name", Second: 100, Third: true}
	
	fmt.Printf("Pair: %+v\n", pair)
	fmt.Printf("Triple: %+v\n", triple)
}

// Method overriding with embedding
type Base struct {
	Name string
}

func (b Base) GetName() string {
	return b.Name
}

func (b Base) Describe() string {
	return fmt.Sprintf("Base: %s", b.Name)
}

type Derived struct {
	Base
	Extra string
}

func (d Derived) GetName() string {
	return fmt.Sprintf("Derived: %s", d.Name)
}

func (d Derived) Describe() string {
	return fmt.Sprintf("Derived: %s (%s)", d.Name, d.Extra)
}

func testMethodOverriding() {
	fmt.Println("\n--- Method Overriding with Embedding ---")
	
	base := Base{Name: "Base Object"}
	fmt.Printf("Base.GetName(): %s\n", base.GetName())
	fmt.Printf("Base.Describe(): %s\n", base.Describe())
	
	derived := Derived{
		Base:  Base{Name: "Derived Object"},
		Extra: "extra info",
	}
	fmt.Printf("Derived.GetName(): %s\n", derived.GetName())
	fmt.Printf("Derived.Describe(): %s\n", derived.Describe())
	
	// Method promotion
	fmt.Printf("Derived.Base.GetName(): %s\n", derived.Base.GetName())
	fmt.Printf("Derived.Base.Describe(): %s\n", derived.Base.Describe())
}

// Interface composition and embedding
type Reader interface {
	Read() ([]byte, error)
}

type Writer interface {
	Write([]byte) (int, error)
}

type Closer interface {
	Close() error
}

type ReadWriter interface {
	Reader
	Writer
}

type ReadWriteCloser interface {
	Reader
	Writer
	Closer
}

type AdvancedReader interface {
	Read() ([]byte, error)
	ReadAt(int) ([]byte, error)
	Size() int
}

func testInterfaceComposition() {
	fmt.Println("\n--- Interface Composition ---")
	
	// Test interface embedding
	var rwc ReadWriteCloser = &MockFile{}
	
	data, err := rwc.Read()
	if err == nil {
		fmt.Printf("Read data: %s\n", string(data))
	}
	
	n, err := rwc.Write([]byte("test data"))
	if err == nil {
		fmt.Printf("Wrote %d bytes\n", n)
	}
	
	err = rwc.Close()
	if err == nil {
		fmt.Println("Closed successfully")
	}
	
	// Test interface satisfaction
	var reader Reader = rwc
	var writer Writer = rwc
	var closer Closer = rwc
	
	fmt.Printf("rwc implements Reader: %t\n", reader != nil)
	fmt.Printf("rwc implements Writer: %t\n", writer != nil)
	fmt.Printf("rwc implements Closer: %t\n", closer != nil)
}

type MockFile struct {
	content []byte
	closed  bool
}

func (m *MockFile) Read() ([]byte, error) {
	return []byte("mock file content"), nil
}

func (m *MockFile) Write(data []byte) (int, error) {
	return len(data), nil
}

func (m *MockFile) Close() error {
	m.closed = true
	return nil
}

func (m *MockFile) ReadAt(offset int) ([]byte, error) {
	return []byte("partial content"), nil
}

func (m *MockFile) Size() int {
	return len(m.content)
}

// Embedding with promotion and method resolution
type Engine interface {
	Start() error
	Stop() error
	GetPower() int
}

type Vehicle struct {
	Brand string
	Model string
}

func (v Vehicle) GetInfo() string {
	return fmt.Sprintf("%s %s", v.Brand, v.Model)
}

type Car struct {
	Vehicle
	Engine
	Year int
}

func (c Car) GetInfo() string {
	return fmt.Sprintf("%s %s %d", c.Brand, c.Model, c.Year)
}

type GasEngine struct {
	HorsePower int
}

func (g GasEngine) Start() error {
	fmt.Println("Gas engine started")
	return nil
}

func (g GasEngine) Stop() error {
	fmt.Println("Gas engine stopped")
	return nil
}

func (g GasEngine) GetPower() int {
	return g.HorsePower
}

func testEmbeddingWithPromotion() {
	fmt.Println("\n--- Embedding with Promotion ---")
	
	car := Car{
		Vehicle: Vehicle{Brand: "Toyota", Model: "Camry"},
		Engine:  GasEngine{HorsePower: 200},
		Year:    2023,
	}
	
	// Method promotion
	fmt.Printf("Car info: %s\n", car.GetInfo())
	fmt.Printf("Vehicle info: %s\n", car.Vehicle.GetInfo())
	
	// Interface method promotion
	err := car.Start()
	if err == nil {
		fmt.Printf("Engine power: %d HP\n", car.GetPower())
		car.Stop()
	}
}

// Generic type sets and approximation
type Numeric interface {
	~int | ~int8 | ~int16 | ~int32 | ~int64 |
	~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64 |
	~float32 | ~float64 | ~complex64 | ~complex128
}

type MyInt int
type MyFloat64 float64

func testGenericTypeSets() {
	fmt.Println("\n--- Generic Type Sets ---")
	
	// Test with custom types that approximate to built-in types
	myInts := []MyInt{1, 2, 3, 4, 5}
	myFloats := []MyFloat64{1.1, 2.2, 3.3}
	
	fmt.Printf("Sum of MyInt: %v\n", SumNumeric(myInts))
	fmt.Printf("Sum of MyFloat64: %v\n", SumNumeric(myFloats))
	
	// Test with built-in types
	regularInts := []int{10, 20, 30}
	fmt.Printf("Sum of int: %v\n", SumNumeric(regularInts))
}

func SumNumeric[T Numeric](slice []T) T {
	var sum T
	for _, v := range slice {
		sum += v
	}
	return sum
}

// Interface satisfaction testing
type Calculator interface {
	Add(a, b float64) float64
	Subtract(a, b float64) float64
	Multiply(a, b float64) float64
	Divide(a, b float64) (float64, error)
}

type BasicCalculator struct{}

func (c BasicCalculator) Add(a, b float64) float64 {
	return a + b
}

func (c BasicCalculator) Subtract(a, b float64) float64 {
	return a - b
}

func (c BasicCalculator) Multiply(a, b float64) float64 {
	return a * b
}

func (c BasicCalculator) Divide(a, b float64) (float64, error) {
	if b == 0 {
		return 0, fmt.Errorf("division by zero")
	}
	return a / b, nil
}

type ScientificCalculator struct {
	BasicCalculator
}

func (c ScientificCalculator) Power(base, exponent float64) float64 {
	result := 1.0
	for i := 0; i < int(exponent); i++ {
		result *= base
	}
	return result
}

func (c ScientificCalculator) Sqrt(x float64) (float64, error) {
	if x < 0 {
		return 0, fmt.Errorf("cannot calculate square root of negative number")
	}
	return x * x, nil // Simplified for demonstration
}

func testInterfaceSatisfaction() {
	fmt.Println("\n--- Interface Satisfaction ---")
	
	// Test basic calculator
	var calc Calculator = BasicCalculator{}
	testCalculator(calc, "BasicCalculator")
	
	// Test scientific calculator (embedding satisfies interface)
	var sciCalc Calculator = ScientificCalculator{}
	testCalculator(sciCalc, "ScientificCalculator")
	
	// Reflection-based interface satisfaction check
	basicType := reflect.TypeOf(BasicCalculator{})
	calcType := reflect.TypeOf((*Calculator)(nil)).Elem()
	
	fmt.Printf("BasicCalculator implements Calculator: %t\n", basicType.Implements(calcType))
	
	sciType := reflect.TypeOf(ScientificCalculator{})
	fmt.Printf("ScientificCalculator implements Calculator: %t\n", sciType.Implements(calcType))
}

func testCalculator(calc Calculator, name string) {
	fmt.Printf("\nTesting %s:\n", name)
	
	a, b := 10.0, 5.0
	fmt.Printf("Add(%.1f, %.1f) = %.1f\n", a, b, calc.Add(a, b))
	fmt.Printf("Subtract(%.1f, %.1f) = %.1f\n", a, b, calc.Subtract(a, b))
	fmt.Printf("Multiply(%.1f, %.1f) = %.1f\n", a, b, calc.Multiply(a, b))
	
	result, err := calc.Divide(a, b)
	if err != nil {
		fmt.Printf("Divide error: %v\n", err)
	} else {
		fmt.Printf("Divide(%.1f, %.1f) = %.1f\n", a, b, result)
	}
	
	// Test division by zero
	_, err = calc.Divide(a, 0)
	if err != nil {
		fmt.Printf("Division by zero handled: %v\n", err)
	}
}