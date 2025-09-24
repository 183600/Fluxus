package main

import (
	"context"
	"fmt"
	"sync"
	"time"
)

func main() {
	fmt.Println("=== Go Generics Tests ===")
	testBasicGenerics()
	testGenericFunctions()
	testGenericStructs()
	testGenericInterfaces()
	testGenericConstraints()
	testGenericTypeParameters()
	testGenericMethodSets()

	fmt.Println("\n=== All generics tests completed successfully! ===")
}

// Basic generic type
type Stack[T any] struct {
	items []T
}

func (s *Stack[T]) Push(item T) {
	s.items = append(s.items, item)
}

func (s *Stack[T]) Pop() (T, bool) {
	if len(s.items) == 0 {
		var zero T
		return zero, false
	}
	item := s.items[len(s.items)-1]
	s.items = s.items[:len(s.items)-1]
	return item, true
}

func (s *Stack[T]) IsEmpty() bool {
	return len(s.items) == 0
}

// Generic function
func Map[T, U any](slice []T, mapper func(T) U) []U {
	result := make([]U, len(slice))
	for i, item := range slice {
		result[i] = mapper(item)
	}
	return result
}

func Filter[T any](slice []T, predicate func(T) bool) []T {
	result := []T{}
	for _, item := range slice {
		if predicate(item) {
			result = append(result, item)
		}
	}
	return result
}

func Reduce[T, U any](slice []T, initial U, reducer func(U, T) U) U {
	result := initial
	for _, item := range slice {
		result = reducer(result, item)
	}
	return result
}

// Generic constraints
type Number interface {
	int | int8 | int16 | int32 | int64 | uint | uint8 | uint16 | uint32 | uint64 | float32 | float64
}

type Stringer interface {
	~string | fmt.Stringer
}

func Sum[T Number](numbers []T) T {
	var sum T
	for _, num := range numbers {
		sum += num
	}
	return sum
}

func Max[T Number](numbers []T) T {
	if len(numbers) == 0 {
		var zero T
		return zero
	}
	max := numbers[0]
	for _, num := range numbers[1:] {
		if num > max {
			max = num
		}
	}
	return max
}

// Generic interface
type Container[T any] interface {
	Add(item T)
	Remove() (T, bool)
	IsEmpty() bool
}

// Generic struct with multiple type parameters
type Pair[K, V any] struct {
	Key   K
	Value V
}

func (p Pair[K, V]) String() string {
	return fmt.Sprintf("Pair{Key: %v, Value: %v}", p.Key, p.Value)
}

type Triple[A, B, C any] struct {
	First  A
	Second B
	Third  C
}

// Generic method
func (p Pair[K, V]) Swap() Pair[V, K] {
	return Pair[V, K]{Key: p.Value, Value: p.Key}
}

// Generic channel function
func FanIn[T any](ctx context.Context, channels ...<-chan T) <-chan T {
	out := make(chan T)
	var wg sync.WaitGroup

	wg.Add(len(channels))
	for _, ch := range channels {
		go func(c <-chan T) {
			defer wg.Done()
			for {
				select {
				case <-ctx.Done():
					return
				case item, ok := <-c:
					if !ok {
						return
					}
					select {
					case out <- item:
					case <-ctx.Done():
						return
					}
				}
			}
		}(ch)
	}

	go func() {
		wg.Wait()
		close(out)
	}()

	return out
}

func testBasicGenerics() {
	fmt.Println("\n--- Basic Generics ---")

	// Stack with different types
	intStack := &Stack[int]{}
	intStack.Push(1)
	intStack.Push(2)
	intStack.Push(3)

	for !intStack.IsEmpty() {
		if item, ok := intStack.Pop(); ok {
			fmt.Printf("Popped int: %d\n", item)
		}
	}

	stringStack := &Stack[string]{}
	stringStack.Push("hello")
	stringStack.Push("world")

	for !stringStack.IsEmpty() {
		if item, ok := stringStack.Pop(); ok {
			fmt.Printf("Popped string: %s\n", item)
		}
	}
}

func testGenericFunctions() {
	fmt.Println("\n--- Generic Functions ---")

	// Map function
	numbers := []int{1, 2, 3, 4, 5}
	squared := Map(numbers, func(x int) int { return x * x })
	fmt.Printf("Original: %v\n", numbers)
	fmt.Printf("Squared: %v\n", squared)

	// Filter function
	evens := Filter(numbers, func(x int) bool { return x%2 == 0 })
	fmt.Printf("Evens: %v\n", evens)

	// Reduce function
	sum := Reduce(numbers, 0, func(acc, x int) int { return acc + x })
	fmt.Printf("Sum: %d\n", sum)

	product := Reduce(numbers, 1, func(acc, x int) int { return acc * x })
	fmt.Printf("Product: %d\n", product)
}

func testGenericStructs() {
	fmt.Println("\n--- Generic Structs ---")

	// Pair
	intStringPair := Pair[int, string]{Key: 1, Value: "one"}
	fmt.Printf("Pair: %s\n", intStringPair)
	fmt.Printf("Swapped: %s\n", intStringPair.Swap())

	// Triple
	triple := Triple[string, int, bool]{First: "hello", Second: 42, Third: true}
	fmt.Printf("Triple: %+v\n", triple)

	// Nested generics
	pairOfPairs := Pair[Pair[int, string], Pair[bool, float64]]{
		Key:   Pair[int, string]{Key: 1, Value: "one"},
		Value: Pair[bool, float64]{Key: true, Value: 3.14},
	}
	fmt.Printf("Pair of pairs: %+v\n", pairOfPairs)
}

func testGenericInterfaces() {
	fmt.Println("\n--- Generic Interfaces ---")

	// Using Container interface
	var container Container[int] = &Stack[int]{}
	container.Add(10)
	container.Add(20)
	container.Add(30)

	for !container.IsEmpty() {
		if item, ok := container.Remove(); ok {
			fmt.Printf("Removed from container: %d\n", item)
		}
	}
}

func testGenericConstraints() {
	fmt.Println("\n--- Generic Constraints ---")

	// Number constraint
	ints := []int{1, 2, 3, 4, 5}
	fmt.Printf("Sum of ints: %d\n", Sum(ints))
	fmt.Printf("Max of ints: %d\n", Max(ints))

	floats := []float64{1.1, 2.2, 3.3, 4.4, 5.5}
	fmt.Printf("Sum of floats: %f\n", Sum(floats))
	fmt.Printf("Max of floats: %f\n", Max(floats))

	// Test with different number types
	int32s := []int32{10, 20, 30}
	fmt.Printf("Sum of int32s: %d\n", Sum(int32s))

	uint64s := []uint64{100, 200, 300}
	fmt.Printf("Sum of uint64s: %d\n", Sum(uint64s))
}

func testGenericTypeParameters() {
	fmt.Println("\n--- Generic Type Parameters ---")

	// Complex generic types
	type Matrix[T Number] [][]T
	matrix := Matrix[int]{
		{1, 2, 3},
		{4, 5, 6},
		{7, 8, 9},
	}
	fmt.Printf("Matrix: %v\n", matrix)

	// Generic function with multiple constraints
	type Addable interface {
		int | float64 | string
	}

	func AddAll[T Addable](items []T) T {
		var result T
		for _, item := range items {
			result += item
		}
		return result
	}

	strings := []string{"hello", " ", "world"}
	fmt.Printf("Concatenated: %s\n", AddAll(strings))
}

func testGenericMethodSets() {
	fmt.Println("\n--- Generic Method Sets ---")

	// Generic struct with methods
	type Box[T any] struct {
		content T
	}

	func (b Box[T]) Get() T {
		return b.content
	}

	func (b *Box[T]) Set(content T) {
		b.content = content
	}

	func (b Box[T]) String() string {
		return fmt.Sprintf("Box(%v)", b.content)
	}

	intBox := Box[int]{content: 42}
	fmt.Printf("Int box: %s\n", intBox)
	fmt.Printf("Int box content: %d\n", intBox.Get())

	intBox.Set(100)
	fmt.Printf("Updated int box: %s\n", intBox)

	stringBox := Box[string]{content: "hello"}
	fmt.Printf("String box: %s\n", stringBox)
}

// Advanced generic patterns
func testAdvancedGenerics() {
	fmt.Println("\n--- Advanced Generics ---")

	// Type inference with generics
	func Identity[T any](x T) T {
		return x
	}

	fmt.Printf("Identity int: %d\n", Identity(42))
	fmt.Printf("Identity string: %s\n", Identity("hello"))
	fmt.Printf("Identity bool: %t\n", Identity(true))

	// Generic composition
	type Optional[T any] struct {
		value T
		valid bool
	}

	func Some[T any](value T) Optional[T] {
		return Optional[T]{value: value, valid: true}
	}

	func None[T any]() Optional[T] {
		return Optional[T]{valid: false}
	}

	func (o Optional[T]) IsSome() bool {
		return o.valid
	}

	func (o Optional[T]) IsNone() bool {
		return !o.valid
	}

	func (o Optional[T]) Unwrap() T {
		if !o.valid {
			panic("Cannot unwrap None")
		}
		return o.value
	}

	someValue := Some(42)
	noneValue := None[int]()

	fmt.Printf("Some value: %v, IsSome: %t\n", someValue.Unwrap(), someValue.IsSome())
	fmt.Printf("None value: IsNone: %t\n", noneValue.IsNone())

	// Generic worker pool
	type WorkerPool[T any] struct {
		tasks   chan T
		workers int
		wg      sync.WaitGroup
	}

	func NewWorkerPool[T any](workers int) *WorkerPool[T] {
		return &WorkerPool[T]{
			tasks:   make(chan T, workers*2),
			workers: workers,
		}
	}

	func (wp *WorkerPool[T]) Start(handler func(T)) {
		wp.wg.Add(wp.workers)
		for i := 0; i < wp.workers; i++ {
			go func() {
				defer wp.wg.Done()
				for task := range wp.tasks {
					handler(task)
				}
			}()
		}
	}

	func (wp *WorkerPool[T]) Submit(task T) {
		wp.tasks <- task
	}

	func (wp *WorkerPool[T]) Stop() {
		close(wp.tasks)
		wp.wg.Wait()
	}

	pool := NewWorkerPool[int](3)
	pool.Start(func(task int) {
		fmt.Printf("Processing task: %d\n", task)
		time.Sleep(time.Millisecond * 10)
	})

	for i := 1; i <= 10; i++ {
		pool.Submit(i)
	}

	pool.Stop()
}