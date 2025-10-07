package main

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"math"
	"os"
	"reflect"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"
)

const (
	GlobalConst = 42
	Pi          = 3.14159
)

var (
	GlobalVar = "global"
	GlobalInt = 100
)

type (
	BasicInterface interface {
		Method1() string
		Method2(int) error
	}

	EmptyInterface interface{}

	EmbeddedInterface interface {
		BasicInterface
		Method3() float64
	}

	StructWithEmbedding struct {
		Name string
		Age  int
		BasicInterface
	}

	GenericStruct[T any] struct {
		Value T
		List  []T
	}

	GenericInterface[T any] interface {
		Process(T) T
		GetValue() T
	}

	Constraint interface {
		~int | ~float64 | ~string
	}

	GenericWithConstraint[T Constraint] struct {
		Data T
	}

	ComplexStruct struct {
		ID          int
		Name        string
		Tags        []string
		Metadata    map[string]interface{}
		CreatedAt   time.Time
		UpdatedAt   *time.Time
		IsActive    bool
		Score       float64
		Data        []byte
		Nested      *NestedStruct
		Interface   BasicInterface
		GenericField GenericStruct[int]
	}

	NestedStruct struct {
		Description string
		Count       int
	}

	ChannelStruct struct {
		DataChan  chan string
		ErrorChan chan error
		QuitChan  chan struct{}
		WaitGroup sync.WaitGroup
		Mutex     sync.Mutex
		RWMutex   sync.RWMutex
		Once      sync.Once
	}
)

func init() {
	fmt.Println("Package initialization")
	GlobalVar = "initialized"
}

func basicFunction() string {
	return "basic function"
}

func functionWithParams(name string, age int, options ...string) (string, error) {
	result := fmt.Sprintf("Name: %s, Age: %d", name, age)
	if len(options) > 0 {
		result += fmt.Sprintf(", Options: %v", options)
	}
	return result, nil
}

func multipleReturnValues() (int, string, error) {
	return 42, "hello", nil
}

func namedReturnValues() (result string, err error) {
	result = "named return"
	return
}

func variadicFunction(nums ...int) int {
	sum := 0
	for _, n := range nums {
		sum += n
	}
	return sum
}

func deferFunction() {
	defer fmt.Println("Deferred 1")
	defer fmt.Println("Deferred 2")
	fmt.Println("Normal execution")
}

func panicRecoverFunction() {
	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("Recovered from panic: %v\n", r)
		}
	}()
	
	fmt.Println("Before panic")
	panic("test panic")
	fmt.Println("After panic (unreachable)")
}

func goroutineFunction() {
	var wg sync.WaitGroup
	wg.Add(3)
	
	go func() {
		defer wg.Done()
		fmt.Println("Goroutine 1")
	}()
	
	go func(id int) {
		defer wg.Done()
		fmt.Printf("Goroutine %d\n", id)
	}(2)
	
	go func() {
		defer wg.Done()
		for i := 0; i < 3; i++ {
			fmt.Printf("Goroutine 3: %d\n", i)
			time.Sleep(10 * time.Millisecond)
		}
	}()
	
	wg.Wait()
}

func channelFunction() {
	ch1 := make(chan string)
	ch2 := make(chan int, 10)
	ch3 := make(chan struct{})
	
	go func() {
		defer func() {
			if r := recover(); r != nil {
				fmt.Printf("Goroutine recovered: %v\n", r)
			}
		}()
		
		select {
		case ch1 <- "hello":
		case <-time.After(50 * time.Millisecond):
			fmt.Println("Channel 1 send timeout")
		}
		
		select {
		case ch2 <- 42:
		case <-time.After(50 * time.Millisecond):
			fmt.Println("Channel 2 send timeout")
		}
		
		close(ch3)
	}()
	
	select {
	case msg := <-ch1:
		fmt.Printf("Received from ch1: %s\n", msg)
	case num := <-ch2:
		fmt.Printf("Received from ch2: %d\n", num)
	case <-ch3:
		fmt.Println("ch3 closed")
	case <-time.After(100 * time.Millisecond):
		fmt.Println("Timeout")
	}
	
	time.Sleep(50 * time.Millisecond)
}

func rangeFunction() {
	numbers := []int{1, 2, 3, 4, 5}
	
	for i, num := range numbers {
		fmt.Printf("Index: %d, Value: %d\n", i, num)
	}
	
	m := map[string]int{"a": 1, "b": 2, "c": 3}
	for key, value := range m {
		fmt.Printf("Key: %s, Value: %d\n", key, value)
	}
	
	str := "hello"
	for i, ch := range str {
		fmt.Printf("Index: %d, Rune: %c\n", i, ch)
	}
}

func typeSwitchFunction(i interface{}) {
	switch v := i.(type) {
	case int:
		fmt.Printf("Integer: %d\n", v)
	case string:
		fmt.Printf("String: %s\n", v)
	case []int:
		fmt.Printf("Int slice: %v\n", v)
	case map[string]string:
		fmt.Printf("String map: %v\n", v)
	case BasicInterface:
		fmt.Printf("BasicInterface: %v\n", v)
	case nil:
		fmt.Println("nil")
	default:
		fmt.Printf("Unknown type: %T\n", v)
	}
}

func genericFunction[T any](value T) T {
	return value
}

func genericWithConstraint[T Constraint](values []T) T {
	var result T
	for _, v := range values {
		switch any(v).(type) {
		case int:
			result = any(any(result).(int) + any(v).(int)).(T)
		case float64:
			result = any(any(result).(float64) + any(v).(float64)).(T)
		case string:
			result = any(any(result).(string) + any(v).(string)).(T)
		}
	}
	return result
}

func genericStructMethod[T any](gs GenericStruct[T]) T {
	return gs.Value
}

func methodFunction() {
	cs := &ComplexStruct{
		ID:        1,
		Name:      "test",
		Tags:      []string{"tag1", "tag2"},
		Metadata:  map[string]interface{}{"key": "value"},
		CreatedAt: time.Now(),
		IsActive:  true,
		Score:     99.5,
		Data:      []byte("binary data"),
	}
	
	cs.PrintInfo()
	cs.UpdateScore(95.0)
	fmt.Printf("Updated score: %.2f\n", cs.Score)
}

func (cs *ComplexStruct) PrintInfo() {
	fmt.Printf("ID: %d, Name: %s, Active: %t\n", cs.ID, cs.Name, cs.IsActive)
}

func (cs *ComplexStruct) UpdateScore(newScore float64) {
	cs.Score = newScore
	now := time.Now()
	cs.UpdatedAt = &now
}

func reflectionFunction() {
	type Person struct {
		Name string `json:"name"`
		Age  int    `json:"age"`
	}
	
	p := Person{Name: "Alice", Age: 30}
	
	v := reflect.ValueOf(p)
	t := reflect.TypeOf(p)
	
	fmt.Printf("Type: %s\n", t.Name())
	for i := 0; i < v.NumField(); i++ {
		field := t.Field(i)
		value := v.Field(i)
		fmt.Printf("Field: %s, Tag: %s, Value: %v\n", field.Name, field.Tag.Get("json"), value)
	}
	
	jsonData, _ := json.Marshal(p)
	fmt.Printf("JSON: %s\n", string(jsonData))
	
	var p2 Person
	json.Unmarshal(jsonData, &p2)
	fmt.Printf("Unmarshaled: %+v\n", p2)
}

func errorHandlingFunction() error {
	file, err := os.Open("nonexistent.txt")
	if err != nil {
		return fmt.Errorf("failed to open file: %w", err)
	}
	defer file.Close()
	
	return nil
}

func contextFunction() {
	ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
	defer cancel()
	
	done := make(chan bool)
	
	go func() {
		select {
		case <-ctx.Done():
			fmt.Printf("Context cancelled: %v\n", ctx.Err())
		case <-time.After(50 * time.Millisecond):
			fmt.Println("Work completed")
			done <- true
		}
	}()
	
	<-done
}

func interfaceFunction() {
	var iface BasicInterface = &TestImplementation{name: "test"}
	
	result := iface.Method1()
	fmt.Printf("Interface result: %s\n", result)
	
	err := iface.Method2(42)
	if err != nil {
		fmt.Printf("Method2 error: %v\n", err)
	}
}

type TestImplementation struct {
	name string
}

func (t *TestImplementation) Method1() string {
	return fmt.Sprintf("Hello from %s", t.name)
}

func (t *TestImplementation) Method2(num int) error {
	if num < 0 {
		return errors.New("number must be non-negative")
	}
	return nil
}

func advancedSliceOperations() {
	numbers := make([]int, 0, 10)
	numbers = append(numbers, 1, 2, 3, 4, 5)
	
	copySlice := make([]int, len(numbers))
	copy(copySlice, numbers)
	
	subSlice := numbers[1:4]
	
	filtered := make([]int, 0)
	for _, n := range numbers {
		if n%2 == 0 {
			filtered = append(filtered, n)
		}
	}
	
	mapped := make([]string, len(numbers))
	for i, n := range numbers {
		mapped[i] = strconv.Itoa(n)
	}
	
	sort.Ints(numbers)
	
	fmt.Printf("Original: %v\n", numbers)
	fmt.Printf("Copy: %v\n", copySlice)
	fmt.Printf("Subslice: %v\n", subSlice)
	fmt.Printf("Filtered: %v\n", filtered)
	fmt.Printf("Mapped: %v\n", mapped)
}

func advancedMapOperations() {
	m := make(map[string]int)
	m["one"] = 1
	m["two"] = 2
	m["three"] = 3
	
	if value, exists := m["two"]; exists {
		fmt.Printf("Key 'two' exists with value: %d\n", value)
	}
	
	for key := range m {
		fmt.Printf("Key: %s\n", key)
	}
	
	delete(m, "two")
	
	m2 := map[string]string{
		"en": "Hello",
		"es": "Hola",
		"fr": "Bonjour",
	}
	
	for k, v := range m2 {
		fmt.Printf("%s: %s\n", k, v)
	}
}

func stringOperations() {
	str := "Hello, World!"
	
	fmt.Printf("Length: %d\n", len(str))
	fmt.Printf("Upper: %s\n", strings.ToUpper(str))
	fmt.Printf("Lower: %s\n", strings.ToLower(str))
	fmt.Printf("Contains: %t\n", strings.Contains(str, "World"))
	fmt.Printf("Index: %d\n", strings.Index(str, "World"))
	
	parts := strings.Split(str, ", ")
	fmt.Printf("Split: %v\n", parts)
	
	joined := strings.Join(parts, " | ")
	fmt.Printf("Joined: %s\n", joined)
	
	trimmed := strings.TrimSpace("  hello  ")
	fmt.Printf("Trimmed: '%s'\n", trimmed)
}

func mathOperations() {
	fmt.Printf("Pi: %.6f\n", math.Pi)
	fmt.Printf("Sqrt(16): %.2f\n", math.Sqrt(16))
	fmt.Printf("Pow(2, 10): %.0f\n", math.Pow(2, 10))
	fmt.Printf("Floor(3.7): %.0f\n", math.Floor(3.7))
	fmt.Printf("Ceil(3.2): %.0f\n", math.Ceil(3.2))
	fmt.Printf("Abs(-5): %.0f\n", math.Abs(-5))
	fmt.Printf("Max(10, 20): %.0f\n", math.Max(10, 20))
	fmt.Printf("Min(10, 20): %.0f\n", math.Min(10, 20))
}

func fileOperations() {
	content := []byte("Hello, File System!")
	
	err := os.WriteFile("test.txt", content, 0644)
	if err != nil {
		fmt.Printf("Error writing file: %v\n", err)
		return
	}
	
	readContent, err := os.ReadFile("test.txt")
	if err != nil {
		fmt.Printf("Error reading file: %v\n", err)
		return
	}
	
	fmt.Printf("File content: %s\n", string(readContent))
	
	file, err := os.Open("test.txt")
	if err != nil {
		fmt.Printf("Error opening file: %v\n", err)
		return
	}
	defer file.Close()
	
	stat, err := file.Stat()
	if err != nil {
		fmt.Printf("Error getting file stats: %v\n", err)
		return
	}
	
	fmt.Printf("File size: %d bytes\n", stat.Size())
	fmt.Printf("File mode: %s\n", stat.Mode())
	fmt.Printf("File modified: %s\n", stat.ModTime())
	
	err = os.Remove("test.txt")
	if err != nil {
		fmt.Printf("Error removing file: %v\n", err)
	}
}

func closureFunction() func() int {
	counter := 0
	return func() int {
		counter++
		return counter
	}
}

func higherOrderFunction() {
	numbers := []int{1, 2, 3, 4, 5}
	
	double := func(n int) int {
		return n * 2
	}
	
	isEven := func(n int) bool {
		return n%2 == 0
	}
	
	result := filterAndMap(numbers, isEven, double)
	fmt.Printf("Filtered and doubled: %v\n", result)
}

func filterAndMap(numbers []int, predicate func(int) bool, transform func(int) int) []int {
	result := make([]int, 0)
	for _, n := range numbers {
		if predicate(n) {
			result = append(result, transform(n))
		}
	}
	return result
}

func benchmarkFunction() {
	start := time.Now()
	
	sum := 0
	for i := 0; i < 1000000; i++ {
		sum += i
	}
	
	elapsed := time.Since(start)
	fmt.Printf("Sum: %d, Time: %v\n", sum, elapsed)
}

func systemInfoFunction() {
	fmt.Printf("OS: %s\n", runtime.GOOS)
	fmt.Printf("Arch: %s\n", runtime.GOARCH)
	fmt.Printf("Go Version: %s\n", runtime.Version())
	fmt.Printf("NumCPU: %d\n", runtime.NumCPU())
	fmt.Printf("NumGoroutine: %d\n", runtime.NumGoroutine())
	fmt.Printf("GOMAXPROCS: %d\n", runtime.GOMAXPROCS(0))
}

func main() {
	fmt.Println("=== Comprehensive Go Syntax Test ===")
	
	fmt.Println("\n--- Basic Functions ---")
	fmt.Println(basicFunction())
	
	fmt.Println("\n--- Function with Params ---")
	result, err := functionWithParams("Alice", 30, "option1", "option2")
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Println(result)
	}
	
	fmt.Println("\n--- Multiple Return Values ---")
	a, b, c := multipleReturnValues()
	fmt.Printf("Values: %d, %s, %v\n", a, b, c)
	
	fmt.Println("\n--- Named Return Values ---")
	fmt.Println(namedReturnValues())
	
	fmt.Println("\n--- Variadic Function ---")
	fmt.Printf("Sum: %d\n", variadicFunction(1, 2, 3, 4, 5))
	
	fmt.Println("\n--- Defer Function ---")
	deferFunction()
	
	fmt.Println("\n--- Panic/Recover Function ---")
	panicRecoverFunction()
	
	fmt.Println("\n--- Goroutine Function ---")
	goroutineFunction()
	
	fmt.Println("\n--- Channel Function ---")
	channelFunction()
	
	fmt.Println("\n--- Range Function ---")
	rangeFunction()
	
	fmt.Println("\n--- Type Switch Function ---")
	typeSwitchFunction(42)
	typeSwitchFunction("hello")
	typeSwitchFunction([]int{1, 2, 3})
	typeSwitchFunction(nil)
	
	fmt.Println("\n--- Generic Function ---")
	fmt.Printf("Generic string: %v\n", genericFunction("hello"))
	fmt.Printf("Generic int: %v\n", genericFunction(42))
	
	fmt.Println("\n--- Generic with Constraint ---")
	intSum := genericWithConstraint([]int{1, 2, 3, 4, 5})
	fmt.Printf("Int sum: %v\n", intSum)
	
	strConcat := genericWithConstraint([]string{"hello", " ", "world"})
	fmt.Printf("String concat: %v\n", strConcat)
	
	fmt.Println("\n--- Method Function ---")
	methodFunction()
	
	fmt.Println("\n--- Reflection Function ---")
	reflectionFunction()
	
	fmt.Println("\n--- Error Handling Function ---")
	if err := errorHandlingFunction(); err != nil {
		fmt.Printf("Error: %v\n", err)
	}
	
	fmt.Println("\n--- Context Function ---")
	contextFunction()
	
	fmt.Println("\n--- Interface Function ---")
	interfaceFunction()
	
	fmt.Println("\n--- Advanced Slice Operations ---")
	advancedSliceOperations()
	
	fmt.Println("\n--- Advanced Map Operations ---")
	advancedMapOperations()
	
	fmt.Println("\n--- String Operations ---")
	stringOperations()
	
	fmt.Println("\n--- Math Operations ---")
	mathOperations()
	
	fmt.Println("\n--- File Operations ---")
	fileOperations()
	
	fmt.Println("\n--- Closure Function ---")
	counter := closureFunction()
	for i := 0; i < 3; i++ {
		fmt.Printf("Counter: %d\n", counter())
	}
	
	fmt.Println("\n--- Higher Order Function ---")
	higherOrderFunction()
	
	fmt.Println("\n--- Benchmark Function ---")
	benchmarkFunction()
	
	fmt.Println("\n--- System Info Function ---")
	systemInfoFunction()
	
	fmt.Println("\n=== Test Completed Successfully ===")
}