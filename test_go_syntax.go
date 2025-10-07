package main

import (
	"fmt"
	"math"
	"math/rand"
	"os"
	"runtime"
	"sync"
	"time"
)

// 基础类型和变量
var globalInt int = 42
var globalString string = "Hello, World!"
var globalBool bool = true
var globalFloat float64 = 3.14159
const (
	Pi = 3.14159
	MaxInt = math.MaxInt64
	MinInt = math.MinInt64
)

// 接口定义
type Shape interface {
	Area() float64
	Perimeter() float64
}

// 结构体定义
type Rectangle struct {
	Width, Height float64
}

type Circle struct {
	Radius float64
}

// 方法定义
func (r Rectangle) Area() float64 {
	return r.Width * r.Height
}

func (r Rectangle) Perimeter() float64 {
	return 2 * (r.Width + r.Height)
}

func (c Circle) Area() float64 {
	return math.Pi * c.Radius * c.Radius
}

func (c Circle) Perimeter() float64 {
	return 2 * math.Pi * c.Radius
}

// 类型约束
type Number interface {
	int | int8 | int16 | int32 | int64 | uint | uint8 | uint16 | uint32 | uint64 | float32 | float64
}

// 泛型函数
func PrintSlice[T any](slice []T) {
	for _, v := range slice {
		fmt.Println(v)
	}
}

func Min[T Number](a, b T) T {
	if a < b {
		return a
	}
	return b
}

// 嵌套结构体
type Person struct {
	Name string
	Age  int
	Address struct {
		Street  string
		City    string
		Country string
	}
}

// 组合和嵌入
type Animal struct {
	Name string
}

func (a Animal) Speak() string {
	return a.Name + " makes a sound"
}

type Dog struct {
	Animal
	Breed string
}

func (d Dog) Speak() string {
	return d.Name + " barks"
}

// 通道和并发
func worker(id int, jobs <-chan int, results chan<- int, wg *sync.WaitGroup) {
	defer wg.Done()
	for j := range jobs {
		fmt.Printf("Worker %d processing job %d\n", id, j)
		time.Sleep(time.Duration(rand.Intn(1000)) * time.Millisecond)
		results <- j * 2
	}
}

// 错误处理
func divide(a, b float64) (float64, error) {
	if b == 0 {
		return 0, fmt.Errorf("division by zero")
	}
	return a / b, nil
}

// 延迟函数和恐慌恢复
func recoverFromPanic() {
	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("Recovered from panic: %v\n", r)
		}
	}()
	panic("This is a panic")
}

// 指针操作
func modifyValue(x *int) {
	*x = 100
}

// 切片操作
func sliceOperations() {
	var numbers []int
	numbers = append(numbers, 1, 2, 3, 4, 5)

	subSlice := numbers[1:3]
	copied := make([]int, len(numbers))
	copy(copied, numbers)

	fmt.Printf("Original: %v\n", numbers)
	fmt.Printf("Subslice: %v\n", subSlice)
	fmt.Printf("Copied: %v\n", copied)
}

// 映射操作
func mapOperations() {
	m := make(map[string]int)
	m["one"] = 1
	m["two"] = 2
	m["three"] = 3

	if value, exists := m["two"]; exists {
		fmt.Printf("Value for 'two': %d\n", value)
	}

	delete(m, "one")

	for key, value := range m {
		fmt.Printf("Key: %s, Value: %d\n", key, value)
	}
}

// 闭包和高阶函数
func adder() func(int) int {
	sum := 0
	return func(x int) int {
		sum += x
		return sum
	}
}

// 数组操作
func arrayOperations() {
	var arr [5]int = [5]int{1, 2, 3, 4, 5}
	var matrix [3][3]int = [3][3]int{
		{1, 2, 3},
		{4, 5, 6},
		{7, 8, 9},
	}

	fmt.Printf("Array: %v\n", arr)
	fmt.Printf("Matrix: %v\n", matrix)
}

// 类型断言和类型开关
func typeAssertion(i interface{}) {
	switch v := i.(type) {
	case int:
		fmt.Printf("Type: int, Value: %d\n", v)
	case string:
		fmt.Printf("Type: string, Value: %s\n", v)
	case bool:
		fmt.Printf("Type: bool, Value: %t\n", v)
	default:
		fmt.Printf("Unknown type: %T\n", v)
	}
}

// 结构体标签和反射
type Student struct {
	Name  string `json:"name"`
	Age   int    `json:"age"`
	Grade string `json:"grade"`
}

// 变参函数
func sum(numbers ...int) int {
	total := 0
	for _, num := range numbers {
		total += num
	}
	return total
}

// 多返回值
func divideAndRemainder(a, b int) (int, int) {
	quotient := a / b
	remainder := a % b
	return quotient, remainder
}

// 递归函数
func factorial(n int) int {
	if n <= 1 {
		return 1
	}
	return n * factorial(n-1)
}

// defer 语句
func deferExample() {
	defer fmt.Println("First defer")
	defer fmt.Println("Second defer")
	fmt.Println("Function body")
}

// select 语句
func selectExample() {
	ch1 := make(chan string)
	ch2 := make(chan string)

	go func() {
		time.Sleep(1 * time.Second)
		ch1 <- "Message from channel 1"
	}()

	go func() {
		time.Sleep(2 * time.Second)
		ch2 <- "Message from channel 2"
	}()

	select {
	case msg1 := <-ch1:
		fmt.Println(msg1)
	case msg2 := <-ch2:
		fmt.Println(msg2)
	case <-time.After(3 * time.Second):
		fmt.Println("Timeout")
	}
}

// goroutine 池
func workerPool() {
	const numWorkers = 3
	const numJobs = 5

	jobs := make(chan int, numJobs)
	results := make(chan int, numJobs)
	var wg sync.WaitGroup

	for i := 1; i <= numWorkers; i++ {
		wg.Add(1)
		go worker(i, jobs, results, &wg)
	}

	for j := 1; j <= numJobs; j++ {
		jobs <- j
	}
	close(jobs)

	wg.Wait()
	close(results)

	for result := range results {
		fmt.Printf("Result: %d\n", result)
	}
}

// 文件操作
func fileOperations() {
	content := []byte("Hello, World!\n")
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

	fmt.Printf("File content: %s\n", readContent)
}

// 主函数
func main() {
	fmt.Println("=== Go 语法综合测试 ===")

	// 基础类型和变量
	fmt.Printf("Global Int: %d\n", globalInt)
	fmt.Printf("Global String: %s\n", globalString)
	fmt.Printf("Global Bool: %t\n", globalBool)
	fmt.Printf("Global Float: %f\n", globalFloat)
	fmt.Printf("Constants: Pi=%.2f, MaxInt=%d, MinInt=%d\n", Pi, MaxInt, MinInt)

	// 结构体和接口
	rect := Rectangle{Width: 10, Height: 5}
	circle := Circle{Radius: 3}

	fmt.Printf("Rectangle Area: %.2f, Perimeter: %.2f\n", rect.Area(), rect.Perimeter())
	fmt.Printf("Circle Area: %.2f, Perimeter: %.2f\n", circle.Area(), circle.Perimeter())

	// 泛型测试
	fmt.Println("=== 泛型测试 ===")
	intSlice := []int{1, 2, 3, 4, 5}
	stringSlice := []string{"a", "b", "c"}
	PrintSlice(intSlice)
	PrintSlice(stringSlice)
	fmt.Printf("Min(5, 3): %d\n", Min(5, 3))

	// 嵌套结构体
	person := Person{
		Name: "John Doe",
		Age:  30,
		Address: struct {
			Street  string
			City    string
			Country string
		}{
			Street:  "123 Main St",
			City:    "New York",
			Country: "USA",
		},
	}
	fmt.Printf("Person: %+v\n", person)

	// 组合和嵌入
	animal := Animal{Name: "Generic Animal"}
	dog := Dog{Animal: Animal{Name: "Buddy"}, Breed: "Golden Retriever"}
	fmt.Printf("Animal: %s\n", animal.Speak())
	fmt.Printf("Dog: %s\n", dog.Speak())

	// 并发和通道
	fmt.Println("=== 并发测试 ===")
	go func() {
		fmt.Println("Goroutine running")
	}()
	time.Sleep(100 * time.Millisecond)

	// Worker pool
	fmt.Println("=== Worker Pool ===")
	workerPool()

	// 错误处理
	fmt.Println("=== 错误处理 ===")
	result, err := divide(10, 2)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("10 / 2 = %.2f\n", result)
	}

	result, err = divide(10, 0)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("10 / 0 = %.2f\n", result)
	}

	// 恐慌恢复
	fmt.Println("=== 恐慌恢复 ===")
	recoverFromPanic()

	// 指针操作
	fmt.Println("=== 指针操作 ===")
	x := 50
	modifyValue(&x)
	fmt.Printf("Modified x: %d\n", x)

	// 切片操作
	fmt.Println("=== 切片操作 ===")
	sliceOperations()

	// 映射操作
	fmt.Println("=== 映射操作 ===")
	mapOperations()

	// 闭包和高阶函数
	fmt.Println("=== 闭包测试 ===")
	add := adder()
	fmt.Printf("Add 1: %d\n", add(1))
	fmt.Printf("Add 2: %d\n", add(2))
	fmt.Printf("Add 3: %d\n", add(3))

	// 数组操作
	fmt.Println("=== 数组操作 ===")
	arrayOperations()

	// 类型断言
	fmt.Println("=== 类型断言 ===")
	typeAssertion(42)
	typeAssertion("hello")
	typeAssertion(true)
	typeAssertion(3.14)

	// 变参函数
	fmt.Println("=== 变参函数 ===")
	fmt.Printf("Sum(1, 2, 3, 4, 5): %d\n", sum(1, 2, 3, 4, 5))

	// 多返回值
	fmt.Println("=== 多返回值 ===")
	q, r := divideAndRemainder(17, 5)
	fmt.Printf("17 / 5: Quotient=%d, Remainder=%d\n", q, r)

	// 递归函数
	fmt.Println("=== 递归函数 ===")
	fmt.Printf("Factorial(5): %d\n", factorial(5))

	// defer 语句
	fmt.Println("=== Defer 测试 ===")
	deferExample()

	// select 语句
	fmt.Println("=== Select 语句 ===")
	selectExample()

	// 文件操作
	fmt.Println("=== 文件操作 ===")
	fileOperations()

	// 运行时信息
	fmt.Println("=== 运行时信息 ===")
	fmt.Printf("GOMAXPROCS: %d\n", runtime.GOMAXPROCS(0))
	fmt.Printf("NumCPU: %d\n", runtime.NumCPU())
	fmt.Printf("NumGoroutine: %d\n", runtime.NumGoroutine())

	// 随机数
	fmt.Println("=== 随机数 ===")
	rand.Seed(time.Now().UnixNano())
	for i := 0; i < 3; i++ {
		fmt.Printf("Random number %d: %d\n", i+1, rand.Intn(100))
	}

	// 复数
	fmt.Println("=== 复数 ===")
	complexNum := complex(3, 4)
	fmt.Printf("Complex number: %v\n", complexNum)
	fmt.Printf("Real part: %f, Imaginary part: %f\n", real(complexNum), imag(complexNum))

	// 位操作
	fmt.Println("=== 位操作 ===")
	var a uint8 = 0b10101010
	var b uint8 = 0b01010101
	fmt.Printf("a & b: %08b\n", a&b)
	fmt.Printf("a | b: %08b\n", a|b)
	fmt.Printf("a ^ b: %08b\n", a^b)
	fmt.Printf("a << 2: %08b\n", a<<2)
	fmt.Printf("a >> 2: %08b\n", a>>2)

	fmt.Println("=== 测试完成 ===")
}