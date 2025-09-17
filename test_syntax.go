package main

import (
	"fmt"
	"math"
	"os"
	"strings"
	"time"
)

// 基本类型和常量
const (
	MaxInt = int(^uint(0) >> 1)
	MinInt = -MaxInt - 1
	PI     = 3.14159265359
)

// 类型别名
type (
	IntSlice []int
	StringMap map[string]string
)

// 结构体定义
type Person struct {
	Name    string
	Age     int
	Address *Address
}

type Address struct {
	Street string
	City   string
}

// 接口定义
type Shape interface {
	Area() float64
	Perimeter() float64
}

type Circle struct {
	Radius float64
}

type Rectangle struct {
	Width  float64
	Height float64
}

// 泛型函数
func Max[T ~int | ~float64](a, b T) T {
	if a > b {
		return a
	}
	return b
}

func Min[T int | float64](a, b T) T {
	if a < b {
		return a
	}
	return b
}

// 方法实现
func (c Circle) Area() float64 {
	return PI * c.Radius * c.Radius
}

func (c Circle) Perimeter() float64 {
	return 2 * PI * c.Radius
}

func (r Rectangle) Area() float64 {
	return r.Width * r.Height
}

func (r Rectangle) Perimeter() float64 {
	return 2 * (r.Width + r.Height)
}

// 通道操作
func channelExample() {
	ch := make(chan int, 5)

	// 协程1
	go func() {
		for i := 0; i < 5; i++ {
			ch <- i
			time.Sleep(100 * time.Millisecond)
		}
		close(ch)
	}()

	// 协程2
	go func() {
		for val := range ch {
			fmt.Printf("Received: %d\n", val)
		}
	}()
}

// 错误处理
func divide(a, b float64) (float64, error) {
	if b == 0 {
		return 0, fmt.Errorf("division by zero")
	}
	return a / b, nil
}

// 递归函数
func factorial(n int) int {
	if n <= 1 {
		return 1
	}
	return n * factorial(n-1)
}

// 闭包函数
func adder() func(int) int {
	sum := 0
	return func(x int) int {
		sum += x
		return sum
	}
}

// 变参函数
func sum(nums ...int) int {
	total := 0
	for _, num := range nums {
		total += num
	}
	return total
}

// defer, panic, recover 示例
func deferExample() {
	defer fmt.Println("Deferred 1")
	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("Recovered: %v\n", r)
		}
	}()

	fmt.Println("Before panic")
	panic("Something went wrong!")
}

// 指针操作
func pointerExample(x *int) {
	*x = 100
}

// 切片操作
func sliceExample() []int {
	s := make([]int, 0, 10)

	// 切片操作
	s = append(s, 1, 2, 3)
	s = append(s, 4, 5, 6)

	// 切片截取
	sub := s[1:4]

	// 切片复制
	copied := make([]int, len(sub))
	copy(copied, sub)

	return copied
}

// 映射操作
func mapExample() StringMap {
	m := make(StringMap)
	m["key1"] = "value1"
	m["key2"] = "value2"

	// 遍历map
	for k, v := range m {
		fmt.Printf("%s: %s\n", k, v)
	}

	return m
}

// 结构体方法
func (p *Person) Greet() string {
	return fmt.Sprintf("Hello, my name is %s and I'm %d years old", p.Name, p.Age)
}

func (p *Person) SetAge(age int) {
	p.Age = age
}

// select语句
func selectExample() {
	ch1 := make(chan string)
	ch2 := make(chan string)

	go func() { ch1 <- "message1" }()
	go func() { ch2 <- "message2" }()

	select {
	case msg1 := <-ch1:
		fmt.Printf("Received from ch1: %s\n", msg1)
	case msg2 := <-ch2:
		fmt.Printf("Received from ch2: %s\n", msg2)
	case <-time.After(1 * time.Second):
		fmt.Println("Timeout")
	}
}

// 类型断言
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

// 复合字面量
func compositeLiterals() {
	// 切片字面量
	slice := []int{1, 2, 3, 4, 5}

	// 映射字面量
	mapping := map[string]int{"one": 1, "two": 2, "three": 3}

	// 结构体字面量
	person := Person{
		Name: "John",
		Age:  30,
		Address: &Address{
			Street: "123 Main St",
			City:   "New York",
		},
	}

	fmt.Printf("Slice: %v\n", slice)
	fmt.Printf("Map: %v\n", mapping)
	fmt.Printf("Person: %+v\n", person)
}

// 位运算
func bitwiseOperations() int {
	a := 5  // 101
	b := 3  // 011

	fmt.Printf("a & b: %d\n", a&b)   // 001
	fmt.Printf("a | b: %d\n", a|b)   // 111
	fmt.Printf("a ^ b: %d\n", a^b)   // 110
	fmt.Printf("a << 1: %d\n", a<<1) // 1010
	fmt.Printf("a >> 1: %d\n", a>>1) // 10
	fmt.Printf("^a: %d\n", ^a)      // -6 (two's complement)

	return a & b
}

// 类型转换
func typeConversions() {
	var i int = 42
	var f float64 = float64(i)
	var u uint = uint(f)
	var s string = string(rune(i))

	fmt.Printf("int: %d, float64: %f, uint: %d, string: %s\n", i, f, u, s)
}

// 数组操作
func arrayExample() [5]int {
	var arr [5]int
	arr[0] = 1
	arr[1] = 2
	arr[2] = 3
	arr[3] = 4
	arr[4] = 5

	// 数组遍历
	for i, v := range arr {
		fmt.Printf("arr[%d] = %d\n", i, v)
	}

	return arr
}

// 标签和goto
func gotoExample() {
	i := 0
	start:
	if i < 3 {
		fmt.Printf("i = %d\n", i)
		i++
		goto start
	}
}

// 函数类型
type BinaryOp func(int, int) int

func apply(op BinaryOp, a, b int) int {
	return op(a, b)
}

func add(a, b int) int {
	return a + b
}

func subtract(a, b int) int {
	return a - b
}

// 缓冲通道
func bufferedChannel() {
	ch := make(chan int, 3)
	ch <- 1
	ch <- 2
	ch <- 3

	fmt.Println("Buffered channel example")
}

// 空接口
func emptyInterface() {
	var i interface{}
	i = 42
	fmt.Printf("Value: %v, Type: %T\n", i, i)

	i = "hello"
	fmt.Printf("Value: %v, Type: %T\n", i, i)

	i = true
	fmt.Printf("Value: %v, Type: %T\n", i, i)
}

// main函数
func main() {
	fmt.Println("=== Go Syntax Test Started ===")

	// 基本输出
	fmt.Printf("Hello, World!\n")
	fmt.Printf("PI = %.2f\n", PI)

	// 变量声明
	var x int = 42
	var y = 3.14
	z := "Go Programming"

	fmt.Printf("x = %d, y = %f, z = %s\n", x, y, z)

	// 控制结构
	if x > 40 {
		fmt.Printf("x is greater than 40\n")
	}

	// for循环
	for i := 0; i < 5; i++ {
		fmt.Printf("Loop iteration: %d\n", i)
	}

	// switch语句
	switch day := time.Now().Weekday(); day {
	case time.Saturday, time.Sunday:
		fmt.Printf("Weekend\n")
	default:
		fmt.Printf("Weekday\n")
	}

	// 函数调用
	result := add(10, 20)
	fmt.Printf("10 + 20 = %d\n", result)

	// 错误处理
	divResult, err := divide(10, 2)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("10 / 2 = %f\n", divResult)
	}

	// 递归调用
	fact := factorial(5)
	fmt.Printf("5! = %d\n", fact)

	// 闭包调用
	addFunc := adder()
	fmt.Printf("Adder: %d\n", addFunc(5))
	fmt.Printf("Adder: %d\n", addFunc(10))

	// 变参函数
	total := sum(1, 2, 3, 4, 5)
	fmt.Printf("Sum: %d\n", total)

	// 泛型函数
	maxVal := Max(10, 20)
	minVal := Min(10, 20)
	fmt.Printf("Max: %d, Min: %d\n", maxVal, minVal)

	// 指针操作
	ptr := &x
	pointerExample(ptr)
	fmt.Printf("Pointer example: %d\n", x)

	// 切片操作
	sliceResult := sliceExample()
	fmt.Printf("Slice result: %v\n", sliceResult)

	// 映射操作
	mapResult := mapExample()
	fmt.Printf("Map result: %v\n", mapResult)

	// 结构体操作
	person := &Person{
		Name: "Alice",
		Age:  25,
		Address: &Address{
			Street: "456 Oak St",
			City:   "Los Angeles",
		},
	}
	fmt.Printf("%s\n", person.Greet())
	person.SetAge(26)
	fmt.Printf("%s\n", person.Greet())

	// 接口实现
	var shapes []Shape = []Shape{
		Circle{Radius: 5},
		Rectangle{Width: 4, Height: 6},
	}

	for _, shape := range shapes {
		fmt.Printf("Shape area: %f\n", shape.Area())
		fmt.Printf("Shape perimeter: %f\n", shape.Perimeter())
	}

	// 通道操作
	channelExample()

	// select语句
	selectExample()

	// 类型断言
	typeAssertion(42)
	typeAssertion("hello")
	typeAssertion(true)

	// 复合字面量
	compositeLiterals()

	// 位运算
	bitwiseOperations()

	// 类型转换
	typeConversions()

	// 数组操作
	arrayResult := arrayExample()
	fmt.Printf("Array result: %v\n", arrayResult)

	// goto示例
	gotoExample()

	// 函数类型
	apply(add, 5, 3)
	apply(subtract, 5, 3)

	// 缓冲通道
	bufferedChannel()

	// 空接口
	emptyInterface()

	// defer示例（不实际panic）
	fmt.Println("Defer test:")
	defer fmt.Println("Deferred message")

	// 环境变量
	fmt.Printf("PATH: %s\n", os.Getenv("PATH"))

	// 字符串操作
	str := "Hello, World!"
	fmt.Printf("Uppercase: %s\n", strings.ToUpper(str))
	fmt.Printf("Lowercase: %s\n", strings.ToLower(str))
	fmt.Printf("Contains 'World': %t\n", strings.Contains(str, "World"))

	// 数学函数
	fmt.Printf("Sqrt(16): %f\n", math.Sqrt(16))
	fmt.Printf("Pow(2, 3): %f\n", math.Pow(2, 3))
	fmt.Printf("Sin(0): %f\n", math.Sin(0))

	// 终止程序
	fmt.Println("=== Go Syntax Test Completed Successfully ===")
	os.Exit(0)
}