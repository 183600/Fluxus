// 全面的Go语言特性测试文件
// 涵盖Go语言的核心特性，用于测试编译器支持程度

package main

import (
	"fmt"
)

// 1. 基本变量声明和类型
func testBasicVariables() {
	// 基本类型
	var a int = 10
	var b float64 = 3.14
	var c string = "hello"
	var d bool = true
	
	// 短变量声明
	e := 20
	f := 2.71
	g := "world"
	h := false
	
	fmt.Printf("Basic variables: %d, %f, %s, %t\n", a, b, c, d)
	fmt.Printf("Short declarations: %d, %f, %s, %t\n", e, f, g, h)
}

// 2. 数组和切片
func testArraysAndSlices() {
	// 数组
	var arr [3]int = [3]int{1, 2, 3}
	
	// 切片
	var slice []int = []int{4, 5, 6}
	slice = append(slice, 7)
	
	fmt.Printf("Array: %v\n", arr)
	fmt.Printf("Slice: %v\n", slice)
}

// 3. 映射（Map）
func testMaps() {
	// 创建map
	m := make(map[string]int)
	m["key1"] = 10
	m["key2"] = 20
	
	// 字面量方式
	m2 := map[string]int{
		"a": 1,
		"b": 2,
	}
	
	fmt.Printf("Map 1: %v\n", m)
	fmt.Printf("Map 2: %v\n", m2)
}

// 4. 结构体
type Person struct {
	Name string
	Age  int
}

func (p Person) String() string {
	return fmt.Sprintf("Person{Name: %s, Age: %d}", p.Name, p.Age)
}

func testStructs() {
	p1 := Person{Name: "Alice", Age: 30}
	p2 := Person{"Bob", 25}
	
	fmt.Printf("Struct 1: %s\n", p1.String())
	fmt.Printf("Struct 2: %s\n", p2.String())
}

// 5. 指针
func testPointers() {
	x := 42
	p := &x
	
	fmt.Printf("Value: %d, Pointer: %p, Value through pointer: %d\n", x, p, *p)
	
	*p = 24
	fmt.Printf("After modification: %d\n", x)
}

// 6. 函数作为值和闭包
func testFunctions() {
	// 函数作为变量
	add := func(a, b int) int {
		return a + b
	}
	
	// 闭包
	multiplier := func(factor int) func(int) int {
		return func(x int) int {
			return x * factor
		}
	}
	
	double := multiplier(2)
	
	fmt.Printf("Add function: %d\n", add(3, 4))
	fmt.Printf("Closure result: %d\n", double(5))
}

// 7. 多返回值
func divide(a, b int) (int, int, error) {
	if b == 0 {
		return 0, 0, fmt.Errorf("division by zero")
	}
	return a / b, a % b, nil
}

func testMultipleReturns() {
	quotient, remainder, err := divide(10, 3)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("10 / 3 = %d remainder %d\n", quotient, remainder)
	}
}

// 8. 接口
type Shape interface {
	Area() float64
}

type Rectangle struct {
	Width, Height float64
}

func (r Rectangle) Area() float64 {
	return r.Width * r.Height
}

type Circle struct {
	Radius float64
}

func (c Circle) Area() float64 {
	return 3.14159 * c.Radius * c.Radius
}

func testInterfaces() {
	var shapes []Shape
	shapes = append(shapes, Rectangle{Width: 3, Height: 4})
	shapes = append(shapes, Circle{Radius: 5})
	
	for i, shape := range shapes {
		fmt.Printf("Shape %d area: %f\n", i, shape.Area())
	}
}

// 9. 控制流结构
func testControlFlow() {
	// if-else
	x := 10
	if x > 5 {
		fmt.Printf("x is greater than 5\n")
	} else {
		fmt.Printf("x is less than or equal to 5\n")
	}
	
	// for循环
	fmt.Printf("For loop: ")
	for i := 0; i < 5; i++ {
		fmt.Printf("%d ", i)
	}
	fmt.Printf("\n")
	
	// range循环
	numbers := []int{1, 2, 3, 4, 5}
	fmt.Printf("Range loop: ")
	for _, num := range numbers {
		fmt.Printf("%d ", num)
	}
	fmt.Printf("\n")
	
	// switch语句
	day := 3
	switch day {
	case 1:
		fmt.Printf("Monday\n")
	case 2:
		fmt.Printf("Tuesday\n")
	case 3:
		fmt.Printf("Wednesday\n")
	default:
		fmt.Printf("Other day\n")
	}
}

// 10. 错误处理
func testErrorHandling() {
	_, _, err := divide(10, 0)
	if err != nil {
		fmt.Printf("Caught error: %v\n", err)
	}
}

// 11. 延迟执行（defer）
func testDefer() {
	fmt.Printf("Starting defer test\n")
	defer fmt.Printf("This will be printed last\n")
	fmt.Printf("Middle of function\n")
}

// 12. 方法和接收者
type Counter struct {
	count int
}

func (c *Counter) Increment() {
	c.count++
}

func (c Counter) Value() int {
	return c.count
}

func testMethods() {
	counter := &Counter{}
	counter.Increment()
	counter.Increment()
	fmt.Printf("Counter value: %d\n", counter.Value())
}

// 13. 空接口和类型断言
func testEmptyInterface() {
	var anything interface{}
	
	anything = 42
	if val, ok := anything.(int); ok {
		fmt.Printf("It's an int: %d\n", val)
	}
	
	anything = "hello"
	if val, ok := anything.(string); ok {
		fmt.Printf("It's a string: %s\n", val)
	}
}

// 14. 字符串操作
func testStrings() {
	s1 := "Hello"
	s2 := "World"
	combined := s1 + " " + s2
	
	fmt.Printf("Combined string: %s\n", combined)
	fmt.Printf("String length: %d\n", len(combined))
	
	// 字符串切片
	fmt.Printf("Substring: %s\n", combined[0:5])
}

// 15. 常量
const (
	Pi = 3.14159
	E  = 2.71828
)

func testConstants() {
	fmt.Printf("Pi: %f, E: %f\n", Pi, E)
}

// 16. 类型别名和自定义类型
type MyInt int
type StringSlice []string

func testCustomTypes() {
	var x MyInt = 10
	var strings StringSlice = []string{"a", "b", "c"}
	
	fmt.Printf("Custom int: %d\n", x)
	fmt.Printf("String slice: %v\n", strings)
}

// 17. 嵌套结构体
type Address struct {
	Street string
	City   string
}

type PersonWithAddress struct {
	Person
	Address Address
}

func testNestedStructs() {
	p := PersonWithAddress{
		Person:  Person{Name: "Charlie", Age: 35},
		Address: Address{Street: "123 Main St", City: "Anytown"},
	}
	
	fmt.Printf("Person with address: %s, %s, %s\n", p.Name, p.Address.Street, p.Address.City)
}

// 18. 递归函数
func factorial(n int) int {
	if n <= 1 {
		return 1
	}
	return n * factorial(n-1)
}

func fibonacci(n int) int {
	if n <= 1 {
		return n
	}
	return fibonacci(n-1) + fibonacci(n-2)
}

func testRecursion() {
	fmt.Printf("Factorial of 5: %d\n", factorial(5))
	fmt.Printf("Fibonacci of 7: %d\n", fibonacci(7))
}

// 19. 变参函数
func sum(numbers ...int) int {
	total := 0
	for _, num := range numbers {
		total += num
	}
	return total
}

func testVariadicFunctions() {
	result := sum(1, 2, 3, 4, 5)
	fmt.Printf("Sum of 1,2,3,4,5: %d\n", result)
}

// 20. 类型转换
func testTypeConversions() {
	var i int = 42
	var f float64 = float64(i)
	var s string = fmt.Sprintf("%d", i)
	
	fmt.Printf("Int: %d, Float: %f, String: %s\n", i, f, s)
}

// 主函数 - 运行所有测试
func main() {
	fmt.Printf("=== Go语言特性全面测试 ===\n\n")
	
	fmt.Printf("1. 测试基本变量:\n")
	testBasicVariables()
	
	fmt.Printf("\n2. 测试数组和切片:\n")
	testArraysAndSlices()
	
	fmt.Printf("\n3. 测试映射:\n")
	testMaps()
	
	fmt.Printf("\n4. 测试结构体:\n")
	testStructs()
	
	fmt.Printf("\n5. 测试指针:\n")
	testPointers()
	
	fmt.Printf("\n6. 测试函数:\n")
	testFunctions()
	
	fmt.Printf("\n7. 测试多返回值:\n")
	testMultipleReturns()
	
	fmt.Printf("\n8. 测试接口:\n")
	testInterfaces()
	
	fmt.Printf("\n9. 测试控制流:\n")
	testControlFlow()
	
	fmt.Printf("\n10. 测试错误处理:\n")
	testErrorHandling()
	
	fmt.Printf("\n11. 测试defer:\n")
	testDefer()
	
	fmt.Printf("\n12. 测试方法:\n")
	testMethods()
	
	fmt.Printf("\n13. 测试空接口:\n")
	testEmptyInterface()
	
	fmt.Printf("\n14. 测试字符串:\n")
	testStrings()
	
	fmt.Printf("\n15. 测试常量:\n")
	testConstants()
	
	fmt.Printf("\n16. 测试自定义类型:\n")
	testCustomTypes()
	
	fmt.Printf("\n17. 测试嵌套结构体:\n")
	testNestedStructs()
	
	fmt.Printf("\n18. 测试递归:\n")
	testRecursion()
	
	fmt.Printf("\n19. 测试变参函数:\n")
	testVariadicFunctions()
	
	fmt.Printf("\n20. 测试类型转换:\n")
	testTypeConversions()
	
	fmt.Printf("\n=== 所有测试完成 ===\n")
}