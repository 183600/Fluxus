package main

import (
	"fmt"
	"math"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"time"
)

// 接口定义
type Shape interface {
	Area() float64
	Perimeter() float64
	String() string
}

// 结构体定义
type Rectangle struct {
	Width  float64
	Height float64
	Name   string
}

// 结构体方法
func (r Rectangle) Area() float64 {
	return r.Width * r.Height
}

func (r Rectangle) Perimeter() float64 {
	return 2 * (r.Width + r.Height)
}

func (r Rectangle) String() string {
	return fmt.Sprintf("Rectangle: %s, Width: %.2f, Height: %.2f", r.Name, r.Width, r.Height)
}

// 指针接收者方法
func (r *Rectangle) Scale(factor float64) {
	r.Width *= factor
	r.Height *= factor
}

// 另一个结构体
type Circle struct {
	Radius float64
	Name   string
}

func (c Circle) Area() float64 {
	return math.Pi * c.Radius * c.Radius
}

func (c Circle) Perimeter() float64 {
	return 2 * math.Pi * c.Radius
}

func (c Circle) String() string {
	return fmt.Sprintf("Circle: %s, Radius: %.2f", c.Name, c.Radius)
}

// 嵌入式结构体
type ColoredRectangle struct {
	Rectangle
	Color string
}

// 函数类型
type MathOperation func(int, int) int

// 高阶函数
func ApplyOperation(a, b int, op MathOperation) int {
	return op(a, b)
}

// 闭包
func MakeMultiplier(factor int) func(int) int {
	return func(x int) int {
		return x * factor
	}
}

// 通道和goroutine
func Worker(id int, jobs <-chan int, results chan<- int, wg *sync.WaitGroup) {
	defer wg.Done()
	for job := range jobs {
		fmt.Printf("Worker %d processing job %d\n", id, job)
		time.Sleep(time.Millisecond * 100)
		results <- job * job
	}
}

// 错误处理
func SafeDivide(a, b float64) (float64, error) {
	if b == 0 {
		return 0, fmt.Errorf("division by zero")
	}
	return a / b, nil
}

// defer语句
func DeferExample() {
	defer fmt.Println("This runs last")
	defer fmt.Println("This runs second")
	fmt.Println("This runs first")
}

// panic和recover
func PanicRecoverExample() {
	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("Recovered from panic: %v\n", r)
		}
	}()
	
	fmt.Println("About to panic")
	panic("something went wrong")
	fmt.Println("This won't be printed")
}

// select语句
func SelectExample() {
	ch1 := make(chan string)
	ch2 := make(chan string)
	
	go func() {
		time.Sleep(time.Second * 1)
		ch1 <- "message from ch1"
	}()
	
	go func() {
		time.Sleep(time.Second * 2)
		ch2 <- "message from ch2"
	}()
	
	for i := 0; i < 2; i++ {
		select {
		case msg1 := <-ch1:
			fmt.Println("Received:", msg1)
		case msg2 := <-ch2:
			fmt.Println("Received:", msg2)
		case <-time.After(time.Second * 3):
			fmt.Println("Timeout")
		}
	}
}

// 结构体标签
type Person struct {
	Name    string `json:"name" xml:"name"`
	Age     int    `json:"age" xml:"age"`
	Email   string `json:"email,omitempty"`
	private string `json:"-\"`
}

// 方法值和方法表达式
func MethodValueExample() {
	rect := Rectangle{Width: 10, Height: 5, Name: "TestRect"}
	
	// 方法值
	areaFunc := rect.Area
	fmt.Printf("Area using method value: %.2f\n", areaFunc())
	
	// 方法表达式
	areaExpr := Rectangle.Area
	fmt.Printf("Area using method expression: %.2f\n", areaExpr(rect))
}

// 接口类型断言
func InterfaceAssertionExample() {
	var shapes []Shape = []Shape{
		Rectangle{Width: 10, Height: 5, Name: "Rect1"},
		Circle{Radius: 7, Name: "Circle1"},
		Rectangle{Width: 8, Height: 3, Name: "Rect2"},
	}
	
	for _, shape := range shapes {
		fmt.Println(shape.String())
		fmt.Printf("Area: %.2f, Perimeter: %.2f\n", shape.Area(), shape.Perimeter())
		
		// 类型断言
		if rect, ok := shape.(Rectangle); ok {
			fmt.Printf("  This is a rectangle with name: %s\n", rect.Name)
		}
		
		// 类型switch
		switch s := shape.(type) {
		case Rectangle:
			fmt.Printf("  Rectangle width: %.2f\n", s.Width)
		case Circle:
			fmt.Printf("  Circle radius: %.2f\n", s.Radius)
		default:
			fmt.Printf("  Unknown shape type\n")
		}
	}
}

// 数组和切片操作
func ArraySliceExample() {
	// 数组
	var arr [5]int = [5]int{1, 2, 3, 4, 5}
	fmt.Printf("Array: %v, Length: %d\n", arr, len(arr))
	
	// 切片
	slice := []int{10, 20, 30, 40, 50}
	fmt.Printf("Slice: %v, Length: %d, Capacity: %d\n", slice, len(slice), cap(slice))
	
	// 切片操作
	subSlice := slice[1:4]
	fmt.Printf("Sub-slice [1:4]: %v\n", subSlice)
	
	// append
	slice = append(slice, 60, 70)
	fmt.Printf("After append: %v\n", slice)
	
	// copy
	newSlice := make([]int, len(slice))
	copy(newSlice, slice)
	fmt.Printf("Copied slice: %v\n", newSlice)
	
	// 二维切片
	matrix := [][]int{
		{1, 2, 3},
		{4, 5, 6},
		{7, 8, 9},
	}
	fmt.Printf("Matrix: %v\n", matrix)
}

// Map操作
func MapExample() {
	// 创建map
	m := make(map[string]int)
	m["apple"] = 5
	m["banana"] = 3
	m["orange"] = 7
	
	fmt.Printf("Map: %v\n", m)
	fmt.Printf("Apple count: %d\n", m["apple"])
	
	// 检查存在性
	if count, exists := m["grape"]; exists {
		fmt.Printf("Grape count: %d\n", count)
	} else {
		fmt.Println("Grape not found")
	}
	
	// 删除
	delete(m, "banana")
	fmt.Printf("After deletion: %v\n", m)
	
	// 字面量初始化
	scores := map[string]float64{
		"math":    95.5,
		"english": 88.0,
		"science": 92.5,
	}
	fmt.Printf("Scores: %v\n", scores)
	
	// 遍历
	for subject, score := range scores {
		fmt.Printf("%s: %.1f\n", subject, score)
	}
}

// 字符串操作
func StringExample() {
	str := "Hello, 世界!"
	fmt.Printf("String: %s, Length: %d\n", str, len(str))
	fmt.Printf("Runes count: %d\n", len([]rune(str)))
	
	// 字符串操作
	fmt.Printf("Upper: %s\n", strings.ToUpper(str))
	fmt.Printf("Contains '世界': %t\n", strings.Contains(str, "世界"))
	fmt.Printf("Replace: %s\n", strings.ReplaceAll(str, "Hello", "Hi"))
	
	// 字符串转换
	numStr := "123"
	if num, err := strconv.Atoi(numStr); err == nil {
		fmt.Printf("Converted number: %d\n", num)
	}
	
	// 格式化
	formatted := fmt.Sprintf("Pi: %.2f, E: %.3f", math.Pi, math.E)
	fmt.Printf("Formatted: %s\n", formatted)
}

// 指针操作
func PointerExample() {
	x := 42
	p := &x
	
	fmt.Printf("x = %d, address = %p\n", x, &x)
	fmt.Printf("p = %p, *p = %d\n", p, *p)
	
	*p = 100
	fmt.Printf("After *p = 100, x = %d\n", x)
	
	// 指针数组
	arr := [3]int{1, 2, 3}
	var ptrs [3]*int
	for i := 0; i < len(arr); i++ {
		ptrs[i] = &arr[i]
	}
	
	for i, ptr := range ptrs {
		fmt.Printf("ptrs[%d] = %p, *ptrs[%d] = %d\n", i, ptr, i, *ptr)
	}
}

// 常量、变量、iota
const (
	Sunday = iota
	Monday
	Tuesday
	Wednesday
	Thursday
	Friday
	Saturday
)

const (
	FlagNone = 1 << iota
	FlagRead
	FlagWrite
	FlagExecute
)

// 包级变量
var (
	globalVar   = "I'm global"
	buildTime   string
	goVersion   = runtime.Version()
	numCPU      = runtime.NumCPU()
)

// init函数
func init() {
	fmt.Println("Package initialization")
	buildTime = time.Now().Format(time.RFC3339)
}

// 主函数
func main() {
	fmt.Println("=== Comprehensive Go Syntax Test ===")
	fmt.Printf("Go version: %s, CPUs: %d\n", goVersion, numCPU)
	fmt.Printf("Build time: %s\n", buildTime)
	fmt.Printf("Global var: %s\n", globalVar)
	
	fmt.Println("\n1. Constants and iota:")
	fmt.Printf("Days of week: %d %d %d %d %d %d %d\n", Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)
	fmt.Printf("Flags: %d %d %d %d\n", FlagNone, FlagRead, FlagWrite, FlagExecute)
	
	fmt.Println("\n2. Array and Slice operations:")
	ArraySliceExample()
	
	fmt.Println("\n3. Map operations:")
	MapExample()
	
	fmt.Println("\n4. String operations:")
	StringExample()
	
	fmt.Println("\n5. Pointer operations:")
	PointerExample()
	
	fmt.Println("\n6. Interface and Polymorphism:")
	InterfaceAssertionExample()
	
	fmt.Println("\n7. Method values and expressions:")
	MethodValueExample()
	
	fmt.Println("\n8. Goroutines and Channels:")
	const numJobs = 5
	jobs := make(chan int, numJobs)
	results := make(chan int, numJobs)
	var wg sync.WaitGroup
	
	// 启动worker goroutines
	for w := 1; w <= 3; w++ {
		wg.Add(1)
		go Worker(w, jobs, results, &wg)
	}
	
	// 发送任务
	for j := 1; j <= numJobs; j++ {
		jobs <- j
	}
	close(jobs)
	
	// 等待所有worker完成
	go func() {
		wg.Wait()
		close(results)
	}()
	
	// 收集结果
	for result := range results {
		fmt.Printf("Result: %d\n", result)
	}
	
	fmt.Println("\n9. Error handling:")
	if result, err := SafeDivide(10, 2); err == nil {
		fmt.Printf("10 / 2 = %.2f\n", result)
	}
	if _, err := SafeDivide(10, 0); err != nil {
		fmt.Printf("Error: %v\n", err)
	}
	
	fmt.Println("\n10. Defer example:")
	DeferExample()
	
	fmt.Println("\n11. Panic and recover:")
	PanicRecoverExample()
	
	fmt.Println("\n12. Select example:")
	SelectExample()
	
	fmt.Println("\n13. High-order functions and closures:")
	add := func(a, b int) int { return a + b }
	multiply := func(a, b int) int { return a * b }
	
	fmt.Printf("ApplyOperation(5, 3, add) = %d\n", ApplyOperation(5, 3, add))
	fmt.Printf("ApplyOperation(5, 3, multiply) = %d\n", ApplyOperation(5, 3, multiply))
	
	double := MakeMultiplier(2)
	triple := MakeMultiplier(3)
	fmt.Printf("double(5) = %d, triple(5) = %d\n", double(5), triple(5))
	
	fmt.Println("\n14. Final shape calculations:")
	shapes := []Shape{
		Rectangle{Width: 10, Height: 5, Name: "Rect1"},
		Circle{Radius: 7, Name: "Circle1"},
		Rectangle{Width: 8, Height: 3, Name: "Rect2"},
	}
	
	totalArea := 0.0
	for _, shape := range shapes {
		fmt.Println(shape.String())
		area := shape.Area()
		fmt.Printf("Area: %.2f\n", area)
		totalArea += area
	}
	fmt.Printf("Total area: %.2f\n", totalArea)
	
	fmt.Println("\n=== Test completed successfully! ===")
}