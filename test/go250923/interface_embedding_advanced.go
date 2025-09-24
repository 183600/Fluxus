package main

import (
	"fmt"
	"time"
)

// 方法重写和嵌入
type Animal struct {
	Name string
}

func (a Animal) Speak() string {
	return "Some generic animal sound"
}

func (a Animal) Move() string {
	return "Moving somehow"
}

type Dog struct {
	Animal
	Breed string
}

// 方法重写
func (d Dog) Speak() string {
	return "Woof!"
}

// 嵌入和方法提升
type Cat struct {
	Animal
	Color string
}

// 接口组合
type Walker interface {
	Walk() string
}

type Swimmer interface {
	Swim() string
}

type FlyingSwimmer interface {
	Walker
	Swimmer
	Fly() string
}

// 类型别名
type Age = int
type Score = float64
type ID = string

// 接口嵌入和组合
type Reader interface {
	Read([]byte) (int, error)
}

type Writer interface {
	Write([]byte) (int, error)
}

type ReadWriter interface {
	Reader
	Writer
}

// 结构体嵌入的高级用法
type Base struct {
	ID   int
	Name string
}

type Extended struct {
	Base
	Email    string
	embedded // 匿名嵌入
}

type embedded struct {
	Value int
}

// 接口实现和类型断言
type Shape interface {
	Area() float64
	Perimeter() float64
}

type Rectangle struct {
	Width, Height float64
}

func (r Rectangle) Area() float64 {
	return r.Width * r.Height
}

func (r Rectangle) Perimeter() float64 {
	return 2 * (r.Width + r.Height)
}

// 方法值和方法表达式
type Calculator struct{}

func (c Calculator) Add(a, b int) int {
	return a + b
}

func (c Calculator) Multiply(a, b int) int {
	return a * b
}

func main() {
	// 测试方法重写和嵌入
	dog := Dog{
		Animal: Animal{Name: "Buddy"},
		Breed:  "Golden Retriever",
	}
	
	fmt.Printf("Dog %s says: %s\n", dog.Name, dog.Speak())
	fmt.Printf("Dog %s is: %s\n", dog.Name, dog.Move())
	
	// 测试类型别名
	var age Age = 25
	var score Score = 95.5
	var id ID = "user123"
	
	fmt.Printf("Age: %d, Score: %.1f, ID: %s\n", age, score, id)
	
	// 测试接口组合
	var rw ReadWriter
	fmt.Printf("ReadWriter interface created: %v\n", rw != nil)
	
	// 测试结构体嵌入
	extended := Extended{
		Base: Base{ID: 1, Name: "Test"},
		Email: "test@example.com",
		embedded: embedded{Value: 42},
	}
	
	fmt.Printf("Extended struct - ID: %d, Name: %s, Email: %s, Value: %d\n", 
		extended.ID, extended.Name, extended.Email, extended.Value)
	
	// 测试接口实现
	rect := Rectangle{Width: 5, Height: 3}
	var shape Shape = rect
	
	fmt.Printf("Rectangle area: %.2f\n", shape.Area())
	fmt.Printf("Rectangle perimeter: %.2f\n", shape.Perimeter())
	
	// 测试类型断言
	if rectShape, ok := shape.(Rectangle); ok {
		fmt.Printf("Type assertion successful - Width: %.2f, Height: %.2f\n", 
			rectShape.Width, rectShape.Height)
	}
	
	// 测试方法值
	calc := Calculator{}
	addMethod := calc.Add
	multiplyMethod := calc.Multiply
	
	fmt.Printf("Method value - Add: %d\n", addMethod(5, 3))
	fmt.Printf("Method value - Multiply: %d\n", multiplyMethod(4, 7))
	
	// 测试方法表达式
	addExpr := Calculator.Add
	multiplyExpr := Calculator.Multiply
	
	fmt.Printf("Method expression - Add: %d\n", addExpr(calc, 10, 20))
	fmt.Printf("Method expression - Multiply: %d\n", multiplyExpr(calc, 6, 8))
	
	// 测试接口满意度
	var _ Shape = Rectangle{}
	fmt.Println("Rectangle implements Shape interface: true")
}