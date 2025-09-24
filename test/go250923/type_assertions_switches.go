package main

import (
	"fmt"
	"testing"
)

// 类型断言基础
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

func TestTypeAssertions(t *testing.T) {
	shapes := []Shape{
		Rectangle{Width: 10, Height: 5},
		Circle{Radius: 3},
		Rectangle{Width: 4, Height: 6},
	}

	for i, shape := range shapes {
		// 类型断言基础
		if rect, ok := shape.(Rectangle); ok {
			fmt.Printf("Shape %d is Rectangle: width=%.2f, height=%.2f\n", i, rect.Width, rect.Height)
		} else if circle, ok := shape.(Circle); ok {
			fmt.Printf("Shape %d is Circle: radius=%.2f\n", i, circle.Radius)
		}

		// 类型切换
		switch v := shape.(type) {
		case Rectangle:
			fmt.Printf("Rectangle area: %.2f\n", v.Area())
		case Circle:
			fmt.Printf("Circle area: %.2f\n", v.Area())
		default:
			fmt.Printf("Unknown shape type\n")
		}
	}
}

// 空接口和类型断言
type AnyValue struct {
	value interface{}
}

func (av AnyValue) GetType() string {
	switch av.value.(type) {
	case int:
		return "int"
	case float64:
		return "float64"
	case string:
		return "string"
	case bool:
		return "bool"
	case []interface{}:
		return "slice"
	case map[string]interface{}:
		return "map"
	default:
		return "unknown"
	}
}

func TestEmptyInterfaceAssertions(t *testing.T) {
	values := []AnyValue{
		{value: 42},
		{value: 3.14},
		{value: "hello"},
		{value: true},
		{value: []interface{}{1, 2, 3}},
		{value: map[string]interface{}{"key": "value"}},
	}

	for _, v := range values {
		fmt.Printf("Type: %s\n", v.GetType())
		
		// 具体类型断言
		if val, ok := v.value.(int); ok {
			fmt.Printf("  Integer value: %d\n", val)
		} else if val, ok := v.value.(string); ok {
			fmt.Printf("  String value: %s\n", val)
		} else if val, ok := v.value.(float64); ok {
			fmt.Printf("  Float value: %.2f\n", val)
		}
	}
}

// 接口组合和类型断言
type Writer interface {
	Write([]byte) (int, error)
}

type Closer interface {
	Close() error
}

type WriteCloser interface {
	Writer
	Closer
}

type MockWriteCloser struct {
	data []byte
}

func (m *MockWriteCloser) Write(p []byte) (int, error) {
	m.data = append(m.data, p...)
	return len(p), nil
}

func (m *MockWriteCloser) Close() error {
	return nil
}

func TestInterfaceComposition(t *testing.T) {
	var wc WriteCloser = &MockWriteCloser{}
	
	// 检查是否实现了多个接口
	if _, ok := wc.(Writer); ok {
		fmt.Println("Implements Writer interface")
	}
	
	if _, ok := wc.(Closer); ok {
		fmt.Println("Implements Closer interface")
	}
	
	// 类型切换到具体类型
	switch v := wc.(type) {
	case *MockWriteCloser:
		fmt.Printf("Concrete type: %T\n", v)
	default:
		fmt.Printf("Other type: %T\n", v)
	}
}

// 错误类型断言
type CustomError struct {
	Code    int
	Message string
}

func (e CustomError) Error() string {
	return fmt.Sprintf("Error %d: %s", e.Code, e.Message)
}

func TestErrorTypeAssertions(t *testing.T) {
	var err error = CustomError{Code: 404, Message: "Not Found"}
	
	// 错误类型断言
	if customErr, ok := err.(CustomError); ok {
		fmt.Printf("Custom error - Code: %d, Message: %s\n", customErr.Code, customErr.Message)
	}
	
	// 错误类型切换
	switch e := err.(type) {
	case CustomError:
		fmt.Printf("CustomError: %d - %s\n", e.Code, e.Message)
	case *CustomError:
		fmt.Printf("*CustomError: %d - %s\n", e.Code, e.Message)
	default:
		fmt.Printf("Unknown error type: %v\n", e)
	}
}

// 指针和值类型断言
type Data struct {
	Value int
}

func TestPointerValueAssertions(t *testing.T) {
	var i interface{} = &Data{Value: 42}
	
	// 指针类型断言
	if ptr, ok := i.(*Data); ok {
		fmt.Printf("Pointer to Data: Value=%d\n", ptr.Value)
	}
	
	// 值类型断言（会失败）
	if val, ok := i.(Data); ok {
		fmt.Printf("Data value: %v\n", val)
	} else {
		fmt.Println("Not a Data value type")
	}
	
	// 存储值类型
	i = Data{Value: 100}
	if val, ok := i.(Data); ok {
		fmt.Printf("Data value: %d\n", val.Value)
	}
}

// 嵌套类型断言
type NestedInterface interface {
	DoSomething() string
}

type NestedStruct struct {
	Name string
}

func (ns NestedStruct) DoSomething() string {
	return ns.Name
}

func TestNestedAssertions(t *testing.T) {
	var outer interface{} = NestedInterface(NestedStruct{Name: "test"})
	
	// 嵌套类型断言
	if nested, ok := outer.(NestedInterface); ok {
		fmt.Printf("NestedInterface: %s\n", nested.DoSomething())
		
		// 进一步断言到具体类型
		if concrete, ok := nested.(NestedStruct); ok {
			fmt.Printf("Concrete type: %+v\n", concrete)
		}
	}
}

func main() {
	t := &testing.T{}
	fmt.Println("=== Type Assertions and Switches Tests ===")
	TestTypeAssertions(t)
	fmt.Println("\n=== Empty Interface Tests ===")
	TestEmptyInterfaceAssertions(t)
	fmt.Println("\n=== Interface Composition Tests ===")
	TestInterfaceComposition(t)
	fmt.Println("\n=== Error Type Tests ===")
	TestErrorTypeAssertions(t)
	fmt.Println("\n=== Pointer/Value Tests ===")
	TestPointerValueAssertions(t)
	fmt.Println("\n=== Nested Assertions Tests ===")
	TestNestedAssertions(t)
}