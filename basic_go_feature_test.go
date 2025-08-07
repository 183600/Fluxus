// 基础Go特性测试 - 针对编译器基本支持测试
package main

import "fmt"

// 1. 最基本的函数和变量
func testBasic() {
	x := 10
	y := 20
	result := x + y
	fmt.Printf("Basic arithmetic: %d + %d = %d\n", x, y, result)
}

// 2. 简单的if条件
func testSimpleIf() {
	x := 15
	if x > 10 {
		fmt.Printf("x is greater than 10\n")
	} else {
		fmt.Printf("x is not greater than 10\n")
	}
}

// 3. 简单的for循环
func testSimpleFor() {
	fmt.Printf("Counting: ")
	for i := 0; i < 5; i++ {
		fmt.Printf("%d ", i)
	}
	fmt.Printf("\n")
}

// 4. 函数调用和参数传递
func add(a int, b int) int {
	return a + b
}

func testFunctionCalls() {
	result := add(5, 7)
	fmt.Printf("Function call result: %d\n", result)
}

// 5. 字符串基本操作
func testStrings() {
	name := "Go"
	greeting := "Hello, " + name
	fmt.Printf("String concatenation: %s\n", greeting)
}

// 6. 变量声明的不同方式
func testVariableDeclarations() {
	var a int = 10
	var b = 20
	c := 30
	
	fmt.Printf("Variables: a=%d, b=%d, c=%d\n", a, b, c)
}

// 7. 布尔运算
func testBooleans() {
	x := true
	y := false
	
	fmt.Printf("Boolean operations: %t && %t = %t\n", x, y, x && y)
	fmt.Printf("Boolean operations: %t || %t = %t\n", x, y, x || y)
}

// 8. 简单的数组
func testSimpleArray() {
	numbers := [3]int{1, 2, 3}
	fmt.Printf("Array: %v\n", numbers)
	fmt.Printf("First element: %d\n", numbers[0])
}

// 9. 多个返回值（简化版）
func divideSimple(a int, b int) (int, int) {
	return a / b, a % b
}

func testMultiReturn() {
	quotient, remainder := divideSimple(10, 3)
	fmt.Printf("10 / 3 = %d remainder %d\n", quotient, remainder)
}

// 10. 递归函数（简化版）
func fibonacciSimple(n int) int {
	if n <= 1 {
		return n
	}
	return fibonacciSimple(n-1) + fibonacciSimple(n-2)
}

func testRecursion() {
	result := fibonacciSimple(6)
	fmt.Printf("Fibonacci(6) = %d\n", result)
}

func main() {
	fmt.Printf("=== 基础Go特性测试 ===\n")
	
	testBasic()
	testSimpleIf()
	testSimpleFor()
	testFunctionCalls()
	testStrings()
	testVariableDeclarations()
	testBooleans()
	testSimpleArray()
	testMultiReturn()
	testRecursion()
	
	fmt.Printf("=== 基础测试完成 ===\n")
}