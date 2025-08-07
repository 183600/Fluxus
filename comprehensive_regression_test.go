package main

import "fmt"

// 测试1: 基础变量和类型
func testBasicTypes() {
	// 整数
	var intVar int = 42
	shortInt := 10
	
	// 浮点数
	var floatVar float64 = 3.14
	shortFloat := 2.71
	
	// 字符串
	var stringVar string = "hello"
	shortString := "world"
	
	// 布尔值
	var boolVar bool = true
	shortBool := false
	
	fmt.Printf("=== 基础类型测试 ===\n")
	fmt.Printf("整数: %d, %d\n", intVar, shortInt)
	fmt.Printf("浮点数: %f, %f\n", floatVar, shortFloat)
	fmt.Printf("字符串: %s, %s\n", stringVar, shortString)
	fmt.Printf("布尔值: %t, %t\n", boolVar, shortBool)
}

// 测试2: 算术运算
func testArithmetic() {
	a := 15
	b := 7
	
	fmt.Printf("=== 算术运算测试 ===\n")
	fmt.Printf("%d + %d = %d\n", a, b, a+b)
	fmt.Printf("%d - %d = %d\n", a, b, a-b)
	fmt.Printf("%d * %d = %d\n", a, b, a*b)
	fmt.Printf("%d / %d = %d\n", a, b, a/b)
	fmt.Printf("%d %% %d = %d\n", a, b, a%b)
}

// 测试3: 控制流 - if语句
func testControlFlow() {
	x := 25
	
	fmt.Printf("=== 控制流测试 ===\n")
	
	if x > 30 {
		fmt.Printf("%d is greater than 30\n", x)
	} else if x > 20 {
		fmt.Printf("%d is greater than 20 but not greater than 30\n", x)
	} else {
		fmt.Printf("%d is 20 or less\n", x)
	}
}

// 测试4: 循环
func testLoops() {
	fmt.Printf("=== 循环测试 ===\n")
	
	fmt.Printf("for loop: ")
	for i := 0; i < 5; i++ {
		fmt.Printf("%d ", i)
	}
	fmt.Printf("\n")
}

// 测试5: 函数调用和参数传递
func add(a int, b int) int {
	return a + b
}

func multiply(x int, y int) int {
	return x * y
}

func testFunctions() {
	fmt.Printf("=== 函数测试 ===\n")
	
	result1 := add(8, 12)
	result2 := multiply(6, 7)
	
	fmt.Printf("add(8, 12) = %d\n", result1)
	fmt.Printf("multiply(6, 7) = %d\n", result2)
}

// 测试6: 多参数println
func testPrintln() {
	fmt.Printf("=== Printf/Println测试 ===\n")
	
	// 测试多参数println
	fmt.Println("多参数println:", 100, 200, 300)
	fmt.Println("字符串和数字:", "result", 42)
	fmt.Println("混合类型:", "answer", 42, true)
	
	// 测试多参数printf
	name := "测试"
	age := 25
	score := 95.5
	fmt.Printf("Printf测试: %s年龄%d岁，得分%.1f分\n", name, age, score)
}

// 测试7: 递归函数
func factorial(n int) int {
	if n <= 1 {
		return 1
	}
	return n * factorial(n-1)
}

func fibonacciSimple(n int) int {
	if n <= 1 {
		return n
	}
	return fibonacciSimple(n-1) + fibonacciSimple(n-2)
}

func testRecursion() {
	fmt.Printf("=== 递归测试 ===\n")
	
	fact5 := factorial(5)
	fib7 := fibonacciSimple(7)
	
	fmt.Printf("factorial(5) = %d\n", fact5)
	fmt.Printf("fibonacci(7) = %d\n", fib7)
}

// 测试8: 复杂表达式
func testComplexExpressions() {
	fmt.Printf("=== 复杂表达式测试 ===\n")
	
	a := 10
	b := 5
	c := 3
	
	// 复杂算术表达式
	result := ((a + b) * c) - (a / b)
	fmt.Printf("((10 + 5) * 3) - (10 / 5) = %d\n", result)
	
	// 布尔表达式
	condition := (a > b) && (b > c)
	fmt.Printf("(10 > 5) && (5 > 3) = %t\n", condition)
}

// 主函数 - 运行所有测试
func main() {
	fmt.Printf("====== Go编译器回归测试套件 ======\n\n")
	
	testBasicTypes()
	fmt.Printf("\n")
	
	testArithmetic()
	fmt.Printf("\n")
	
	testControlFlow()
	fmt.Printf("\n")
	
	testLoops()
	fmt.Printf("\n")
	
	testFunctions()
	fmt.Printf("\n")
	
	testPrintln()
	fmt.Printf("\n")
	
	testRecursion()
	fmt.Printf("\n")
	
	testComplexExpressions()
	fmt.Printf("\n")
	
	fmt.Printf("====== 所有回归测试完成 ======\n")
}