package main

import "fmt"

func main() {
	fmt.Println("=== 基础语法测试 ===")

	// 基础类型和变量
	var globalInt int = 42
	var globalString string = "Hello, World!"
	var globalBool bool = true
	var globalFloat float64 = 3.14159

	fmt.Printf("Global Int: %d\n", globalInt)
	fmt.Printf("Global String: %s\n", globalString)
	fmt.Printf("Global Bool: %t\n", globalBool)
	fmt.Printf("Global Float: %f\n", globalFloat)

	// 切片操作
	numbers := []int{1, 2, 3, 4, 5}
	fmt.Printf("Slice: %v\n", numbers)

	// 映射操作
	m := make(map[string]int)
	m["one"] = 1
	m["two"] = 2
	fmt.Printf("Map: %v\n", m)

	// 控制结构
	for i := 0; i < 3; i++ {
		fmt.Printf("i: %d\n", i)
	}

	// 函数调用
	result := add(10, 20)
	fmt.Printf("Add result: %d\n", result)

	fmt.Println("=== 测试完成 ===")
}

func add(a, b int) int {
	return a + b
}