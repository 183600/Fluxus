package main

import "fmt"

func main() {
	// Test multiple format specifiers in a single printf call
	name := "测试"
	age := 25
	score := 95.5
	passed := true
	
	// Multiple format specifiers: %s, %d, %.1f, %t
	fmt.Printf("学生 %s 年龄 %d 岁，得分 %.1f 分，通过: %t\n", name, age, score, passed)
	
	// Test %.2f precision
	pi := 3.14159
	fmt.Printf("圆周率: %.2f\n", pi)
	
	// Test %f basic float
	value := 123.456
	fmt.Printf("数值: %f\n", value)
}