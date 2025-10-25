package main

import (
	"fmt"
	"strings"
)

// 标记测试用例
// 包含各种标识符、关键字、运算符、字面量和分隔符的测试

// 标识符测试
var (
	identifier1      int
	_identifier2     string
	identifier_3     bool
	Identifier4      float64
	identifier5Name  []int
	identifier6_name map[string]int
)

// 关键字测试
func keywordTest() {
	// var, func, package, import 等已在文件中使用
	
	// if, for, range 测试
	if true {
		for i := 0; i < 10; i++ {
			fmt.Println(i)
		}
		
		items := []int{1, 2, 3}
		for _, item := range items {
			fmt.Println(item)
		}
	}
	
	// switch 测试
	switch 1 {
	case 1:
		fmt.Println("one")
	case 2:
		fmt.Println("two")
	default:
		fmt.Println("other")
	}
	
	// defer, go 测试
	defer fmt.Println("deferred")
	go func() {
		fmt.Println("goroutine")
	}()
}

// 运算符测试
func operatorTest() {
	// 算术运算符
	a := 10 + 5
	b := 10 - 5
	c := 10 * 5
	d := 10 / 5
	e := 10 % 5
	
	// 比较运算符
	f := 10 == 5
	g := 10 != 5
	h := 10 > 5
	i := 10 < 5
	j := 10 >= 5
	k := 10 <= 5
	
	// 逻辑运算符
	l := true && false
	m := true 
