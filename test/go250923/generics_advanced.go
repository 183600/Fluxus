package main

import (
	"fmt"
	"constraints"
)

// 泛型接口
type Comparable[T any] interface {
	Compare(other T) int
}

// 泛型结构体
type Container[T any] struct {
	items []T
}

func (c *Container[T]) Add(item T) {
	c.items = append(c.items, item)
}

func (c *Container[T]) Get(index int) T {
	return c.items[index]
}

// 泛型函数 - 多个类型参数
func Pair[T, U any](first T, second U) (T, U) {
	return first, second
}

// 泛型约束 - 类型集合
func Max[T constraints.Ordered](a, b T) T {
	if a > b {
		return a
	}
	return b
}

// 泛型约束 - 近似类型
type Number interface {
	int | int64 | float64
}

func Sum[T Number](numbers []T) T {
	var sum T
	for _, n := range numbers {
		sum += n
	}
	return sum
}

// 泛型联合类型
func Process[T string | int](value T) {
	switch v := any(value).(type) {
	case string:
		fmt.Printf("String: %s\n", v)
	case int:
		fmt.Printf("Int: %d\n", v)
	}
}

// 泛型方法
func (c *Container[T]) Filter(predicate func(T) bool) *Container[T] {
	result := &Container[T]{}
	for _, item := range c.items {
		if predicate(item) {
			result.Add(item)
		}
	}
	return result
}

type Person struct {
	Name string
	Age  int
}

func (p Person) Compare(other Person) int {
	if p.Age < other.Age {
		return -1
	} else if p.Age > other.Age {
		return 1
	}
	return 0
}

func main() {
	// 测试泛型结构体
	container := &Container[int]{}
	container.Add(1)
	container.Add(2)
	container.Add(3)
	fmt.Printf("Container item: %d\n", container.Get(0))

	// 测试多个类型参数
	first, second := Pair("hello", 42)
	fmt.Printf("Pair: %s, %d\n", first, second)

	// 测试泛型约束
	max := Max(10, 20)
	fmt.Printf("Max: %d\n", max)

	// 测试数字求和
	numbers := []int{1, 2, 3, 4, 5}
	total := Sum(numbers)
	fmt.Printf("Sum: %d\n", total)

	// 测试联合类型
	Process("hello")
	Process(42)

	// 测试泛型方法
	evenContainer := container.Filter(func(n int) bool {
		return n%2 == 0
	})
	fmt.Printf("Even numbers count: %d\n", len(evenContainer.items))

	// 测试泛型接口
	people := []Person{
		{Name: "Alice", Age: 30},
		{Name: "Bob", Age: 25},
	}
	fmt.Printf("Person comparison: %d\n", people[0].Compare(people[1]))
}