package main

import "fmt"

func main() {
	// 1. If statements
	x := 10
	if x > 5 {
		fmt.Println("x is greater than 5")
	} else if x < 5 {
		fmt.Println("x is less than 5")
	} else {
		fmt.Println("x equals 5")
	}

	// 2. If with initialization
	if y := x * 2; y > 15 {
		fmt.Printf("y=%d is greater than 15\n", y)
	}

	// 3. For loops - traditional
	fmt.Print("Traditional for loop: ")
	for i := 0; i < 5; i++ {
		fmt.Printf("%d ", i)
	}
	fmt.Println()

	// 4. For loops - while style
	fmt.Print("While-style for loop: ")
	j := 0
	for j < 3 {
		fmt.Printf("%d ", j)
		j++
	}
	fmt.Println()

	// 5. For loops - infinite with break
	fmt.Print("Infinite loop with break: ")
	k := 0
	for {
		if k >= 3 {
			break
		}
		fmt.Printf("%d ", k)
		k++
	}
	fmt.Println()

	// 6. For range - slice
	slice := []string{"a", "b", "c"}
	fmt.Print("Range over slice: ")
	for i, v := range slice {
		fmt.Printf("[%d]=%s ", i, v)
	}
	fmt.Println()

	// 7. For range - map
	m := map[string]int{"one": 1, "two": 2, "three": 3}
	fmt.Print("Range over map: ")
	for k, v := range m {
		fmt.Printf("%s=%d ", k, v)
	}
	fmt.Println()

	// 8. Switch statements
	switch x {
	case 5:
		fmt.Println("x is 5")
	case 10:
		fmt.Println("x is 10")
	default:
		fmt.Println("x is something else")
	}

	// 9. Switch with initialization
	switch y := x % 3; y {
	case 0:
		fmt.Println("x is divisible by 3")
	case 1:
		fmt.Println("x mod 3 is 1")
	case 2:
		fmt.Println("x mod 3 is 2")
	}

	// 10. Type switch
	var i interface{} = "hello"
	switch v := i.(type) {
	case string:
		fmt.Printf("String: %s\n", v)
	case int:
		fmt.Printf("Integer: %d\n", v)
	default:
		fmt.Printf("Unknown type: %T\n", v)
	}
}