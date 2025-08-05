package main

import (
	"fmt"
	"slices"
	"maps"
	"cmp"
)

// Test Go 1.21+ new features

func main() {
	fmt.Println("=== Go 1.21+ New Features Test ===")

	// Test 1: Built-in min/max functions
	fmt.Println("\n1. Built-in min/max functions:")
	fmt.Printf("min(10, 5, 8, 3) = %d\n", min(10, 5, 8, 3))
	fmt.Printf("max(10, 5, 8, 3) = %d\n", max(10, 5, 8, 3))
	fmt.Printf("min(3.14, 2.71, 1.41) = %.2f\n", min(3.14, 2.71, 1.41))
	fmt.Printf("max(\"apple\", \"banana\", \"cherry\") = %s\n", max("apple", "banana", "cherry"))

	// Test 2: Built-in clear function
	fmt.Println("\n2. Built-in clear function:")
	
	// Clear slice
	slice := []int{1, 2, 3, 4, 5}
	fmt.Printf("Before clear - slice: %v, len: %d, cap: %d\n", slice, len(slice), cap(slice))
	clear(slice)
	fmt.Printf("After clear - slice: %v, len: %d, cap: %d\n", slice, len(slice), cap(slice))
	
	// Clear map
	m := map[string]int{"a": 1, "b": 2, "c": 3}
	fmt.Printf("Before clear - map: %v, len: %d\n", m, len(m))
	clear(m)
	fmt.Printf("After clear - map: %v, len: %d\n", m, len(m))

	// Test 3: Range over integer (Go 1.22 preview)
	fmt.Println("\n3. Range over integer:")
	fmt.Print("Range 5: ")
	for i := range 5 {
		fmt.Printf("%d ", i)
	}
	fmt.Println()

	// Test 4: slices package functions
	fmt.Println("\n4. slices package functions:")
	
	numbers := []int{3, 1, 4, 1, 5, 9, 2, 6}
	fmt.Printf("Original slice: %v\n", numbers)
	
	// slices.Contains
	fmt.Printf("Contains 5: %t\n", slices.Contains(numbers, 5))
	fmt.Printf("Contains 7: %t\n", slices.Contains(numbers, 7))
	
	// slices.Index
	index := slices.Index(numbers, 4)
	fmt.Printf("Index of 4: %d\n", index)
	
	// slices.Clone
	cloned := slices.Clone(numbers)
	fmt.Printf("Cloned slice: %v\n", cloned)
	
	// slices.Sort
	sortable := slices.Clone(numbers)
	slices.Sort(sortable)
	fmt.Printf("Sorted slice: %v\n", sortable)
	
	// slices.Reverse
	reversed := slices.Clone(numbers)
	slices.Reverse(reversed)
	fmt.Printf("Reversed slice: %v\n", reversed)
	
	// slices.Max and slices.Min
	fmt.Printf("Min element: %d\n", slices.Min(numbers))
	fmt.Printf("Max element: %d\n", slices.Max(numbers))
	
	// slices.Equal
	fmt.Printf("Original equals cloned: %t\n", slices.Equal(numbers, cloned))
	fmt.Printf("Original equals sorted: %t\n", slices.Equal(numbers, sortable))

	// Test 5: maps package functions
	fmt.Println("\n5. maps package functions:")
	
	testMap := map[string]int{
		"apple":  5,
		"banana": 3,
		"cherry": 8,
		"date":   2,
	}
	fmt.Printf("Original map: %v\n", testMap)
	
	// maps.Keys
	keys := maps.Keys(testMap)
	fmt.Printf("Keys: %v\n", keys)
	
	// maps.Values
	values := maps.Values(testMap)
	fmt.Printf("Values: %v\n", values)
	
	// maps.Clone
	clonedMap := maps.Clone(testMap)
	fmt.Printf("Cloned map: %v\n", clonedMap)
	
	// maps.Equal
	fmt.Printf("Original equals cloned: %t\n", maps.Equal(testMap, clonedMap))
	
	// Modify cloned map to test equality
	clonedMap["elderberry"] = 1
	fmt.Printf("After modification - equals: %t\n", maps.Equal(testMap, clonedMap))

	// Test 6: cmp.Ordered and comparison
	fmt.Println("\n6. cmp package:")
	
	fmt.Printf("cmp.Compare(10, 5) = %d\n", cmp.Compare(10, 5))
	fmt.Printf("cmp.Compare(5, 10) = %d\n", cmp.Compare(5, 10))
	fmt.Printf("cmp.Compare(5, 5) = %d\n", cmp.Compare(5, 5))
	fmt.Printf("cmp.Compare(\"apple\", \"banana\") = %d\n", cmp.Compare("apple", "banana"))

	// Test 7: Advanced slices operations
	fmt.Println("\n7. Advanced slices operations:")
	
	data := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
	
	// slices.BinarySearch
	target := 7
	index, found := slices.BinarySearch(data, target)
	fmt.Printf("BinarySearch for %d: index=%d, found=%t\n", target, index, found)
	
	// slices.Insert
	inserted := slices.Insert(slices.Clone(data), 3, 99, 100)
	fmt.Printf("Insert 99,100 at index 3: %v\n", inserted)
	
	// slices.Delete
	deleted := slices.Delete(slices.Clone(data), 2, 5)
	fmt.Printf("Delete elements from index 2 to 5: %v\n", deleted)
	
	// slices.Replace
	replaced := slices.Replace(slices.Clone(data), 2, 5, 99, 100, 101)
	fmt.Printf("Replace elements from index 2 to 5: %v\n", replaced)

	// Test 8: String operations with slices
	fmt.Println("\n8. String slices operations:")
	
	words := []string{"banana", "apple", "cherry", "date"}
	fmt.Printf("Original words: %v\n", words)
	
	slices.Sort(words)
	fmt.Printf("Sorted words: %v\n", words)
	
	fmt.Printf("Contains 'apple': %t\n", slices.Contains(words, "apple"))
	fmt.Printf("Index of 'cherry': %d\n", slices.Index(words, "cherry"))

	// Test 9: Custom comparison with slices
	fmt.Println("\n9. Custom comparison with slices:")
	
	type Person struct {
		Name string
		Age  int
	}
	
	people := []Person{
		{"Alice", 30},
		{"Bob", 25},
		{"Charlie", 35},
		{"Diana", 28},
	}
	
	fmt.Printf("Original people: %v\n", people)
	
	// Sort by age using custom comparison
	slices.SortFunc(people, func(a, b Person) int {
		return cmp.Compare(a.Age, b.Age)
	})
	fmt.Printf("Sorted by age: %v\n", people)
	
	// Sort by name
	slices.SortFunc(people, func(a, b Person) int {
		return cmp.Compare(a.Name, b.Name)
	})
	fmt.Printf("Sorted by name: %v\n", people)

	// Test 10: Performance comparison functions
	fmt.Println("\n10. Performance-oriented operations:")
	
	// Large slice for performance testing
	large := make([]int, 1000)
	for i := range large {
		large[i] = i
	}
	
	// Test Contains performance
	fmt.Printf("Contains 500 in large slice: %t\n", slices.Contains(large, 500))
	fmt.Printf("Contains 1500 in large slice: %t\n", slices.Contains(large, 1500))
	
	// Test BinarySearch performance  
	slices.Sort(large) // Ensure sorted for binary search
	index, found = slices.BinarySearch(large, 500)
	fmt.Printf("BinarySearch for 500: index=%d, found=%t\n", index, found)

	fmt.Println("\n=== Go 1.21+ Features Tests Completed ===")
}