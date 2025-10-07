package main

import (
	"fmt"
	"sort"
)

func main() {
	fmt.Println("=== Go Slices and Maps Advanced Tests ===")
	testAdvancedSlices()
	testSliceManipulation()
	testAdvancedMaps()
	testMapOperations()
	testSliceAndMapCombinations()
	testMultiDimensionalSlices()

	fmt.Println("\n=== All advanced slices and maps tests completed successfully! ===")
}

func testAdvancedSlices() {
	fmt.Println("\n--- Advanced Slices ---")

	// Slice slicing with different bounds
	numbers := []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}

	// Various slice operations
	fmt.Printf("Original: %v\n", numbers)
	fmt.Printf("Slice [2:5]: %v\n", numbers[2:5])
	fmt.Printf("Slice [:5]: %v\n", numbers[:5])
	fmt.Printf("Slice [5:]: %v\n", numbers[5:])
	fmt.Printf("Slice [:]: %v\n", numbers[:])

	// Slice copying
	src := []int{1, 2, 3, 4, 5}
	dst := make([]int, 3)
	n := copy(dst, src)
	fmt.Printf("Copied %d elements: %v\n", n, dst)

	// Slice growth with append
	slice := []int{1, 2, 3}
	fmt.Printf("Initial: len=%d, cap=%d, %v\n", len(slice), cap(slice), slice)
	slice = append(slice, 4, 5, 6)
	fmt.Printf("After append: len=%d, cap=%d, %v\n", len(slice), cap(slice), slice)
}

func testSliceManipulation() {
	fmt.Println("\n--- Slice Manipulation ---")

	// Delete from slice (remove element at index)
	removeElement := func(slice []int, index int) []int {
		return append(slice[:index], slice[index+1:]...)
	}

	numbers := []int{10, 20, 30, 40, 50}
	fmt.Printf("Original: %v\n", numbers)
	numbers = removeElement(numbers, 2)
	fmt.Printf("After removing index 2: %v\n", numbers)

	// Insert into slice
	insertElement := func(slice []int, index int, value int) []int {
		slice = append(slice, 0)       // Extend slice
		copy(slice[index+1:], slice[index:]) // Shift elements
		slice[index] = value           // Insert new value
		return slice
	}

	numbers = insertElement(numbers, 2, 35)
	fmt.Printf("After inserting 35 at index 2: %v\n", numbers)

	// Filter slice
	filterSlice := func(slice []int, predicate func(int) bool) []int {
		result := []int{}
		for _, v := range slice {
			if predicate(v) {
				result = append(result, v)
			}
		}
		return result
	}

	evens := filterSlice(numbers, func(x int) bool { return x%2 == 0 })
	fmt.Printf("Even numbers: %v\n", evens)
}

func testAdvancedMaps() {
	fmt.Println("\n--- Advanced Maps ---")

	// Map with struct values
	type Person struct {
		Name string
		Age  int
	}

	people := make(map[int]Person)
	people[1] = Person{Name: "Alice", Age: 25}
	people[2] = Person{Name: "Bob", Age: 30}

	// Iterate over map
	for id, person := range people {
		fmt.Printf("ID %d: %s, age %d\n", id, person.Name, person.Age)
	}

	// Check if key exists
	if person, exists := people[1]; exists {
		fmt.Printf("Found person: %s\n", person.Name)
	}

	// Delete from map
	delete(people, 2)
	fmt.Printf("Map after deletion: %v\n", people)

	// Nested maps
	nestedMap := make(map[string]map[string]int)
	nestedMap["fruits"] = map[string]int{"apple": 5, "banana": 3}
	nestedMap["vegetables"] = map[string]int{"carrot": 10, "broccoli": 7}

	for category, items := range nestedMap {
		fmt.Printf("%s: %v\n", category, items)
	}
}

func testMapOperations() {
	fmt.Println("\n--- Map Operations ---")

	// Map keys and values
	ages := map[string]int{"Alice": 25, "Bob": 30, "Charlie": 35}

	// Get keys
	keys := make([]string, 0, len(ages))
	for k := range ages {
		keys = append(keys, k)
	}
	fmt.Printf("Keys: %v\n", keys)

	// Get values
	values := make([]int, 0, len(ages))
	for _, v := range ages {
		values = append(values, v)
	}
	fmt.Printf("Values: %v\n", values)

	// Map transformation
	squaredAges := make(map[string]int)
	for name, age := range ages {
		squaredAges[name] = age * age
	}
	fmt.Printf("Squared ages: %v\n", squaredAges)

	// Map filtering
	filterAges := make(map[string]int)
	for name, age := range ages {
		if age > 28 {
			filterAges[name] = age
		}
	}
	fmt.Printf("Filtered ages (>28): %v\n", filterAges)

	// Map merge
	map1 := map[string]int{"a": 1, "b": 2}
	map2 := map[string]int{"b": 3, "c": 4}
	merged := make(map[string]int)

	for k, v := range map1 {
		merged[k] = v
	}
	for k, v := range map2 {
		merged[k] = v
	}
	fmt.Printf("Merged map: %v\n", merged)
}

func testSliceAndMapCombinations() {
	fmt.Println("\n--- Slice and Map Combinations ---")

	// Slice of maps
	data := []map[string]interface{}{
		{"name": "Alice", "age": 25, "city": "New York"},
		{"name": "Bob", "age": 30, "city": "Los Angeles"},
		{"name": "Charlie", "age": 35, "city": "Chicago"},
	}

	for _, person := range data {
		fmt.Printf("%s is %d years old and lives in %s\n",
			person["name"], person["age"], person["city"])
	}

	// Map of slices
	categories := map[string][]string{
		"fruits":    {"apple", "banana", "orange"},
		"vegetables": {"carrot", "broccoli", "spinach"},
		"grains":    {"rice", "wheat", "oats"},
	}

	for category, items := range categories {
		fmt.Printf("%s: %v\n", category, items)
	}

	// Group by operation
	people := []Person{
		{Name: "Alice", Age: 25},
		{Name: "Bob", Age: 30},
		{Name: "Charlie", Age: 25},
		{Name: "David", Age: 30},
	}

	groupedByAge := make(map[int][]Person)
	for _, person := range people {
		groupedByAge[person.Age] = append(groupedByAge[person.Age], person)
	}

	for age, group := range groupedByAge {
		fmt.Printf("Age %d: %v\n", age, group)
	}
}

func testMultiDimensionalSlices() {
	fmt.Println("\n--- Multi-dimensional Slices ---")

	// 2D slice (matrix)
	matrix := [][]int{
		{1, 2, 3},
		{4, 5, 6},
		{7, 8, 9},
	}

	for i, row := range matrix {
		for j, val := range row {
			fmt.Printf("matrix[%d][%d] = %d\n", i, j, val)
		}
	}

	// Dynamic 2D slice creation
	rows := 3
	cols := 4
	dynamicMatrix := make([][]int, rows)
	for i := range dynamicMatrix {
		dynamicMatrix[i] = make([]int, cols)
	}

	// Fill with values
	for i := 0; i < rows; i++ {
		for j := 0; j < cols; j++ {
			dynamicMatrix[i][j] = i*cols + j + 1
		}
	}

	fmt.Printf("Dynamic matrix: %v\n", dynamicMatrix)

	// 3D slice
	cube := make([][][]int, 2)
	for i := range cube {
		cube[i] = make([][]int, 2)
		for j := range cube[i] {
			cube[i][j] = make([]int, 2)
		}
	}

	// Fill 3D cube
	counter := 1
	for i := 0; i < 2; i++ {
		for j := 0; j < 2; j++ {
			for k := 0; k < 2; k++ {
				cube[i][j][k] = counter
				counter++
			}
		}
	}

	fmt.Printf("3D cube: %v\n", cube)
}

// Helper function for sorting slices of structs
func testSortingSlices() {
	fmt.Println("\n--- Sorting Slices ---")

	type Product struct {
		Name  string
		Price float64
	}

	products := []Product{
		{"Apple", 1.99},
		{"Banana", 0.99},
		{"Cherry", 3.49},
	}

	// Sort by price
	sort.Slice(products, func(i, j int) bool {
		return products[i].Price < products[j].Price
	})

	fmt.Printf("Products sorted by price: %v\n", products)

	// Sort by name
	sort.Slice(products, func(i, j int) bool {
		return products[i].Name < products[j].Name
	})

	fmt.Printf("Products sorted by name: %v\n", products)
}