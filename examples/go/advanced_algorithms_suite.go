package main

import (
	"fmt"
	"sort"
	"sync"
	"time"
)

// Bubble Sort implementation
func bubbleSort(arr []int) []int {
	n := len(arr)
	for i := 0; i < n-1; i++ {
		for j := 0; j < n-i-1; j++ {
			if arr[j] > arr[j+1] {
				arr[j], arr[j+1] = arr[j+1], arr[j]
			}
		}
	}
	return arr
}

// Quick Sort implementation
func quickSort(arr []int) []int {
	if len(arr) <= 1 {
		return arr
	}
	
	pivot := arr[len(arr)/2]
	left := []int{}
	right := []int{}
	
	for _, num := range arr {
		if num < pivot {
			left = append(left, num)
		} else if num > pivot {
			right = append(right, num)
		}
	}
	
	return append(append(quickSort(left), pivot), quickSort(right)...)
}

// Binary Search implementation
func binarySearch(arr []int, target int) int {
	left, right := 0, len(arr)-1
	
	for left <= right {
		mid := left + (right-left)/2
		if arr[mid] == target {
			return mid
		} else if arr[mid] < target {
			left = mid + 1
		} else {
			right = mid - 1
		}
	}
	return -1
}

// Linear Search implementation
func linearSearch(arr []int, target int) int {
	for i, num := range arr {
		if num == target {
			return i
		}
	}
	return -1
}

// Concurrent data processing
func processDataConcurrently(data []int, workers int) []int {
	results := make([]int, len(data))
	var wg sync.WaitGroup
	chunkSize := len(data) / workers
	
	for i := 0; i < workers; i++ {
		wg.Add(1)
		go func(workerID int) {
			defer wg.Done()
			start := workerID * chunkSize
			end := start + chunkSize
			if workerID == workers-1 {
				end = len(data)
			}
			
			for j := start; j < end; j++ {
				results[j] = data[j] * 2
			}
		}(i)
	}
	
	wg.Wait()
	return results
}

// Matrix operations
func matrixMultiply(a, b [][]int) [][]int {
	if len(a[0]) != len(b) {
		return nil
	}
	
	result := make([][]int, len(a))
	for i := range result {
		result[i] = make([]int, len(b[0]))
	}
	
	for i := 0; i < len(a); i++ {
		for j := 0; j < len(b[0]); j++ {
			for k := 0; k < len(b); k++ {
				result[i][j] += a[i][k] * b[k][j]
			}
		}
	}
	return result
}

// Prime number generator
func generatePrimes(n int) []int {
	if n < 2 {
		return []int{}
	}
	
	primes := []int{2}
	for num := 3; len(primes) < n; num += 2 {
		isPrime := true
		for _, p := range primes {
			if p*p > num {
				break
			}
			if num%p == 0 {
				isPrime = false
				break
			}
		}
		if isPrime {
			primes = append(primes, num)
		}
	}
	return primes
}

// Factorial calculation
func factorial(n int) int {
	if n <= 1 {
		return 1
	}
	return n * factorial(n-1)
}

// GCD calculation using Euclidean algorithm
func gcd(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

// LCM calculation
func lcm(a, b int) int {
	return a * b / gcd(a, b)
}

func main() {
	fmt.Println("=== Advanced Algorithms Suite ===")
	
	// Test sorting algorithms
	fmt.Println("\n--- Sorting Algorithms ---")
	testData := []int{64, 34, 25, 12, 22, 11, 90}
	
	fmt.Printf("Original array: %v\n", testData)
	fmt.Printf("Bubble sorted: %v\n", bubbleSort(append([]int{}, testData...)))
	fmt.Printf("Quick sorted: %v\n", quickSort(append([]int{}, testData...)))
	
	// Test search algorithms
	fmt.Println("\n--- Search Algorithms ---")
	sortedData := []int{1, 3, 5, 7, 9, 11, 13, 15}
	target := 7
	fmt.Printf("Searching for %d in %v\n", target, sortedData)
	fmt.Printf("Binary search index: %d\n", binarySearch(sortedData, target))
	fmt.Printf("Linear search index: %d\n", linearSearch(sortedData, target))
	
	// Test concurrent processing
	fmt.Println("\n--- Concurrent Processing ---")
	data := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
	fmt.Printf("Original data: %v\n", data)
	processed := processDataConcurrently(data, 4)
	fmt.Printf("Processed data (x2): %v\n", processed)
	
	// Test matrix operations
	fmt.Println("\n--- Matrix Operations ---")
	a := [][]int{{1, 2, 3}, {4, 5, 6}}
	b := [][]int{{7, 8}, {9, 10}, {11, 12}}
	fmt.Printf("Matrix A: %v\n", a)
	fmt.Printf("Matrix B: %v\n", b)
	result := matrixMultiply(a, b)
	if result != nil {
		fmt.Printf("Matrix multiplication result: %v\n", result)
	}
	
	// Test prime generation
	fmt.Println("\n--- Prime Numbers ---")
	primes := generatePrimes(10)
	fmt.Printf("First 10 primes: %v\n", primes)
	
	// Test mathematical functions
	fmt.Println("\n--- Mathematical Functions ---")
	num := 5
	fmt.Printf("Factorial of %d: %d\n", num, factorial(num))
	fmt.Printf("GCD of 48 and 18: %d\n", gcd(48, 18))
	fmt.Printf("LCM of 12 and 18: %d\n", lcm(12, 18))
	
	// Performance comparison
	fmt.Println("\n--- Performance Comparison ---")
	largeData := make([]int, 1000)
	for i := range largeData {
		largeData[i] = 1000 - i
	}
	
	start := time.Now()
	bubbleSort(append([]int{}, largeData...))
	bubbleTime := time.Since(start)
	
	start = time.Now()
	quickSort(append([]int{}, largeData...))
	quickTime := time.Since(start)
	
	start = time.Now()
	sort.Ints(append([]int{}, largeData...))
	builtinTime := time.Since(start)
	
	fmt.Printf("Bubble sort time: %v\n", bubbleTime)
	fmt.Printf("Quick sort time: %v\n", quickTime)
	fmt.Printf("Built-in sort time: %v\n", builtinTime)
}