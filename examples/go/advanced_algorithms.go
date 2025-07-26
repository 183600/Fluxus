package main

import (
	"crypto/rand"
	"fmt"
	"math"
	"math/big"
	"sort"
	"strconv"
	"strings"
	"time"
)

// Matrix operations
type Matrix [][]int

func (m Matrix) String() string {
	var result strings.Builder
	for _, row := range m {
		result.WriteString(fmt.Sprintf("%v\n", row))
	}
	return result.String()
}

func (m Matrix) Multiply(other Matrix) Matrix {
	if len(m[0]) != len(other) {
		panic("Invalid matrix dimensions for multiplication")
	}

	result := make(Matrix, len(m))
	for i := range result {
		result[i] = make([]int, len(other[0]))
	}

	for i := 0; i < len(m); i++ {
		for j := 0; j < len(other[0]); j++ {
			for k := 0; k < len(other); k++ {
				result[i][j] += m[i][k] * other[k][j]
			}
		}
	}
	return result
}

// Advanced sorting algorithms
func quickSort(arr []int, low, high int) {
	if low < high {
		pi := partition(arr, low, high)
		quickSort(arr, low, pi-1)
		quickSort(arr, pi+1, high)
	}
}

func partition(arr []int, low, high int) int {
	pivot := arr[high]
	i := low - 1

	for j := low; j < high; j++ {
		if arr[j] < pivot {
			i++
			arr[i], arr[j] = arr[j], arr[i]
		}
	}
	arr[i+1], arr[high] = arr[high], arr[i+1]
	return i + 1
}

func mergeSort(arr []int) []int {
	if len(arr) <= 1 {
		return arr
	}

	mid := len(arr) / 2
	left := mergeSort(arr[:mid])
	right := mergeSort(arr[mid:])

	return merge(left, right)
}

func merge(left, right []int) []int {
	result := make([]int, 0, len(left)+len(right))
	i, j := 0, 0

	for i < len(left) && j < len(right) {
		if left[i] <= right[j] {
			result = append(result, left[i])
			i++
		} else {
			result = append(result, right[j])
			j++
		}
	}

	result = append(result, left[i:]...)
	result = append(result, right[j:]...)
	return result
}

// Number theory functions
func isPrime(n int) bool {
	if n < 2 {
		return false
	}
	if n == 2 {
		return true
	}
	if n%2 == 0 {
		return false
	}

	for i := 3; i*i <= n; i += 2 {
		if n%i == 0 {
			return false
		}
	}
	return true
}

func gcd(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func lcm(a, b int) int {
	return (a * b) / gcd(a, b)
}

func generateRandomPrime(bits int) (*big.Int, error) {
	return rand.Prime(rand.Reader, bits)
}

// Performance testing utilities
func timeFunction(name string, fn func()) {
	start := time.Now()
	fn()
	duration := time.Since(start)
	fmt.Printf("%s took: %v\n", name, duration)
}

func main() {
	fmt.Println("=== Advanced Algorithms and Data Structures ===\n")

	// Matrix operations
	fmt.Println("Matrix Operations:")
	matrix1 := Matrix{{1, 2}, {3, 4}}
	matrix2 := Matrix{{5, 6}, {7, 8}}
	
	fmt.Println("Matrix 1:")
	fmt.Print(matrix1)
	fmt.Println("Matrix 2:")
	fmt.Print(matrix2)
	
	result := matrix1.Multiply(matrix2)
	fmt.Println("Matrix 1 × Matrix 2:")
	fmt.Print(result)

	// Sorting algorithms comparison
	fmt.Println("\n=== Sorting Algorithms Performance ===")
	
	// Generate random data
	size := 10000
	data := make([]int, size)
	for i := range data {
		data[i] = int(rand.Int31n(10000))
	}

	// Test different sorting algorithms
	quickData := make([]int, len(data))
	copy(quickData, data)
	timeFunction("QuickSort", func() {
		quickSort(quickData, 0, len(quickData)-1)
	})

	mergeData := make([]int, len(data))
	copy(mergeData, data)
	timeFunction("MergeSort", func() {
		mergeSort(mergeData)
	})

	builtinData := make([]int, len(data))
	copy(builtinData, data)
	timeFunction("Built-in Sort", func() {
		sort.Ints(builtinData)
	})

	// Number theory demonstrations
	fmt.Println("\n=== Number Theory ===")
	
	fmt.Println("Prime numbers up to 100:")
	primes := []int{}
	for i := 2; i <= 100; i++ {
		if isPrime(i) {
			primes = append(primes, i)
		}
	}
	fmt.Println(primes)

	fmt.Printf("GCD(48, 18) = %d\n", gcd(48, 18))
	fmt.Printf("LCM(48, 18) = %d\n", lcm(48, 18))

	// Generate cryptographic-quality random prime
	fmt.Println("\nGenerating random 64-bit prime:")
	prime, err := generateRandomPrime(64)
	if err != nil {
		fmt.Printf("Error generating prime: %v\n", err)
	} else {
		fmt.Printf("Random 64-bit prime: %s\n", prime.String())
	}

	// Mathematical calculations
	fmt.Println("\n=== Mathematical Calculations ===")
	
	// Calculate pi using Monte Carlo method
	n := 1000000
	inside := 0
	for i := 0; i < n; i++ {
		x, _ := rand.Int(rand.Reader, big.NewInt(1000000))
		y, _ := rand.Int(rand.Reader, big.NewInt(1000000))
		
		xf := float64(x.Int64()) / 1000000.0
		yf := float64(y.Int64()) / 1000000.0
		
		if xf*xf+yf*yf <= 1.0 {
			inside++
		}
	}
	pi := 4.0 * float64(inside) / float64(n)
	fmt.Printf("Estimated π using Monte Carlo method: %f\n", pi)
	fmt.Printf("Actual π: %f\n", math.Pi)
	fmt.Printf("Error: %f\n", math.Abs(pi-math.Pi))

	// Fibonacci sequence with memoization
	fmt.Println("\nFibonacci sequence with memoization:")
	memo := make(map[int]int64)
	
	var fibonacci func(int) int64
	fibonacci = func(n int) int64 {
		if n <= 1 {
			return int64(n)
		}
		if val, exists := memo[n]; exists {
			return val
		}
		memo[n] = fibonacci(n-1) + fibonacci(n-2)
		return memo[n]
	}

	for i := 0; i <= 20; i++ {
		fmt.Printf("F(%d) = %d\n", i, fibonacci(i))
	}

	fmt.Println("\nProgram completed successfully!")
}