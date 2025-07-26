package main

import (
	"fmt"
	"math"
	"sort"
	"strings"
	"time"
)

type TreeNode struct {
	val   int
	left  *TreeNode
	right *TreeNode
}

func insertBST(root *TreeNode, val int) *TreeNode {
	if root == nil {
		return &TreeNode{val: val}
	}
	if val < root.val {
		root.left = insertBST(root.left, val)
	} else {
		root.right = insertBST(root.right, val)
	}
	return root
}

func searchBST(root *TreeNode, val int) bool {
	if root == nil {
		return false
	}
	if root.val == val {
		return true
	}
	if val < root.val {
		return searchBST(root.left, val)
	}
	return searchBST(root.right, val)
}

func inorderTraversal(root *TreeNode, result *[]int) {
	if root != nil {
		inorderTraversal(root.left, result)
		*result = append(*result, root.val)
		inorderTraversal(root.right, result)
	}
}

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

func isPrime(n int) bool {
	if n <= 1 {
		return false
	}
	if n <= 3 {
		return true
	}
	if n%2 == 0 || n%3 == 0 {
		return false
	}
	for i := 5; i*i <= n; i += 6 {
		if n%i == 0 || n%(i+2) == 0 {
			return false
		}
	}
	return true
}

func fibonacci(n int) int {
	if n <= 1 {
		return n
	}
	return fibonacci(n-1) + fibonacci(n-2)
}

func fibonacciDP(n int) int {
	if n <= 1 {
		return n
	}
	dp := make([]int, n+1)
	dp[0], dp[1] = 0, 1
	for i := 2; i <= n; i++ {
		dp[i] = dp[i-1] + dp[i-2]
	}
	return dp[n]
}

func gcd(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func lcm(a, b int) int {
	return a * b / gcd(a, b)
}

func reverseString(s string) string {
	runes := []rune(s)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

func isPalindrome(s string) bool {
	s = strings.ToLower(strings.ReplaceAll(s, " ", ""))
	for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
		if s[i] != s[j] {
			return false
		}
	}
	return true
}

func binarySearch(arr []int, target int) int {
	left, right := 0, len(arr)-1
	for left <= right {
		mid := left + (right-left)/2
		if arr[mid] == target {
			return mid
		}
		if arr[mid] < target {
			left = mid + 1
		} else {
			right = mid - 1
		}
	}
	return -1
}

func calculatePi(iterations int) float64 {
	pi := 0.0
	for i := 0; i < iterations; i++ {
		sign := math.Pow(-1, float64(i))
		pi += sign / (2*float64(i) + 1)
	}
	return 4 * pi
}

func matrixMultiply(a, b [][]int) [][]int {
	rows, cols, inner := len(a), len(b[0]), len(b)
	result := make([][]int, rows)
	for i := range result {
		result[i] = make([]int, cols)
	}
	for i := 0; i < rows; i++ {
		for j := 0; j < cols; j++ {
			for k := 0; k < inner; k++ {
				result[i][j] += a[i][k] * b[k][j]
			}
		}
	}
	return result
}

func main() {
	fmt.Println("=== Comprehensive Go Algorithms Demo ===")
	
	// BST Operations
	fmt.Println("\n1. Binary Search Tree Operations:")
	var root *TreeNode
	values := []int{50, 30, 70, 20, 40, 60, 80}
	for _, val := range values {
		root = insertBST(root, val)
	}
	
	var inorder []int
	inorderTraversal(root, &inorder)
	fmt.Printf("Inorder traversal: %v\n", inorder)
	fmt.Printf("Search 40: %t\n", searchBST(root, 40))
	fmt.Printf("Search 90: %t\n", searchBST(root, 90))
	
	// Sorting Algorithms
	fmt.Println("\n2. Sorting Algorithms:")
	arr1 := []int{64, 34, 25, 12, 22, 11, 90}
	fmt.Printf("Original array: %v\n", arr1)
	
	arr2 := make([]int, len(arr1))
	copy(arr2, arr1)
	quickSort(arr2, 0, len(arr2)-1)
	fmt.Printf("Quick sort result: %v\n", arr2)
	
	arr3 := mergeSort(arr1)
	fmt.Printf("Merge sort result: %v\n", arr3)
	
	// Mathematical Operations
	fmt.Println("\n3. Mathematical Operations:")
	fmt.Printf("Is 17 prime? %t\n", isPrime(17))
	fmt.Printf("Is 15 prime? %t\n", isPrime(15))
	
	fmt.Printf("Fibonacci(10) recursive: %d\n", fibonacci(10))
	fmt.Printf("Fibonacci(10) DP: %d\n", fibonacciDP(10))
	
	fmt.Printf("GCD(48, 18): %d\n", gcd(48, 18))
	fmt.Printf("LCM(12, 8): %d\n", lcm(12, 8))
	
	fmt.Printf("Pi approximation (1000 iterations): %.6f\n", calculatePi(1000))
	
	// String Operations
	fmt.Println("\n4. String Operations:")
	text := "Hello World"
	fmt.Printf("Original: %s\n", text)
	fmt.Printf("Reversed: %s\n", reverseString(text))
	
	palindrome1 := "A man a plan a canal Panama"
	palindrome2 := "Hello World"
	fmt.Printf("Is '%s' palindrome? %t\n", palindrome1, isPalindrome(palindrome1))
	fmt.Printf("Is '%s' palindrome? %t\n", palindrome2, isPalindrome(palindrome2))
	
	// Search Algorithm
	fmt.Println("\n5. Search Algorithm:")
	sortedArr := []int{2, 3, 4, 10, 40}
	target := 10
	result := binarySearch(sortedArr, target)
	fmt.Printf("Binary search for %d in %v: index %d\n", target, sortedArr, result)
	
	// Matrix Operations
	fmt.Println("\n6. Matrix Operations:")
	matrix1 := [][]int{{1, 2}, {3, 4}}
	matrix2 := [][]int{{5, 6}, {7, 8}}
	product := matrixMultiply(matrix1, matrix2)
	fmt.Printf("Matrix multiplication result: %v\n", product)
	
	// Performance Test
	fmt.Println("\n7. Performance Test:")
	start := time.Now()
	for i := 0; i < 10000; i++ {
		fibonacciDP(20)
	}
	duration := time.Since(start)
	fmt.Printf("10000 Fibonacci(20) calculations took: %v\n", duration)
	
	fmt.Println("\n=== Demo Complete ===")
}