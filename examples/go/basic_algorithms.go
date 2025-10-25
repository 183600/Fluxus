package main

import (
	"fmt"
	"math"
	"sort"
	"strings"
)

func fibonacci(n int) int {
	if n <= 1 {
		return n
	}
	return fibonacci(n-1) + fibonacci(n-2)
}

func fibonacciIterative(n int) int {
	if n <= 1 {
		return n
	}
	a, b := 0, 1
	for i := 2; i <= n; i++ {
		a, b = b, a+b
	}
	return b
}

func isPrime(n int) bool {
	if n < 2 {
		return false
	}
	for i := 2; i <= int(math.Sqrt(float64(n))); i++ {
		if n%i == 0 {
			return false
		}
	}
	return true
}

func factorial(n int) int {
	if n <= 1 {
		return 1
	}
	return n * factorial(n-1)
}

func gcd(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

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

func binarySearch(arr []int, target int) int {
	left, right := 0, len(arr)-1
	for left <= right {
		mid := (left + right) / 2
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

func reverseString(s string) string {
	runes := []rune(s)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

func isPalindrome(s string) bool {
	s = strings.ToLower(strings.ReplaceAll(s, " ", ""))
	return s == reverseString(s)
}

func main() {
	fmt.Println("Basic Algorithms Demo")
	fmt.Println("====================")
	
	// Fibonacci
	fmt.Printf("Fibonacci(10) = %d\n", fibonacci(10))
	fmt.Printf("Fibonacci Iterative(10) = %d\n", fibonacciIterative(10))
	
	// Prime check
	fmt.Printf("Is 17 prime? %t\n", isPrime(17))
	fmt.Printf("Is 18 prime? %t\n", isPrime(18))
	
	// Factorial
	fmt.Printf("Factorial(5) = %d\n", factorial(5))
	
	// GCD
	fmt.Printf("GCD(48, 18) = %d\n", gcd(48, 18))
	
	// Sorting
	arr := []int{64, 34, 25, 12, 22, 11, 90}
	fmt.Printf("Original array: %v\n", arr)
	sortedArr := make([]int, len(arr))
	copy(sortedArr, arr)
	bubbleSort(sortedArr)
	fmt.Printf("Bubble sorted: %v\n", sortedArr)
	
	// Binary search
	sort.Ints(arr)
	fmt.Printf("Binary search for 25: index %d\n", binarySearch(arr, 25))
	
	// String operations
	text := "Hello World"
	fmt.Printf("Original: %s\n", text)
	fmt.Printf("Reversed: %s\n", reverseString(text))
	fmt.Printf("Is palindrome? %t\n", isPalindrome(text))
	fmt.Printf("Is 'racecar' palindrome? %t\n", isPalindrome("racecar"))
}