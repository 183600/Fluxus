package main

import (
	"fmt"
	"math"
)

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

func generatePrimes(limit int) []int {
	var primes []int
	for i := 2; i <= limit; i++ {
		if isPrime(i) {
			primes = append(primes, i)
		}
	}
	return primes
}

func main() {
	fmt.Println("Prime Number Generator")
	primes := generatePrimes(100)
	fmt.Printf("First %d primes: %v\n", len(primes), primes)
	
	testNumbers := []int{17, 25, 31, 49, 67}
	for _, num := range testNumbers {
		fmt.Printf("%d is prime: %t\n", num, isPrime(num))
	}
}