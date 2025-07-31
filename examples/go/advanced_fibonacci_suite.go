package main

import (
	"fmt"
	"math"
	"math/big"
	"sort"
	"sync"
	"time"
)

// FibonacciSuite provides various Fibonacci implementations with benchmarking
type FibonacciSuite struct {
	cache      map[int]*big.Int
	matrixCache map[int][][]*big.Int
	mutex      sync.RWMutex
}

// NewFibonacciSuite creates a new Fibonacci suite
func NewFibonacciSuite() *FibonacciSuite {
	return &FibonacciSuite{
		cache:       make(map[int]*big.Int),
		matrixCache: make(map[int][][]*big.Int),
	}
}

// timeFunction measures execution time of a function
func timeFunction(name string, fn func() interface{}) interface{} {
	start := time.Now()
	result := fn()
	duration := time.Since(start)
	fmt.Printf("%s took %v\n", name, duration)
	return result
}

// IterativeFibonacci calculates Fibonacci using iterative approach - O(n) time, O(1) space
func (fs *FibonacciSuite) IterativeFibonacci(n int) *big.Int {
	if n <= 1 {
		return big.NewInt(int64(n))
	}
	
	a, b := big.NewInt(0), big.NewInt(1)
	for i := 2; i <= n; i++ {
		temp := new(big.Int).Add(a, b)
		a, b = b, temp
	}
	return b
}

// RecursiveFibonacci calculates Fibonacci using naive recursion - O(2^n) time
func (fs *FibonacciSuite) RecursiveFibonacci(n int) *big.Int {
	if n <= 1 {
		return big.NewInt(int64(n))
	}
	
	left := fs.RecursiveFibonacci(n - 1)
	right := fs.RecursiveFibonacci(n - 2)
	return new(big.Int).Add(left, right)
}

// MemoizedFibonacci calculates Fibonacci with memoization - O(n) time, O(n) space
func (fs *FibonacciSuite) MemoizedFibonacci(n int) *big.Int {
	fs.mutex.RLock()
	if val, exists := fs.cache[n]; exists {
		fs.mutex.RUnlock()
		return new(big.Int).Set(val)
	}
	fs.mutex.RUnlock()
	
	if n <= 1 {
		result := big.NewInt(int64(n))
		fs.mutex.Lock()
		fs.cache[n] = new(big.Int).Set(result)
		fs.mutex.Unlock()
		return result
	}
	
	result := new(big.Int).Add(
		fs.MemoizedFibonacci(n-1),
		fs.MemoizedFibonacci(n-2),
	)
	
	fs.mutex.Lock()
	fs.cache[n] = new(big.Int).Set(result)
	fs.mutex.Unlock()
	
	return result
}

// MatrixFibonacci calculates Fibonacci using matrix exponentiation - O(log n) time
func (fs *FibonacciSuite) MatrixFibonacci(n int) *big.Int {
	if n <= 1 {
		return big.NewInt(int64(n))
	}
	
	baseMatrix := [][]*big.Int{
		{big.NewInt(1), big.NewInt(1)},
		{big.NewInt(1), big.NewInt(0)},
	}
	
	result := fs.matrixPower(baseMatrix, n-1)
	return result[0][0]
}

// matrixPower calculates matrix^power using fast exponentiation
func (fs *FibonacciSuite) matrixPower(matrix [][]*big.Int, power int) [][]*big.Int {
	if power == 1 {
		return fs.copyMatrix(matrix)
	}
	
	if power%2 == 0 {
		half := fs.matrixPower(matrix, power/2)
		return fs.matrixMultiply(half, half)
	}
	
	return fs.matrixMultiply(matrix, fs.matrixPower(matrix, power-1))
}

// matrixMultiply multiplies two 2x2 matrices
func (fs *FibonacciSuite) matrixMultiply(a, b [][]*big.Int) [][]*big.Int {
	result := make([][]*big.Int, 2)
	for i := range result {
		result[i] = make([]*big.Int, 2)
		for j := range result[i] {
			result[i][j] = big.NewInt(0)
		}
	}
	
	for i := 0; i < 2; i++ {
		for j := 0; j < 2; j++ {
			for k := 0; k < 2; k++ {
				temp := new(big.Int).Mul(a[i][k], b[k][j])
				result[i][j].Add(result[i][j], temp)
			}
		}
	}
	
	return result
}

// copyMatrix creates a deep copy of a 2x2 matrix
func (fs *FibonacciSuite) copyMatrix(matrix [][]*big.Int) [][]*big.Int {
	result := make([][]*big.Int, 2)
	for i := range result {
		result[i] = make([]*big.Int, 2)
		for j := range result[i] {
			result[i][j] = new(big.Int).Set(matrix[i][j])
		}
	}
	return result
}

// FastDoublingFibonacci uses fast doubling method - O(log n) time
func (fs *FibonacciSuite) FastDoublingFibonacci(n int) *big.Int {
	if n == 0 {
		return big.NewInt(0)
	}
	
	c, d := fs.fibPair(n)
	return c
}

// fibPair calculates F(k) and F(k+1) simultaneously
func (fs *FibonacciSuite) fibPair(k int) (*big.Int, *big.Int) {
	if k == 0 {
		return big.NewInt(0), big.NewInt(1)
	}
	
	m := k / 2
	c, d := fs.fibPair(m)
	
	// c² and d²
	cSquared := new(big.Int).Mul(c, c)
	dSquared := new(big.Int).Mul(d, d)
	
	if k%2 == 0 {
		// F(2m) = c(2d - c)
		// F(2m+1) = c² + d²
		temp := new(big.Int).Sub(new(big.Int).Mul(d, big.NewInt(2)), c)
		f2m := new(big.Int).Mul(c, temp)
		f2mPlus1 := new(big.Int).Add(cSquared, dSquared)
		return f2m, f2mPlus1
	} else {
		// F(2m+1) = c² + d²
		// F(2m+2) = d(2c + d)
		f2mPlus1 := new(big.Int).Add(cSquared, dSquared)
		temp := new(big.Int).Add(new(big.Int).Mul(c, big.NewInt(2)), d)
		f2mPlus2 := new(big.Int).Mul(d, temp)
		return f2mPlus1, f2mPlus2
	}
}

// BinetFormula calculates Fibonacci using Binet's formula - O(1) time for small n
func (fs *FibonacciSuite) BinetFormula(n int) *big.Int {
	if n > 70 { // Avoid floating point precision issues
		return fs.IterativeFibonacci(n)
	}
	
	phi := (1.0 + math.Sqrt(5.0)) / 2.0
	psi := (1.0 - math.Sqrt(5.0)) / 2.0
	
	result := (math.Pow(phi, float64(n)) - math.Pow(psi, float64(n))) / math.Sqrt(5.0)
	return big.NewInt(int64(result + 0.5))
}

// LucasNumbers calculates Lucas numbers (similar to Fibonacci but starts with 2, 1)
func (fs *FibonacciSuite) LucasNumbers(n int) *big.Int {
	if n == 0 {
		return big.NewInt(2)
	}
	if n == 1 {
		return big.NewInt(1)
	}
	
	a, b := big.NewInt(2), big.NewInt(1)
	for i := 2; i <= n; i++ {
		temp := new(big.Int).Add(a, b)
		a, b = b, temp
	}
	return b
}

// TribonacciFibonacci calculates Tribonacci numbers (sum of previous 3 numbers)
func (fs *FibonacciSuite) TribonacciFibonacci(n int) *big.Int {
	if n <= 1 {
		if n == 0 {
			return big.NewInt(0)
		}
		return big.NewInt(1)
	}
	if n == 2 {
		return big.NewInt(1)
	}
	
	a, b, c := big.NewInt(0), big.NewInt(1), big.NewInt(1)
	for i := 3; i <= n; i++ {
		temp := new(big.Int).Add(new(big.Int).Add(a, b), c)
		a, b, c = b, c, temp
	}
	return c
}

// GenerateFibonacciSequence generates first n Fibonacci numbers
func (fs *FibonacciSuite) GenerateFibonacciSequence(count int) []*big.Int {
	if count <= 0 {
		return nil
	}
	
	sequence := make([]*big.Int, count)
	if count >= 1 {
		sequence[0] = big.NewInt(0)
	}
	if count >= 2 {
		sequence[1] = big.NewInt(1)
	}
	
	for i := 2; i < count; i++ {
		sequence[i] = new(big.Int).Add(sequence[i-1], sequence[i-2])
	}
	
	return sequence
}

// FibonacciChannel generates Fibonacci numbers through a channel
func (fs *FibonacciSuite) FibonacciChannel(limit int) <-chan *big.Int {
	ch := make(chan *big.Int)
	
	go func() {
		defer close(ch)
		a, b := big.NewInt(0), big.NewInt(1)
		
		for i := 0; i < limit; i++ {
			ch <- new(big.Int).Set(a)
			a, b = b, new(big.Int).Add(a, b)
		}
	}()
	
	return ch
}

// BenchmarkResult stores benchmark information
type BenchmarkResult struct {
	Method   string
	N        int
	Duration time.Duration
	Result   string
	Error    error
}

// BenchmarkAll benchmarks all Fibonacci implementations
func (fs *FibonacciSuite) BenchmarkAll(testValues []int) []BenchmarkResult {
	var results []BenchmarkResult
	
	methods := []struct {
		name string
		fn   func(int) *big.Int
	}{
		{"Iterative", fs.IterativeFibonacci},
		{"Matrix Power", fs.MatrixFibonacci},
		{"Fast Doubling", fs.FastDoublingFibonacci},
		{"Binet Formula", fs.BinetFormula},
		{"Memoized", fs.MemoizedFibonacci},
	}
	
	for _, n := range testValues {
		fmt.Printf("\nComputing Fibonacci(%d):\n", n)
		
		for _, method := range methods {
			// Skip naive recursive for large values
			if method.name == "Naive Recursive" && n > 35 {
				results = append(results, BenchmarkResult{
					Method: method.name,
					N:      n,
					Result: "Skipped (too slow)",
				})
				continue
			}
			
			start := time.Now()
			result := method.fn(n)
			duration := time.Since(start)
			
			resultStr := result.String()
			if len(resultStr) > 50 {
				resultStr = resultStr[:50] + "..."
			}
			
			benchResult := BenchmarkResult{
				Method:   method.name,
				N:        n,
				Duration: duration,
				Result:   resultStr,
			}
			
			results = append(results, benchResult)
			fmt.Printf("  %s: %s (%v)\n", method.name, resultStr, duration)
		}
	}
	
	return results
}

// ParallelFibonacci calculates multiple Fibonacci numbers in parallel
func (fs *FibonacciSuite) ParallelFibonacci(values []int) map[int]*big.Int {
	results := make(map[int]*big.Int)
	var mutex sync.Mutex
	var wg sync.WaitGroup
	
	for _, n := range values {
		wg.Add(1)
		go func(num int) {
			defer wg.Done()
			result := fs.FastDoublingFibonacci(num)
			
			mutex.Lock()
			results[num] = result
			mutex.Unlock()
		}(n)
	}
	
	wg.Wait()
	return results
}

// PrintFibonacciStats prints detailed statistics about Fibonacci calculations
func (fs *FibonacciSuite) PrintFibonacciStats(n int) {
	fmt.Printf("\n=== Fibonacci(%d) Statistics ===\n", n)
	
	// Calculate using different methods
	result := fs.FastDoublingFibonacci(n)
	
	fmt.Printf("Value: %s\n", result.String())
	fmt.Printf("Number of digits: %d\n", len(result.String()))
	
	if n > 0 {
		// Golden ratio approximation
		phi := (1.0 + math.Sqrt(5.0)) / 2.0
		approx := math.Pow(phi, float64(n)) / math.Sqrt(5.0)
		fmt.Printf("Golden ratio approximation: %.0f\n", approx)
		
		// Ratio to previous Fibonacci number (approaches golden ratio)
		if n > 1 {
			prev := fs.FastDoublingFibonacci(n - 1)
			ratio := new(big.Float).SetInt(result)
			ratio.Quo(ratio, new(big.Float).SetInt(prev))
			ratioFloat, _ := ratio.Float64()
			fmt.Printf("Ratio F(n)/F(n-1): %.10f\n", ratioFloat)
			fmt.Printf("Golden ratio: %.10f\n", phi)
			fmt.Printf("Difference from golden ratio: %.10f\n", math.Abs(ratioFloat-phi))
		}
	}
}

func main() {
	fmt.Println("Advanced Fibonacci Suite in Go")
	fmt.Println("=" + fmt.Sprintf("%50s", "").Replace(" ", "=", -1))
	
	fs := NewFibonacciSuite()
	
	// Test small values with all methods including naive recursive
	fmt.Println("\n1. Testing small values:")
	smallTests := []int{10, 15, 20}
	
	for _, n := range smallTests {
		fmt.Printf("\nFibonacci(%d) using naive recursion:\n", n)
		result := timeFunction("Naive Recursive", func() interface{} {
			return fs.RecursiveFibonacci(n)
		})
		fmt.Printf("Result: %s\n", result.(*big.Int).String())
	}
	
	// Benchmark larger values
	fmt.Println("\n2. Benchmarking larger values:")
	testValues := []int{30, 50, 100, 500, 1000}
	results := fs.BenchmarkAll(testValues)
	
	// Sort results by performance for summary
	sort.Slice(results, func(i, j int) bool {
		return results[i].Duration < results[j].Duration
	})
	
	// Demonstrate sequence generation
	fmt.Println("\n3. First 20 Fibonacci numbers:")
	sequence := fs.GenerateFibonacciSequence(20)
	for i, num := range sequence {
		fmt.Printf("F(%d) = %s\n", i, num.String())
	}
	
	// Demonstrate channel-based generation
	fmt.Println("\n4. Using Fibonacci channel (first 15):")
	count := 0
	for fib := range fs.FibonacciChannel(15) {
		fmt.Printf("F(%d) = %s\n", count, fib.String())
		count++
	}
	
	// Test related sequences
	fmt.Println("\n5. Related sequences:")
	fmt.Printf("Lucas(10): %s\n", fs.LucasNumbers(10).String())
	fmt.Printf("Tribonacci(10): %s\n", fs.TribonacciFibonacci(10).String())
	
	// Parallel computation demo
	fmt.Println("\n6. Parallel computation:")
	parallelValues := []int{100, 200, 300, 400, 500}
	start := time.Now()
	parallelResults := fs.ParallelFibonacci(parallelValues)
	parallelDuration := time.Since(start)
	
	fmt.Printf("Computed %d Fibonacci numbers in parallel: %v\n", len(parallelValues), parallelDuration)
	for _, n := range parallelValues {
		result := parallelResults[n].String()
		if len(result) > 30 {
			result = result[:30] + "..."
		}
		fmt.Printf("F(%d) = %s\n", n, result)
	}
	
	// Large number test with detailed statistics
	fmt.Println("\n7. Large number analysis:")
	largeN := 10000
	start = time.Now()
	largeResult := fs.FastDoublingFibonacci(largeN)
	largeDuration := time.Since(start)
	
	fmt.Printf("Fibonacci(%d) computed in %v\n", largeN, largeDuration)
	fmt.Printf("Result has %d digits\n", len(largeResult.String()))
	fmt.Printf("First 50 digits: %s...\n", largeResult.String()[:50])
	
	// Print detailed statistics for a medium-sized number
	fs.PrintFibonacciStats(100)
	
	// Performance comparison summary
	fmt.Println("\n8. Performance Summary:")
	fmt.Println("Method performance ranking (fastest to slowest):")
	methodTimes := make(map[string]time.Duration)
	methodCounts := make(map[string]int)
	
	for _, result := range results {
		if result.Duration > 0 && result.Error == nil {
			methodTimes[result.Method] += result.Duration
			methodCounts[result.Method]++
		}
	}
	
	type methodAvg struct {
		name string
		avg  time.Duration
	}
	
	var avgTimes []methodAvg
	for method, totalTime := range methodTimes {
		count := methodCounts[method]
		if count > 0 {
			avgTimes = append(avgTimes, methodAvg{
				name: method,
				avg:  totalTime / time.Duration(count),
			})
		}
	}
	
	sort.Slice(avgTimes, func(i, j int) bool {
		return avgTimes[i].avg < avgTimes[j].avg
	})
	
	for i, method := range avgTimes {
		fmt.Printf("%d. %s: %v average\n", i+1, method.name, method.avg)
	}
	
	fmt.Println("\nFibonacci suite demonstration completed!")
}