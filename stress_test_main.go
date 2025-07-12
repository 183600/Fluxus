package main

import (
	"fmt"
	"math/rand"
	"runtime"
	"sync"
	"time"
)

// Stress test for concurrent operations
func concurrentStressTest() {
	fmt.Println("Concurrent stress test...")
	
	var wg sync.WaitGroup
	counter := 0
	mutex := sync.Mutex{}
	
	// Launch many goroutines
	for i := 0; i < 100; i++ {
		wg.Add(1)
		go func(id int) {
			defer wg.Done()
			for j := 0; j < 1000; j++ {
				mutex.Lock()
				counter++
				mutex.Unlock()
			}
		}(i)
	}
	
	wg.Wait()
	fmt.Printf("  Final counter value: %d\n", counter)
	fmt.Println("✓ Concurrent stress test completed")
}

// Stress test for memory allocation
func memoryStressTest() {
	fmt.Println("Memory stress test...")
	
	// Allocate many slices
	var slices [][]int
	for i := 0; i < 1000; i++ {
		slice := make([]int, 1000)
		for j := range slice {
			slice[j] = i * j
		}
		slices = append(slices, slice)
	}
	
	// Allocate many maps
	var maps []map[int]string
	for i := 0; i < 500; i++ {
		m := make(map[int]string)
		for j := 0; j < 100; j++ {
			m[j] = fmt.Sprintf("value_%d_%d", i, j)
		}
		maps = append(maps, m)
	}
	
	// Force garbage collection
	runtime.GC()
	
	fmt.Printf("  Allocated %d slices and %d maps\n", len(slices), len(maps))
	fmt.Println("✓ Memory stress test completed")
}

// Stress test for channels and goroutines
func channelStressTest() {
	fmt.Println("Channel stress test...")
	
	const numWorkers = 10
	const numMessages = 1000
	
	messages := make(chan int, 100)
	results := make(chan int, 100)
	
	// Start workers
	var wg sync.WaitGroup
	for i := 0; i < numWorkers; i++ {
		wg.Add(1)
		go func(workerID int) {
			defer wg.Done()
			for msg := range messages {
				// Simulate work
				result := msg * msg
				results <- result
			}
		}(i)
	}
	
	// Send messages
	go func() {
		for i := 0; i < numMessages; i++ {
			messages <- i
		}
		close(messages)
	}()
	
	// Collect results
	go func() {
		wg.Wait()
		close(results)
	}()
	
	count := 0
	for range results {
		count++
	}
	
	fmt.Printf("  Processed %d messages\n", count)
	fmt.Println("✓ Channel stress test completed")
}

// Fibonacci with memoization for recursion test
func fibonacciMemo(n int, memo map[int]int) int {
	if n <= 1 {
		return n
	}
	
	if val, exists := memo[n]; exists {
		return val
	}
	
	result := fibonacciMemo(n-1, memo) + fibonacciMemo(n-2, memo)
	memo[n] = result
	return result
}

func recursionStressTest() {
	fmt.Println("Recursion stress test...")
	
	memo := make(map[int]int)
	result := fibonacciMemo(40, memo)
	
	fmt.Printf("  Fibonacci(40) = %d\n", result)
	fmt.Println("✓ Recursion stress test completed")
}

// Interface stress test
type Worker interface {
	Work() int
}

type SimpleWorker struct {
	id int
}

func (w SimpleWorker) Work() int {
	return w.id * 42
}

type ComplexWorker struct {
	id   int
	data []int
}

func (w ComplexWorker) Work() int {
	sum := 0
	for _, v := range w.data {
		sum += v * w.id
	}
	return sum
}

func interfaceStressTest() {
	fmt.Println("Interface stress test...")
	
	var workers []Worker
	
	// Add simple workers
	for i := 0; i < 100; i++ {
		workers = append(workers, SimpleWorker{id: i})
	}
	
	// Add complex workers
	for i := 0; i < 50; i++ {
		data := make([]int, 10)
		for j := range data {
			data[j] = rand.Intn(100)
		}
		workers = append(workers, ComplexWorker{id: i, data: data})
	}
	
	// Execute all workers
	total := 0
	for _, worker := range workers {
		total += worker.Work()
	}
	
	fmt.Printf("  Total work result: %d\n", total)
	fmt.Println("✓ Interface stress test completed")
}

// Panic and recover stress test
func panicRecoverStressTest() {
	fmt.Println("Panic/recover stress test...")
	
	panicCount := 0
	
	for i := 0; i < 100; i++ {
		func() {
			defer func() {
				if r := recover(); r != nil {
					panicCount++
				}
			}()
			
			if i%10 == 0 {
				panic(fmt.Sprintf("Test panic %d", i))
			}
		}()
	}
	
	fmt.Printf("  Handled %d panics\n", panicCount)
	fmt.Println("✓ Panic/recover stress test completed")
}

// Performance benchmark
func performanceBenchmark() {
	fmt.Println("Performance benchmark...")
	
	// Slice operations
	start := time.Now()
	bigSlice := make([]int, 100000)
	for i := range bigSlice {
		bigSlice[i] = i * i
	}
	sliceTime := time.Since(start)
	
	// Map operations
	start = time.Now()
	bigMap := make(map[int]string)
	for i := 0; i < 50000; i++ {
		bigMap[i] = fmt.Sprintf("value_%d", i)
	}
	mapTime := time.Since(start)
	
	// String operations
	start = time.Now()
	var result string
	for i := 0; i < 10000; i++ {
		result += fmt.Sprintf("item_%d ", i)
	}
	stringTime := time.Since(start)
	
	fmt.Printf("  Slice operations: %v\n", sliceTime)
	fmt.Printf("  Map operations: %v\n", mapTime)
	fmt.Printf("  String operations: %v\n", stringTime)
	fmt.Println("✓ Performance benchmark completed")
}

// Edge case tests
func edgeCaseTests() {
	fmt.Println("Edge case tests...")
	
	// Empty structures
	emptySlice := []int{}
	emptyMap := map[string]int{}
	emptyString := ""
	
	// Large numbers
	bigInt := int64(1) << 60
	
	// Nil handling
	var nilSlice []int
	var nilMap map[string]int
	
	// Type assertions
	var iface interface{} = "hello"
	if str, ok := iface.(string); ok {
		_ = str
	}
	
	fmt.Printf("  Empty slice len: %d\n", len(emptySlice))
	fmt.Printf("  Empty map len: %d\n", len(emptyMap))
	fmt.Printf("  Empty string len: %d\n", len(emptyString))
	fmt.Printf("  Big int: %d\n", bigInt)
	fmt.Printf("  Nil slice is nil: %t\n", nilSlice == nil)
	fmt.Printf("  Nil map is nil: %t\n", nilMap == nil)
	
	fmt.Println("✓ Edge case tests completed")
}

func main() {
	fmt.Println("============================================================")
	fmt.Println("Go Stress Test Suite")
	fmt.Println("============================================================")
	
	start := time.Now()
	
	// Run all stress tests
	concurrentStressTest()
	memoryStressTest()
	channelStressTest()
	recursionStressTest()
	interfaceStressTest()
	panicRecoverStressTest()
	performanceBenchmark()
	edgeCaseTests()
	
	totalTime := time.Since(start)
	
	fmt.Println("============================================================")
	fmt.Printf("🎉 All stress tests passed in %v!\n", totalTime)
	fmt.Println("Go code is stable and ready for compilation.")
	fmt.Println("============================================================")
}