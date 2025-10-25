package main

import (
	"fmt"
	"math/rand"
	"sort"
	"sync"
	"time"
)

// ProfiledSortingAlgorithms provides comprehensive sorting implementations with profiling
type ProfiledSortingAlgorithms struct {
	comparisons int64
	swaps       int64
	mutex       sync.RWMutex
}

// NewProfiledSortingAlgorithms creates a new profiled sorting suite
func NewProfiledSortingAlgorithms() *ProfiledSortingAlgorithms {
	return &ProfiledSortingAlgorithms{}
}

// resetCounters resets performance counters
func (psa *ProfiledSortingAlgorithms) resetCounters() {
	psa.mutex.Lock()
	defer psa.mutex.Unlock()
	psa.comparisons = 0
	psa.swaps = 0
}

// compare compares two elements and increments counter
func (psa *ProfiledSortingAlgorithms) compare(a, b int) bool {
	psa.mutex.Lock()
	psa.comparisons++
	psa.mutex.Unlock()
	return a < b
}

// swap swaps two elements and increments counter
func (psa *ProfiledSortingAlgorithms) swap(arr []int, i, j int) {
	psa.mutex.Lock()
	psa.swaps++
	psa.mutex.Unlock()
	arr[i], arr[j] = arr[j], arr[i]
}

// getCounters returns current counter values thread-safely
func (psa *ProfiledSortingAlgorithms) getCounters() (int64, int64) {
	psa.mutex.RLock()
	defer psa.mutex.RUnlock()
	return psa.comparisons, psa.swaps
}

// BubbleSort implements bubble sort - O(n²) time, O(1) space
func (psa *ProfiledSortingAlgorithms) BubbleSort(arr []int) []int {
	result := make([]int, len(arr))
	copy(result, arr)
	n := len(result)
	
	for i := 0; i < n; i++ {
		swapped := false
		for j := 0; j < n-i-1; j++ {
			if !psa.compare(result[j], result[j+1]) {
				psa.swap(result, j, j+1)
				swapped = true
			}
		}
		if !swapped {
			break
		}
	}
	
	return result
}

// SelectionSort implements selection sort - O(n²) time, O(1) space
func (psa *ProfiledSortingAlgorithms) SelectionSort(arr []int) []int {
	result := make([]int, len(arr))
	copy(result, arr)
	n := len(result)
	
	for i := 0; i < n; i++ {
		minIdx := i
		for j := i + 1; j < n; j++ {
			if psa.compare(result[j], result[minIdx]) {
				minIdx = j
			}
		}
		if minIdx != i {
			psa.swap(result, i, minIdx)
		}
	}
	
	return result
}

// InsertionSort implements insertion sort - O(n²) time, O(1) space
func (psa *ProfiledSortingAlgorithms) InsertionSort(arr []int) []int {
	result := make([]int, len(arr))
	copy(result, arr)
	
	for i := 1; i < len(result); i++ {
		key := result[i]
		j := i - 1
		
		for j >= 0 && !psa.compare(result[j], key) {
			result[j+1] = result[j]
			j--
		}
		result[j+1] = key
	}
	
	return result
}

// MergeSort implements merge sort - O(n log n) time, O(n) space
func (psa *ProfiledSortingAlgorithms) MergeSort(arr []int) []int {
	if len(arr) <= 1 {
		return arr
	}
	
	mid := len(arr) / 2
	left := psa.MergeSort(arr[:mid])
	right := psa.MergeSort(arr[mid:])
	
	return psa.merge(left, right)
}

// merge helper function for merge sort
func (psa *ProfiledSortingAlgorithms) merge(left, right []int) []int {
	result := make([]int, 0, len(left)+len(right))
	i, j := 0, 0
	
	for i < len(left) && j < len(right) {
		if psa.compare(left[i], right[j]) {
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

// QuickSort implements quicksort - O(n log n) average, O(n²) worst case
func (psa *ProfiledSortingAlgorithms) QuickSort(arr []int) []int {
	result := make([]int, len(arr))
	copy(result, arr)
	psa.quickSortHelper(result, 0, len(result)-1)
	return result
}

// quickSortHelper recursive helper for quicksort
func (psa *ProfiledSortingAlgorithms) quickSortHelper(arr []int, low, high int) {
	if low < high {
		pi := psa.partition(arr, low, high)
		psa.quickSortHelper(arr, low, pi-1)
		psa.quickSortHelper(arr, pi+1, high)
	}
}

// partition function for quicksort
func (psa *ProfiledSortingAlgorithms) partition(arr []int, low, high int) int {
	pivot := arr[high]
	i := low - 1
	
	for j := low; j < high; j++ {
		if psa.compare(arr[j], pivot) || arr[j] == pivot {
			i++
			psa.swap(arr, i, j)
		}
	}
	
	psa.swap(arr, i+1, high)
	return i + 1
}

// HeapSort implements heap sort - O(n log n) time, O(1) space
func (psa *ProfiledSortingAlgorithms) HeapSort(arr []int) []int {
	result := make([]int, len(arr))
	copy(result, arr)
	n := len(result)
	
	// Build max heap
	for i := n/2 - 1; i >= 0; i-- {
		psa.heapify(result, n, i)
	}
	
	// Extract elements from heap one by one
	for i := n - 1; i > 0; i-- {
		psa.swap(result, 0, i)
		psa.heapify(result, i, 0)
	}
	
	return result
}

// heapify helper for heap sort
func (psa *ProfiledSortingAlgorithms) heapify(arr []int, n, i int) {
	largest := i
	left := 2*i + 1
	right := 2*i + 2
	
	if left < n && !psa.compare(arr[left], arr[largest]) {
		largest = left
	}
	
	if right < n && !psa.compare(arr[right], arr[largest]) {
		largest = right
	}
	
	if largest != i {
		psa.swap(arr, i, largest)
		psa.heapify(arr, n, largest)
	}
}

// CountingSort implements counting sort - O(n + k) time where k is range
func (psa *ProfiledSortingAlgorithms) CountingSort(arr []int) []int {
	if len(arr) == 0 {
		return arr
	}
	
	// Find min and max
	minVal, maxVal := arr[0], arr[0]
	for _, val := range arr {
		if val < minVal {
			minVal = val
		}
		if val > maxVal {
			maxVal = val
		}
	}
	
	rangeVal := maxVal - minVal + 1
	count := make([]int, rangeVal)
	output := make([]int, len(arr))
	
	// Count occurrences
	for _, val := range arr {
		count[val-minVal]++
	}
	
	// Calculate cumulative count
	for i := 1; i < rangeVal; i++ {
		count[i] += count[i-1]
	}
	
	// Build output array
	for i := len(arr) - 1; i >= 0; i-- {
		output[count[arr[i]-minVal]-1] = arr[i]
		count[arr[i]-minVal]--
	}
	
	return output
}

// RadixSort implements radix sort - O(d * (n + k)) time
func (psa *ProfiledSortingAlgorithms) RadixSort(arr []int) []int {
	if len(arr) == 0 {
		return arr
	}
	
	// Separate positive and negative numbers
	positive := make([]int, 0)
	negative := make([]int, 0)
	
	for _, val := range arr {
		if val >= 0 {
			positive = append(positive, val)
		} else {
			negative = append(negative, -val)
		}
	}
	
	// Sort positive numbers
	if len(positive) > 0 {
		positive = psa.radixSortPositive(positive)
	}
	
	// Sort negative numbers and reverse
	if len(negative) > 0 {
		negative = psa.radixSortPositive(negative)
		// Reverse and negate
		for i := 0; i < len(negative)/2; i++ {
			negative[i], negative[len(negative)-1-i] = negative[len(negative)-1-i], negative[i]
		}
		for i := range negative {
			negative[i] = -negative[i]
		}
	}
	
	// Combine results
	result := make([]int, 0, len(arr))
	result = append(result, negative...)
	result = append(result, positive...)
	
	return result
}

// radixSortPositive sorts positive numbers using radix sort
func (psa *ProfiledSortingAlgorithms) radixSortPositive(arr []int) []int {
	if len(arr) == 0 {
		return arr
	}
	
	result := make([]int, len(arr))
	copy(result, arr)
	
	// Find maximum number to know number of digits
	maxNum := result[0]
	for _, val := range result {
		if val > maxNum {
			maxNum = val
		}
	}
	
	// Do counting sort for every digit
	exp := 1
	for maxNum/exp > 0 {
		result = psa.countingSortByDigit(result, exp)
		exp *= 10
	}
	
	return result
}

// countingSortByDigit counting sort by specific digit for radix sort
func (psa *ProfiledSortingAlgorithms) countingSortByDigit(arr []int, exp int) []int {
	n := len(arr)
	output := make([]int, n)
	count := make([]int, 10)
	
	// Count occurrences of each digit
	for _, val := range arr {
		index := (val / exp) % 10
		count[index]++
	}
	
	// Calculate cumulative count
	for i := 1; i < 10; i++ {
		count[i] += count[i-1]
	}
	
	// Build output array
	for i := n - 1; i >= 0; i-- {
		index := (arr[i] / exp) % 10
		output[count[index]-1] = arr[i]
		count[index]--
	}
	
	return output
}

// ShellSort implements shell sort - O(n^1.5) average time
func (psa *ProfiledSortingAlgorithms) ShellSort(arr []int) []int {
	result := make([]int, len(arr))
	copy(result, arr)
	n := len(result)
	
	// Start with a big gap, then reduce the gap
	gap := n / 2
	for gap > 0 {
		for i := gap; i < n; i++ {
			temp := result[i]
			j := i
			
			for j >= gap && !psa.compare(result[j-gap], temp) {
				result[j] = result[j-gap]
				j -= gap
			}
			result[j] = temp
		}
		gap /= 2
	}
	
	return result
}

// CocktailSort implements cocktail shaker sort - O(n²) time
func (psa *ProfiledSortingAlgorithms) CocktailSort(arr []int) []int {
	result := make([]int, len(arr))
	copy(result, arr)
	n := len(result)
	
	swapped := true
	start := 0
	end := n - 1
	
	for swapped {
		swapped = false
		
		// Forward pass
		for i := start; i < end; i++ {
			if !psa.compare(result[i], result[i+1]) {
				psa.swap(result, i, i+1)
				swapped = true
			}
		}
		
		if !swapped {
			break
		}
		
		end--
		swapped = false
		
		// Backward pass
		for i := end - 1; i >= start; i-- {
			if !psa.compare(result[i], result[i+1]) {
				psa.swap(result, i, i+1)
				swapped = true
			}
		}
		
		start++
	}
	
	return result
}

// GnomeSort implements gnome sort - O(n²) time
func (psa *ProfiledSortingAlgorithms) GnomeSort(arr []int) []int {
	result := make([]int, len(arr))
	copy(result, arr)
	n := len(result)
	
	index := 0
	for index < n {
		if index == 0 {
			index++
		}
		if psa.compare(result[index-1], result[index]) || result[index-1] == result[index] {
			index++
		} else {
			psa.swap(result, index-1, index)
			index--
		}
	}
	
	return result
}

// BenchmarkResult stores benchmark information for sorting algorithms
type SortBenchmarkResult struct {
	Algorithm    string
	ArraySize    int
	Duration     time.Duration
	Comparisons  int64
	Swaps        int64
	IsCorrect    bool
	TestCaseName string
}

// BenchmarkAlgorithm benchmarks a single sorting algorithm
func (psa *ProfiledSortingAlgorithms) BenchmarkAlgorithm(
	algorithm func([]int) []int,
	arr []int,
	name string,
	testCaseName string,
) SortBenchmarkResult {
	psa.resetCounters()
	expected := make([]int, len(arr))
	copy(expected, arr)
	sort.Ints(expected)
	
	start := time.Now()
	result := algorithm(arr)
	duration := time.Since(start)
	
	comparisons, swaps := psa.getCounters()
	
	// Check correctness
	isCorrect := len(result) == len(expected)
	if isCorrect {
		for i := range result {
			if result[i] != expected[i] {
				isCorrect = false
				break
			}
		}
	}
	
	return SortBenchmarkResult{
		Algorithm:    name,
		ArraySize:    len(arr),
		Duration:     duration,
		Comparisons:  comparisons,
		Swaps:        swaps,
		IsCorrect:    isCorrect,
		TestCaseName: testCaseName,
	}
}

// GenerateTestArrays generates various test arrays for benchmarking
func GenerateTestArrays() []struct {
	name string
	data []int
} {
	rand.Seed(time.Now().UnixNano())
	
	testArrays := []struct {
		name string
		data []int
	}{
		// Small random array
		{
			name: "Small Random",
			data: func() []int {
				arr := make([]int, 50)
				for i := range arr {
					arr[i] = rand.Intn(100) + 1
				}
				return arr
			}(),
		},
		
		// Already sorted
		{
			name: "Already Sorted",
			data: func() []int {
				arr := make([]int, 50)
				for i := range arr {
					arr[i] = i + 1
				}
				return arr
			}(),
		},
		
		// Reverse sorted
		{
			name: "Reverse Sorted",
			data: func() []int {
				arr := make([]int, 50)
				for i := range arr {
					arr[i] = 50 - i
				}
				return arr
			}(),
		},
		
		// Nearly sorted
		{
			name: "Nearly Sorted",
			data: func() []int {
				arr := make([]int, 50)
				for i := range arr {
					arr[i] = i + 1
				}
				// Make a few swaps
				for i := 0; i < 5; i++ {
					j := rand.Intn(50)
					k := rand.Intn(50)
					arr[j], arr[k] = arr[k], arr[j]
				}
				return arr
			}(),
		},
		
		// With duplicates
		{
			name: "With Duplicates",
			data: func() []int {
				arr := make([]int, 45)
				for i := 0; i < 15; i++ {
					arr[i*3] = 1
					arr[i*3+1] = 2
					arr[i*3+2] = 3
				}
				return arr
			}(),
		},
		
		// All same elements
		{
			name: "All Same",
			data: func() []int {
				arr := make([]int, 30)
				for i := range arr {
					arr[i] = 5
				}
				return arr
			}(),
		},
		
		// Large random (for efficient algorithms only)
		{
			name: "Large Random",
			data: func() []int {
				arr := make([]int, 2000)
				for i := range arr {
					arr[i] = rand.Intn(5000) + 1
				}
				return arr
			}(),
		},
	}
	
	return testArrays
}

// BenchmarkAll benchmarks all sorting algorithms on various test cases
func (psa *ProfiledSortingAlgorithms) BenchmarkAll(testArrays []struct {
	name string
	data []int
}) []SortBenchmarkResult {
	
	algorithms := []struct {
		name string
		fn   func([]int) []int
	}{
		{"Bubble Sort", psa.BubbleSort},
		{"Selection Sort", psa.SelectionSort},
		{"Insertion Sort", psa.InsertionSort},
		{"Merge Sort", psa.MergeSort},
		{"Quick Sort", psa.QuickSort},
		{"Heap Sort", psa.HeapSort},
		{"Counting Sort", psa.CountingSort},
		{"Radix Sort", psa.RadixSort},
		{"Shell Sort", psa.ShellSort},
		{"Cocktail Sort", psa.CocktailSort},
		{"Gnome Sort", psa.GnomeSort},
	}
	
	var allResults []SortBenchmarkResult
	
	fmt.Println("Comprehensive Sorting Algorithm Benchmarks")
	fmt.Println("=" + fmt.Sprintf("%80s", "").Replace(" ", "=", -1))
	
	for _, testCase := range testArrays {
		fmt.Printf("\nTest Case: %s (Size: %d)\n", testCase.name, len(testCase.data))
		fmt.Println("-" + fmt.Sprintf("%60s", "").Replace(" ", "-", -1))
		fmt.Printf("%-15s %-12s %-12s %-10s %-8s\n", "Algorithm", "Time (ms)", "Comparisons", "Swaps", "Correct")
		fmt.Println("-" + fmt.Sprintf("%60s", "").Replace(" ", "-", -1))
		
		for _, algo := range algorithms {
			// Skip slow algorithms for large arrays
			if len(testCase.data) > 1000 && 
			   (algo.name == "Bubble Sort" || algo.name == "Selection Sort" || 
			    algo.name == "Cocktail Sort" || algo.name == "Gnome Sort") {
				fmt.Printf("%-15s %-12s %-12s %-10s %-8s\n", 
					algo.name, "SKIPPED", "(too slow)", "", "")
				continue
			}
			
			result := psa.BenchmarkAlgorithm(algo.fn, testCase.data, algo.name, testCase.name)
			allResults = append(allResults, result)
			
			timeMs := float64(result.Duration.Nanoseconds()) / float64(time.Millisecond)
			correctStr := "✓"
			if !result.IsCorrect {
				correctStr = "✗"
			}
			
			fmt.Printf("%-15s %-12.3f %-12d %-10d %-8s\n",
				result.Algorithm, timeMs, result.Comparisons, result.Swaps, correctStr)
		}
	}
	
	return allResults
}

// PrintPerformanceSummary prints a detailed performance summary
func PrintPerformanceSummary(results []SortBenchmarkResult) {
	fmt.Println("\n" + "=" + fmt.Sprintf("%50s", "").Replace(" ", "=", -1))
	fmt.Println("PERFORMANCE SUMMARY")
	fmt.Println("=" + fmt.Sprintf("%50s", "").Replace(" ", "=", -1))
	
	// Group results by algorithm
	algorithmStats := make(map[string]struct {
		totalTime     time.Duration
		totalComps    int64
		totalSwaps    int64
		testCount     int
		correctCount  int
	})
	
	for _, result := range results {
		if result.Duration > 0 { // Only include non-skipped results
			stats := algorithmStats[result.Algorithm]
			stats.totalTime += result.Duration
			stats.totalComps += result.Comparisons
			stats.totalSwaps += result.Swaps
			stats.testCount++
			if result.IsCorrect {
				stats.correctCount++
			}
			algorithmStats[result.Algorithm] = stats
		}
	}
	
	// Create sorted list of algorithms by average time
	type algorithmSummary struct {
		name        string
		avgTime     time.Duration
		avgComps    int64
		avgSwaps    int64
		correctness float64
	}
	
	var summaries []algorithmSummary
	for name, stats := range algorithmStats {
		if stats.testCount > 0 {
			summaries = append(summaries, algorithmSummary{
				name:        name,
				avgTime:     stats.totalTime / time.Duration(stats.testCount),
				avgComps:    stats.totalComps / int64(stats.testCount),
				avgSwaps:    stats.totalSwaps / int64(stats.testCount),
				correctness: float64(stats.correctCount) / float64(stats.testCount) * 100,
			})
		}
	}
	
	sort.Slice(summaries, func(i, j int) bool {
		return summaries[i].avgTime < summaries[j].avgTime
	})
	
	fmt.Printf("\n%-15s %-12s %-12s %-10s %-12s\n", 
		"Algorithm", "Avg Time(ms)", "Avg Comps", "Avg Swaps", "Correctness%")
	fmt.Println("-" + fmt.Sprintf("%65s", "").Replace(" ", "-", -1))
	
	for _, summary := range summaries {
		avgTimeMs := float64(summary.avgTime.Nanoseconds()) / float64(time.Millisecond)
		fmt.Printf("%-15s %-12.3f %-12d %-10d %-12.1f\n",
			summary.name, avgTimeMs, summary.avgComps, summary.avgSwaps, summary.correctness)
	}
	
	// Find best performers in different categories
	fmt.Println("\nBest Performers:")
	if len(summaries) > 0 {
		fmt.Printf("Fastest: %s (%.3f ms avg)\n", 
			summaries[0].name, 
			float64(summaries[0].avgTime.Nanoseconds())/float64(time.Millisecond))
		
		// Find algorithm with fewest comparisons
		minComps := summaries[0]
		for _, s := range summaries[1:] {
			if s.avgComps < minComps.avgComps {
				minComps = s
			}
		}
		fmt.Printf("Fewest Comparisons: %s (%d avg)\n", minComps.name, minComps.avgComps)
		
		// Find algorithm with fewest swaps
		minSwaps := summaries[0]
		for _, s := range summaries[1:] {
			if s.avgSwaps < minSwaps.avgSwaps {
				minSwaps = s
			}
		}
		fmt.Printf("Fewest Swaps: %s (%d avg)\n", minSwaps.name, minSwaps.avgSwaps)
	}
}

// ParallelSortBenchmark demonstrates parallel sorting using goroutines
func ParallelSortBenchmark(data []int, numWorkers int) {
	fmt.Printf("\nParallel Sorting Benchmark (%d workers):\n", numWorkers)
	fmt.Println("-" + fmt.Sprintf("%40s", "").Replace(" ", "-", -1))
	
	// Divide data into chunks
	chunkSize := len(data) / numWorkers
	if chunkSize == 0 {
		chunkSize = 1
		numWorkers = len(data)
	}
	
	var wg sync.WaitGroup
	chunks := make([][]int, numWorkers)
	
	start := time.Now()
	
	// Create and sort chunks in parallel
	for i := 0; i < numWorkers; i++ {
		startIdx := i * chunkSize
		endIdx := startIdx + chunkSize
		if i == numWorkers-1 {
			endIdx = len(data)
		}
		
		chunks[i] = make([]int, endIdx-startIdx)
		copy(chunks[i], data[startIdx:endIdx])
		
		wg.Add(1)
		go func(chunk []int, workerID int) {
			defer wg.Done()
			sort.Ints(chunk) // Use Go's built-in sort for parallel chunks
			fmt.Printf("Worker %d sorted %d elements\n", workerID, len(chunk))
		}(chunks[i], i)
	}
	
	wg.Wait()
	
	// Merge sorted chunks
	result := chunks[0]
	for i := 1; i < numWorkers; i++ {
		result = mergeSorted(result, chunks[i])
	}
	
	duration := time.Since(start)
	
	fmt.Printf("Parallel sort completed in %v\n", duration)
	fmt.Printf("Sorted %d elements using %d workers\n", len(result), numWorkers)
	
	// Verify result is sorted
	isSorted := true
	for i := 1; i < len(result); i++ {
		if result[i] < result[i-1] {
			isSorted = false
			break
		}
	}
	fmt.Printf("Result is correctly sorted: %v\n", isSorted)
}

// mergeSorted merges two sorted slices
func mergeSorted(a, b []int) []int {
	result := make([]int, 0, len(a)+len(b))
	i, j := 0, 0
	
	for i < len(a) && j < len(b) {
		if a[i] <= b[j] {
			result = append(result, a[i])
			i++
		} else {
			result = append(result, b[j])
			j++
		}
	}
	
	result = append(result, a[i:]...)
	result = append(result, b[j:]...)
	return result
}

func main() {
	fmt.Println("Advanced Sorting Algorithms Suite in Go")
	fmt.Println("=" + fmt.Sprintf("%50s", "").Replace(" ", "=", -1))
	
	psa := NewProfiledSortingAlgorithms()
	
	// Generate test cases
	testArrays := GenerateTestArrays()
	
	// Run comprehensive benchmarks
	results := psa.BenchmarkAll(testArrays)
	
	// Print performance summary
	PrintPerformanceSummary(results)
	
	// Demonstrate special features
	fmt.Println("\nSpecial Demonstrations:")
	fmt.Println("=" + fmt.Sprintf("%30s", "").Replace(" ", "=", -1))
	
	// Parallel sorting demonstration
	largeData := make([]int, 10000)
	for i := range largeData {
		largeData[i] = rand.Intn(50000)
	}
	
	ParallelSortBenchmark(largeData, 4)
	
	// Test with mixed positive/negative numbers for radix sort
	fmt.Println("\nMixed positive/negative numbers test (Radix Sort):")
	mixedArray := []int{-5, 3, -2, 8, -1, 4, 0, -7, 6, 2, -3, 9}
	fmt.Printf("Original: %v\n", mixedArray)
	sortedMixed := psa.RadixSort(mixedArray)
	fmt.Printf("Sorted:   %v\n", sortedMixed)
	
	// Test stability demonstration
	fmt.Println("\nStability test (custom objects would be needed for full test):")
	stabilityArray := []int{3, 1, 4, 1, 5, 9, 2, 6, 5, 3}
	fmt.Printf("Original: %v\n", stabilityArray)
	
	// Compare stable vs unstable sorts
	fmt.Printf("Merge Sort (stable):     %v\n", psa.MergeSort(stabilityArray))
	psa.resetCounters()
	fmt.Printf("Quick Sort (unstable):   %v\n", psa.QuickSort(stabilityArray))
	
	fmt.Println("\nSorting algorithms demonstration completed!")
}