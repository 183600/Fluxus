package main

import (
	"fmt"
	"math/rand"
	"runtime"
	"sort"
	"sync"
	"time"
)

type SortAlgorithm func([]int)

func bubbleSort(arr []int) {
	n := len(arr)
	for i := 0; i < n-1; i++ {
		for j := 0; j < n-i-1; j++ {
			if arr[j] > arr[j+1] {
				arr[j], arr[j+1] = arr[j+1], arr[j]
			}
		}
	}
}

func quickSort(arr []int) {
	quickSortHelper(arr, 0, len(arr)-1)
}

func quickSortHelper(arr []int, low, high int) {
	if low < high {
		pi := partition(arr, low, high)
		quickSortHelper(arr, low, pi-1)
		quickSortHelper(arr, pi+1, high)
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

func mergeSort(arr []int) {
	if len(arr) <= 1 {
		return
	}
	
	mid := len(arr) / 2
	left := make([]int, mid)
	right := make([]int, len(arr)-mid)
	
	copy(left, arr[:mid])
	copy(right, arr[mid:])
	
	mergeSort(left)
	mergeSort(right)
	
	merge(arr, left, right)
}

func merge(arr, left, right []int) {
	i, j, k := 0, 0, 0
	
	for i < len(left) && j < len(right) {
		if left[i] <= right[j] {
			arr[k] = left[i]
			i++
		} else {
			arr[k] = right[j]
			j++
		}
		k++
	}
	
	for i < len(left) {
		arr[k] = left[i]
		i++
		k++
	}
	
	for j < len(right) {
		arr[k] = right[j]
		j++
		k++
	}
}

func heapSort(arr []int) {
	n := len(arr)
	
	for i := n/2 - 1; i >= 0; i-- {
		heapify(arr, n, i)
	}
	
	for i := n - 1; i > 0; i-- {
		arr[0], arr[i] = arr[i], arr[0]
		heapify(arr, i, 0)
	}
}

func heapify(arr []int, n, i int) {
	largest := i
	left := 2*i + 1
	right := 2*i + 2
	
	if left < n && arr[left] > arr[largest] {
		largest = left
	}
	
	if right < n && arr[right] > arr[largest] {
		largest = right
	}
	
	if largest != i {
		arr[i], arr[largest] = arr[largest], arr[i]
		heapify(arr, n, largest)
	}
}

func goSort(arr []int) {
	sort.Ints(arr)
}

func generateRandomArray(size int) []int {
	arr := make([]int, size)
	for i := range arr {
		arr[i] = rand.Intn(10000)
	}
	return arr
}

func copyArray(arr []int) []int {
	copy := make([]int, len(arr))
	for i, v := range arr {
		copy[i] = v
	}
	return copy
}

func benchmark(name string, sortFunc SortAlgorithm, arr []int) time.Duration {
	data := copyArray(arr)
	
	start := time.Now()
	sortFunc(data)
	duration := time.Since(start)
	
	fmt.Printf("%-12s: %v\n", name, duration)
	return duration
}

func parallelBenchmark(algorithms map[string]SortAlgorithm, sizes []int) {
	fmt.Println("Parallel Sorting Benchmark")
	fmt.Println("==========================")
	
	for _, size := range sizes {
		fmt.Printf("\nArray size: %d elements\n", size)
		fmt.Println("------------------------")
		
		originalArray := generateRandomArray(size)
		
		var wg sync.WaitGroup
		results := make(map[string]time.Duration)
		var mu sync.Mutex
		
		for name, sortFunc := range algorithms {
			wg.Add(1)
			go func(n string, sf SortAlgorithm) {
				defer wg.Done()
				data := copyArray(originalArray)
				start := time.Now()
				sf(data)
				duration := time.Since(start)
				
				mu.Lock()
				results[n] = duration
				mu.Unlock()
			}(name, sortFunc)
		}
		
		wg.Wait()
		
		type result struct {
			name     string
			duration time.Duration
		}
		
		var sortedResults []result
		for name, duration := range results {
			sortedResults = append(sortedResults, result{name, duration})
		}
		
		sort.Slice(sortedResults, func(i, j int) bool {
			return sortedResults[i].duration < sortedResults[j].duration
		})
		
		for i, r := range sortedResults {
			status := ""
			if i == 0 {
				status = " (fastest)"
			} else if i == len(sortedResults)-1 {
				status = " (slowest)"
			}
			fmt.Printf("%-12s: %v%s\n", r.name, r.duration, status)
		}
	}
}

func main() {
	rand.Seed(time.Now().UnixNano())
	
	fmt.Printf("Go Sorting Algorithm Benchmark\n")
	fmt.Printf("CPU cores: %d\n", runtime.NumCPU())
	fmt.Printf("Go version: %s\n\n", runtime.Version())
	
	algorithms := map[string]SortAlgorithm{
		"Bubble":    bubbleSort,
		"Quick":     quickSort,
		"Merge":     mergeSort,
		"Heap":      heapSort,
		"Go Built-in": goSort,
	}
	
	sizes := []int{1000, 5000, 10000}
	
	parallelBenchmark(algorithms, sizes)
	
	fmt.Println("\nMemory usage analysis:")
	var m runtime.MemStats
	runtime.ReadMemStats(&m)
	fmt.Printf("Allocated memory: %d KB\n", m.Alloc/1024)
	fmt.Printf("Total allocations: %d\n", m.TotalAlloc/1024)
	fmt.Printf("System memory: %d KB\n", m.Sys/1024)
}