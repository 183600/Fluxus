package main

import (
	"fmt"
	"sort"
	"time"
)

func bubbleSort(arr []int) []int {
	n := len(arr)
	result := make([]int, len(arr))
	copy(result, arr)
	
	for i := 0; i < n; i++ {
		for j := 0; j < n-i-1; j++ {
			if result[j] > result[j+1] {
				result[j], result[j+1] = result[j+1], result[j]
			}
		}
	}
	return result
}

func quickSort(arr []int) []int {
	if len(arr) <= 1 {
		return arr
	}
	
	pivot := arr[len(arr)/2]
	var left, middle, right []int
	
	for _, v := range arr {
		if v < pivot {
			left = append(left, v)
		} else if v == pivot {
			middle = append(middle, v)
		} else {
			right = append(right, v)
		}
	}
	
	result := make([]int, 0)
	result = append(result, quickSort(left)...)
	result = append(result, middle...)
	result = append(result, quickSort(right)...)
	
	return result
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
	result := make([]int, 0)
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
	
	for i < len(left) {
		result = append(result, left[i])
		i++
	}
	
	for j < len(right) {
		result = append(result, right[j])
		j++
	}
	
	return result
}

func heapSort(arr []int) []int {
	result := make([]int, len(arr))
	copy(result, arr)
	n := len(result)
	
	// Build heap
	for i := n/2 - 1; i >= 0; i-- {
		heapify(result, n, i)
	}
	
	// Extract elements
	for i := n - 1; i > 0; i-- {
		result[0], result[i] = result[i], result[0]
		heapify(result, i, 0)
	}
	
	return result
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

func insertionSort(arr []int) []int {
	result := make([]int, len(arr))
	copy(result, arr)
	
	for i := 1; i < len(result); i++ {
		key := result[i]
		j := i - 1
		
		for j >= 0 && result[j] > key {
			result[j+1] = result[j]
			j--
		}
		result[j+1] = key
	}
	
	return result
}

func selectionSort(arr []int) []int {
	result := make([]int, len(arr))
	copy(result, arr)
	n := len(result)
	
	for i := 0; i < n; i++ {
		minIdx := i
		for j := i + 1; j < n; j++ {
			if result[j] < result[minIdx] {
				minIdx = j
			}
		}
		result[i], result[minIdx] = result[minIdx], result[i]
	}
	
	return result
}

func countingSort(arr []int) []int {
	if len(arr) == 0 {
		return arr
	}
	
	maxVal := arr[0]
	minVal := arr[0]
	
	for _, v := range arr {
		if v > maxVal {
			maxVal = v
		}
		if v < minVal {
			minVal = v
		}
	}
	
	rangeVal := maxVal - minVal + 1
	count := make([]int, rangeVal)
	output := make([]int, len(arr))
	
	for _, v := range arr {
		count[v-minVal]++
	}
	
	for i := 1; i < len(count); i++ {
		count[i] += count[i-1]
	}
	
	for i := len(arr) - 1; i >= 0; i-- {
		output[count[arr[i]-minVal]-1] = arr[i]
		count[arr[i]-minVal]--
	}
	
	return output
}

func radixSort(arr []int) []int {
	if len(arr) == 0 {
		return arr
	}
	
	result := make([]int, len(arr))
	copy(result, arr)
	
	maxVal := result[0]
	for _, v := range result {
		if v > maxVal {
			maxVal = v
		}
	}
	
	for exp := 1; maxVal/exp > 0; exp *= 10 {
		countingSortForRadix(result, exp)
	}
	
	return result
}

func countingSortForRadix(arr []int, exp int) {
	n := len(arr)
	output := make([]int, n)
	count := make([]int, 10)
	
	for i := 0; i < n; i++ {
		count[(arr[i]/exp)%10]++
	}
	
	for i := 1; i < 10; i++ {
		count[i] += count[i-1]
	}
	
	for i := n - 1; i >= 0; i-- {
		output[count[(arr[i]/exp)%10]-1] = arr[i]
		count[(arr[i]/exp)%10]--
	}
	
	for i := 0; i < n; i++ {
		arr[i] = output[i]
	}
}

func bucketSort(arr []int) []int {
	if len(arr) == 0 {
		return arr
	}
	
	bucketCount := len(arr)
	maxVal := arr[0]
	minVal := arr[0]
	
	for _, v := range arr {
		if v > maxVal {
			maxVal = v
		}
		if v < minVal {
			minVal = v
		}
	}
	
	buckets := make([][]int, bucketCount)
	
	for _, num := range arr {
		index := (num - minVal) * bucketCount / (maxVal - minVal + 1)
		if index == bucketCount {
			index = bucketCount - 1
		}
		buckets[index] = append(buckets[index], num)
	}
	
	for i := range buckets {
		buckets[i] = insertionSort(buckets[i])
	}
	
	result := make([]int, 0)
	for _, bucket := range buckets {
		result = append(result, bucket...)
	}
	
	return result
}

func testSortingAlgorithms() {
	testArrays := [][]int{
		{64, 34, 25, 12, 22, 11, 90},
		{5, 2, 4, 6, 1, 3},
		{1},
		{},
		{3, 3, 3, 3},
		{9, 8, 7, 6, 5, 4, 3, 2, 1},
	}
	
	algorithms := []struct {
		name string
		fn   func([]int) []int
	}{
		{"Bubble Sort", bubbleSort},
		{"Quick Sort", quickSort},
		{"Merge Sort", mergeSort},
		{"Heap Sort", heapSort},
		{"Insertion Sort", insertionSort},
		{"Selection Sort", selectionSort},
		{"Counting Sort", countingSort},
		{"Radix Sort", radixSort},
		{"Bucket Sort", bucketSort},
	}
	
	for _, alg := range algorithms {
		fmt.Printf("Testing %s:\n", alg.name)
		for i, arr := range testArrays {
			original := make([]int, len(arr))
			copy(original, arr)
			
			start := time.Now()
			sorted := alg.fn(arr)
			duration := time.Since(start)
			
			expected := make([]int, len(original))
			copy(expected, original)
			sort.Ints(expected)
			
			status := "PASS"
			if !equal(sorted, expected) {
				status = "FAIL"
			}
			
			fmt.Printf("  Test %d: %v -> %v [%s] (%v)\n", 
				i+1, original, sorted, status, duration)
		}
		fmt.Println()
	}
}

func equal(a, b []int) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func main() {
	fmt.Println("=== Go Sorting Algorithms Demo ===")
	testSortingAlgorithms()
}