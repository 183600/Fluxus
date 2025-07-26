package main

import (
	"fmt"
	"math/rand"
	"sort"
	"time"
)

type SortingAlgorithms struct {
	data []int
}

func NewSortingAlgorithms(size int) *SortingAlgorithms {
	rand.Seed(time.Now().UnixNano())
	data := make([]int, size)
	for i := range data {
		data[i] = rand.Intn(1000)
	}
	return &SortingAlgorithms{data: data}
}

func (s *SortingAlgorithms) BubbleSort() []int {
	arr := make([]int, len(s.data))
	copy(arr, s.data)
	
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

func (s *SortingAlgorithms) QuickSort(arr []int, low, high int) {
	if low < high {
		pi := s.partition(arr, low, high)
		s.QuickSort(arr, low, pi-1)
		s.QuickSort(arr, pi+1, high)
	}
}

func (s *SortingAlgorithms) partition(arr []int, low, high int) int {
	pivot := arr[high]
	i := low - 1

	for j := low; j <= high-1; j++ {
		if arr[j] < pivot {
			i++
			arr[i], arr[j] = arr[j], arr[i]
		}
	}
	arr[i+1], arr[high] = arr[high], arr[i+1]
	return i + 1
}

func (s *SortingAlgorithms) MergeSort(arr []int) []int {
	if len(arr) <= 1 {
		return arr
	}

	mid := len(arr) / 2
	left := s.MergeSort(arr[:mid])
	right := s.MergeSort(arr[mid:])

	return s.merge(left, right)
}

func (s *SortingAlgorithms) merge(left, right []int) []int {
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

func measureTime(name string, fn func()) {
	start := time.Now()
	fn()
	duration := time.Since(start)
	fmt.Printf("%s took: %v\n", name, duration)
}

func main() {
	fmt.Println("Advanced Sorting Algorithms Benchmark")
	fmt.Println("=====================================")

	sorter := NewSortingAlgorithms(1000)
	fmt.Printf("Original data (first 10): %v\n", sorter.data[:10])

	// Bubble Sort
	measureTime("Bubble Sort", func() {
		result := sorter.BubbleSort()
		fmt.Printf("Bubble sorted (first 10): %v\n", result[:10])
	})

	// Quick Sort
	measureTime("Quick Sort", func() {
		arr := make([]int, len(sorter.data))
		copy(arr, sorter.data)
		sorter.QuickSort(arr, 0, len(arr)-1)
		fmt.Printf("Quick sorted (first 10): %v\n", arr[:10])
	})

	// Merge Sort
	measureTime("Merge Sort", func() {
		result := sorter.MergeSort(sorter.data)
		fmt.Printf("Merge sorted (first 10): %v\n", result[:10])
	})

	// Built-in Sort
	measureTime("Built-in Sort", func() {
		arr := make([]int, len(sorter.data))
		copy(arr, sorter.data)
		sort.Ints(arr)
		fmt.Printf("Built-in sorted (first 10): %v\n", arr[:10])
	})

	fmt.Println("\nBenchmark completed!")
}