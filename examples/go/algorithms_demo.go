package main

import "fmt"

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

func findMax(arr []int) int {
    if len(arr) == 0 {
        return 0
    }
    max := arr[0]
    for _, val := range arr {
        if val > max {
            max = val
        }
    }
    return max
}

func main() {
    fmt.Println("Algorithms and Data Structures Demo")
    
    // Test bubble sort
    arr1 := []int{64, 34, 25, 12, 22, 11, 90}
    fmt.Printf("Original array: %v\n", arr1)
    bubbleSort(arr1)
    fmt.Printf("Bubble sorted: %v\n", arr1)
    
    // Test quick sort
    arr2 := []int{10, 7, 8, 9, 1, 5}
    fmt.Printf("Original array: %v\n", arr2)
    quickSort(arr2, 0, len(arr2)-1)
    fmt.Printf("Quick sorted: %v\n", arr2)
    
    // Test binary search
    sortedArr := []int{2, 3, 4, 10, 40}
    target := 10
    result := binarySearch(sortedArr, target)
    if result != -1 {
        fmt.Printf("Element %d found at index %d\n", target, result)
    } else {
        fmt.Printf("Element %d not found\n", target)
    }
    
    // Test find max
    arr3 := []int{5, 2, 8, 1, 9, 3}
    maxVal := findMax(arr3)
    fmt.Printf("Maximum value in %v is %d\n", arr3, maxVal)
    
    // Test with different arrays
    testArrays := [][]int{
        {5, 2, 8, 1, 9},
        {100, 50, 75, 25},
        {1, 3, 2, 4, 5},
    }
    
    fmt.Println("Testing with multiple arrays:")
    for i, arr := range testArrays {
        original := make([]int, len(arr))
        copy(original, arr)
        
        bubbleSort(arr)
        fmt.Printf("Array %d: %v -> %v\n", i+1, original, arr)
    }
}