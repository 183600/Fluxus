package main

import "fmt"

func factorial(n int) int {
    if n <= 1 {
        return 1
    }
    return n * factorial(n-1)
}

func primeCheck(n int) bool {
    if n < 2 {
        return false
    }
    
    for i := 2; i*i <= n; i++ {
        if n%i == 0 {
            return false
        }
    }
    
    return true
}

func gcd(a, b int) int {
    for b != 0 {
        temp := b
        b = a % b
        a = temp
    }
    return a
}

func bubbleSort(arr []int) []int {
    n := len(arr)
    
    for i := 0; i < n; i++ {
        for j := 0; j < n-i-1; j++ {
            if arr[j] > arr[j+1] {
                temp := arr[j]
                arr[j] = arr[j+1]
                arr[j+1] = temp
            }
        }
    }
    
    return arr
}

func binarySearch(arr []int, target int) int {
    left := 0
    right := len(arr) - 1
    
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

func printArray(arr []int) {
    fmt.Printf("[")
    for i, v := range arr {
        if i > 0 {
            fmt.Printf(" ")
        }
        fmt.Printf("%d", v)
    }
    fmt.Printf("]")
}

func main() {
    fmt.Println("=== Mathematical Functions ===")
    fmt.Printf("Factorial of 5: %d\n", factorial(5))
    fmt.Printf("Factorial of 6: %d\n", factorial(6))
    
    fmt.Printf("Prime check for 17: %t\n", primeCheck(17))
    fmt.Printf("Prime check for 18: %t\n", primeCheck(18))
    
    fmt.Printf("GCD of 48 and 18: %d\n", gcd(48, 18))
    fmt.Printf("GCD of 56 and 42: %d\n", gcd(56, 42))
    
    fmt.Println("\n=== Sorting and Searching ===")
    testArray := []int{64, 34, 25, 12, 22, 11, 90}
    fmt.Print("Original array: ")
    printArray(testArray)
    fmt.Println()
    
    // Make a copy for sorting
    sortedArray := make([]int, len(testArray))
    copy(sortedArray, testArray)
    sortedArray = bubbleSort(sortedArray)
    
    fmt.Print("Sorted array: ")
    printArray(sortedArray)
    fmt.Println()
    
    fmt.Printf("Binary search for 22: %d\n", binarySearch(sortedArray, 22))
    fmt.Printf("Binary search for 50: %d\n", binarySearch(sortedArray, 50))
    
    fmt.Println("\n=== All tests completed! ===")
}