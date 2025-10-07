package main

import (
    "fmt"
    "math/rand"
    "time"
)

func quickSort(arr []int) []int {
    if len(arr) <= 1 {
        return arr
    }

    pivot := arr[len(arr)/2]
    left := []int{}
    right := []int{}
    middle := []int{}

    for _, num := range arr {
        if num < pivot {
            left = append(left, num)
        } else if num > pivot {
            right = append(right, num)
        } else {
            middle = append(middle, num)
        }
    }

    left = quickSort(left)
    right = quickSort(right)

    return append(append(left, middle...), right...)
}

func main() {
    rand.Seed(time.Now().UnixNano())

    numbers := make([]int, 20)
    for i := 0; i < 20; i++ {
        numbers[i] = rand.Intn(100) + 1
    }

    fmt.Println("Original array:", numbers)
    sorted := quickSort(numbers)
    fmt.Println("Sorted array:  ", sorted)
}