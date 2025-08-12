package main

func main() {
    // Simple test to check array/slice behavior
    arr := [3]int{1, 2, 3}
    slice := []int{4, 5, 6}
    
    // Print the arrays directly first
    println("Array:", arr[0], arr[1], arr[2])
    println("Slice:", slice[0], slice[1], slice[2])
    
    // Then test individual access
    a := arr[0]
    b := slice[1]
    println("Individual access:")
    println(a)
    println(b)
}