package main

func main() {
    slice := []int{1, 2, 3}
    sum := 0
    for _, v := range slice {
        sum += v
    }
    println(sum)
}
