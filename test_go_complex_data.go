package main

import "fmt"

func main() {
    // Complex nested data structure
    data := make(map[string]map[string][]int)
    
    // Initialize
    data["group1"] = make(map[string][]int)
    data["group1"]["set1"] = []int{1, 2, 3}
    data["group1"]["set2"] = []int{4, 5, 6}
    
    data["group2"] = make(map[string][]int)
    data["group2"]["set1"] = []int{7, 8, 9}
    data["group2"]["set2"] = []int{10, 11, 12}
    
    // Access and sum
    sum := 0
    for _, group := range data {
        for _, set := range group {
            for _, value := range set {
                sum += value
            }
        }
    }
    
    fmt.Println(sum)
}
