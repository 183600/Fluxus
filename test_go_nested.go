package main

import "fmt"

func main() {
    count := 0
    for i := 0; i < 10; i++ {
        for j := 0; j < 10; j++ {
            for k := 0; k < 10; k++ {
                count++
            }
        }
    }
    fmt.Println(count)
}
