package main

import "fmt"

// Interface test
type Shape interface {
    Area() float64
}

type Rectangle struct {
    Width, Height float64
}

func (r Rectangle) Area() float64 {
    return r.Width * r.Height
}

// Goroutines and channels
func worker(ch chan int) {
    ch <- 42
}

func main() {
    // Interface usage
    var s Shape = Rectangle{Width: 10, Height: 5}
    fmt.Println("Area:", s.Area())
    
    // Channel usage
    ch := make(chan int)
    go worker(ch)
    result := <-ch
    fmt.Println("Channel result:", result)
}