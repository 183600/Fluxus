package main

import (
    "fmt"
    "math"
)

func main() {
    numbers := []float64{1.0, 2.0, 3.0, 4.0, 5.0}
    sum := 0.0

    for _, num := range numbers {
        sum += num
    }

    average := sum / float64(len(numbers))
    variance := 0.0

    for _, num := range numbers {
        variance += math.Pow(num-average, 2)
    }

    variance /= float64(len(numbers))
    stdDev := math.Sqrt(variance)

    fmt.Printf("Average: %.2f\n", average)
    fmt.Printf("Variance: %.2f\n", variance)
    fmt.Printf("Standard Deviation: %.2f\n", stdDev)
}