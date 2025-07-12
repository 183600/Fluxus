package main

import (
	"fmt"
	"runtime"
	"time"
)

func main() {
	fmt.Println("Go Memory and Performance Analysis")
	fmt.Println("==================================================")
	
	// Memory stats
	var m1, m2 runtime.MemStats
	runtime.ReadMemStats(&m1)
	fmt.Printf("Initial memory: %d KB\n", m1.Alloc/1024)
	
	// Allocate memory
	var slices [][]int
	for i := 0; i < 1000; i++ {
		slice := make([]int, 1000)
		for j := range slice {
			slice[j] = i * j
		}
		slices = append(slices, slice)
	}
	
	runtime.ReadMemStats(&m2)
	fmt.Printf("Peak memory: %d KB\n", m2.Alloc/1024)
	fmt.Printf("Memory increase: %d KB\n", (m2.Alloc-m1.Alloc)/1024)
	
	// CPU performance test
	start := time.Now()
	sum := 0
	for i := 0; i < 10000000; i++ {
		sum += i * i
	}
	duration := time.Since(start)
	
	fmt.Printf("CPU test duration: %v\n", duration)
	fmt.Printf("Goroutines: %d\n", runtime.NumGoroutine())
	fmt.Printf("CPUs: %d\n", runtime.NumCPU())
	
	// Garbage collection test
	runtime.GC()
	var m3 runtime.MemStats
	runtime.ReadMemStats(&m3)
	fmt.Printf("Memory after GC: %d KB\n", m3.Alloc/1024)
	
	fmt.Println("✓ Performance verification completed")
}