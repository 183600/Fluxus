package main

import (
	"fmt"
	"sync"
	"time"
)

type WorkerPool struct {
	workers    int
	jobQueue   chan Job
	resultChan chan Result
	wg         sync.WaitGroup
}

type Job struct {
	ID   int
	Data int
}

type Result struct {
	JobID  int
	Result int
	Worker int
}

func NewWorkerPool(workers int, jobQueueSize int) *WorkerPool {
	return &WorkerPool{
		workers:    workers,
		jobQueue:   make(chan Job, jobQueueSize),
		resultChan: make(chan Result, jobQueueSize),
	}
}

func (wp *WorkerPool) Start() {
	for i := 0; i < wp.workers; i++ {
		wp.wg.Add(1)
		go wp.worker(i)
	}
}

func (wp *WorkerPool) worker(workerID int) {
	defer wp.wg.Done()
	for job := range wp.jobQueue {
		// Simulate work by calculating factorial
		result := wp.factorial(job.Data)
		wp.resultChan <- Result{
			JobID:  job.ID,
			Result: result,
			Worker: workerID,
		}
	}
}

func (wp *WorkerPool) factorial(n int) int {
	if n <= 1 {
		return 1
	}
	result := 1
	for i := 2; i <= n; i++ {
		result *= i
	}
	return result
}

func (wp *WorkerPool) AddJob(job Job) {
	wp.jobQueue <- job
}

func (wp *WorkerPool) Stop() {
	close(wp.jobQueue)
	wp.wg.Wait()
	close(wp.resultChan)
}

func (wp *WorkerPool) GetResults() <-chan Result {
	return wp.resultChan
}

func main() {
	fmt.Println("Advanced Worker Pool with Factorial Calculation")
	fmt.Println("==============================================")

	// Create worker pool with 5 workers
	pool := NewWorkerPool(5, 100)
	pool.Start()

	// Add jobs
	numJobs := 20
	for i := 0; i < numJobs; i++ {
		job := Job{
			ID:   i,
			Data: i + 1, // Calculate factorial of (i+1)
		}
		pool.AddJob(job)
	}

	// Collect results
	go func() {
		pool.Stop()
	}()

	results := make([]Result, 0, numJobs)
	for result := range pool.GetResults() {
		results = append(results, result)
		fmt.Printf("Job %d processed by Worker %d: %d! = %d\n", 
			result.JobID, result.Worker, result.JobID+1, result.Result)
	}

	fmt.Printf("\nProcessed %d jobs with %d workers\n", len(results), pool.workers)

	// Demonstrate concurrent map processing
	fmt.Println("\nConcurrent Map Processing:")
	fmt.Println("==========================")

	numbers := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
	results2 := make([]int, len(numbers))
	var wg2 sync.WaitGroup

	start := time.Now()
	for i, num := range numbers {
		wg2.Add(1)
		go func(index, value int) {
			defer wg2.Done()
			// Simulate heavy computation
			time.Sleep(100 * time.Millisecond)
			results2[index] = value * value
		}(i, num)
	}

	wg2.Wait()
	duration := time.Since(start)

	fmt.Printf("Squared numbers: %v\n", results2)
	fmt.Printf("Concurrent processing took: %v\n", duration)

	// Sequential comparison
	start = time.Now()
	sequential := make([]int, len(numbers))
	for i, num := range numbers {
		time.Sleep(100 * time.Millisecond)
		sequential[i] = num * num
	}
	sequentialDuration := time.Since(start)

	fmt.Printf("Sequential processing took: %v\n", sequentialDuration)
	fmt.Printf("Speedup: %.2fx\n", float64(sequentialDuration)/float64(duration))
}