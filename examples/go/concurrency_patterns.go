package main

import (
	"context"
	"fmt"
	"math/rand"
	"runtime"
	"sync"
	"sync/atomic"
	"time"
)

// Worker pool implementation
type Job struct {
	ID     int
	Data   int
	Result chan int
}

type WorkerPool struct {
	jobs       chan Job
	workers    int
	wg         sync.WaitGroup
	ctx        context.Context
	cancel     context.CancelFunc
	processed  int64
}

func NewWorkerPool(workers int) *WorkerPool {
	ctx, cancel := context.WithCancel(context.Background())
	return &WorkerPool{
		jobs:    make(chan Job, workers*2),
		workers: workers,
		ctx:     ctx,
		cancel:  cancel,
	}
}

func (wp *WorkerPool) Start() {
	for i := 0; i < wp.workers; i++ {
		wp.wg.Add(1)
		go wp.worker(i)
	}
}

func (wp *WorkerPool) worker(id int) {
	defer wp.wg.Done()
	fmt.Printf("Worker %d started\n", id)

	for {
		select {
		case job := <-wp.jobs:
			// Simulate work with random processing time
			processingTime := time.Duration(rand.Intn(100)) * time.Millisecond
			time.Sleep(processingTime)
			
			result := job.Data * job.Data // Simple computation
			job.Result <- result
			
			atomic.AddInt64(&wp.processed, 1)
			fmt.Printf("Worker %d processed job %d (data: %d, result: %d)\n", 
				id, job.ID, job.Data, result)

		case <-wp.ctx.Done():
			fmt.Printf("Worker %d stopping...\n", id)
			return
		}
	}
}

func (wp *WorkerPool) Submit(job Job) {
	select {
	case wp.jobs <- job:
	case <-wp.ctx.Done():
		close(job.Result)
	}
}

func (wp *WorkerPool) Stop() {
	wp.cancel()
	wp.wg.Wait()
	close(wp.jobs)
	fmt.Printf("Worker pool stopped. Total jobs processed: %d\n", atomic.LoadInt64(&wp.processed))
}

// Pipeline pattern implementation
type Pipeline struct {
	input  chan int
	output chan int
	stages []func(int) int
	ctx    context.Context
	cancel context.CancelFunc
	wg     sync.WaitGroup
}

func NewPipeline(stages ...func(int) int) *Pipeline {
	ctx, cancel := context.WithCancel(context.Background())
	return &Pipeline{
		input:  make(chan int, 10),
		output: make(chan int, 10),
		stages: stages,
		ctx:    ctx,
		cancel: cancel,
	}
}

func (p *Pipeline) Start() {
	channels := []chan int{p.input}
	
	// Create intermediate channels
	for i := 0; i < len(p.stages)-1; i++ {
		channels = append(channels, make(chan int, 10))
	}
	channels = append(channels, p.output)

	// Start each stage
	for i, stage := range p.stages {
		p.wg.Add(1)
		go p.runStage(i, stage, channels[i], channels[i+1])
	}
}

func (p *Pipeline) runStage(id int, stage func(int) int, input, output chan int) {
	defer p.wg.Done()
	defer close(output)

	fmt.Printf("Stage %d started\n", id)
	
	for {
		select {
		case data, ok := <-input:
			if !ok {
				fmt.Printf("Stage %d finished\n", id)
				return
			}
			result := stage(data)
			fmt.Printf("Stage %d: %d -> %d\n", id, data, result)
			
			select {
			case output <- result:
			case <-p.ctx.Done():
				return
			}
		case <-p.ctx.Done():
			fmt.Printf("Stage %d stopping...\n", id)
			return
		}
	}
}

func (p *Pipeline) Process(data int) {
	select {
	case p.input <- data:
	case <-p.ctx.Done():
	}
}

func (p *Pipeline) Results() <-chan int {
	return p.output
}

func (p *Pipeline) Stop() {
	close(p.input)
	p.wg.Wait()
	p.cancel()
}

// Rate limiter implementation
type RateLimiter struct {
	rate     time.Duration
	tokens   chan struct{}
	ctx      context.Context
	cancel   context.CancelFunc
}

func NewRateLimiter(requestsPerSecond int) *RateLimiter {
	ctx, cancel := context.WithCancel(context.Background())
	rl := &RateLimiter{
		rate:   time.Second / time.Duration(requestsPerSecond),
		tokens: make(chan struct{}, requestsPerSecond),
		ctx:    ctx,
		cancel: cancel,
	}

	// Fill initial tokens
	for i := 0; i < requestsPerSecond; i++ {
		rl.tokens <- struct{}{}
	}

	// Start token refiller
	go rl.refillTokens()
	return rl
}

func (rl *RateLimiter) refillTokens() {
	ticker := time.NewTicker(rl.rate)
	defer ticker.Stop()

	for {
		select {
		case <-ticker.C:
			select {
			case rl.tokens <- struct{}{}:
			default: // Channel full, skip
			}
		case <-rl.ctx.Done():
			return
		}
	}
}

func (rl *RateLimiter) Allow() bool {
	select {
	case <-rl.tokens:
		return true
	default:
		return false
	}
}

func (rl *RateLimiter) Wait() {
	<-rl.tokens
}

func (rl *RateLimiter) Stop() {
	rl.cancel()
}

// Fan-out/Fan-in pattern
func fanOut(input <-chan int, workers int) []<-chan int {
	outputs := make([]<-chan int, workers)
	
	for i := 0; i < workers; i++ {
		output := make(chan int)
		outputs[i] = output
		
		go func(out chan<- int) {
			defer close(out)
			for data := range input {
				// Simulate processing
				time.Sleep(time.Duration(rand.Intn(50)) * time.Millisecond)
				out <- data * 2
			}
		}(output)
	}
	
	return outputs
}

func fanIn(inputs ...<-chan int) <-chan int {
	output := make(chan int)
	var wg sync.WaitGroup
	
	for _, input := range inputs {
		wg.Add(1)
		go func(in <-chan int) {
			defer wg.Done()
			for data := range in {
				output <- data
			}
		}(input)
	}
	
	go func() {
		wg.Wait()
		close(output)
	}()
	
	return output
}

func main() {
	fmt.Println("=== Advanced Concurrency Patterns Demo ===")
	fmt.Printf("Running on %d CPU cores\n\n", runtime.NumCPU())

	// Worker Pool Demo
	fmt.Println("1. Worker Pool Pattern:")
	pool := NewWorkerPool(3)
	pool.Start()

	// Submit jobs
	var results []chan int
	for i := 1; i <= 10; i++ {
		result := make(chan int, 1)
		results = append(results, result)
		
		job := Job{
			ID:     i,
			Data:   i * 10,
			Result: result,
		}
		pool.Submit(job)
	}

	// Collect results
	fmt.Println("Collecting results...")
	for i, result := range results {
		res := <-result
		fmt.Printf("Job %d result: %d\n", i+1, res)
		close(result)
	}

	pool.Stop()
	fmt.Println()

	// Pipeline Demo
	fmt.Println("2. Pipeline Pattern:")
	pipeline := NewPipeline(
		func(x int) int { return x * 2 },      // Double
		func(x int) int { return x + 10 },     // Add 10
		func(x int) int { return x * x },      // Square
	)
	
	pipeline.Start()

	// Send data through pipeline
	go func() {
		for i := 1; i <= 5; i++ {
			pipeline.Process(i)
			time.Sleep(100 * time.Millisecond)
		}
		pipeline.Stop()
	}()

	// Collect pipeline results
	fmt.Println("Pipeline results:")
	for result := range pipeline.Results() {
		fmt.Printf("Final result: %d\n", result)
	}
	fmt.Println()

	// Rate Limiter Demo
	fmt.Println("3. Rate Limiter Pattern:")
	limiter := NewRateLimiter(5) // 5 requests per second
	
	fmt.Println("Testing rate limiting (5 requests/sec):")
	start := time.Now()
	for i := 1; i <= 10; i++ {
		limiter.Wait() // This will block if rate limit exceeded
		fmt.Printf("Request %d processed at %v\n", i, time.Since(start))
	}
	
	limiter.Stop()
	fmt.Println()

	// Fan-out/Fan-in Demo
	fmt.Println("4. Fan-out/Fan-in Pattern:")
	input := make(chan int)
	
	// Start fan-out with 3 workers
	outputs := fanOut(input, 3)
	
	// Fan-in to single output
	result := fanIn(outputs...)
	
	// Send data
	go func() {
		defer close(input)
		for i := 1; i <= 9; i++ {
			fmt.Printf("Sending: %d\n", i)
			input <- i
		}
	}()

	// Collect fan-in results
	fmt.Println("Fan-in results:")
	for res := range result {
		fmt.Printf("Received: %d\n", res)
	}
	fmt.Println()

	// Goroutine leak detection
	fmt.Println("5. Goroutine Monitoring:")
	initialGoroutines := runtime.NumGoroutine()
	fmt.Printf("Initial goroutines: %d\n", initialGoroutines)

	// Create some temporary goroutines
	ctx, cancel := context.WithCancel(context.Background())
	for i := 0; i < 10; i++ {
		go func(id int) {
			select {
			case <-ctx.Done():
				fmt.Printf("Goroutine %d stopping\n", id)
				return
			case <-time.After(5 * time.Second):
				fmt.Printf("Goroutine %d timeout\n", id)
				return
			}
		}(i)
	}

	fmt.Printf("Goroutines after creating 10: %d\n", runtime.NumGoroutine())
	
	// Cancel all goroutines
	cancel()
	time.Sleep(100 * time.Millisecond) // Give them time to stop
	
	finalGoroutines := runtime.NumGoroutine()
	fmt.Printf("Final goroutines: %d\n", finalGoroutines)
	
	if finalGoroutines <= initialGoroutines+1 { // +1 for potential GC goroutine
		fmt.Println("✓ No goroutine leaks detected")
	} else {
		fmt.Printf("⚠ Potential goroutine leak: %d extra goroutines\n", 
			finalGoroutines-initialGoroutines)
	}

	fmt.Println("\nConcurrency patterns demo completed!")
}