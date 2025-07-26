package main

import (
	"context"
	"fmt"
	"math/rand"
	"runtime"
	"sync"
	"time"
)

// Worker Pool Pattern
type Job struct {
	ID     int
	Data   interface{}
	Result chan interface{}
}

type WorkerPool struct {
	jobs    chan Job
	results chan interface{}
	workers int
	wg      sync.WaitGroup
}

func NewWorkerPool(numWorkers int) *WorkerPool {
	return &WorkerPool{
		jobs:    make(chan Job, 100),
		results: make(chan interface{}, 100),
		workers: numWorkers,
	}
}

func (wp *WorkerPool) Start(ctx context.Context, processor func(interface{}) interface{}) {
	for i := 0; i < wp.workers; i++ {
		wp.wg.Add(1)
		go wp.worker(ctx, i, processor)
	}
}

func (wp *WorkerPool) worker(ctx context.Context, id int, processor func(interface{}) interface{}) {
	defer wp.wg.Done()
	
	for {
		select {
		case job := <-wp.jobs:
			result := processor(job.Data)
			select {
			case job.Result <- result:
			case <-ctx.Done():
				return
			}
		case <-ctx.Done():
			return
		}
	}
}

func (wp *WorkerPool) Submit(job Job) {
	wp.jobs <- job
}

func (wp *WorkerPool) Stop() {
	close(wp.jobs)
	wp.wg.Wait()
	close(wp.results)
}

// Producer-Consumer Pattern
type Buffer struct {
	items []interface{}
	mutex sync.RWMutex
	size  int
	cond  *sync.Cond
}

func NewBuffer(size int) *Buffer {
	b := &Buffer{
		items: make([]interface{}, 0, size),
		size:  size,
	}
	b.cond = sync.NewCond(&b.mutex)
	return b
}

func (b *Buffer) Put(item interface{}) {
	b.mutex.Lock()
	defer b.mutex.Unlock()
	
	for len(b.items) >= b.size {
		b.cond.Wait()
	}
	
	b.items = append(b.items, item)
	b.cond.Broadcast()
}

func (b *Buffer) Get() interface{} {
	b.mutex.Lock()
	defer b.mutex.Unlock()
	
	for len(b.items) == 0 {
		b.cond.Wait()
	}
	
	item := b.items[0]
	b.items = b.items[1:]
	b.cond.Broadcast()
	return item
}

// Fan-out Fan-in Pattern
func fanOut(input <-chan int, workers int) []<-chan int {
	outputs := make([]<-chan int, workers)
	
	for i := 0; i < workers; i++ {
		output := make(chan int)
		outputs[i] = output
		
		go func(out chan<- int) {
			defer close(out)
			for data := range input {
				// Simulate work
				time.Sleep(time.Millisecond * time.Duration(rand.Intn(100)))
				out <- data * data
			}
		}(output)
	}
	
	return outputs
}

func fanIn(inputs ...<-chan int) <-chan int {
	output := make(chan int)
	var wg sync.WaitGroup
	
	wg.Add(len(inputs))
	for _, input := range inputs {
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

// Pipeline Pattern
func pipeline(input <-chan int) <-chan string {
	// Stage 1: Square numbers
	squared := make(chan int)
	go func() {
		defer close(squared)
		for num := range input {
			squared <- num * num
		}
	}()
	
	// Stage 2: Filter even numbers
	filtered := make(chan int)
	go func() {
		defer close(filtered)
		for num := range squared {
			if num%2 == 0 {
				filtered <- num
			}
		}
	}()
	
	// Stage 3: Convert to string
	output := make(chan string)
	go func() {
		defer close(output)
		for num := range filtered {
			output <- fmt.Sprintf("Result: %d", num)
		}
	}()
	
	return output
}

// Rate Limiter Pattern
type RateLimiter struct {
	ticker   *time.Ticker
	requests chan struct{}
}

func NewRateLimiter(rps int) *RateLimiter {
	return &RateLimiter{
		ticker:   time.NewTicker(time.Second / time.Duration(rps)),
		requests: make(chan struct{}, rps),
	}
}

func (rl *RateLimiter) Start() {
	go func() {
		for range rl.ticker.C {
			select {
			case rl.requests <- struct{}{}:
			default:
			}
		}
	}()
}

func (rl *RateLimiter) Wait() {
	<-rl.requests
}

func (rl *RateLimiter) Stop() {
	rl.ticker.Stop()
	close(rl.requests)
}

// Circuit Breaker Pattern
type CircuitBreaker struct {
	maxFailures int
	resetTime   time.Duration
	failures    int
	lastFailure time.Time
	state       string
	mutex       sync.RWMutex
}

func NewCircuitBreaker(maxFailures int, resetTime time.Duration) *CircuitBreaker {
	return &CircuitBreaker{
		maxFailures: maxFailures,
		resetTime:   resetTime,
		state:       "closed",
	}
}

func (cb *CircuitBreaker) Call(fn func() error) error {
	cb.mutex.Lock()
	defer cb.mutex.Unlock()
	
	if cb.state == "open" {
		if time.Since(cb.lastFailure) > cb.resetTime {
			cb.state = "half-open"
			cb.failures = 0
		} else {
			return fmt.Errorf("circuit breaker is open")
		}
	}
	
	err := fn()
	if err != nil {
		cb.failures++
		cb.lastFailure = time.Now()
		
		if cb.failures >= cb.maxFailures {
			cb.state = "open"
		}
		return err
	}
	
	if cb.state == "half-open" {
		cb.state = "closed"
	}
	cb.failures = 0
	return nil
}

func demonstrateWorkerPool() {
	fmt.Println("Worker Pool Pattern Demo")
	fmt.Println("========================")
	
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	
	pool := NewWorkerPool(3)
	
	processor := func(data interface{}) interface{} {
		num := data.(int)
		time.Sleep(time.Millisecond * 100) // Simulate work
		return num * num
	}
	
	pool.Start(ctx, processor)
	
	// Submit jobs
	for i := 1; i <= 10; i++ {
		result := make(chan interface{}, 1)
		pool.Submit(Job{ID: i, Data: i, Result: result})
		
		go func(id int, res <-chan interface{}) {
			select {
			case r := <-res:
				fmt.Printf("Job %d result: %v\n", id, r)
			case <-ctx.Done():
			}
		}(i, result)
	}
	
	time.Sleep(2 * time.Second)
	pool.Stop()
	fmt.Println()
}

func demonstrateProducerConsumer() {
	fmt.Println("Producer-Consumer Pattern Demo")
	fmt.Println("==============================")
	
	buffer := NewBuffer(5)
	var wg sync.WaitGroup
	
	// Producer
	wg.Add(1)
	go func() {
		defer wg.Done()
		for i := 1; i <= 10; i++ {
			fmt.Printf("Producing: %d\n", i)
			buffer.Put(i)
			time.Sleep(time.Millisecond * 200)
		}
	}()
	
	// Consumer
	wg.Add(1)
	go func() {
		defer wg.Done()
		for i := 0; i < 10; i++ {
			item := buffer.Get()
			fmt.Printf("Consuming: %v\n", item)
			time.Sleep(time.Millisecond * 300)
		}
	}()
	
	wg.Wait()
	fmt.Println()
}

func demonstrateFanOutFanIn() {
	fmt.Println("Fan-out Fan-in Pattern Demo")
	fmt.Println("===========================")
	
	input := make(chan int)
	
	// Start fan-out
	outputs := fanOut(input, 3)
	
	// Fan-in results
	result := fanIn(outputs...)
	
	// Send data
	go func() {
		defer close(input)
		for i := 1; i <= 10; i++ {
			input <- i
		}
	}()
	
	// Collect results
	for squared := range result {
		fmt.Printf("Squared: %d\n", squared)
	}
	fmt.Println()
}

func demonstratePipeline() {
	fmt.Println("Pipeline Pattern Demo")
	fmt.Println("=====================")
	
	input := make(chan int)
	output := pipeline(input)
	
	// Send data
	go func() {
		defer close(input)
		for i := 1; i <= 10; i++ {
			input <- i
		}
	}()
	
	// Collect results
	for result := range output {
		fmt.Println(result)
	}
	fmt.Println()
}

func demonstrateRateLimiter() {
	fmt.Println("Rate Limiter Pattern Demo")
	fmt.Println("=========================")
	
	limiter := NewRateLimiter(2) // 2 requests per second
	limiter.Start()
	defer limiter.Stop()
	
	start := time.Now()
	for i := 1; i <= 5; i++ {
		limiter.Wait()
		fmt.Printf("Request %d at %v\n", i, time.Since(start))
	}
	fmt.Println()
}

func demonstrateCircuitBreaker() {
	fmt.Println("Circuit Breaker Pattern Demo")
	fmt.Println("============================")
	
	cb := NewCircuitBreaker(3, 2*time.Second)
	
	// Simulate failing function
	failingFunc := func() error {
		if rand.Float32() < 0.7 { // 70% failure rate
			return fmt.Errorf("simulated failure")
		}
		return nil
	}
	
	for i := 1; i <= 10; i++ {
		err := cb.Call(failingFunc)
		if err != nil {
			fmt.Printf("Call %d failed: %v\n", i, err)
		} else {
			fmt.Printf("Call %d succeeded\n", i)
		}
		time.Sleep(time.Millisecond * 300)
	}
	fmt.Println()
}

func main() {
	fmt.Printf("Concurrency Patterns Demo (Go %s)\n", runtime.Version())
	fmt.Printf("Using %d CPU cores\n\n", runtime.NumCPU())
	
	rand.Seed(time.Now().UnixNano())
	
	demonstrateWorkerPool()
	demonstrateProducerConsumer()
	demonstrateFanOutFanIn()
	demonstratePipeline()
	demonstrateRateLimiter()
	demonstrateCircuitBreaker()
	
	fmt.Println("All concurrency patterns demonstrated!")
}