package main

import (
	"context"
	"fmt"
	"sync"
	"time"
)

// Pub/Sub pattern types
type Message struct {
	Topic   string
	Payload interface{}
}

type PubSub struct {
	subscribers map[string][]chan Message
	mu          sync.RWMutex
}

func NewPubSub() *PubSub {
	return &PubSub{
		subscribers: make(map[string][]chan Message),
	}
}

func (ps *PubSub) Subscribe(topic string) <-chan Message {
	ps.mu.Lock()
	defer ps.mu.Unlock()
	
	ch := make(chan Message, 10)
	ps.subscribers[topic] = append(ps.subscribers[topic], ch)
	return ch
}

func (ps *PubSub) Publish(topic string, payload interface{}) {
	ps.mu.RLock()
	defer ps.mu.RUnlock()
	
	msg := Message{Topic: topic, Payload: payload}
	for _, ch := range ps.subscribers[topic] {
		select {
		case ch <- msg:
		default:
			fmt.Printf("Subscriber channel full for topic %s\n", topic)
		}
	}
}

func (ps *PubSub) Close() {
	ps.mu.Lock()
	defer ps.mu.Unlock()
	
	for _, channels := range ps.subscribers {
		for _, ch := range channels {
			close(ch)
		}
	}
}

// Rate limiter types
type RateLimiter struct {
	ticker *time.Ticker
	done   chan bool
}

func NewRateLimiter(rate int) *RateLimiter {
	return &RateLimiter{
		ticker: time.NewTicker(time.Second / time.Duration(rate)),
		done:   make(chan bool),
	}
}

func (rl *RateLimiter) Wait() {
	<-rl.ticker.C
}

func (rl *RateLimiter) Stop() {
	rl.ticker.Stop()
	close(rl.done)
}

// Circuit breaker types
type CircuitState int

const (
	StateClosed CircuitState = iota
	StateOpen
	StateHalfOpen
)

type CircuitBreaker struct {
	state          CircuitState
	failureCount   int
	failureThreshold int
	resetTimeout   time.Duration
	lastFailureTime time.Time
	mu             sync.Mutex
}

func NewCircuitBreaker(failureThreshold int, resetTimeout time.Duration) *CircuitBreaker {
	return &CircuitBreaker{
		state:          StateClosed,
		failureThreshold: failureThreshold,
		resetTimeout:   resetTimeout,
	}
}

func (cb *CircuitBreaker) Call(fn func() error) error {
	cb.mu.Lock()
	defer cb.mu.Unlock()
	
	// Check if we should attempt to reset from open to half-open
	if cb.state == StateOpen && time.Since(cb.lastFailureTime) > cb.resetTimeout {
		cb.state = StateHalfOpen
		cb.failureCount = 0
	}
	
	// Don't allow calls if circuit is open
	if cb.state == StateOpen {
		return fmt.Errorf("circuit breaker is open")
	}
	
	// Execute the function
	err := fn()
	
	if err != nil {
		cb.failureCount++
		cb.lastFailureTime = time.Now()
		
		if cb.failureCount >= cb.failureThreshold {
			cb.state = StateOpen
			fmt.Printf("Circuit breaker opened after %d failures\n", cb.failureCount)
		}
		return err
	}
	
	// Success case
	if cb.state == StateHalfOpen {
		cb.state = StateClosed
		fmt.Println("Circuit breaker closed after successful call")
	}
	cb.failureCount = 0
	return nil
}

func main() {
	fmt.Println("=== Go Advanced Concurrency and Channel Patterns ===")
	testChannelOfChannels()
	testChannelSelectPatterns()
	testGoroutinePatterns()
	testContextPropagation()
	testAdvancedWorkerPools()
	testPipelinePatterns()
	testFanOutFanIn()
	testRateLimiting()
	testCircuitBreaker()
	fmt.Println("\n=== All advanced concurrency tests completed successfully! ===")
}

func testChannelOfChannels() {
	fmt.Println("\n--- Channel of Channels ---")
	
	// Create a channel that carries other channels
	channelChan := make(chan chan int, 3)
	
	// Create worker channels
	for i := 0; i < 3; i++ {
		workerChan := make(chan int, 5)
		channelChan <- workerChan
		
		// Start a worker for this channel
		go func(id int, ch chan int) {
			for num := range ch {
				fmt.Printf("Worker %d processed: %d\n", id, num)
			}
			close(ch)
		}(i, workerChan)
	}
	close(channelChan)
	
	// Use the channels
	for ch := range channelChan {
		ch <- 42
		ch <- 100
	}
}

func testChannelSelectPatterns() {
	fmt.Println("\n--- Advanced Channel Select Patterns ---")
	
	// Multiple defaults (simulated)
	ch1 := make(chan string, 1)
	ch2 := make(chan string, 1)
	ch3 := make(chan string, 1)
	
	ch1 <- "message1"
	ch2 <- "message2"
	
	// Select with priority
	select {
	case msg := <-ch1:
		fmt.Printf("Priority 1: %s\n", msg)
	case msg := <-ch2:
		fmt.Printf("Priority 2: %s\n", msg)
	case msg := <-ch3:
		fmt.Printf("Priority 3: %s\n", msg)
	default:
		fmt.Println("No messages available")
	}
	
	// Select with timeout and default
	timeout := time.After(100 * time.Millisecond)
	
	select {
	case msg := <-ch1:
		fmt.Printf("Received: %s\n", msg)
	case <-timeout:
		fmt.Println("Timeout occurred")
	default:
		fmt.Println("Would block, continuing")
	}
	
	// Select with random case selection
	numbers := make(chan int)
	strings := make(chan string)
	
	go func() {
		for i := 0; i < 5; i++ {
			select {
			case numbers <- i:
				fmt.Printf("Sent number: %d\n", i)
			case strings <- fmt.Sprintf("string-%d", i):
				fmt.Printf("Sent string: string-%d\n", i)
			}
			time.Sleep(10 * time.Millisecond)
		}
	}()
	
	// Consume from both channels
	for i := 0; i < 5; i++ {
		select {
		case num := <-numbers:
			fmt.Printf("Received number: %d\n", num)
		case str := <-strings:
			fmt.Printf("Received string: %s\n", str)
		}
	}
}

func testGoroutinePatterns() {
	fmt.Println("\n--- Advanced Goroutine Patterns ---")
	
	// Worker pool pattern
	testWorkerPool()
	
	// Generator pattern
	testGeneratorPattern()
	
	// Pub/Sub pattern
	testPubSubPattern()
}

func testWorkerPool() {
	fmt.Println("\nWorker Pool Pattern:")
	
	jobs := make(chan int, 10)
	results := make(chan int, 10)
	
	// Start workers
	numWorkers := 3
	var wg sync.WaitGroup
	
	for w := 0; w < numWorkers; w++ {
		wg.Add(1)
		go func(workerID int) {
			defer wg.Done()
			for job := range jobs {
				result := job * job // Simulate work
				fmt.Printf("Worker %d processed job %d, result: %d\n", workerID, job, result)
				results <- result
			}
		}(w)
	}
	
	// Send jobs
	for j := 1; j <= 5; j++ {
		jobs <- j
	}
	close(jobs)
	
	// Collect results
	go func() {
		wg.Wait()
		close(results)
	}()
	
	for result := range results {
		fmt.Printf("Final result: %d\n", result)
	}
}

func testGeneratorPattern() {
	fmt.Println("\nGenerator Pattern:")
	
	// Fibonacci generator
	fibonacci := func() <-chan int {
		ch := make(chan int)
		go func() {
			defer close(ch)
			a, b := 0, 1
			for i := 0; i < 10; i++ {
				ch <- a
				a, b = b, a+b
			}
		}()
		return ch
	}
	
	for num := range fibonacci() {
		fmt.Printf("Fibonacci: %d\n", num)
	}
	
	// Infinite generator with context
	counter := func(ctx context.Context) <-chan int {
		ch := make(chan int)
		go func() {
			defer close(ch)
			i := 0
			for {
				select {
				case <-ctx.Done():
					fmt.Println("Generator stopped by context")
					return
				case ch <- i:
					i++
					time.Sleep(50 * time.Millisecond)
				}
			}
		}()
		return ch
	}
	
	ctx, cancel := context.WithTimeout(context.Background(), 200*time.Millisecond)
	defer cancel()
	
	for num := range counter(ctx) {
		fmt.Printf("Counter: %d\n", num)
	}
}

func testPubSubPattern() {
	fmt.Println("\nPub/Sub Pattern:")
	
	pubsub := NewPubSub()
	
	// Subscribe to topics
	newsChan := pubsub.Subscribe("news")
	sportsChan := pubsub.Subscribe("sports")
	
	// Start subscribers
	go func() {
		for msg := range newsChan {
			fmt.Printf("News subscriber received: %v\n", msg.Payload)
		}
	}()
	
	go func() {
		for msg := range sportsChan {
			fmt.Printf("Sports subscriber received: %v\n", msg.Payload)
		}
	}()
	
	// Publish messages
	pubsub.Publish("news", "Breaking news: Go 1.21 released!")
	pubsub.Publish("sports", "Game result: Team A wins 3-1")
	pubsub.Publish("news", "Weather update: Sunny today")
	
	time.Sleep(100 * time.Millisecond)
	pubsub.Close()
}

func testContextPropagation() {
	fmt.Println("\n--- Context Propagation ---")
	
	// Context with values
	ctx := context.Background()
	ctx = context.WithValue(ctx, "userID", "12345")
	ctx = context.WithValue(ctx, "requestID", "req-abc-123")
	
	// Function that uses context values
	processRequest := func(ctx context.Context) {
		userID := ctx.Value("userID")
		requestID := ctx.Value("requestID")
		fmt.Printf("Processing request - UserID: %v, RequestID: %v\n", userID, requestID)
		
		// Simulate some work
		time.Sleep(50 * time.Millisecond)
		
		select {
		case <-ctx.Done():
			fmt.Println("Request cancelled")
			return
		default:
			fmt.Println("Request completed successfully")
		}
	}
	
	processRequest(ctx)
	
	// Context with cancellation propagation
	parentCtx, cancel := context.WithCancel(context.Background())
	
	// Start a goroutine that propagates cancellation
	go func() {
		childCtx, childCancel := context.WithCancel(parentCtx)
		defer childCancel()
		
		// Simulate work
		for i := 0; i < 10; i++ {
			select {
			case <-childCtx.Done():
				fmt.Println("Child context cancelled")
				return
			default:
				fmt.Printf("Child working... %d\n", i)
				time.Sleep(50 * time.Millisecond)
			}
		}
	}()
	
	// Cancel after a short time
	time.Sleep(150 * time.Millisecond)
	cancel()
	
	// Give time for cancellation to propagate
	time.Sleep(50 * time.Millisecond)
}

func testAdvancedWorkerPools() {
	fmt.Println("\n--- Advanced Worker Pools ---")
	
	type Job struct {
		ID   int
		Data interface{}
	}
	
	type Result struct {
		JobID int
		Value interface{}
		Error error
	}
	
	jobs := make(chan Job, 10)
	results := make(chan Result, 10)
	
	// Advanced worker with error handling
	worker := func(id int, jobs <-chan Job, results chan<- Result, wg *sync.WaitGroup) {
		defer wg.Done()
		
		for job := range jobs {
			fmt.Printf("Worker %d processing job %d\n", id, job.ID)
			
			// Simulate work with potential errors
			switch v := job.Data.(type) {
			case int:
				if v < 0 {
					results <- Result{JobID: job.ID, Error: fmt.Errorf("negative value: %d", v)}
				} else {
					results <- Result{JobID: job.ID, Value: v * v}
				}
			case string:
				if v == "" {
					results <- Result{JobID: job.ID, Error: fmt.Errorf("empty string")}
				} else {
					results <- Result{JobID: job.ID, Value: fmt.Sprintf("processed: %s", v)}
				}
			default:
				results <- Result{JobID: job.ID, Error: fmt.Errorf("unsupported type: %T", v)}
			}
		}
	}
	
	// Start workers
	var wg sync.WaitGroup
	numWorkers := 3
	
	for w := 0; w < numWorkers; w++ {
		wg.Add(1)
		go worker(w, jobs, results, &wg)
	}
	
	// Send jobs
	testData := []interface{}{5, -3, "hello", "", 10, 2.5, "world"}
	for i, data := range testData {
		jobs <- Job{ID: i, Data: data}
	}
	close(jobs)
	
	// Collect results in a separate goroutine
	go func() {
		wg.Wait()
		close(results)
	}()
	
	// Process results
	for result := range results {
		if result.Error != nil {
			fmt.Printf("Job %d failed: %v\n", result.JobID, result.Error)
		} else {
			fmt.Printf("Job %d succeeded: %v\n", result.JobID, result.Value)
		}
	}
}

func testPipelinePatterns() {
	fmt.Println("\n--- Pipeline Patterns ---")
	
	// Stage 1: Generate numbers
	generate := func(ctx context.Context) <-chan int {
		out := make(chan int)
		go func() {
			defer close(out)
			for i := 1; i <= 10; i++ {
				select {
				case <-ctx.Done():
					return
				case out <- i:
				}
			}
		}()
		return out
	}
	
	// Stage 2: Square numbers
	square := func(ctx context.Context, in <-chan int) <-chan int {
		out := make(chan int)
		go func() {
			defer close(out)
			for n := range in {
				select {
				case <-ctx.Done():
					return
				case out <- n * n:
				}
			}
		}()
		return out
	}
	
	// Stage 3: Filter even numbers
	filterEven := func(ctx context.Context, in <-chan int) <-chan int {
		out := make(chan int)
		go func() {
			defer close(out)
			for n := range in {
				if n%2 == 0 {
					select {
					case <-ctx.Done():
						return
					case out <- n:
					}
				}
			}
		}()
		return out
	}
	
	// Build and run pipeline
	ctx := context.Background()
	pipeline := filterEven(ctx, square(ctx, generate(ctx)))
	
	fmt.Println("Pipeline results:")
	for result := range pipeline {
		fmt.Printf("Result: %d\n", result)
	}
}

func testFanOutFanIn() {
	fmt.Println("\n--- Fan-out, Fan-in Pattern ---")
	
	// Worker function
	worker := func(ctx context.Context, in <-chan int, id int) <-chan int {
		out := make(chan int)
		go func() {
			defer close(out)
			for n := range in {
				result := n * n // Process the number
				fmt.Printf("Worker %d processing %d -> %d\n", id, n, result)
				select {
				case <-ctx.Done():
					return
				case out <- result:
				}
			}
		}()
		return out
	}
	
	// Fan-out: distribute work to multiple workers
	fanOut := func(ctx context.Context, in <-chan int, workers int) []<-chan int {
		outs := make([]<-chan int, workers)
		for i := 0; i < workers; i++ {
			outs[i] = worker(ctx, in, i)
		}
		return outs
	}
	
	// Fan-in: combine results from multiple workers
	fanIn := func(ctx context.Context, ins []<-chan int) <-chan int {
		var wg sync.WaitGroup
		out := make(chan int)
		
		// Start a goroutine for each input channel
		for _, in := range ins {
			wg.Add(1)
			go func(ch <-chan int) {
				defer wg.Done()
				for n := range ch {
					select {
					case <-ctx.Done():
						return
					case out <- n:
					}
				}
			}(in)
		}
		
		// Close output channel when all workers are done
		go func() {
			wg.Wait()
			close(out)
		}()
		
		return out
	}
	
	// Generate input
	generate := func(ctx context.Context) <-chan int {
		out := make(chan int)
		go func() {
			defer close(out)
			for i := 1; i <= 10; i++ {
				select {
				case <-ctx.Done():
					return
				case out <- i:
				}
			}
		}()
		return out
	}
	
	// Run fan-out/fan-in pattern
	ctx := context.Background()
	input := generate(ctx)
	workers := fanOut(ctx, input, 3)
	results := fanIn(ctx, workers)
	
	fmt.Println("Fan-out/fan-in results:")
	for result := range results {
		fmt.Printf("Final result: %d\n", result)
	}
}

func testRateLimiting() {
	fmt.Println("\n--- Rate Limiting Pattern ---")
	
	// Create rate limiter: 3 operations per second
	limiter := NewRateLimiter(3)
	defer limiter.Stop()
	
	// Simulate API calls with rate limiting
	for i := 0; i < 7; i++ {
		limiter.Wait()
		fmt.Printf("API call %d at %s\n", i+1, time.Now().Format("15:04:05.000"))
	}
}

func testCircuitBreaker() {
	fmt.Println("\n--- Circuit Breaker Pattern ---")
	
	// Simulate service with intermittent failures
	failureCount := 0
	simulateService := func() error {
		failureCount++
		if failureCount%3 == 0 { // Fail every 3rd call
			return fmt.Errorf("service unavailable")
		}
		return nil
	}
	
	cb := NewCircuitBreaker(2, 1*time.Second)
	
	// Test circuit breaker
	for i := 0; i < 8; i++ {
		err := cb.Call(simulateService)
		if err != nil {
			fmt.Printf("Call %d failed: %v\n", i+1, err)
		} else {
			fmt.Printf("Call %d succeeded\n", i+1)
		}
		time.Sleep(200 * time.Millisecond)
	}
}