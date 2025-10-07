package main

import (
	"context"
	"fmt"
	"math/rand"
	"sync"
	"sync/atomic"
	"time"
)

func main() {
	fmt.Println("=== Go Context and Sync Tests ===")
	testContextBasics()
	testContextWithTimeout()
	testContextWithCancel()
	testContextWithDeadline()
	testContextValues()
	testMutexBasics()
	testRWMutex()
	testWaitGroup()
	testAtomicOperations()
	testCondVariable()
	testOnce()
	testPool()
	testWorkerPools()
	testRateLimiting()
	testCircuitBreaker()

	fmt.Println("\n=== All context and sync tests completed successfully! ===")
}

func testContextBasics() {
	fmt.Println("\n--- Context Basics ---")

	// Background context
	ctx := context.Background()
	fmt.Printf("Background context: %v\n", ctx)

	// TODO context
	todoCtx := context.TODO()
	fmt.Printf("TODO context: %v\n", todoCtx)

	// Context cancellation check
	fmt.Printf("Context done: %v\n", ctx.Done())
	fmt.Printf("Context err: %v\n", ctx.Err())
	fmt.Printf("Context deadline: %v\n", ctx.Deadline())
}

func testContextWithTimeout() {
	fmt.Println("\n--- Context With Timeout ---")

	// Context with timeout
	ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
	defer cancel()

	select {
	case <-time.After(200 * time.Millisecond):
		fmt.Println("This should not print")
	case <-ctx.Done():
		fmt.Printf("Context timed out: %v\n", ctx.Err())
	}

	// Context with longer timeout
	ctx2, cancel2 := context.WithTimeout(context.Background(), 200*time.Millisecond)
	defer cancel2()

	select {
	case <-time.After(50 * time.Millisecond):
		fmt.Println("Operation completed before timeout")
	case <-ctx2.Done():
		fmt.Printf("Context timed out: %v\n", ctx2.Err())
	}
}

func testContextWithCancel() {
	fmt.Println("\n--- Context With Cancel ---")

	ctx, cancel := context.WithCancel(context.Background())

	// Start goroutine that waits for context cancellation
	go func() {
		<-ctx.Done()
		fmt.Printf("Context cancelled: %v\n", ctx.Err())
	}()

	time.Sleep(50 * time.Millisecond)
	cancel() // Cancel the context
	time.Sleep(50 * time.Millisecond)
}

func testContextWithDeadline() {
	fmt.Println("\n--- Context With Deadline ---")

	// Set deadline in the future
	deadline := time.Now().Add(100 * time.Millisecond)
	ctx, cancel := context.WithDeadline(context.Background(), deadline)
	defer cancel()

	select {
	case <-time.After(200 * time.Millisecond):
		fmt.Println("This should not print")
	case <-ctx.Done():
		fmt.Printf("Context deadline exceeded: %v\n", ctx.Err())
	}

	// Check if deadline was set
	if d, ok := ctx.Deadline(); ok {
		fmt.Printf("Deadline was set to: %v\n", d)
	}
}

func testContextValues() {
	fmt.Println("\n--- Context Values ---")

	type contextKey string

	// Create context with values
	ctx := context.WithValue(context.Background(), contextKey("user_id"), 123)
	ctx = context.WithValue(ctx, contextKey("user_name"), "Alice")
	ctx = context.WithValue(ctx, contextKey("is_admin"), true)

	// Retrieve values
	if userID := ctx.Value(contextKey("user_id")); userID != nil {
		fmt.Printf("User ID: %v\n", userID)
	}

	if userName := ctx.Value(contextKey("user_name")); userName != nil {
		fmt.Printf("User Name: %v\n", userName)
	}

	if isAdmin := ctx.Value(contextKey("is_admin")); isAdmin != nil {
		fmt.Printf("Is Admin: %v\n", isAdmin)
	}

	// Non-existent key
	if missing := ctx.Value(contextKey("missing_key")); missing == nil {
		fmt.Println("Missing key returns nil")
	}
}

func testMutexBasics() {
	fmt.Println("\n--- Mutex Basics ---")

	var mutex sync.Mutex
	var counter int

	// Start multiple goroutines that increment the counter
	var wg sync.WaitGroup
	for i := 0; i < 10; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for j := 0; j < 1000; j++ {
				mutex.Lock()
				counter++
				mutex.Unlock()
			}
		}()
	}

	wg.Wait()
	fmt.Printf("Final counter value: %d\n", counter)
}

func testRWMutex() {
	fmt.Println("\n--- RWMutex ---")

	var rwMutex sync.RWMutex
	var data map[string]int
	data = make(map[string]int)

	// Writer goroutines
	var writeWg sync.WaitGroup
	for i := 0; i < 5; i++ {
		writeWg.Add(1)
		go func(id int) {
			defer writeWg.Done()
			for j := 0; j < 10; j++ {
				rwMutex.Lock()
				data[fmt.Sprintf("key_%d_%d", id, j)] = id*100 + j
				rwMutex.Unlock()
				time.Sleep(time.Millisecond)
			}
		}(i)
	}

	// Reader goroutines
	var readWg sync.WaitGroup
	for i := 0; i < 20; i++ {
		readWg.Add(1)
		go func() {
			defer readWg.Done()
			for j := 0; j < 10; j++ {
				rwMutex.RLock()
				count := len(data)
				rwMutex.RUnlock()
				fmt.Printf("Reader saw %d items\n", count)
				time.Sleep(time.Millisecond)
			}
		}()
	}

	writeWg.Wait()
	readWg.Wait()
	fmt.Printf("Final data size: %d\n", len(data))
}

func testWaitGroup() {
	fmt.Println("\n--- WaitGroup ---")

	var wg sync.WaitGroup

	// Start multiple goroutines
	for i := 0; i < 5; i++ {
		wg.Add(1)
		go func(id int) {
			defer wg.Done()
			fmt.Printf("Worker %d starting\n", id)
			time.Sleep(time.Duration(rand.Intn(100)) * time.Millisecond)
			fmt.Printf("Worker %d completed\n", id)
		}(i)
	}

	// Wait for all goroutines to complete
	fmt.Println("Waiting for workers to complete...")
	wg.Wait()
	fmt.Println("All workers completed")
}

func testAtomicOperations() {
	fmt.Println("\n--- Atomic Operations ---")

	var counter int64

	// Start multiple goroutines that increment the counter atomically
	var wg sync.WaitGroup
	for i := 0; i < 10; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for j := 0; j < 1000; j++ {
				atomic.AddInt64(&counter, 1)
			}
		}()
	}

	wg.Wait()
	fmt.Printf("Final atomic counter value: %d\n", atomic.LoadInt64(&counter))

	// Compare and swap
	var value int64 = 42
	swapped := atomic.CompareAndSwapInt64(&value, 42, 100)
	fmt.Printf("CAS successful: %t, new value: %d\n", swapped, atomic.LoadInt64(&value))

	// Store and load
	atomic.StoreInt64(&value, 200)
	loaded := atomic.LoadInt64(&value)
	fmt.Printf("Stored and loaded value: %d\n", loaded)
}

func testCondVariable() {
	fmt.Println("\n--- Condition Variable ---")

	var mutex sync.Mutex
	cond := sync.NewCond(&mutex)
	var ready bool

	// Waiter goroutine
	go func() {
		cond.L.Lock()
		for !ready {
			fmt.Println("Waiter waiting...")
			cond.Wait()
		}
		fmt.Println("Waiter awakened!")
		cond.L.Unlock()
	}()

	time.Sleep(100 * time.Millisecond)

	// Signaler goroutine
	cond.L.Lock()
	ready = true
	fmt.Println("Signaler setting ready and broadcasting...")
	cond.Broadcast()
	cond.L.Unlock()

	time.Sleep(50 * time.Millisecond)
}

func testOnce() {
	fmt.Println("\n--- Once ---")

	var once sync.Once
	var counter int

	// Start multiple goroutines that call Do
	var wg sync.WaitGroup
	for i := 0; i < 5; i++ {
		wg.Add(1)
		go func(id int) {
			defer wg.Done()
			once.Do(func() {
				fmt.Printf("Initialization by goroutine %d\n", id)
				counter = 100
			})
			fmt.Printf("Goroutine %d saw counter: %d\n", id, counter)
		}(i)
	}

	wg.Wait()
	fmt.Printf("Final counter value: %d\n", counter)
}

func testPool() {
	fmt.Println("\n--- Pool ---")

	// Create a pool of integers
	pool := &sync.Pool{
		New: func() interface{} {
			return fmt.Sprintf("Item %d", rand.Intn(1000))
		},
	}

	// Get and put items
	item1 := pool.Get()
	fmt.Printf("Got item: %s\n", item1)

	item2 := pool.Get()
	fmt.Printf("Got item: %s\n", item2)

	// Put items back
	pool.Put("Custom item 1")
	pool.Put("Custom item 2")

	item3 := pool.Get()
	fmt.Printf("Got item after put: %s\n", item3)

	item4 := pool.Get()
	fmt.Printf("Got item after put: %s\n", item4)
}

func testWorkerPools() {
	fmt.Println("\n--- Worker Pools ---")

	const numWorkers = 3
	const numJobs = 10

	var wg sync.WaitGroup
	jobs := make(chan int, numJobs)
	results := make(chan int, numJobs)

	// Start workers
	for w := 1; w <= numWorkers; w++ {
		wg.Add(1)
		go worker(w, jobs, results, &wg)
	}

	// Send jobs
	for j := 1; j <= numJobs; j++ {
		jobs <- j
	}
	close(jobs)

	// Wait for workers to complete
	go func() {
		wg.Wait()
		close(results)
	}()

	// Collect results
	for result := range results {
		fmt.Printf("Result: %d\n", result)
	}
}

func worker(id int, jobs <-chan int, results chan<- int, wg *sync.WaitGroup) {
	defer wg.Done()
	for j := range jobs {
		fmt.Printf("Worker %d processing job %d\n", id, j)
		time.Sleep(time.Duration(rand.Intn(100)) * time.Millisecond)
		results <- j * 2
	}
}

func testRateLimiting() {
	fmt.Println("\n--- Rate Limiting ---")

	// Simple rate limiter using ticker
	limiter := time.NewTicker(100 * time.Millisecond)
	defer limiter.Stop()

	requests := 10
	for i := 0; i < requests; i++ {
		<-limiter.C
		fmt.Printf("Request %d processed at %v\n", i+1, time.Now().Format("15:04:05.000"))
	}
}

func testCircuitBreaker() {
	fmt.Println("\n--- Circuit Breaker ---")

	type CircuitBreaker struct {
		failures     int
		maxFailures  int
		lastFailure  time.Time
		timeout      time.Duration
		mutex        sync.Mutex
	}

	NewCircuitBreaker := func(maxFailures int, timeout time.Duration) *CircuitBreaker {
		return &CircuitBreaker{
			maxFailures: maxFailures,
			timeout:      timeout,
		}
	}

	(cb *CircuitBreaker) AllowRequest() bool {
		cb.mutex.Lock()
		defer cb.mutex.Unlock()

		if cb.failures >= cb.maxFailures {
			if time.Since(cb.lastFailure) < cb.timeout {
				return false
			}
			// Reset after timeout
			cb.failures = 0
		}
		return true
	}

	(cb *CircuitBreaker) RecordFailure() {
		cb.mutex.Lock()
		defer cb.mutex.Unlock()
		cb.failures++
		cb.lastFailure = time.Now()
	}

	(cb *CircuitBreaker) RecordSuccess() {
		cb.mutex.Lock()
		defer cb.mutex.Unlock()
		cb.failures = 0
	}

	cb := NewCircuitBreaker(3, time.Second)

	// Simulate requests
	for i := 0; i < 10; i++ {
		if cb.AllowRequest() {
			fmt.Printf("Request %d allowed\n", i+1)
			// Simulate random failure
			if rand.Intn(10) < 7 { // 70% failure rate
				cb.RecordFailure()
				fmt.Printf("Request %d failed\n", i+1)
			} else {
				cb.RecordSuccess()
				fmt.Printf("Request %d succeeded\n", i+1)
			}
		} else {
			fmt.Printf("Request %d blocked by circuit breaker\n", i+1)
		}
		time.Sleep(200 * time.Millisecond)
	}
}

// Advanced context patterns
func testAdvancedContext() {
	fmt.Println("\n--- Advanced Context Patterns ---")

	// Context hierarchy
	parentCtx := context.Background()
	childCtx, childCancel := context.WithCancel(parentCtx)
	grandchildCtx, grandchildCancel := context.WithTimeout(childCtx, 200*time.Millisecond)

	// Cancel parent affects all children
	go func() {
		time.Sleep(50 * time.Millisecond)
		childCancel()
	}()

	select {
	case <-grandchildCtx.Done():
		fmt.Printf("Grandchild context cancelled: %v\n", grandchildCtx.Err())
	}

	grandchildCancel()

	// Context with multiple values
	ctx := context.WithValue(context.Background(), "key1", "value1")
	ctx = context.WithValue(ctx, "key2", "value2")
	ctx = context.WithValue(ctx, "key3", "value3")

	// Extract all values (note: this is not typically done in practice)
	fmt.Printf("Context values - key1: %v, key2: %v, key3: %v\n",
		ctx.Value("key1"), ctx.Value("key2"), ctx.Value("key3"))
}