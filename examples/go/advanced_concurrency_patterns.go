package main

import (
    "context"
    "fmt"
    "sync"
    "time"
)

// Worker pool pattern
type Job struct {
    ID   int
    Data string
}

type Result struct {
    JobID  int
    Output string
}

func worker(id int, jobs <-chan Job, results chan<- Result, wg *sync.WaitGroup) {
    defer wg.Done()
    for job := range jobs {
        time.Sleep(100 * time.Millisecond) // Simulate work
        result := Result{
            JobID:  job.ID,
            Output: fmt.Sprintf("Worker %d processed job %d: %s", id, job.ID, job.Data),
        }
        results <- result
    }
}

// Pipeline pattern
func generateNumbers(ctx context.Context, max int) <-chan int {
    numbers := make(chan int)
    go func() {
        defer close(numbers)
        for i := 1; i <= max; i++ {
            select {
            case numbers <- i:
            case <-ctx.Done():
                return
            }
        }
    }()
    return numbers
}

func squareNumbers(ctx context.Context, numbers <-chan int) <-chan int {
    squares := make(chan int)
    go func() {
        defer close(squares)
        for num := range numbers {
            select {
            case squares <- num * num:
            case <-ctx.Done():
                return
            }
        }
    }()
    return squares
}

func filterEven(ctx context.Context, numbers <-chan int) <-chan int {
    evens := make(chan int)
    go func() {
        defer close(evens)
        for num := range numbers {
            if num%2 == 0 {
                select {
                case evens <- num:
                case <-ctx.Done():
                    return
                }
            }
        }
    }()
    return evens
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
                out <- data * data // Square the number
            }
        }(output)
    }
    return outputs
}

func fanIn(inputs ...<-chan int) <-chan int {
    var wg sync.WaitGroup
    multiplexed := make(chan int)
    
    multiplex := func(c <-chan int) {
        defer wg.Done()
        for i := range c {
            multiplexed <- i
        }
    }
    
    wg.Add(len(inputs))
    for _, c := range inputs {
        go multiplex(c)
    }
    
    go func() {
        wg.Wait()
        close(multiplexed)
    }()
    
    return multiplexed
}

// Rate limiter
type RateLimiter struct {
    ticker *time.Ticker
    tokens chan struct{}
}

func NewRateLimiter(rate time.Duration, burst int) *RateLimiter {
    rl := &RateLimiter{
        ticker: time.NewTicker(rate),
        tokens: make(chan struct{}, burst),
    }
    
    // Fill the token bucket initially
    for i := 0; i < burst; i++ {
        rl.tokens <- struct{}{}
    }
    
    go func() {
        for range rl.ticker.C {
            select {
            case rl.tokens <- struct{}{}:
            default: // bucket is full
            }
        }
    }()
    
    return rl
}

func (rl *RateLimiter) Wait() {
    <-rl.tokens
}

func (rl *RateLimiter) Stop() {
    rl.ticker.Stop()
}

// Circuit breaker pattern
type CircuitBreaker struct {
    maxFailures  int
    resetTimeout time.Duration
    failures     int
    lastFailTime time.Time
    state        string // "closed", "open", "half-open"
    mutex        sync.RWMutex
}

func NewCircuitBreaker(maxFailures int, resetTimeout time.Duration) *CircuitBreaker {
    return &CircuitBreaker{
        maxFailures:  maxFailures,
        resetTimeout: resetTimeout,
        state:        "closed",
    }
}

func (cb *CircuitBreaker) Call(fn func() error) error {
    cb.mutex.Lock()
    defer cb.mutex.Unlock()
    
    if cb.state == "open" {
        if time.Since(cb.lastFailTime) > cb.resetTimeout {
            cb.state = "half-open"
            cb.failures = 0
        } else {
            return fmt.Errorf("circuit breaker is open")
        }
    }
    
    err := fn()
    if err != nil {
        cb.failures++
        cb.lastFailTime = time.Now()
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

func main() {
    fmt.Println("=== Advanced Concurrency Patterns Demo ===")
    
    // Worker Pool Demo
    fmt.Println("\n1. Worker Pool Pattern:")
    jobs := make(chan Job, 10)
    results := make(chan Result, 10)
    var wg sync.WaitGroup
    
    // Start 3 workers
    for i := 1; i <= 3; i++ {
        wg.Add(1)
        go worker(i, jobs, results, &wg)
    }
    
    // Send jobs
    for i := 1; i <= 5; i++ {
        jobs <- Job{ID: i, Data: fmt.Sprintf("data-%d", i)}
    }
    close(jobs)
    
    // Close results channel when all workers are done
    go func() {
        wg.Wait()
        close(results)
    }()
    
    // Collect results
    for result := range results {
        fmt.Printf("  %s\n", result.Output)
    }
    
    // Pipeline Demo
    fmt.Println("\n2. Pipeline Pattern:")
    ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
    defer cancel()
    
    numbers := generateNumbers(ctx, 20)
    squares := squareNumbers(ctx, numbers)
    evens := filterEven(ctx, squares)
    
    fmt.Print("  Even squares: ")
    for even := range evens {
        fmt.Printf("%d ", even)
    }
    fmt.Println()
    
    // Fan-out/Fan-in Demo
    fmt.Println("\n3. Fan-out/Fan-in Pattern:")
    input := make(chan int, 10)
    
    // Send input data
    go func() {
        defer close(input)
        for i := 1; i <= 5; i++ {
            input <- i
        }
    }()
    
    // Fan out to 3 workers
    outputs := fanOut(input, 3)
    
    // Fan in the results
    result := fanIn(outputs...)
    
    fmt.Print("  Squared results: ")
    for val := range result {
        fmt.Printf("%d ", val)
    }
    fmt.Println()
    
    // Rate Limiter Demo
    fmt.Println("\n4. Rate Limiter Pattern:")
    limiter := NewRateLimiter(200*time.Millisecond, 3)
    defer limiter.Stop()
    
    start := time.Now()
    for i := 1; i <= 5; i++ {
        limiter.Wait()
        fmt.Printf("  Request %d processed at %v\n", i, time.Since(start))
    }
    
    // Circuit Breaker Demo
    fmt.Println("\n5. Circuit Breaker Pattern:")
    cb := NewCircuitBreaker(2, 1*time.Second)
    
    // Simulate failing function
    failingFunc := func() error {
        return fmt.Errorf("service unavailable")
    }
    
    // Test circuit breaker
    for i := 1; i <= 5; i++ {
        err := cb.Call(failingFunc)
        if err != nil {
            fmt.Printf("  Call %d failed: %v\n", i, err)
        } else {
            fmt.Printf("  Call %d succeeded\n", i)
        }
    }
    
    fmt.Println("\n=== Demo completed ===")
}