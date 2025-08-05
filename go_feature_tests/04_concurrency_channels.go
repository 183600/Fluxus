package main

import (
	"fmt"
	"sync"
	"time"
)

// 1. Basic goroutine
func simpleGoroutine(id int) {
	fmt.Printf("Goroutine %d is running\n", id)
}

// 2. Goroutine with channels
func producer(ch chan<- int) {
	for i := 0; i < 5; i++ {
		ch <- i
		fmt.Printf("Produced: %d\n", i)
	}
	close(ch)
}

func consumer(ch <-chan int) {
	for value := range ch {
		fmt.Printf("Consumed: %d\n", value)
	}
}

// 3. Select statement
func selectExample() {
	ch1 := make(chan string)
	ch2 := make(chan string)

	go func() {
		time.Sleep(1 * time.Second)
		ch1 <- "from ch1"
	}()

	go func() {
		time.Sleep(2 * time.Second)
		ch2 <- "from ch2"
	}()

	for i := 0; i < 2; i++ {
		select {
		case msg1 := <-ch1:
			fmt.Println("Received:", msg1)
		case msg2 := <-ch2:
			fmt.Println("Received:", msg2)
		case <-time.After(3 * time.Second):
			fmt.Println("Timeout!")
		}
	}
}

// 4. Worker pool pattern
func worker(id int, jobs <-chan int, results chan<- int, wg *sync.WaitGroup) {
	defer wg.Done()
	for job := range jobs {
		fmt.Printf("Worker %d processing job %d\n", id, job)
		time.Sleep(100 * time.Millisecond) // Simulate work
		results <- job * 2
	}
}

func workerPool() {
	jobs := make(chan int, 10)
	results := make(chan int, 10)
	var wg sync.WaitGroup

	// Start workers
	for w := 1; w <= 3; w++ {
		wg.Add(1)
		go worker(w, jobs, results, &wg)
	}

	// Send jobs
	for j := 1; j <= 5; j++ {
		jobs <- j
	}
	close(jobs)

	// Wait for workers to finish
	go func() {
		wg.Wait()
		close(results)
	}()

	// Collect results
	for result := range results {
		fmt.Printf("Result: %d\n", result)
	}
}

// 5. Mutex for synchronization
type Counter struct {
	mu    sync.Mutex
	value int
}

func (c *Counter) Increment() {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.value++
}

func (c *Counter) Value() int {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.value
}

func testMutex() {
	counter := &Counter{}
	var wg sync.WaitGroup

	// Start multiple goroutines to increment counter
	for i := 0; i < 10; i++ {
		wg.Add(1)
		go func(id int) {
			defer wg.Done()
			for j := 0; j < 100; j++ {
				counter.Increment()
			}
			fmt.Printf("Goroutine %d finished\n", id)
		}(i)
	}

	wg.Wait()
	fmt.Printf("Final counter value: %d\n", counter.Value())
}

// 6. Channel directions
func sender(ch chan<- int) {
	for i := 0; i < 3; i++ {
		ch <- i
	}
	close(ch)
}

func receiver(ch <-chan int) {
	for value := range ch {
		fmt.Printf("Received from directional channel: %d\n", value)
	}
}

func main() {
	fmt.Println("=== Concurrency and Channels Test ===")

	// Test 1: Basic goroutines
	fmt.Println("\n1. Basic Goroutines:")
	for i := 1; i <= 3; i++ {
		go simpleGoroutine(i)
	}
	time.Sleep(100 * time.Millisecond) // Wait for goroutines

	// Test 2: Producer-Consumer
	fmt.Println("\n2. Producer-Consumer Pattern:")
	ch := make(chan int, 3)
	go producer(ch)
	consumer(ch)

	// Test 3: Select statement
	fmt.Println("\n3. Select Statement:")
	selectExample()

	// Test 4: Worker pool
	fmt.Println("\n4. Worker Pool:")
	workerPool()

	// Test 5: Mutex
	fmt.Println("\n5. Mutex Synchronization:")
	testMutex()

	// Test 6: Channel directions
	fmt.Println("\n6. Channel Directions:")
	dirCh := make(chan int)
	go sender(dirCh)
	receiver(dirCh)

	fmt.Println("\n=== Concurrency Tests Completed ===")
}