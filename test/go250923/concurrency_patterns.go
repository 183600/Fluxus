package main

import (
	"context"
	"fmt"
	"sync"
	"time"
)

func main() {
	fmt.Println("=== Basic Goroutine Example ===")
	basicGoroutineExample()

	fmt.Println("\n=== Channel Example ===")
	channelExample()

	fmt.Println("\n=== Worker Pool Example ===")
	workerPoolExample()

	fmt.Println("\n=== Fan Out / Fan In Example ===")
	fanOutFanInExample()

	fmt.Println("\n=== Context Example ===")
	contextExample()

	fmt.Println("\n=== Mutex Example ===")
	mutexExample()

	fmt.Println("\n=== WaitGroup Example ===")
	waitGroupExample()

	fmt.Println("\n=== Select Example ===")
	selectExample()
}

func basicGoroutineExample() {
	var wg sync.WaitGroup

	for i := 1; i <= 3; i++ {
		wg.Add(1)
		go func(id int) {
			defer wg.Done()
			fmt.Printf("Goroutine %d started\n", id)
			time.Sleep(time.Duration(id) * 100 * time.Millisecond)
			fmt.Printf("Goroutine %d finished\n", id)
		}(i)
	}

	wg.Wait()
	fmt.Println("All goroutines finished")
}

func channelExample() {
	ch := make(chan int)

	go func() {
		for i := 1; i <= 5; i++ {
			ch <- i
			fmt.Printf("Sent: %d\n", i)
		}
		close(ch)
	}()

	for num := range ch {
		fmt.Printf("Received: %d\n", num)
	}

	fmt.Println("Channel closed")
}

func workerPoolExample() {
	const numWorkers = 3
	const numJobs = 5

	jobs := make(chan int, numJobs)
	results := make(chan int, numJobs)

	var wg sync.WaitGroup

	for w := 1; w <= numWorkers; w++ {
		wg.Add(1)
		go worker(w, jobs, results, &wg)
	}

	for j := 1; j <= numJobs; j++ {
		jobs <- j
	}
	close(jobs)

	go func() {
		wg.Wait()
		close(results)
	}()

	for result := range results {
		fmt.Printf("Worker result: %d\n", result)
	}
}

func worker(id int, jobs <-chan int, results chan<- int, wg *sync.WaitGroup) {
	defer wg.Done()
	for j := range jobs {
		fmt.Printf("Worker %d processing job %d\n", id, j)
		time.Sleep(100 * time.Millisecond)
		results <- j * 2
	}
}

func fanOutFanInExample() {
	jobs := make(chan int, 10)
	results := make(chan int, 10)

	go fanOut(jobs, results)
	go fanIn(results)

	for i := 1; i <= 5; i++ {
		jobs <- i
	}
	close(jobs)

	for result := range results {
		fmt.Printf("Fan-in result: %d\n", result)
	}
}

func fanOut(jobs <-chan int, results chan<- int) {
	var wg sync.WaitGroup
	for job := range jobs {
		wg.Add(1)
		go func(j int) {
			defer wg.Done()
			square := j * j
			fmt.Printf("Fan-out: %d squared is %d\n", j, square)
			results <- square
		}(job)
	}
	wg.Wait()
	close(results)
}

func fanIn(results <-chan int) {
	for result := range results {
		fmt.Printf("Fan-in received: %d\n", result)
	}
}

func contextExample() {
	ctx, cancel := context.WithTimeout(context.Background(), 200*time.Millisecond)
	defer cancel()

	go func() {
		select {
		case <-time.After(300 * time.Millisecond):
			fmt.Println("Operation completed (should not happen)")
		case <-ctx.Done():
			fmt.Println("Operation cancelled by context:", ctx.Err())
		}
	}()

	time.Sleep(100 * time.Millisecond)
	fmt.Println("Context example completed")
}

func mutexExample() {
	var counter int
	var mutex sync.Mutex
	var wg sync.WaitGroup

	const numGoroutines = 5

	for i := 0; i < numGoroutines; i++ {
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

func waitGroupExample() {
	var wg sync.WaitGroup

	for i := 1; i <= 3; i++ {
		wg.Add(1)
		go func(id int) {
			defer wg.Done()
			fmt.Printf("Task %d started\n", id)
			time.Sleep(time.Duration(id) * 50 * time.Millisecond)
			fmt.Printf("Task %d completed\n", id)
		}(i)
	}

	fmt.Println("Waiting for all tasks to complete...")
	wg.Wait()
	fmt.Println("All tasks completed")
}

func selectExample() {
	ch1 := make(chan string)
	ch2 := make(chan string)

	go func() {
		time.Sleep(100 * time.Millisecond)
		ch1 <- "from channel 1"
	}()

	go func() {
		time.Sleep(150 * time.Millisecond)
		ch2 <- "from channel 2"
	}()

	for i := 0; i < 2; i++ {
		select {
		case msg1 := <-ch1:
			fmt.Printf("Received: %s\n", msg1)
		case msg2 := <-ch2:
			fmt.Printf("Received: %s\n", msg2)
		case <-time.After(200 * time.Millisecond):
			fmt.Println("Timeout reached")
		}
	}

	fmt.Println("Select example completed")
}