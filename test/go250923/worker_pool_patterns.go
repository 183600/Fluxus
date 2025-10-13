package main

import (
	"fmt"
	"sync"
	"time"
)

// 基础工作池模式
func testBasicWorkerPool() {
	fmt.Println("=== Testing basic worker pool ===")
	
	jobs := make(chan int, 10)
	results := make(chan int, 10)
	
	// 启动3个worker
	numWorkers := 3
	var wg sync.WaitGroup
	
	for w := 1; w <= numWorkers; w++ {
		wg.Add(1)
		go worker(w, jobs, results, &wg)
	}
	
	// 发送5个任务
	numJobs := 5
	for j := 1; j <= numJobs; j++ {
		jobs <- j
	}
	close(jobs)
	
	// 收集结果
	go func() {
		wg.Wait()
		close(results)
	}()
	
	// 打印结果
	for result := range results {
		fmt.Printf("Result: %d\n", result)
	}
}

func worker(id int, jobs <-chan int, results chan<- int, wg *sync.WaitGroup) {
	defer wg.Done()
	for job := range jobs {
		fmt.Printf("Worker %d processing job %d\n", id, job)
		time.Sleep(100 * time.Millisecond)
		results <- job * 2
	}
	fmt.Printf("Worker %d finished\n", id)
}

// 带错误处理的工作池
type JobResult struct {
	JobID  int
	Result int
	Err    error
}

func testWorkerPoolWithErrors() {
	fmt.Println("\n=== Testing worker pool with error handling ===")
	
	jobs := make(chan int, 10)
	results := make(chan JobResult, 10)
	
	numWorkers := 2
	var wg sync.WaitGroup
	
	for w := 1; w <= numWorkers; w++ {
		wg.Add(1)
		go workerWithError(w, jobs, results, &wg)
	}
	
	// 发送任务，其中一些会失败（偶数任务）
	numJobs := 6
	for j := 1; j <= numJobs; j++ {
		jobs <- j
	}
	close(jobs)
	
	go func() {
		wg.Wait()
		close(results)
	}()
	
	// 处理结果和错误
	successCount := 0
	errorCount := 0
	for result := range results {
		if result.Err != nil {
			fmt.Printf("Job %d failed: %v\n", result.JobID, result.Err)
			errorCount++
		} else {
			fmt.Printf("Job %d succeeded: %d\n", result.JobID, result.Result)
			successCount++
		}
	}
	fmt.Printf("Success: %d, Errors: %d\n", successCount, errorCount)
}

func workerWithError(id int, jobs <-chan int, results chan<- JobResult, wg *sync.WaitGroup) {
	defer wg.Done()
	for job := range jobs {
		// 模拟：偶数任务会失败
		if job%2 == 0 {
			results <- JobResult{
				JobID: job,
				Err:   fmt.Errorf("worker %d: job %d failed (even number)", id, job),
			}
		} else {
			time.Sleep(50 * time.Millisecond)
			results <- JobResult{
				JobID:  job,
				Result: job * 2,
				Err:    nil,
			}
		}
	}
}

// 动态工作池大小
type Task struct {
	ID   int
	Load int // 任务负载
}

func testDynamicWorkerPool() {
	fmt.Println("\n=== Testing dynamic worker pool ===")
	
	tasks := make(chan Task, 20)
	results := make(chan int, 20)
	done := make(chan bool)
	
	// 根据任务负载动态调整worker数量
	var workerWg sync.WaitGroup
	currentWorkers := 2
	
	// 启动初始worker
	for i := 0; i < currentWorkers; i++ {
		workerWg.Add(1)
		go dynamicWorker(i, tasks, results, &workerWg)
	}
	
	// 任务生成器
	go func() {
		for i := 1; i <= 10; i++ {
			tasks <- Task{ID: i, Load: i % 3}
		}
		close(tasks)
	}()
	
	// 结果收集器
	go func() {
		workerWg.Wait()
		close(results)
		done <- true
	}()
	
	// 收集结果
	for result := range results {
		fmt.Printf("Task completed: %d\n", result)
	}
	
	<-done
	fmt.Printf("Completed with %d workers\n", currentWorkers)
}

func dynamicWorker(id int, tasks <-chan Task, results chan<- int, wg *sync.WaitGroup) {
	defer wg.Done()
	for task := range tasks {
		// 根据负载调整处理时间
		processingTime := time.Duration(task.Load*50) * time.Millisecond
		time.Sleep(processingTime)
		fmt.Printf("Worker %d completed task %d (load: %d)\n", id, task.ID, task.Load)
		results <- task.ID
	}
}

// 带取消的工作池
func testWorkerPoolWithCancellation() {
	fmt.Println("\n=== Testing worker pool with cancellation ===")
	
	jobs := make(chan int, 20)
	results := make(chan int, 20)
	cancel := make(chan struct{})
	var wg sync.WaitGroup
	
	// 启动worker
	numWorkers := 3
	for w := 1; w <= numWorkers; w++ {
		wg.Add(1)
		go cancellableWorker(w, jobs, results, cancel, &wg)
	}
	
	// 发送任务
	go func() {
		for j := 1; j <= 20; j++ {
			select {
			case jobs <- j:
			case <-cancel:
				return
			}
		}
		close(jobs)
	}()
	
	// 模拟：接收5个结果后取消
	received := 0
	for result := range results {
		received++
		fmt.Printf("Received result: %d\n", result)
		if received >= 5 {
			fmt.Println("Cancelling remaining work...")
			close(cancel)
			break
		}
	}
	
	// 清空结果通道
	go func() {
		for range results {
		}
	}()
	
	wg.Wait()
	fmt.Println("All workers stopped")
}

func cancellableWorker(id int, jobs <-chan int, results chan<- int, cancel <-chan struct{}, wg *sync.WaitGroup) {
	defer wg.Done()
	for {
		select {
		case job, ok := <-jobs:
			if !ok {
				return
			}
			// 模拟工作
			time.Sleep(100 * time.Millisecond)
			select {
			case results <- job * 2:
				fmt.Printf("Worker %d completed job %d\n", id, job)
			case <-cancel:
				fmt.Printf("Worker %d cancelled\n", id)
				return
			}
		case <-cancel:
			fmt.Printf("Worker %d cancelled\n", id)
			return
		}
	}
}

// 优先级工作池
type PriorityJob struct {
	ID       int
	Priority int // 1=高, 2=中, 3=低
}

func testPriorityWorkerPool() {
	fmt.Println("\n=== Testing priority worker pool ===")
	
	highPriority := make(chan PriorityJob, 10)
	mediumPriority := make(chan PriorityJob, 10)
	lowPriority := make(chan PriorityJob, 10)
	results := make(chan int, 30)
	var wg sync.WaitGroup
	
	// 启动worker
	numWorkers := 2
	for w := 1; w <= numWorkers; w++ {
		wg.Add(1)
		go priorityWorker(w, highPriority, mediumPriority, lowPriority, results, &wg)
	}
	
	// 发送不同优先级的任务
	go func() {
		for i := 1; i <= 9; i++ {
			switch i % 3 {
			case 0:
				highPriority <- PriorityJob{ID: i, Priority: 1}
			case 1:
				mediumPriority <- PriorityJob{ID: i, Priority: 2}
			case 2:
				lowPriority <- PriorityJob{ID: i, Priority: 3}
			}
		}
		close(highPriority)
		close(mediumPriority)
		close(lowPriority)
	}()
	
	go func() {
		wg.Wait()
		close(results)
	}()
	
	// 收集结果
	for result := range results {
		fmt.Printf("Completed job: %d\n", result)
	}
}

func priorityWorker(id int, high, medium, low <-chan PriorityJob, results chan<- int, wg *sync.WaitGroup) {
	defer wg.Done()
	for {
		select {
		case job, ok := <-high:
			if !ok {
				high = nil
			} else {
				fmt.Printf("Worker %d: HIGH priority job %d\n", id, job.ID)
				time.Sleep(50 * time.Millisecond)
				results <- job.ID
			}
		default:
			select {
			case job, ok := <-high:
				if !ok {
					high = nil
				} else {
					fmt.Printf("Worker %d: HIGH priority job %d\n", id, job.ID)
					time.Sleep(50 * time.Millisecond)
					results <- job.ID
				}
			case job, ok := <-medium:
				if !ok {
					medium = nil
				} else {
					fmt.Printf("Worker %d: MEDIUM priority job %d\n", id, job.ID)
					time.Sleep(50 * time.Millisecond)
					results <- job.ID
				}
			case job, ok := <-low:
				if !ok {
					low = nil
				} else {
					fmt.Printf("Worker %d: LOW priority job %d\n", id, job.ID)
					time.Sleep(50 * time.Millisecond)
					results <- job.ID
				}
			default:
				if high == nil && medium == nil && low == nil {
					return
				}
				time.Sleep(10 * time.Millisecond)
			}
		}
	}
}

func main() {
	testBasicWorkerPool()
	testWorkerPoolWithErrors()
	testDynamicWorkerPool()
	testWorkerPoolWithCancellation()
	testPriorityWorkerPool()
	fmt.Println("\n=== Worker pool patterns tests completed ===")
}
