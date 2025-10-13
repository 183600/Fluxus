package main

import (
	"context"
	"fmt"
	"sync"
	"time"
)

// 基础管道模式
func generator(nums ...int) <-chan int {
	out := make(chan int)
	go func() {
		defer close(out)
		for _, n := range nums {
			out <- n
		}
	}()
	return out
}

func square(in <-chan int) <-chan int {
	out := make(chan int)
	go func() {
		defer close(out)
		for n := range in {
			out <- n * n
		}
	}()
	return out
}

func testBasicPipeline() {
	fmt.Println("=== Testing basic pipeline ===")
	
	// 设置管道: generator -> square
	numbers := generator(1, 2, 3, 4, 5)
	squares := square(numbers)
	
	// 消费结果
	for n := range squares {
		fmt.Printf("Square: %d\n", n)
	}
}

// 带错误处理的管道
type Result struct {
	Value int
	Err   error
}

func generatorWithError(nums ...int) <-chan Result {
	out := make(chan Result)
	go func() {
		defer close(out)
		for _, n := range nums {
			// 模拟错误：负数会产生错误
			if n < 0 {
				out <- Result{Err: fmt.Errorf("negative number: %d", n)}
			} else {
				out <- Result{Value: n}
			}
		}
	}()
	return out
}

func squareWithError(in <-chan Result) <-chan Result {
	out := make(chan Result)
	go func() {
		defer close(out)
		for res := range in {
			if res.Err != nil {
				out <- res
			} else {
				out <- Result{Value: res.Value * res.Value}
			}
		}
	}()
	return out
}

func testPipelineWithError() {
	fmt.Println("\n=== Testing pipeline with error handling ===")
	
	numbers := generatorWithError(1, -2, 3, 4, -5)
	squares := squareWithError(numbers)
	
	for res := range squares {
		if res.Err != nil {
			fmt.Printf("Error: %v\n", res.Err)
		} else {
			fmt.Printf("Square: %d\n", res.Value)
		}
	}
}

// Fan-out/Fan-in 模式
func fanOut(in <-chan int, workers int) []<-chan int {
	channels := make([]<-chan int, workers)
	for i := 0; i < workers; i++ {
		channels[i] = worker(in, i)
	}
	return channels
}

func worker(in <-chan int, id int) <-chan int {
	out := make(chan int)
	go func() {
		defer close(out)
		for n := range in {
			// 模拟耗时处理
			time.Sleep(50 * time.Millisecond)
			result := n * n
			fmt.Printf("Worker %d: %d -> %d\n", id, n, result)
			out <- result
		}
	}()
	return out
}

func fanIn(channels ...<-chan int) <-chan int {
	out := make(chan int)
	var wg sync.WaitGroup
	
	multiplex := func(ch <-chan int) {
		defer wg.Done()
		for v := range ch {
			out <- v
		}
	}
	
	wg.Add(len(channels))
	for _, ch := range channels {
		go multiplex(ch)
	}
	
	go func() {
		wg.Wait()
		close(out)
	}()
	
	return out
}

func testFanOutFanIn() {
	fmt.Println("\n=== Testing fan-out/fan-in pattern ===")
	
	// 生成数据
	nums := generator(1, 2, 3, 4, 5, 6)
	
	// Fan-out: 使用3个worker并行处理
	workerChannels := fanOut(nums, 3)
	
	// Fan-in: 合并所有worker的输出
	results := fanIn(workerChannels...)
	
	// 收集结果
	var resultSlice []int
	for r := range results {
		resultSlice = append(resultSlice, r)
	}
	fmt.Printf("Results: %v\n", resultSlice)
}

// 带取消的管道
func generatorWithCancel(ctx context.Context, nums ...int) <-chan int {
	out := make(chan int)
	go func() {
		defer close(out)
		for _, n := range nums {
			select {
			case out <- n:
			case <-ctx.Done():
				fmt.Println("Generator cancelled")
				return
			}
		}
	}()
	return out
}

func squareWithCancel(ctx context.Context, in <-chan int) <-chan int {
	out := make(chan int)
	go func() {
		defer close(out)
		for {
			select {
			case n, ok := <-in:
				if !ok {
					return
				}
				select {
				case out <- n * n:
				case <-ctx.Done():
					fmt.Println("Square stage cancelled")
					return
				}
			case <-ctx.Done():
				fmt.Println("Square stage cancelled")
				return
			}
		}
	}()
	return out
}

func testPipelineWithCancellation() {
	fmt.Println("\n=== Testing pipeline with cancellation ===")
	
	ctx, cancel := context.WithCancel(context.Background())
	
	// 创建管道
	nums := generatorWithCancel(ctx, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
	squares := squareWithCancel(ctx, nums)
	
	// 处理前3个结果后取消
	count := 0
	for n := range squares {
		fmt.Printf("Received: %d\n", n)
		count++
		if count >= 3 {
			fmt.Println("Cancelling pipeline...")
			cancel()
			break
		}
	}
	
	// 清空管道
	for range squares {
	}
	
	time.Sleep(100 * time.Millisecond)
}

// 有界并行管道
func boundedParallel(ctx context.Context, in <-chan int, maxGoroutines int) <-chan int {
	out := make(chan int)
	var wg sync.WaitGroup
	
	// 使用信号量限制并发
	sem := make(chan struct{}, maxGoroutines)
	
	go func() {
		defer close(out)
		for n := range in {
			n := n // 捕获循环变量
			
			// 获取信号量
			select {
			case sem <- struct{}{}:
			case <-ctx.Done():
				return
			}
			
			wg.Add(1)
			go func() {
				defer wg.Done()
				defer func() { <-sem }() // 释放信号量
				
				// 模拟耗时操作
				time.Sleep(50 * time.Millisecond)
				
				select {
				case out <- n * n:
				case <-ctx.Done():
					return
				}
			}()
		}
		
		wg.Wait()
	}()
	
	return out
}

func testBoundedParallel() {
	fmt.Println("\n=== Testing bounded parallel pipeline ===")
	
	ctx := context.Background()
	nums := generator(1, 2, 3, 4, 5, 6, 7, 8)
	
	// 限制最多2个并发goroutine
	results := boundedParallel(ctx, nums, 2)
	
	for r := range results {
		fmt.Printf("Result: %d\n", r)
	}
}

// 分阶段管道
func stage1(in <-chan int) <-chan int {
	out := make(chan int)
	go func() {
		defer close(out)
		for n := range in {
			fmt.Printf("Stage 1: processing %d\n", n)
			time.Sleep(20 * time.Millisecond)
			out <- n * 2
		}
	}()
	return out
}

func stage2(in <-chan int) <-chan int {
	out := make(chan int)
	go func() {
		defer close(out)
		for n := range in {
			fmt.Printf("Stage 2: processing %d\n", n)
			time.Sleep(20 * time.Millisecond)
			out <- n + 10
		}
	}()
	return out
}

func stage3(in <-chan int) <-chan int {
	out := make(chan int)
	go func() {
		defer close(out)
		for n := range in {
			fmt.Printf("Stage 3: processing %d\n", n)
			time.Sleep(20 * time.Millisecond)
			out <- n * n
		}
	}()
	return out
}

func testMultiStagePipeline() {
	fmt.Println("\n=== Testing multi-stage pipeline ===")
	
	// 创建多阶段管道
	input := generator(1, 2, 3, 4)
	s1 := stage1(input)
	s2 := stage2(s1)
	s3 := stage3(s2)
	
	// 收集结果
	for result := range s3 {
		fmt.Printf("Final result: %d\n", result)
	}
}

func main() {
	testBasicPipeline()
	testPipelineWithError()
	testFanOutFanIn()
	testPipelineWithCancellation()
	testBoundedParallel()
	testMultiStagePipeline()
	fmt.Println("\n=== Pipeline patterns tests completed ===")
}
