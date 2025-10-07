package main

import (
	"context"
	"fmt"
	"time"
)

// 测试超时和截止时间模式
func testTimeoutPatterns() {
	fmt.Println("=== Testing timeout and deadline patterns ===")
	
	// 模式1: 使用context.WithTimeout
	fmt.Println("\n--- Pattern 1: context.WithTimeout ---")
	testContextWithTimeout()
	
	// 模式2: 使用time.After
	fmt.Println("\n--- Pattern 2: time.After ---")
	testTimeAfterTimeout()
	
	// 模式3: 使用context.WithDeadline
	fmt.Println("\n--- Pattern 3: context.WithDeadline ---")
	testContextWithDeadline()
	
	// 模式4: 级联超时
	fmt.Println("\n--- Pattern 4: cascading timeouts ---")
	testCascadingTimeouts()
	
	// 模式5: 超时与重试结合
	fmt.Println("\n--- Pattern 5: timeout with retry ---")
	testTimeoutWithRetry()
}

func testContextWithTimeout() {
	ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
	defer cancel()
	
	start := time.Now()
	select {
	case <-time.After(50 * time.Millisecond):
		fmt.Printf("Operation completed in %v (within timeout)\n", time.Since(start))
	case <-ctx.Done():
		fmt.Printf("Operation timed out after %v\n", time.Since(start))
	}
}

func testTimeAfterTimeout() {
	done := make(chan bool)
	
	go func() {
		time.Sleep(200 * time.Millisecond)
		done <- true
	}()
	
	select {
	case <-done:
		fmt.Println("Operation completed (within timeout)")
	case <-time.After(100 * time.Millisecond):
		fmt.Println("Operation timed out using time.After")
	}
}

func testContextWithDeadline() {
	deadline := time.Now().Add(100 * time.Millisecond)
	ctx, cancel := context.WithDeadline(context.Background(), deadline)
	defer cancel()
	
	select {
	case <-time.After(50 * time.Millisecond):
		fmt.Println("Operation completed before deadline")
	case <-ctx.Done():
		fmt.Printf("Operation missed deadline: %v\n", ctx.Err())
	}
}

func testCascadingTimeouts() {
	// 模拟一个请求链，每个层级都有自己的超时时间
	outerCtx, outerCancel := context.WithTimeout(context.Background(), 300*time.Millisecond)
	defer outerCancel()
	
	innerCtx, innerCancel := context.WithTimeout(outerCtx, 200*time.Millisecond)
	defer innerCancel()
	
	// 模拟一个需要250ms的操作
	operation := func(ctx context.Context) error {
		select {
		case <-time.After(250 * time.Millisecond):
			return nil // 操作完成
		case <-ctx.Done():
			return ctx.Err() // 超时
		}
	}
	
	start := time.Now()
	err := operation(innerCtx)
	elapsed := time.Since(start)
	
	if err != nil {
		fmt.Printf("Cascading timeout triggered after %v: %v\n", elapsed, err)
	} else {
		fmt.Printf("Operation completed in %v\n", elapsed)
	}
}

func testTimeoutWithRetry() {
	maxRetries := 3
	baseTimeout := 50 * time.Millisecond
	
	for attempt := 1; attempt <= maxRetries; attempt++ {
		fmt.Printf("Attempt %d with timeout %v...\n", attempt, baseTimeout)
		
		ctx, cancel := context.WithTimeout(context.Background(), baseTimeout)
		
		// 模拟一个可能超时的操作
		success := make(chan bool)
		go func() {
			// 模拟耗时操作，随attempt增加而减少耗时
			sleepTime := time.Duration(100-attempt*20) * time.Millisecond
			time.Sleep(sleepTime)
			success <- true
		}()
		
		select {
		case <-success:
			fmt.Printf("Attempt %d succeeded!\n", attempt)
			cancel()
			return
		case <-ctx.Done():
			fmt.Printf("Attempt %d timed out\n", attempt)
			cancel()
			baseTimeout *= 2 // 指数退避
			time.Sleep(10 * time.Millisecond) // 重试间隔
		}
	}
	
	fmt.Println("All attempts failed")
}

// 测试退避策略
func testBackoffStrategies() {
	fmt.Println("\n=== Testing backoff strategies ===")
	
	// 线性退避
	fmt.Println("\n--- Linear backoff ---")
	testLinearBackoff()
	
	// 指数退避
	fmt.Println("\n--- Exponential backoff ---")
	testExponentialBackoff()
	
	// 带抖动的退避
	fmt.Println("\n--- Jittered backoff ---")
	testJitteredBackoff()
}

func testLinearBackoff() {
	baseDelay := 10 * time.Millisecond
	maxDelay := 100 * time.Millisecond
	
	for attempt := 1; attempt <= 5; attempt++ {
		delay := baseDelay * time.Duration(attempt)
		if delay > maxDelay {
			delay = maxDelay
		}
		fmt.Printf("Attempt %d: waiting %v\n", attempt, delay)
		time.Sleep(delay)
	}
}

func testExponentialBackoff() {
	baseDelay := 10 * time.Millisecond
	maxDelay := 200 * time.Millisecond
	
	for attempt := 1; attempt <= 5; attempt++ {
		delay := baseDelay * time.Duration(1<<uint(attempt-1)) // 2^(attempt-1)
		if delay > maxDelay {
			delay = maxDelay
		}
		fmt.Printf("Attempt %d: waiting %v\n", attempt, delay)
		time.Sleep(delay)
	}
}

func testJitteredBackoff() {
	baseDelay := 50 * time.Millisecond
	
	for attempt := 1; attempt <= 3; attempt++ {
		// 添加随机抖动 (±25%)
		jitter := time.Duration(float64(baseDelay) * (0.75 + 0.5*0.5))
		fmt.Printf("Attempt %d: jittered delay %v\n", attempt, jitter)
		time.Sleep(jitter)
	}
}

func main() {
	testTimeoutPatterns()
	testBackoffStrategies()
	fmt.Println("\n=== Timeout and backoff tests completed ===")
}