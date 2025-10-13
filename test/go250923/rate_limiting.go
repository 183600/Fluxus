package main

import (
	"fmt"
	"sync"
	"time"
)

// 令牌桶算法实现
type TokenBucket struct {
	capacity   int
	tokens     int
	refillRate time.Duration
	mu         sync.Mutex
	lastRefill time.Time
}

func NewTokenBucket(capacity int, refillRate time.Duration) *TokenBucket {
	return &TokenBucket{
		capacity:   capacity,
		tokens:     capacity,
		refillRate: refillRate,
		lastRefill: time.Now(),
	}
}

func (tb *TokenBucket) refill() {
	now := time.Now()
	elapsed := now.Sub(tb.lastRefill)
	tokensToAdd := int(elapsed / tb.refillRate)
	
	if tokensToAdd > 0 {
		tb.tokens += tokensToAdd
		if tb.tokens > tb.capacity {
			tb.tokens = tb.capacity
		}
		tb.lastRefill = now
	}
}

func (tb *TokenBucket) Allow() bool {
	tb.mu.Lock()
	defer tb.mu.Unlock()
	
	tb.refill()
	
	if tb.tokens > 0 {
		tb.tokens--
		return true
	}
	return false
}

func testTokenBucket() {
	fmt.Println("=== Testing token bucket rate limiting ===")
	
	// 创建令牌桶：容量5，每100ms补充1个令牌
	bucket := NewTokenBucket(5, 100*time.Millisecond)
	
	// 快速发送10个请求
	for i := 1; i <= 10; i++ {
		if bucket.Allow() {
			fmt.Printf("Request %d: ALLOWED\n", i)
		} else {
			fmt.Printf("Request %d: RATE LIMITED\n", i)
		}
		time.Sleep(50 * time.Millisecond)
	}
}

// 漏桶算法实现
type LeakyBucket struct {
	capacity   int
	queue      []int
	leakRate   time.Duration
	mu         sync.Mutex
	lastLeak   time.Time
	leaking    bool
	stopLeak   chan struct{}
}

func NewLeakyBucket(capacity int, leakRate time.Duration) *LeakyBucket {
	lb := &LeakyBucket{
		capacity: capacity,
		queue:    make([]int, 0, capacity),
		leakRate: leakRate,
		lastLeak: time.Now(),
		stopLeak: make(chan struct{}),
	}
	go lb.leak()
	return lb
}

func (lb *LeakyBucket) leak() {
	ticker := time.NewTicker(lb.leakRate)
	defer ticker.Stop()
	
	for {
		select {
		case <-ticker.C:
			lb.mu.Lock()
			if len(lb.queue) > 0 {
				item := lb.queue[0]
				lb.queue = lb.queue[1:]
				fmt.Printf("Leaked item: %d\n", item)
			}
			lb.mu.Unlock()
		case <-lb.stopLeak:
			return
		}
	}
}

func (lb *LeakyBucket) Add(item int) bool {
	lb.mu.Lock()
	defer lb.mu.Unlock()
	
	if len(lb.queue) < lb.capacity {
		lb.queue = append(lb.queue, item)
		return true
	}
	return false
}

func (lb *LeakyBucket) Stop() {
	close(lb.stopLeak)
}

func testLeakyBucket() {
	fmt.Println("\n=== Testing leaky bucket rate limiting ===")
	
	// 创建漏桶：容量3，每200ms漏出1个
	bucket := NewLeakyBucket(3, 200*time.Millisecond)
	defer bucket.Stop()
	
	// 快速添加5个项目
	for i := 1; i <= 5; i++ {
		if bucket.Add(i) {
			fmt.Printf("Item %d: ADDED to bucket\n", i)
		} else {
			fmt.Printf("Item %d: REJECTED (bucket full)\n", i)
		}
		time.Sleep(50 * time.Millisecond)
	}
	
	// 等待漏桶排空
	time.Sleep(1 * time.Second)
}

// 固定窗口算法
type FixedWindow struct {
	limit      int
	window     time.Duration
	count      int
	windowEnd  time.Time
	mu         sync.Mutex
}

func NewFixedWindow(limit int, window time.Duration) *FixedWindow {
	return &FixedWindow{
		limit:     limit,
		window:    window,
		count:     0,
		windowEnd: time.Now().Add(window),
	}
}

func (fw *FixedWindow) Allow() bool {
	fw.mu.Lock()
	defer fw.mu.Unlock()
	
	now := time.Now()
	
	// 检查是否需要重置窗口
	if now.After(fw.windowEnd) {
		fw.count = 0
		fw.windowEnd = now.Add(fw.window)
	}
	
	// 检查是否达到限制
	if fw.count < fw.limit {
		fw.count++
		return true
	}
	return false
}

func testFixedWindow() {
	fmt.Println("\n=== Testing fixed window rate limiting ===")
	
	// 创建固定窗口：每秒最多5个请求
	limiter := NewFixedWindow(5, 1*time.Second)
	
	// 发送8个请求
	for i := 1; i <= 8; i++ {
		if limiter.Allow() {
			fmt.Printf("Request %d: ALLOWED (window count: %d)\n", i, limiter.count)
		} else {
			fmt.Printf("Request %d: RATE LIMITED\n", i)
		}
		time.Sleep(150 * time.Millisecond)
	}
	
	// 等待新窗口
	time.Sleep(500 * time.Millisecond)
	fmt.Println("New window started")
	
	// 再发送3个请求
	for i := 9; i <= 11; i++ {
		if limiter.Allow() {
			fmt.Printf("Request %d: ALLOWED (window count: %d)\n", i, limiter.count)
		} else {
			fmt.Printf("Request %d: RATE LIMITED\n", i)
		}
	}
}

// 滑动窗口算法
type SlidingWindow struct {
	limit      int
	window     time.Duration
	requests   []time.Time
	mu         sync.Mutex
}

func NewSlidingWindow(limit int, window time.Duration) *SlidingWindow {
	return &SlidingWindow{
		limit:    limit,
		window:   window,
		requests: make([]time.Time, 0),
	}
}

func (sw *SlidingWindow) Allow() bool {
	sw.mu.Lock()
	defer sw.mu.Unlock()
	
	now := time.Now()
	cutoff := now.Add(-sw.window)
	
	// 移除窗口外的请求
	validRequests := make([]time.Time, 0)
	for _, reqTime := range sw.requests {
		if reqTime.After(cutoff) {
			validRequests = append(validRequests, reqTime)
		}
	}
	sw.requests = validRequests
	
	// 检查是否达到限制
	if len(sw.requests) < sw.limit {
		sw.requests = append(sw.requests, now)
		return true
	}
	return false
}

func testSlidingWindow() {
	fmt.Println("\n=== Testing sliding window rate limiting ===")
	
	// 创建滑动窗口：500ms内最多3个请求
	limiter := NewSlidingWindow(3, 500*time.Millisecond)
	
	// 发送6个请求
	for i := 1; i <= 6; i++ {
		if limiter.Allow() {
			fmt.Printf("Request %d: ALLOWED (active requests: %d)\n", i, len(limiter.requests))
		} else {
			fmt.Printf("Request %d: RATE LIMITED\n", i)
		}
		time.Sleep(150 * time.Millisecond)
	}
}

// 并发速率限制
func testConcurrentRateLimiting() {
	fmt.Println("\n=== Testing concurrent rate limiting ===")
	
	bucket := NewTokenBucket(10, 50*time.Millisecond)
	var wg sync.WaitGroup
	
	// 10个并发goroutine，每个发送5个请求
	numGoroutines := 10
	requestsPerGoroutine := 5
	
	allowed := 0
	denied := 0
	var mu sync.Mutex
	
	for g := 1; g <= numGoroutines; g++ {
		wg.Add(1)
		go func(id int) {
			defer wg.Done()
			for r := 1; r <= requestsPerGoroutine; r++ {
				if bucket.Allow() {
					mu.Lock()
					allowed++
					mu.Unlock()
				} else {
					mu.Lock()
					denied++
					mu.Unlock()
				}
				time.Sleep(10 * time.Millisecond)
			}
		}(g)
	}
	
	wg.Wait()
	fmt.Printf("Total requests: %d, Allowed: %d, Denied: %d\n", allowed+denied, allowed, denied)
}

func main() {
	testTokenBucket()
	testLeakyBucket()
	testFixedWindow()
	testSlidingWindow()
	testConcurrentRateLimiting()
	fmt.Println("\n=== Rate limiting tests completed ===")
}
