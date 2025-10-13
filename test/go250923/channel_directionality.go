package main

import (
	"fmt"
	"time"
)

// 测试通道方向性
func testChannelDirectionality() {
	fmt.Println("=== Testing channel directionality ===")
	
	// 双向通道
	bidirectional := make(chan int)
	
	// 只发送通道
	sendOnly := make(chan<- int)
	
	// 只接收通道
	receiveOnly := make(<-chan int)
	
	fmt.Printf("Bidirectional channel: %T\n", bidirectional)
	fmt.Printf("Send-only channel: %T\n", sendOnly)
	fmt.Printf("Receive-only channel: %T\n", receiveOnly)
	
	// 通道方向转换
	testChannelConversion()
	
	// 函数参数中的通道方向
	testChannelParameters()
	
	// 通道方向和goroutine
	testDirectionalChannelsWithGoroutines()
}

// 通道方向转换
func testChannelConversion() {
	fmt.Println("\n--- Channel direction conversion ---")
	
	ch := make(chan int, 1)
	
	// 双向通道可以转换为只发送或只接收通道
	var sendCh chan<- int = ch
	var recvCh <-chan int = ch
	
	sendCh <- 42
	val := <-recvCh
	fmt.Printf("Sent %d, received %d\n", 42, val)
}

// 函数参数中的通道方向
func sendData(ch chan<- int, data int) {
	ch <- data
	fmt.Printf("Sent: %d\n", data)
}

func receiveData(ch <-chan int) int {
	val := <-ch
	fmt.Printf("Received: %d\n", val)
	return val
}

func testChannelParameters() {
	fmt.Println("\n--- Channel parameters with directionality ---")
	
	ch := make(chan int, 1)
	
	sendData(ch, 100)
	receiveData(ch)
}

// 通道方向和goroutine
func producer(ch chan<- int, count int) {
	for i := 1; i <= count; i++ {
		ch <- i
		time.Sleep(10 * time.Millisecond)
	}
	close(ch)
}

func consumer(ch <-chan int, done chan<- bool) {
	sum := 0
	for val := range ch {
		sum += val
	}
	fmt.Printf("Consumer sum: %d\n", sum)
	done <- true
}

func testDirectionalChannelsWithGoroutines() {
	fmt.Println("\n--- Directional channels with goroutines ---")
	
	dataCh := make(chan int, 5)
	done := make(chan bool)
	
	go producer(dataCh, 5)
	go consumer(dataCh, done)
	
	<-done
}

// 测试缓冲通道与无缓冲通道
func testBufferedVsUnbuffered() {
	fmt.Println("\n=== Testing buffered vs unbuffered channels ===")
	
	// 无缓冲通道
	fmt.Println("\n--- Unbuffered channel ---")
	testUnbufferedChannel()
	
	// 缓冲通道
	fmt.Println("\n--- Buffered channel ---")
	testBufferedChannel()
	
	// 缓冲区满时的行为
	fmt.Println("\n--- Buffer full behavior ---")
	testBufferFull()
}

func testUnbufferedChannel() {
	ch := make(chan int) // 无缓冲
	
	go func() {
		fmt.Println("Goroutine: about to send")
		ch <- 42
		fmt.Println("Goroutine: sent")
	}()
	
	time.Sleep(50 * time.Millisecond)
	fmt.Println("Main: about to receive")
	val := <-ch
	fmt.Printf("Main: received %d\n", val)
}

func testBufferedChannel() {
	ch := make(chan int, 3) // 缓冲大小为3
	
	// 可以发送3个值而不阻塞
	ch <- 1
	ch <- 2
	ch <- 3
	fmt.Printf("Sent 3 values, buffer capacity: %d, length: %d\n", cap(ch), len(ch))
	
	// 接收值
	fmt.Printf("Received: %d\n", <-ch)
	fmt.Printf("Buffer length after one receive: %d\n", len(ch))
}

func testBufferFull() {
	ch := make(chan int, 2)
	
	ch <- 1
	ch <- 2
	fmt.Println("Buffer is full")
	
	// 第三次发送会阻塞，需要在goroutine中
	go func() {
		fmt.Println("Attempting to send third value...")
		ch <- 3
		fmt.Println("Third value sent after receiver consumed")
	}()
	
	time.Sleep(50 * time.Millisecond)
	fmt.Printf("Received: %d\n", <-ch)
	time.Sleep(50 * time.Millisecond)
}

// 通道关闭模式
func testChannelClosingPatterns() {
	fmt.Println("\n=== Testing channel closing patterns ===")
	
	// 模式1: 发送者关闭通道
	fmt.Println("\n--- Pattern 1: Sender closes ---")
	testSenderCloses()
	
	// 模式2: 多个接收者
	fmt.Println("\n--- Pattern 2: Multiple receivers ---")
	testMultipleReceivers()
	
	// 模式3: 检测通道关闭
	fmt.Println("\n--- Pattern 3: Detect channel close ---")
	testDetectClose()
}

func testSenderCloses() {
	ch := make(chan int, 3)
	
	// 发送者
	go func() {
		for i := 1; i <= 3; i++ {
			ch <- i
		}
		close(ch)
		fmt.Println("Sender closed the channel")
	}()
	
	// 接收者使用range
	for val := range ch {
		fmt.Printf("Received: %d\n", val)
	}
	fmt.Println("Range loop ended after channel close")
}

func testMultipleReceivers() {
	ch := make(chan int, 5)
	done := make(chan bool)
	
	// 发送者
	go func() {
		for i := 1; i <= 5; i++ {
			ch <- i
		}
		close(ch)
	}()
	
	// 多个接收者
	for i := 1; i <= 2; i++ {
		go func(id int) {
			for val := range ch {
				fmt.Printf("Receiver %d got: %d\n", id, val)
				time.Sleep(10 * time.Millisecond)
			}
			done <- true
		}(i)
	}
	
	// 等待所有接收者完成
	<-done
	<-done
}

func testDetectClose() {
	ch := make(chan int, 1)
	ch <- 42
	close(ch)
	
	// 使用ok检测通道是否关闭
	val, ok := <-ch
	fmt.Printf("First receive: val=%d, ok=%v\n", val, ok)
	
	val, ok = <-ch
	fmt.Printf("Second receive from closed channel: val=%d, ok=%v\n", val, ok)
}

func main() {
	testChannelDirectionality()
	testBufferedVsUnbuffered()
	testChannelClosingPatterns()
	fmt.Println("\n=== Channel directionality and buffering tests completed ===")
}
