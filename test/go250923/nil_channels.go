package main

import (
	"fmt"
	"time"
)

// 测试nil通道在select中的行为
func testNilChannels() {
	fmt.Println("=== Testing nil channels in select ===")
	
	var ch1 chan int  // nil通道
	ch2 := make(chan int)
	
	// 测试1: nil通道永远不会被选中
	go func() {
		time.Sleep(100 * time.Millisecond)
		ch2 <- 42
	}()
	
	select {
	case <-ch1:  // 永远不会触发，因为ch1是nil
		fmt.Println("Received from nil channel (unexpected)")
	case <-ch2:
		fmt.Println("Received from ch2: 42")
	case <-time.After(200 * time.Millisecond):
		fmt.Println("Timeout (unexpected)")
	}
	
	// 测试2: 动态设置通道为nil
	ch3 := make(chan int)
	ch4 := make(chan int)
	
	go func() {
		time.Sleep(50 * time.Millisecond)
		ch3 <- 100
		ch3 = nil  // 设置为nil
		time.Sleep(50 * time.Millisecond)
		ch4 <- 200
	}()
	
	// 第一次select，ch3可用
	select {
	case val := <-ch3:
		fmt.Printf("First select received: %d\n", val)
	case val := <-ch4:
		fmt.Printf("First select received from ch4: %d\n", val)
	}
	
	// 第二次select，ch3为nil，只能接收ch4
	select {
	case val := <-ch3:
		fmt.Printf("Second select received from ch3: %d (unexpected)\n", val)
	case val := <-ch4:
		fmt.Printf("Second select received from ch4: %d\n", val)
	case <-time.After(100 * time.Millisecond):
		fmt.Println("Second select timeout (unexpected)")
	}
}

// 测试关闭nil通道的panic
func testCloseNilChannel() {
	fmt.Println("\n=== Testing close nil channel ===")
	
	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("Recovered from panic: %v\n", r)
		}
	}()
	
	var nilChan chan int
	// close(nilChan)  // 这会panic，注释掉以避免程序崩溃
	fmt.Println("Closing nil channel would cause panic (commented out for safety)")
}

// 测试nil通道的阻塞行为
func testNilChannelBlocking() {
	fmt.Println("\n=== Testing nil channel blocking behavior ===")
	
	done := make(chan bool)
	
	// 启动一个goroutine尝试从nil通道接收
	go func() {
		fmt.Println("Goroutine: attempting to receive from nil channel...")
		// 使用nil通道字面量
		select {
		case <-(chan string)(nil):  // 直接使用类型转换
			fmt.Println("Goroutine received from nil channel (unexpected)")
		case <-time.After(100 * time.Millisecond):
			fmt.Println("Goroutine: timed out waiting for nil channel (expected)")
		}
		done <- true
	}()
	
	<-done
}

func main() {
	testNilChannels()
	testCloseNilChannel()
	fmt.Println("\n=== Nil channel tests completed ===")
}