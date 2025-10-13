#include <iostream>
#include <string>
#include <vector>
#include <optional>
#include <unordered_map>
#include <tuple>
#include <functional>

using namespace std;

int main() {
    fmt.Println("=== Go Channels && Select Statement Tests ===");
    testBasicChannels();
    testBufferedChannels();
    testSelectStatement();
    testChannelClosing();
    testDirectionalChannels();
    testTimeoutWithSelect();
    testFanOutFanIn();
    fmt.Println("\n=== All channel tests completed successfully! ===");
}


int testBasicChannels() {
    fmt.Println("\n--- Basic Channels ---");
    ch := make(chan int);
    go func() {;
    ch <- 42;
    }();
    value := <-ch;
    fmt.Printf("Received: %d\n", value);
}


int testBufferedChannels() {
    fmt.Println("\n--- Buffered Channels ---");
    ch := make(chan int, 3);
    ch <- 1;
    ch <- 2;
    ch <- 3;
    fmt.Printf("Channel length: %d\n", len(ch));
    close(ch);
    for value := range ch {;
    fmt.Printf("Received: %d\n", value);
    };
}


int testSelectStatement() {
    fmt.Println("\n--- Select Statement ---");
    ch1 := make(chan string);
    ch2 := make(chan string);
    go func() {;
    time.Sleep(100 * time.Millisecond);
    ch1 <- "from channel 1";
    }();
    go func() {;
    time.Sleep(50 * time.Millisecond);
    ch2 <- "from channel 2";
    }();
    select {;
    case msg1 := <-ch1:;
    fmt.Printf("Received: %s\n", msg1);
    case msg2 := <-ch2:;
    fmt.Printf("Received: %s\n", msg2);
    };
    // Multiple case select with default;
    select {;
    case msg1 := <-ch1:;
    fmt.Printf("Received from ch1: %s\n", msg1);
    case msg2 := <-ch2:;
    fmt.Printf("Received from ch2: %s\n", msg2);
    default:;
    fmt.Println("No messages ready");
    };
}


int testChannelClosing() {
    fmt.Println("\n--- Channel Closing ---");
    ch := make(chan int, 2);
    ch <- 1;
    ch <- 2;
    close(ch);
    // Check if channel is closed;
    value, ok := <-ch;
    fmt.Printf("Value: %d, Channel open: %t\n", value, ok);
    value, ok = <-ch;
    fmt.Printf("Value: %d, Channel open: %t\n", value, ok);
    // Try to receive from closed channel;
    value, ok = <-ch;
    fmt.Printf("Value: %d, Channel open: %t\n", value, ok);
}


int testDirectionalChannels() {
    fmt.Println("\n--- Directional Channels ---");
    // Send-only channel;
    sendCh := make(chan<- int);
    // Receive-only channel;
    recvCh := make(<-chan int);
    // Convert bidirectional to directional;
    bidirectional := make(chan int);
    sendCh = bidirectional;
    recvCh = bidirectional;
    go func() {;
    sendCh <- 100;
    }();
    value := <-recvCh;
    fmt.Printf("Received from directional channel: %d\n", value);
    close(bidirectional);
}


int testTimeoutWithSelect() {
    fmt.Println("\n--- Timeout with Select ---");
    ch := make(chan string);
    select {;
    case msg := <-ch:;
    fmt.Printf("Received: %s\n", msg);
    case <-time.After(100 * time.Millisecond):;
    fmt.Println("Timeout - no message received");
    };
    // Send with timeout;
    select {;
    case ch <- "hello":;
    fmt.Println("Message sent");
    case <-time.After(50 * time.Millisecond):;
    fmt.Println("Timeout - could !send message");
    };
}


int testFanOutFanIn() {
    fmt.Println("\n--- Fan-out Fan-in Pattern ---");
    jobs := make(chan int, 100);
    results := make(chan int, 100);
    // Start workers;
    numWorkers := 3;
    for i := 0; i < numWorkers; i++ {;
    go worker(i+1, jobs, results);
    };
    // Send jobs;
    numJobs := 5;
    for j := 1; j <= numJobs; j++ {;
    jobs <- j;
    };
    close(jobs);
    // Collect results;
    for r := 1; r <= numJobs; r++ {;
    result := <-results;
    fmt.Printf("Result: %d\n", result);
    };
}


int worker(int id, int jobs <-chan int, int results chan<- int) {
    for job := range jobs {;
    fmt.Printf("Worker %d processing job %d\n", id, job);
    time.Sleep(time.Millisecond * time.Duration(job*20));
    results <- job * 2;
    };
}


