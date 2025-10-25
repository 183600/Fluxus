// Generated C++ equivalent for test_concurrent_fib.go
// This shows what the compiler should generate

#include <iostream>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <queue>
#include <vector>
#include <functional>
#include <atomic>
#include <chrono>

// Generic Channel class for Go channels
template <typename T>
class Channel {
private:
    std::queue<T> queue_;
    std::mutex mutex_;
    std::condition_variable cv_;
    std::size_t capacity_;

public:
    Channel(std::size_t capacity) : capacity_(capacity) {}

    void send(T value) {
        std::unique_lock<std::mutex> lock(mutex_);
        cv_.wait(lock, [this]() { return queue_.size() < capacity_; });
        queue_.push(value);
        cv_.notify_one();
    }

    T receive() {
        std::unique_lock<std::mutex> lock(mutex_);
        cv_.wait(lock, [this]() { return !queue_.empty(); });
        T value = queue_.front();
        queue_.pop();
        cv_.notify_one();
        return value;
    }
};

// fibonacci calculates the nth Fibonacci number
int fibonacci(int n) {
    if (n <= 1) {
        return n;
    }
    return fibonacci(n-1) + fibonacci(n-2);
}

// concurrentFibonacci calculates Fibonacci numbers concurrently
void concurrentFibonacci(int n, Channel<int>& ch) {
    int result = fibonacci(n);
    ch.send(result);
}

int main() {
    // Calculate first 10 Fibonacci numbers
    std::cout << "Sequential Fibonacci:" << std::endl;
    for (int i = 0; i < 10; i++) {
        int result = fibonacci(i);
        std::cout << "fib(" << i << ") = " << result << std::endl;
    }

    // Calculate Fibonacci numbers concurrently
    std::cout << "\nConcurrent Fibonacci:" << std::endl;
    Channel<int> ch(5);
    
    // Launch goroutines as std::thread
    std::vector<std::thread> threads;
    for (int i = 5; i < 10; i++) {
        threads.emplace_back([i, &ch]() {
            concurrentFibonacci(i, ch);
        });
    }
    
    // Collect results
    for (int i = 0; i < 5; i++) {
        int result = ch.receive();
        std::cout << "Concurrent result: " << result << std::endl;
    }
    
    // Wait for all threads to complete
    for (auto& t : threads) {
        t.join();
    }
    
    return 0;
}