#!/usr/bin/env python3
"""
Advanced Concurrency and Asynchronous Programming in Python
"""

import asyncio
import aiohttp
import concurrent.futures
import multiprocessing
import queue
import threading
import time
import random
from typing import List, Dict, Callable, Any, Optional
from dataclasses import dataclass
from contextlib import asynccontextmanager


@dataclass
class Task:
    """Represents a task for processing."""
    id: int
    data: str
    priority: int = 1
    
    def __lt__(self, other):
        return self.priority < other.priority


class ThreadPool:
    """Custom thread pool implementation."""
    
    def __init__(self, max_workers: int = 4):
        self.max_workers = max_workers
        self.workers = []
        self.task_queue = queue.Queue()
        self.result_queue = queue.Queue()
        self.shutdown_event = threading.Event()
        self._start_workers()
    
    def _start_workers(self):
        """Start worker threads."""
        for i in range(self.max_workers):
            worker = threading.Thread(target=self._worker, args=(i,))
            worker.daemon = True
            worker.start()
            self.workers.append(worker)
    
    def _worker(self, worker_id: int):
        """Worker thread function."""
        print(f"Worker {worker_id} started")
        
        while not self.shutdown_event.is_set():
            try:
                task_func, args, kwargs = self.task_queue.get(timeout=1)
                result = task_func(*args, **kwargs)
                self.result_queue.put(result)
                self.task_queue.task_done()
            except queue.Empty:
                continue
            except Exception as e:
                self.result_queue.put(f"Error: {e}")
                self.task_queue.task_done()
        
        print(f"Worker {worker_id} stopped")
    
    def submit(self, func: Callable, *args, **kwargs):
        """Submit a task to the thread pool."""
        self.task_queue.put((func, args, kwargs))
    
    def get_result(self, timeout: Optional[float] = None):
        """Get a result from the result queue."""
        try:
            return self.result_queue.get(timeout=timeout)
        except queue.Empty:
            return None
    
    def shutdown(self):
        """Shutdown the thread pool."""
        self.shutdown_event.set()
        for worker in self.workers:
            worker.join()


class ProducerConsumer:
    """Producer-Consumer pattern implementation."""
    
    def __init__(self, buffer_size: int = 10):
        self.buffer = queue.Queue(maxsize=buffer_size)
        self.producers_done = threading.Event()
        self.consumers_done = threading.Event()
    
    def producer(self, producer_id: int, items: List[Any]):
        """Producer function."""
        print(f"Producer {producer_id} started")
        for item in items:
            self.buffer.put(item)
            print(f"Producer {producer_id} produced: {item}")
            time.sleep(random.uniform(0.1, 0.3))
        print(f"Producer {producer_id} finished")
    
    def consumer(self, consumer_id: int):
        """Consumer function."""
        print(f"Consumer {consumer_id} started")
        while not (self.producers_done.is_set() and self.buffer.empty()):
            try:
                item = self.buffer.get(timeout=1)
                print(f"Consumer {consumer_id} consumed: {item}")
                time.sleep(random.uniform(0.1, 0.5))
                self.buffer.task_done()
            except queue.Empty:
                continue
        print(f"Consumer {consumer_id} finished")
    
    def run_simulation(self, num_producers: int = 2, num_consumers: int = 3):
        """Run producer-consumer simulation."""
        threads = []
        
        # Start producers
        for i in range(num_producers):
            items = [f"item_{i}_{j}" for j in range(5)]
            producer_thread = threading.Thread(
                target=self.producer, args=(i, items)
            )
            producer_thread.start()
            threads.append(producer_thread)
        
        # Start consumers
        for i in range(num_consumers):
            consumer_thread = threading.Thread(
                target=self.consumer, args=(i,)
            )
            consumer_thread.start()
            threads.append(consumer_thread)
        
        # Wait for producers to finish
        for thread in threads[:num_producers]:
            thread.join()
        self.producers_done.set()
        
        # Wait for consumers to finish
        for thread in threads[num_producers:]:
            thread.join()


class AsyncWebCrawler:
    """Asynchronous web crawler."""
    
    def __init__(self, max_concurrent: int = 10):
        self.max_concurrent = max_concurrent
        self.semaphore = asyncio.Semaphore(max_concurrent)
        self.session = None
    
    async def __aenter__(self):
        self.session = aiohttp.ClientSession()
        return self
    
    async def __aexit__(self, exc_type, exc_val, exc_tb):
        if self.session:
            await self.session.close()
    
    async def fetch_url(self, url: str) -> Dict[str, Any]:
        """Fetch a single URL."""
        async with self.semaphore:
            try:
                async with self.session.get(url, timeout=aiohttp.ClientTimeout(total=5)) as response:
                    content = await response.text()
                    return {
                        'url': url,
                        'status': response.status,
                        'content_length': len(content),
                        'success': True
                    }
            except Exception as e:
                return {
                    'url': url,
                    'error': str(e),
                    'success': False
                }
    
    async def crawl_urls(self, urls: List[str]) -> List[Dict[str, Any]]:
        """Crawl multiple URLs concurrently."""
        tasks = [self.fetch_url(url) for url in urls]
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        processed_results = []
        for result in results:
            if isinstance(result, Exception):
                processed_results.append({
                    'error': str(result),
                    'success': False
                })
            else:
                processed_results.append(result)
        
        return processed_results


class RateLimiter:
    """Token bucket rate limiter."""
    
    def __init__(self, rate: float, burst: int):
        self.rate = rate  # tokens per second
        self.burst = burst  # maximum tokens
        self.tokens = burst
        self.last_update = time.time()
        self.lock = threading.Lock()
    
    def acquire(self, tokens: int = 1) -> bool:
        """Try to acquire tokens."""
        with self.lock:
            now = time.time()
            elapsed = now - self.last_update
            self.tokens = min(self.burst, self.tokens + elapsed * self.rate)
            self.last_update = now
            
            if self.tokens >= tokens:
                self.tokens -= tokens
                return True
            return False
    
    def wait_for_token(self, tokens: int = 1):
        """Wait until tokens are available."""
        while not self.acquire(tokens):
            time.sleep(0.1)


class CircuitBreaker:
    """Circuit breaker pattern implementation."""
    
    def __init__(self, failure_threshold: int = 5, timeout: float = 60):
        self.failure_threshold = failure_threshold
        self.timeout = timeout
        self.failure_count = 0
        self.last_failure_time = None
        self.state = 'CLOSED'  # CLOSED, OPEN, HALF_OPEN
        self.lock = threading.Lock()
    
    def call(self, func: Callable, *args, **kwargs):
        """Call a function through the circuit breaker."""
        with self.lock:
            if self.state == 'OPEN':
                if time.time() - self.last_failure_time > self.timeout:
                    self.state = 'HALF_OPEN'
                else:
                    raise Exception("Circuit breaker is OPEN")
            
            try:
                result = func(*args, **kwargs)
                if self.state == 'HALF_OPEN':
                    self.state = 'CLOSED'
                    self.failure_count = 0
                return result
            except Exception as e:
                self.failure_count += 1
                self.last_failure_time = time.time()
                
                if self.failure_count >= self.failure_threshold:
                    self.state = 'OPEN'
                
                raise e


def cpu_intensive_task(n: int) -> int:
    """CPU-intensive task for multiprocessing demo."""
    total = 0
    for i in range(n):
        total += i ** 2
    return total


def parallel_processing_demo():
    """Demonstrate parallel processing with multiprocessing."""
    print("\n=== Parallel Processing Demo ===")
    
    # Data for processing
    data = [1000000, 2000000, 1500000, 800000, 1200000]
    
    # Sequential processing
    start_time = time.time()
    sequential_results = [cpu_intensive_task(n) for n in data]
    sequential_time = time.time() - start_time
    
    print(f"Sequential processing took: {sequential_time:.2f} seconds")
    
    # Parallel processing
    start_time = time.time()
    with multiprocessing.Pool() as pool:
        parallel_results = pool.map(cpu_intensive_task, data)
    parallel_time = time.time() - start_time
    
    print(f"Parallel processing took: {parallel_time:.2f} seconds")
    print(f"Speedup: {sequential_time / parallel_time:.2f}x")
    print(f"Results match: {sequential_results == parallel_results}")


async def async_pipeline_demo():
    """Demonstrate async pipeline processing."""
    print("\n=== Async Pipeline Demo ===")
    
    async def generate_numbers(count: int):
        """Generate numbers asynchronously."""
        for i in range(count):
            await asyncio.sleep(0.1)
            yield i
    
    async def square_numbers(numbers):
        """Square numbers in the pipeline."""
        async for num in numbers:
            await asyncio.sleep(0.05)
            yield num ** 2
    
    async def filter_even(numbers):
        """Filter even numbers in the pipeline."""
        async for num in numbers:
            if num % 2 == 0:
                yield num
    
    # Process pipeline
    numbers = generate_numbers(10)
    squared = square_numbers(numbers)
    evens = filter_even(squared)
    
    results = []
    async for num in evens:
        results.append(num)
        print(f"Pipeline result: {num}")
    
    print(f"Final results: {results}")


def worker_function(task_id: int, duration: float) -> str:
    """Worker function for thread pool demo."""
    print(f"Processing task {task_id}...")
    time.sleep(duration)
    return f"Task {task_id} completed in {duration:.2f}s"


async def main():
    """Main async function demonstrating various concurrency patterns."""
    print("=== Advanced Concurrency Patterns Demo ===")
    
    # 1. Thread Pool Demo
    print("\n1. Thread Pool Demo:")
    pool = ThreadPool(max_workers=3)
    
    # Submit tasks
    tasks = [(i, random.uniform(0.5, 2.0)) for i in range(5)]
    for task_id, duration in tasks:
        pool.submit(worker_function, task_id, duration)
    
    # Collect results
    results = []
    for _ in range(len(tasks)):
        result = pool.get_result(timeout=5)
        if result:
            results.append(result)
            print(f"  {result}")
    
    pool.shutdown()
    
    # 2. Producer-Consumer Demo
    print("\n2. Producer-Consumer Demo:")
    pc = ProducerConsumer(buffer_size=5)
    pc.run_simulation(num_producers=2, num_consumers=2)
    
    # 3. Rate Limiter Demo
    print("\n3. Rate Limiter Demo:")
    limiter = RateLimiter(rate=2.0, burst=5)  # 2 tokens per second, burst of 5
    
    print("Making rate-limited requests...")
    for i in range(8):
        start = time.time()
        limiter.wait_for_token()
        elapsed = time.time() - start
        print(f"  Request {i+1} processed after {elapsed:.2f}s wait")
    
    # 4. Circuit Breaker Demo
    print("\n4. Circuit Breaker Demo:")
    cb = CircuitBreaker(failure_threshold=3, timeout=2)
    
    def unreliable_service(fail: bool = False):
        if fail:
            raise Exception("Service unavailable")
        return "Service response"
    
    # Test circuit breaker
    for i in range(8):
        try:
            # First 5 calls will fail, triggering circuit breaker
            fail = i < 5
            result = cb.call(unreliable_service, fail=fail)
            print(f"  Call {i+1}: {result}")
        except Exception as e:
            print(f"  Call {i+1}: Failed - {e}")
        time.sleep(0.5)
    
    # 5. Async Web Crawler Demo (simulated)
    print("\n5. Async Pipeline Demo:")
    await async_pipeline_demo()
    
    # 6. Parallel Processing Demo
    parallel_processing_demo()


if __name__ == "__main__":
    # Run the async main function
    asyncio.run(main())