#!/usr/bin/env python3
"""
Advanced Python Asyncio and Concurrency Patterns
Including async generators, context managers, queues, locks, and advanced patterns
"""

import asyncio
import aiohttp
from typing import AsyncGenerator, AsyncContextManager, Optional, List, Dict, Tuple, Any, Callable
import time
import random
import weakref
from contextlib import asynccontextmanager

# Advanced Async Generators

async def fibonacci_async(n: int) -> AsyncGenerator[int, None]:
    """Async generator for Fibonacci numbers"""
    a, b = 0, 1
    for i in range(n):
        await asyncio.sleep(0.1)  # Simulate async work
        yield a
        a, b = b, a + b

async def infinite_counter_async() -> AsyncGenerator[int, None]:
    """Infinite async counter generator"""
    count = 0
    while True:
        await asyncio.sleep(0.5)
        yield count
        count += 1

async def streaming_data_async() -> AsyncGenerator[Dict[str, Any], None]:
    """Simulate streaming data with async generator"""
    for i in range(10):
        data = {
            'timestamp': time.time(),
            'value': random.randint(1, 100),
            'sensor_id': f'sensor_{i % 3}',
            'status': 'ok' if random.random() > 0.1 else 'error'
        }
        await asyncio.sleep(0.2)
        yield data

# Advanced Async Context Managers

class AsyncDatabaseConnection:
    """Async database connection context manager"""
    
    def __init__(self, connection_string: str):
        self.connection_string = connection_string
        self.connected = False
    
    async def __aenter__(self):
        print(f"Opening async database connection: {self.connection_string}")
        await asyncio.sleep(0.1)  # Simulate connection time
        self.connected = True
        return self
    
    async def __aexit__(self, exc_type, exc_val, exc_tb):
        print("Closing async database connection")
        await asyncio.sleep(0.1)  # Simulate disconnection time
        self.connected = False
        if exc_type:
            print(f"Exception occurred: {exc_val}")
        return False  # Don't suppress exceptions
    
    async def execute_query(self, query: str) -> List[Dict[str, Any]]:
        if not self.connected:
            raise RuntimeError("Database not connected")
        
        await asyncio.sleep(0.1)  # Simulate query execution
        return [
            {'id': 1, 'name': 'Alice', 'email': 'alice@example.com'},
            {'id': 2, 'name': 'Bob', 'email': 'bob@example.com'}
        ]

class AsyncFileManager:
    """Async file manager with automatic cleanup"""
    
    def __init__(self, filename: str, mode: str = 'r'):
        self.filename = filename
        self.mode = mode
        self.file = None
    
    async def __aenter__(self):
        print(f"Opening async file: {self.filename}")
        # Simulate async file opening
        await asyncio.sleep(0.01)
        self.file = f"mock_file_handle_{self.filename}"
        return self
    
    async def __aexit__(self, exc_type, exc_val, exc_tb):
        print(f"Closing async file: {self.filename}")
        # Simulate async file closing
        await asyncio.sleep(0.01)
        self.file = None
        return False
    
    async def read_lines(self) -> AsyncGenerator[str, None]:
        """Async generator for reading file lines"""
        # Simulate file content
        lines = [
            "Line 1: Hello, World!",
            "Line 2: Async file reading",
            "Line 3: With context managers"
        ]
        
        for line in lines:
            await asyncio.sleep(0.05)
            yield line
    
    async def write_line(self, line: str) -> None:
        """Write a line to the file"""
        await asyncio.sleep(0.05)
        print(f"Writing to {self.filename}: {line}")

class AsyncResourcePool:
    """Async resource pool for managing limited resources"""
    
    def __init__(self, max_resources: int = 5):
        self.max_resources = max_resources
        self.available_resources = asyncio.Queue(maxsize=max_resources)
        self.all_resources = []
        self.lock = asyncio.Lock()
    
    async def __aenter__(self):
        # Initialize resources
        for i in range(self.max_resources):
            resource = f"resource_{i}"
            await self.available_resources.put(resource)
            self.all_resources.append(resource)
        
        print(f"Initialized resource pool with {self.max_resources} resources")
        return self
    
    async def __aexit__(self, exc_type, exc_val, exc_tb):
        # Clean up all resources
        while not self.available_resources.empty():
            try:
                self.available_resources.get_nowait()
            except asyncio.QueueEmpty:
                break
        
        print("Resource pool cleaned up")
        return False
    
    async def acquire_resource(self, timeout: float = 5.0) -> str:
        """Acquire a resource from the pool"""
        try:
            resource = await asyncio.wait_for(self.available_resources.get(), timeout=timeout)
            print(f"Acquired resource: {resource}")
            return resource
        except asyncio.TimeoutError:
            raise RuntimeError(f"Timeout waiting for resource after {timeout}s")
    
    async def release_resource(self, resource: str) -> None:
        """Release a resource back to the pool"""
        if resource in self.all_resources:
            await self.available_resources.put(resource)
            print(f"Released resource: {resource}")
        else:
            raise ValueError(f"Unknown resource: {resource}")
    
    async def get_pool_stats(self) -> Dict[str, Any]:
        """Get current pool statistics"""
        return {
            'total_resources': self.max_resources,
            'available_resources': self.available_resources.qsize(),
            'in_use_resources': self.max_resources - self.available_resources.qsize()
        }

# Advanced Asyncio Synchronization Primitives

class AsyncReadWriteLock:
    """Async reader-writer lock implementation"""
    
    def __init__(self):
        self._readers = 0
        self._readers_lock = asyncio.Lock()
        self._resource_lock = asyncio.Lock()
    
    async def acquire_read(self):
        """Acquire read lock"""
        async with self._readers_lock:
            self._readers += 1
            if self._readers == 1:
                await self._resource_lock.acquire()
    
    async def release_read(self):
        """Release read lock"""
        async with self._readers_lock:
            self._readers -= 1
            if self._readers == 0:
                self._resource_lock.release()
    
    async def acquire_write(self):
        """Acquire write lock"""
        await self._resource_lock.acquire()
    
    async def release_write(self):
        """Release write lock"""
        self._resource_lock.release()
    
    @asynccontextmanager
    async def read_lock(self):
        """Context manager for read lock"""
        await self.acquire_read()
        try:
            yield
        finally:
            await self.release_read()
    
    @asynccontextmanager
    async def write_lock(self):
        """Context manager for write lock"""
        await self.acquire_write()
        try:
            yield
        finally:
            await self.release_write()

# Advanced Asyncio Patterns

class AsyncWorkerPool:
    """Async worker pool for parallel task execution"""
    
    def __init__(self, num_workers: int = 4):
        self.num_workers = num_workers
        self.workers = []
        self.task_queue = asyncio.Queue()
        self.results = []
        self.shutdown = False
    
    async def start(self):
        """Start the worker pool"""
        self.shutdown = False
        self.workers = [
            asyncio.create_task(self._worker(i))
            for i in range(self.num_workers)
        ]
        print(f"Started worker pool with {self.num_workers} workers")
    
    async def stop(self):
        """Stop the worker pool"""
        self.shutdown = True
        # Add sentinel values to wake up workers
        for _ in range(self.num_workers):
            await self.task_queue.put(None)
        
        # Wait for all workers to finish
        await asyncio.gather(*self.workers, return_exceptions=True)
        print("Worker pool stopped")
    
    async def submit_task(self, task_func, *args, **kwargs):
        """Submit a task to the pool"""
        await self.task_queue.put((task_func, args, kwargs))
    
    async def _worker(self, worker_id: int):
        """Worker coroutine"""
        while not self.shutdown:
            try:
                task_data = await self.task_queue.get()
                if task_data is None:  # Sentinel value
                    break
                
                task_func, args, kwargs = task_data
                result = await task_func(*args, **kwargs)
                self.results.append(result)
                print(f"Worker {worker_id} completed task: {result}")
                
            except Exception as e:
                print(f"Worker {worker_id} error: {e}")
    
    def get_results(self) -> List[Any]:
        """Get all completed results"""
        return self.results.copy()

class AsyncEventBus:
    """Async event bus for pub/sub pattern"""
    
    def __init__(self):
        self.subscribers = {}
        self.lock = asyncio.Lock()
    
    async def subscribe(self, event_type: str, callback: Callable) -> None:
        """Subscribe to an event type"""
        async with self.lock:
            if event_type not in self.subscribers:
                self.subscribers[event_type] = []
            self.subscribers[event_type].append(callback)
            print(f"Subscribed to event type: {event_type}")
    
    async def unsubscribe(self, event_type: str, callback: Callable) -> None:
        """Unsubscribe from an event type"""
        async with self.lock:
            if event_type in self.subscribers and callback in self.subscribers[event_type]:
                self.subscribers[event_type].remove(callback)
                print(f"Unsubscribed from event type: {event_type}")
    
    async def publish(self, event_type: str, data: Any) -> None:
        """Publish an event"""
        async with self.lock:
            if event_type in self.subscribers:
                callbacks = self.subscribers[event_type].copy()
            else:
                callbacks = []
        
        # Notify all subscribers
        tasks = []
        for callback in callbacks:
            task = asyncio.create_task(callback(event_type, data))
            tasks.append(task)
        
        if tasks:
            await asyncio.gather(*tasks, return_exceptions=True)
    
    async def publish_with_ack(self, event_type: str, data: Any, timeout: float = 5.0) -> int:
        """Publish an event and wait for acknowledgments"""
        async with self.lock:
            if event_type in self.subscribers:
                callbacks = self.subscribers[event_type].copy()
            else:
                return 0
        
        # Notify all subscribers with timeout
        ack_count = 0
        tasks = []
        
        for callback in callbacks:
            async def wrapped_callback(cb):
                try:
                    await asyncio.wait_for(cb(event_type, data), timeout=timeout)
                    return True
                except asyncio.TimeoutError:
                    return False
                except Exception:
                    return False
            
            task = asyncio.create_task(wrapped_callback(callback))
            tasks.append(task)
        
        if tasks:
            results = await asyncio.gather(*tasks, return_exceptions=True)
            ack_count = sum(1 for result in results if result is True)
        
        return ack_count

# Advanced Asyncio Task Management

class AsyncTaskManager:
    """Advanced async task manager with cancellation and monitoring"""
    
    def __init__(self):
        self.tasks = {}
        self.task_metadata = {}
        self.lock = asyncio.Lock()
    
    async def create_task(self, coro, task_name: str = None, metadata: Dict[str, Any] = None) -> asyncio.Task:
        """Create and track a new async task"""
        task = asyncio.create_task(coro)
        
        async with self.lock:
            self.tasks[task.get_name()] = task
            self.task_metadata[task.get_name()] = {
                'name': task_name or coro.__name__,
                'created_at': time.time(),
                'status': 'running',
                'metadata': metadata or {}
            }
        
        # Add done callback
        task.add_done_callback(self._task_done_callback)
        
        return task
    
    def _task_done_callback(self, task: asyncio.Task) -> None:
        """Callback when task is done"""
        task_name = task.get_name()
        if task_name in self.task_metadata:
            if task.cancelled():
                self.task_metadata[task_name]['status'] = 'cancelled'
            elif task.exception():
                self.task_metadata[task_name]['status'] = 'failed'
                self.task_metadata[task_name]['error'] = str(task.exception())
            else:
                self.task_metadata[task_name]['status'] = 'completed'
            
            self.task_metadata[task_name]['completed_at'] = time.time()
            self.task_metadata[task_name]['duration'] = (
                self.task_metadata[task_name]['completed_at'] - 
                self.task_metadata[task_name]['created_at']
            )
    
    async def cancel_task(self, task_name: str) -> bool:
        """Cancel a specific task"""
        async with self.lock:
            if task_name in self.tasks:
                task = self.tasks[task_name]
                task.cancel()
                return True
        return False
    
    async def cancel_all_tasks(self) -> int:
        """Cancel all running tasks"""
        cancelled_count = 0
        async with self.lock:
            for task in self.tasks.values():
                if not task.done():
                    task.cancel()
                    cancelled_count += 1
        return cancelled_count
    
    def get_task_status(self, task_name: str) -> Optional[Dict[str, Any]]:
        """Get status of a specific task"""
        return self.task_metadata.get(task_name)
    
    def get_all_tasks_status(self) -> Dict[str, Dict[str, Any]]:
        """Get status of all tasks"""
        return self.task_metadata.copy()
    
    def get_running_tasks(self) -> List[str]:
        """Get names of all running tasks"""
        return [
            name for name, metadata in self.task_metadata.items()
            if metadata['status'] == 'running'
        ]

# Test Functions

async def test_async_generators():
    """Test advanced async generators"""
    print("=== Testing Advanced Async Generators ===")
    
    # Test Fibonacci async generator
    print("\n--- Fibonacci Async Generator ---")
    async for num in fibonacci_async(10):
        print(f"Fibonacci: {num}")
    
    # Test streaming data async generator
    print("\n--- Streaming Data Async Generator ---")
    async for data in streaming_data_async():
        print(f"Sensor data: {data}")
        if data['sensor_id'] == 'sensor_2' and data['status'] == 'error':
            print("Found error in sensor_2, stopping stream")
            break

async def test_async_context_managers():
    """Test advanced async context managers"""
    print("\n=== Testing Advanced Async Context Managers ===")
    
    # Test async database connection
    print("\n--- Async Database Connection ---")
    async with AsyncDatabaseConnection("postgresql://localhost:5432/mydb") as db:
        results = await db.execute_query("SELECT * FROM users")
        print(f"Query results: {results}")
    
    # Test async file manager
    print("\n--- Async File Manager ---")
    async with AsyncFileManager("test.txt", "r") as file_manager:
        async for line in file_manager.read_lines():
            print(f"File line: {line}")
    
    # Test async resource pool
    print("\n--- Async Resource Pool ---")
    async with AsyncResourcePool(max_resources=3) as pool:
        # Acquire resources
        resource1 = await pool.acquire_resource()
        resource2 = await pool.acquire_resource()
        
        stats = await pool.get_pool_stats()
        print(f"Pool stats: {stats}")
        
        # Release resources
        await pool.release_resource(resource1)
        await pool.release_resource(resource2)
        
        final_stats = await pool.get_pool_stats()
        print(f"Final pool stats: {final_stats}")

async def test_async_synchronization():
    """Test async synchronization primitives"""
    print("\n=== Testing Async Synchronization ===")
    
    rw_lock = AsyncReadWriteLock()
    shared_data = []
    
    async def reader_task(task_id: int):
        """Reader task"""
        async with rw_lock.read_lock():
            print(f"Reader {task_id} reading data: {shared_data}")
            await asyncio.sleep(0.1)
            print(f"Reader {task_id} finished reading")
    
    async def writer_task(task_id: int, data: str):
        """Writer task"""
        async with rw_lock.write_lock():
            print(f"Writer {task_id} writing data: {data}")
            shared_data.append(data)
            await asyncio.sleep(0.2)
            print(f"Writer {task_id} finished writing")
    
    # Create reader and writer tasks
    tasks = []
    for i in range(5):
        tasks.append(asyncio.create_task(reader_task(i)))
        if i % 2 == 0:
            tasks.append(asyncio.create_task(writer_task(i, f"data_{i}")))
    
    await asyncio.gather(*tasks)
    print(f"Final shared data: {shared_data}")

async def test_async_worker_pool():
    """Test async worker pool"""
    print("\n=== Testing Async Worker Pool ===")
    
    async def dummy_task(task_id: int, duration: float):
        """Dummy async task"""
        print(f"Task {task_id} started (duration: {duration}s)")
        await asyncio.sleep(duration)
        print(f"Task {task_id} completed")
        return f"Task {task_id} result"
    
    # Create and start worker pool
    pool = AsyncWorkerPool(num_workers=3)
    await pool.start()
    
    # Submit tasks
    tasks_data = [
        (dummy_task, 1, 0.5),
        (dummy_task, 2, 1.0),
        (dummy_task, 3, 0.3),
        (dummy_task, 4, 0.8),
        (dummy_task, 5, 0.2),
    ]
    
    for task_func, task_id, duration in tasks_data:
        await pool.submit_task(task_func, task_id, duration)
    
    # Wait a bit for tasks to complete
    await asyncio.sleep(2.0)
    
    # Stop pool and get results
    await pool.stop()
    results = pool.get_results()
    print(f"Worker pool results: {results}")

async def test_async_event_bus():
    """Test async event bus"""
    print("\n=== Testing Async Event Bus ===")
    
    event_bus = AsyncEventBus()
    
    # Define event handlers
    async def user_created_handler(event_type: str, data: Any):
        print(f"User created handler received: {data}")
        await asyncio.sleep(0.1)
    
    async def email_sent_handler(event_type: str, data: Any):
        print(f"Email sent handler received: {data}")
        await asyncio.sleep(0.2)
    
    async def notification_handler(event_type: str, data: Any):
        print(f"Notification handler received: {data}")
        await asyncio.sleep(0.05)
    
    # Subscribe to events
    await event_bus.subscribe("user_created", user_created_handler)
    await event_bus.subscribe("email_sent", email_sent_handler)
    await event_bus.subscribe("user_created", notification_handler)
    
    # Publish events
    await event_bus.publish("user_created", {"user_id": 123, "name": "Alice"})
    await event_bus.publish("email_sent", {"email_id": 456, "recipient": "alice@example.com"})
    
    # Publish with acknowledgment
    ack_count = await event_bus.publish_with_ack("user_created", {"user_id": 789, "name": "Bob"}, timeout=0.5)
    print(f"Event acknowledged by {ack_count} handlers")

async def test_async_task_manager():
    """Test async task manager"""
    print("\n=== Testing Async Task Manager ===")
    
    task_manager = AsyncTaskManager()
    
    async def sample_task(task_id: int, duration: float):
        """Sample async task"""
        print(f"Task {task_id} started (duration: {duration}s)")
        await asyncio.sleep(duration)
        print(f"Task {task_id} completed")
        return f"Task {task_id} result"
    
    async def failing_task():
        """Task that will fail"""
        await asyncio.sleep(0.1)
        raise ValueError("Simulated task failure")
    
    # Create tasks
    task1 = await task_manager.create_task(sample_task(1, 0.5), "sample_task_1")
    task2 = await task_manager.create_task(sample_task(2, 1.0), "sample_task_2", {"priority": "high"})
    task3 = await task_manager.create_task(failing_task(), "failing_task")
    
    # Wait for tasks to complete
    await asyncio.sleep(1.5)
    
    # Check task status
    print("\nTask Status:")
    all_status = task_manager.get_all_tasks_status()
    for task_name, status in all_status.items():
        print(f"Task {task_name}: {status}")
    
    running_tasks = task_manager.get_running_tasks()
    print(f"Running tasks: {running_tasks}")

# Advanced Asyncio Networking

async def fetch_url(session: aiohttp.ClientSession, url: str, timeout: float = 10.0) -> str:
    """Fetch URL with timeout and error handling"""
    try:
        async with async_timeout.timeout(timeout):
            async with session.get(url) as response:
                if response.status == 200:
                    return await response.text()
                else:
                    return f"HTTP {response.status}"
    except asyncio.TimeoutError:
        return f"Timeout after {timeout}s"
    except Exception as e:
        return f"Error: {e}"

async def test_async_networking():
    """Test advanced async networking"""
    print("\n=== Testing Advanced Async Networking ===")
    
    urls = [
        "https://httpbin.org/delay/1",
        "https://httpbin.org/status/200",
        "https://httpbin.org/json",
        "https://httpbin.org/delay/2",
    ]
    
    async with aiohttp.ClientSession() as session:
        # Fetch multiple URLs concurrently
        tasks = [fetch_url(session, url) for url in urls]
        results = await asyncio.gather(*tasks)
        
        for url, result in zip(urls, results):
            result_preview = result[:100] + "..." if len(result) > 100 else result
            print(f"URL: {url}")
            print(f"Result: {result_preview}")
            print()

# Main test function

async def main():
    """Main test function"""
    print("=== Advanced Python Asyncio and Concurrency Patterns ===")
    
    await test_async_generators()
    await test_async_context_managers()
    await test_async_synchronization()
    await test_async_worker_pool()
    await test_async_event_bus()
    await test_async_task_manager()
    await test_async_networking()
    
    print("\n=== All asyncio tests completed successfully! ===")

if __name__ == "__main__":
    # Run all tests
    asyncio.run(main())