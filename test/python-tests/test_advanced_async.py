# Test Advanced Async/Await and Coroutine Features

import asyncio
import time
from concurrent.futures import ThreadPoolExecutor
import aiohttp  # Note: This requires aiohttp package

async def simple_coroutine():
    """Simple coroutine demonstrating basic async/await"""
    print("Coroutine started")
    await asyncio.sleep(1)
    print("Coroutine completed")
    return "coroutine result"

async def multiple_coroutines():
    """Running multiple coroutines concurrently"""
    print("Starting multiple coroutines")

    tasks = [
        asyncio.create_task(async_operation(i)) for i in range(3)
    ]

    results = await asyncio.gather(*tasks)
    print(f"All results: {results}")
    return results

async def async_operation(id):
    """Individual async operation"""
    print(f"Operation {id} started")
    await asyncio.sleep(id * 0.5)
    print(f"Operation {id} completed")
    return f"result_{id}"

async def sequential_vs_parallel():
    """Compare sequential vs parallel execution"""
    print("=== Sequential Execution ===")
    start = time.time()

    await async_operation(1)
    await async_operation(2)
    await async_operation(3)

    sequential_time = time.time() - start
    print(f"Sequential time: {sequential_time:.2f}s")

    print("\n=== Parallel Execution ===")
    start = time.time()

    await asyncio.gather(
        async_operation(1),
        async_operation(2),
        async_operation(3)
    )

    parallel_time = time.time() - start
    print(f"Parallel time: {parallel_time:.2f}s")

async def async_with_timeout():
    """Async operation with timeout"""
    try:
        async with asyncio.timeout(0.5):
            await asyncio.sleep(1)
            print("This should not be reached")
    except asyncio.TimeoutError:
        print("Timeout occurred as expected")

async def async_context_manager():
    """Async context manager example"""
    class AsyncContextManager:
        async def __aenter__(self):
            print("Entering async context")
            await asyncio.sleep(0.1)
            return self

        async def __aexit__(self, exc_type, exc_val, exc_tb):
            print("Exiting async context")
            await asyncio.sleep(0.1)
            return False

    async with AsyncContextManager() as manager:
        print("Inside async context")
        print(f"Manager: {manager}")

async def async_iterators():
    """Async iterator examples"""
    class AsyncCounter:
        def __init__(self, limit):
            self.limit = limit
            self.count = 0

        def __aiter__(self):
            return self

        async def __anext__(self):
            if self.count >= self.limit:
                raise StopAsyncIteration
            await asyncio.sleep(0.1)
            self.count += 1
            return self.count

    print("=== Async Iterator ===")
    async for num in AsyncCounter(3):
        print(f"Async iteration: {num}")

async def async_generators():
    """Async generator examples"""
    async def async_range(start, end):
        for i in range(start, end):
            await asyncio.sleep(0.1)
            yield i

    print("=== Async Generator ===")
    async for num in async_range(1, 4):
        print(f"Generated: {num}")

    # Async generator comprehension
    gen = (i async for i in async_range(10, 13))
    async for num in gen:
        print(f"Generated from comprehension: {num}")

async def async_exception_handling():
    """Exception handling in async code"""
    async def failing_coroutine():
        await asyncio.sleep(0.1)
        raise ValueError("Intentional error")

    try:
        await failing_coroutine()
    except ValueError as e:
        print(f"Caught expected error: {e}")

    # Exception handling in gather
    tasks = [
        asyncio.create_task(async_operation(1)),
        asyncio.create_task(failing_coroutine()),
        asyncio.create_task(async_operation(2))
    ]

    try:
        results = await asyncio.gather(*tasks)
    except Exception as e:
        print(f"Gather caught error: {e}")

    # Gather with return_exceptions
    tasks = [
        asyncio.create_task(async_operation(1)),
        asyncio.create_task(failing_coroutine()),
        asyncio.create_task(async_operation(2))
    ]

    results = await asyncio.gather(*tasks, return_exceptions=True)
    print(f"Results with exceptions: {results}")

async def producer_consumer_pattern():
    """Producer-consumer pattern with asyncio.Queue"""
    queue = asyncio.Queue(maxsize=5)

    async def producer():
        for i in range(5):
            print(f"Producing item {i}")
            await queue.put(f"item_{i}")
            await asyncio.sleep(0.1)
        await queue.put(None)  # Sentinel value

    async def consumer():
        while True:
            item = await queue.get()
            if item is None:
                break
            print(f"Consuming item {item}")
            await asyncio.sleep(0.2)
            queue.task_done()

    producer_task = asyncio.create_task(producer())
    consumer_task = asyncio.create_task(consumer())

    await producer_task
    await consumer_task

async def async_lock_semaphore():
    """Synchronization primitives"""
    # Async lock
    lock = asyncio.Lock()

    async def locked_operation(id):
        async with lock:
            print(f"Task {id} acquired lock")
            await asyncio.sleep(0.1)
            print(f"Task {id} released lock")

    tasks = [asyncio.create_task(locked_operation(i)) for i in range(3)]
    await asyncio.gather(*tasks)

    # Semaphore
    semaphore = asyncio.Semaphore(2)

    async def limited_operation(id):
        async with semaphore:
            print(f"Task {id} started")
            await asyncio.sleep(0.1)
            print(f"Task {id} completed")

    tasks = [asyncio.create_task(limited_operation(i)) for i in range(5)]
    await asyncio.gather(*tasks)

async def async_event_condition():
    """Event and Condition variables"""
    # Event
    event = asyncio.Event()

    async def event_waiter():
        print("Waiting for event...")
        await event.wait()
        print("Event occurred!")

    async def event_setter():
        await asyncio.sleep(0.1)
        print("Setting event")
        event.set()

    await asyncio.gather(
        event_waiter(),
        event_setter()
    )

    # Condition
    condition = asyncio.Condition()
    shared_data = []

    async def producer_with_condition():
        async with condition:
            shared_data.append("data")
            print("Producer added data")
            condition.notify()

    async def consumer_with_condition():
        async with condition:
            while not shared_data:
                print("Consumer waiting for data...")
                await condition.wait()
            print(f"Consumer got data: {shared_data.pop()}")

    await asyncio.gather(
        producer_with_condition(),
        consumer_with_condition()
    )

async def thread_pool_integration():
    """Integration with thread pool executor"""

    def blocking_operation(seconds):
        """Simulate blocking operation"""
        time.sleep(seconds)
        return f"Blocked for {seconds} seconds"

    loop = asyncio.get_running_loop()

    # Run blocking operation in thread pool
    result = await loop.run_in_executor(
        ThreadPoolExecutor(max_workers=2),
        blocking_operation,
        1
    )

    print(f"Blocking operation result: {result}")

async def advanced_cancel_tasks():
    """Task cancellation"""

    async def long_running_task():
        try:
            print("Long running task started")
            for i in range(10):
                await asyncio.sleep(0.1)
                print(f"Task progress: {i}/10")
        except asyncio.CancelledError:
            print("Task was cancelled")
            raise

    task = asyncio.create_task(long_running_task())
    await asyncio.sleep(0.3)

    print("Cancelling task...")
    task.cancel()

    try:
        await task
    except asyncio.CancelledError:
        print("Task cancelled successfully")

async def async_shield():
    """Shielding coroutines from cancellation"""

    async def shielded_operation():
        try:
            print("Shielded operation started")
            await asyncio.sleep(0.2)
            print("Shielded operation completed")
            return "shielded result"
        except asyncio.CancelledError:
            print("Shielded operation cancelled")
            raise

    try:
        result = await asyncio.shield(shielded_operation())
        print(f"Shielded result: {result}")
    except asyncio.CancelledError:
        print("Outer operation cancelled, but shielded completed")

# Test functions
async def test_basic_async():
    print("=== Basic Async Test ===")
    result = await simple_coroutine()
    print(f"Result: {result}")

async def test_multiple_coroutines():
    print("\n=== Multiple Coroutines Test ===")
    results = await multiple_coroutines()
    print(f"Results: {results}")

async def test_execution_patterns():
    print("\n=== Execution Patterns Test ===")
    await sequential_vs_parallel()

async def test_advanced_features():
    print("\n=== Advanced Features Test ===")
    await async_with_timeout()
    await async_context_manager()
    await async_iterators()
    await async_generators()

async def test_exception_handling():
    print("\n=== Exception Handling Test ===")
    await async_exception_handling()

async def test_patterns():
    print("\n=== Pattern Tests ===")
    await producer_consumer_pattern()
    await async_lock_semaphore()
    await async_event_condition()

async def test_integration():
    print("\n=== Integration Test ===")
    await thread_pool_integration()

async def test_cancellation():
    print("\n=== Cancellation Test ===")
    await advanced_cancel_tasks()
    await async_shield()

async def main():
    """Main test runner"""
    try:
        await test_basic_async()
        await test_multiple_coroutines()
        await test_execution_patterns()
        await test_advanced_features()
        await test_exception_handling()
        await test_patterns()
        await test_integration()
        await test_cancellation()

        print("\n=== All advanced async tests passed! ===")
    except Exception as e:
        print(f"Test failed: {e}")

if __name__ == "__main__":
    asyncio.run(main())