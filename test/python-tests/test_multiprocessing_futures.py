#!/usr/bin/env python3
"""
测试Python的multiprocessing和concurrent.futures高级特性
"""

import multiprocessing
import concurrent.futures
import time
import math
import os
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor, as_completed

def cpu_intensive_task(n):
    """CPU密集型任务"""
    result = 0
    for i in range(n * 1000000):
        result += math.sqrt(i)
    return result

def io_simulation_task(duration):
    """IO模拟任务"""
    time.sleep(duration)
    return f"IO task completed after {duration}s"

def test_multiprocessing_basics():
    """测试multiprocessing基础"""
    print("=== Testing Multiprocessing Basics ===")
    
    # 创建进程
    def worker_function(name, sleep_time):
        print(f"Worker {name} started (PID: {os.getpid()})")
        time.sleep(sleep_time)
        print(f"Worker {name} finished")
        return f"Result from {name}"
    
    # 使用Process创建进程
    processes = []
    for i in range(3):
        p = multiprocessing.Process(target=worker_function, args=(f"worker_{i}", 0.5))
        processes.append(p)
        p.start()
    
    # 等待所有进程完成
    for p in processes:
        p.join()
    
    print(f"Main process PID: {os.getpid()}")

def test_process_pool():
    """测试进程池"""
    print("\n=== Testing Process Pool ===")
    
    # 使用进程池执行CPU密集型任务
    numbers = [10, 20, 30, 40, 50]
    
    start_time = time.time()
    
    with multiprocessing.Pool(processes=4) as pool:
        # map方法
        results = pool.map(cpu_intensive_task, numbers)
        print(f"Pool.map results: {results}")
        
        # apply_async方法
        async_results = []
        for num in numbers:
            async_result = pool.apply_async(cpu_intensive_task, (num,))
            async_results.append(async_result)
        
        # 获取异步结果
        async_values = [result.get() for result in async_results]
        print(f"Pool.apply_async results: {async_values}")
    
    elapsed_time = time.time() - start_time
    print(f"Process pool completed in {elapsed_time:.2f} seconds")

def test_concurrent_futures_process_pool():
    """测试concurrent.futures的ProcessPoolExecutor"""
    print("\n=== Testing ProcessPoolExecutor ===")
    
    numbers = [5, 10, 15, 20, 25]
    
    start_time = time.time()
    
    with ProcessPoolExecutor(max_workers=4) as executor:
        # 使用map
        results = list(executor.map(cpu_intensive_task, numbers))
        print(f"ProcessPoolExecutor.map results: {results}")
        
        # 使用submit和as_completed
        futures = [executor.submit(cpu_intensive_task, num) for num in numbers]
        
        for future in as_completed(futures):
            try:
                result = future.result()
                print(f"Task completed with result: {result}")
            except Exception as e:
                print(f"Task generated an exception: {e}")
    
    elapsed_time = time.time() - start_time
    print(f"ProcessPoolExecutor completed in {elapsed_time:.2f} seconds")

def test_concurrent_futures_thread_pool():
    """测试concurrent.futures的ThreadPoolExecutor"""
    print("\n=== Testing ThreadPoolExecutor ===")
    
    # IO密集型任务更适合线程池
    durations = [0.1, 0.2, 0.3, 0.4, 0.5]
    
    start_time = time.time()
    
    with ThreadPoolExecutor(max_workers=5) as executor:
        # 提交任务
        futures = [executor.submit(io_simulation_task, duration) for duration in durations]
        
        # 等待所有任务完成
        for future in as_completed(futures):
            try:
                result = future.result()
                print(f"Thread task result: {result}")
            except Exception as e:
                print(f"Thread task exception: {e}")
    
    elapsed_time = time.time() - start_time
    print(f"ThreadPoolExecutor completed in {elapsed_time:.2f} seconds")

def mixed_task(task_id, cpu_work, io_duration):
    """混合任务：先CPU计算，再IO等待"""
    print(f"Task {task_id}: Starting CPU work ({cpu_work})")
    cpu_result = cpu_intensive_task(cpu_work // 10)  # 减少CPU工作量
    
    print(f"Task {task_id}: Starting IO wait ({io_duration}s)")
    time.sleep(io_duration)
    
    return f"Task {task_id}: CPU={cpu_result:.0f}, IO={io_duration}s"

def test_mixed_cpu_io_tasks():
    """测试混合CPU和IO任务的执行策略"""
    print("\n=== Testing Mixed CPU/IO Tasks ===")
    
    # 使用进程池处理CPU部分，线程池处理IO部分
    tasks = [
        (1, 50, 0.1),
        (2, 30, 0.2),
        (3, 40, 0.15),
        (4, 20, 0.3),
        (5, 35, 0.25)
    ]
    
    start_time = time.time()
    
    # 使用进程池处理混合任务
    with ProcessPoolExecutor(max_workers=3) as executor:
        futures = [executor.submit(mixed_task, *task) for task in tasks]
        
        for future in as_completed(futures):
            try:
                result = future.result()
                print(f"Mixed task result: {result}")
            except Exception as e:
                print(f"Mixed task exception: {e}")
    
    elapsed_time = time.time() - start_time
    print(f"Mixed tasks completed in {elapsed_time:.2f} seconds")

def test_process_communication():
    """测试进程间通信"""
    print("\n=== Testing Process Communication ===")
    
    def worker_with_queue(input_queue, output_queue, worker_id):
        """从输入队列获取数据，处理后放入输出队列"""
        while True:
            try:
                data = input_queue.get(timeout=1)
                if data is None:  # 结束信号
                    break
                
                result = data * 2
                output_queue.put(f"Worker {worker_id}: {data} -> {result}")
                time.sleep(0.1)  # 模拟处理时间
            except:
                break
    
    # 创建队列
    input_queue = multiprocessing.Queue()
    output_queue = multiprocessing.Queue()
    
    # 启动工作进程
    processes = []
    for i in range(3):
        p = multiprocessing.Process(
            target=worker_with_queue,
            args=(input_queue, output_queue, i)
        )
        p.start()
        processes.append(p)
    
    # 发送任务
    for num in range(10):
        input_queue.put(num)
    
    # 发送结束信号
    for _ in range(3):
        input_queue.put(None)
    
    # 等待所有进程完成
    for p in processes:
        p.join()
    
    # 收集结果
    results = []
    while not output_queue.empty():
        results.append(output_queue.get())
    
    print(f"Process communication results: {results}")

def long_running_task(duration):
    """长时间运行的任务"""
    for i in range(int(duration * 10)):
        time.sleep(0.1)
        print(f"Task progress: {i+1}/{int(duration * 10)}")
    return "Task completed"

def test_timeout_and_cancellation():
    """测试超时和取消"""
    print("\n=== Testing Timeout and Cancellation ===")
    
    # 使用wait设置超时
    with ProcessPoolExecutor(max_workers=2) as executor:
        future = executor.submit(long_running_task, 2)  # 2秒任务
        
        try:
            # 等待1秒超时
            result = future.result(timeout=1)
            print(f"Task result: {result}")
        except concurrent.futures.TimeoutError:
            print("Task timed out after 1 second")
            future.cancel()  # 尝试取消任务
            print("Task cancelled")

if __name__ == "__main__":
    test_multiprocessing_basics()
    test_process_pool()
    test_concurrent_futures_process_pool()
    test_concurrent_futures_thread_pool()
    test_mixed_cpu_io_tasks()
    test_process_communication()
    test_timeout_and_cancellation()
    print(f"\n=== All multiprocessing tests completed (PID: {os.getpid()}) ===")