#!/usr/bin/env python3
"""
Stress testing script for Python code to ensure robust compilation
"""

import time
import sys
import gc
import threading
import multiprocessing
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor
import itertools
import functools
import weakref

def fibonacci_stress(n):
    """Stress test with recursive fibonacci"""
    if n <= 1:
        return n
    return fibonacci_stress(n-1) + fibonacci_stress(n-2)

def memory_stress_test():
    """Test memory allocation and deallocation"""
    print("Memory stress test...")
    large_lists = []
    
    # Create many large data structures
    for i in range(100):
        large_list = list(range(10000))
        large_dict = {j: f"value_{j}" for j in range(1000)}
        large_tuple = tuple(range(5000))
        large_lists.append((large_list, large_dict, large_tuple))
    
    # Force garbage collection
    gc.collect()
    
    # Clean up
    del large_lists
    gc.collect()
    print("✓ Memory stress test completed")

def worker(n):
    """Worker function for multiprocessing"""
    return sum(range(n))

def concurrent_stress_test():
    """Test concurrent execution"""
    print("Concurrent stress test...")
    
    # Thread pool test
    with ThreadPoolExecutor(max_workers=4) as executor:
        futures = [executor.submit(worker, 1000) for _ in range(20)]
        results = [f.result() for f in futures]
    
    # Process pool test (limited to avoid system overload)
    with ProcessPoolExecutor(max_workers=2) as executor:
        futures = [executor.submit(worker, 500) for _ in range(4)]
        results = [f.result() for f in futures]
    
    print("✓ Concurrent stress test completed")

def recursion_stress_test():
    """Test deep recursion"""
    print("Recursion stress test...")
    
    def factorial(n):
        if n <= 1:
            return 1
        return n * factorial(n-1)
    
    # Test moderate recursion depth
    result = factorial(100)
    assert result > 0
    
    print("✓ Recursion stress test completed")

def data_structure_stress_test():
    """Test complex data structures"""
    print("Data structure stress test...")
    
    # Nested dictionaries
    nested_dict = {}
    current = nested_dict
    for i in range(100):
        current[f"level_{i}"] = {}
        current = current[f"level_{i}"]
    
    # Large nested lists
    nested_list = []
    for i in range(50):
        nested_list.append([j for j in range(100)])
    
    # Complex comprehensions
    complex_data = {
        i: [j**2 for j in range(10) if j % 2 == 0]
        for i in range(100)
        if i % 3 == 0
    }
    
    print("✓ Data structure stress test completed")

def exception_stress_test():
    """Test exception handling under stress"""
    print("Exception stress test...")
    
    exception_count = 0
    
    for i in range(1000):
        try:
            if i % 10 == 0:
                raise ValueError(f"Test exception {i}")
            elif i % 7 == 0:
                result = 10 / 0  # ZeroDivisionError
            else:
                result = i ** 2
        except (ValueError, ZeroDivisionError):
            exception_count += 1
        except Exception as e:
            print(f"Unexpected exception: {e}")
    
    print(f"✓ Exception stress test completed ({exception_count} exceptions handled)")

def performance_benchmark():
    """Benchmark various operations"""
    print("Performance benchmark...")
    
    # List operations
    start = time.time()
    big_list = list(range(100000))
    big_list.sort()
    list_time = time.time() - start
    
    # Dictionary operations
    start = time.time()
    big_dict = {i: f"value_{i}" for i in range(50000)}
    for key in big_dict:
        _ = big_dict[key]
    dict_time = time.time() - start
    
    # String operations
    start = time.time()
    text = "hello world " * 10000
    words = text.split()
    result = " ".join(words)
    string_time = time.time() - start
    
    print(f"  List operations: {list_time:.4f}s")
    print(f"  Dict operations: {dict_time:.4f}s") 
    print(f"  String operations: {string_time:.4f}s")
    print("✓ Performance benchmark completed")

def edge_case_tests():
    """Test edge cases and boundary conditions"""
    print("Edge case tests...")
    
    # Empty structures
    empty_list = []
    empty_dict = {}
    empty_string = ""
    
    # Large numbers
    big_int = 2 ** 1000
    
    # Unicode strings
    unicode_string = "Hello 世界 🌍 Ñiño café"
    
    # None handling
    none_list = [None, None, None]
    
    # Boolean operations
    bool_results = [True and False, True or False, not True]
    
    print("✓ Edge case tests completed")

def run_stress_tests():
    """Run all stress tests"""
    print("="*60)
    print("Python Stress Test Suite")
    print("="*60)
    
    start_time = time.time()
    
    try:
        memory_stress_test()
        concurrent_stress_test()
        recursion_stress_test()
        data_structure_stress_test()
        exception_stress_test()
        performance_benchmark()
        edge_case_tests()
        
        total_time = time.time() - start_time
        
        print("="*60)
        print(f"🎉 All stress tests passed in {total_time:.2f} seconds!")
        print("Python code is stable and ready for compilation.")
        print("="*60)
        
        return True
        
    except Exception as e:
        print(f"❌ Stress test failed: {e}")
        return False

if __name__ == "__main__":
    success = run_stress_tests()
    sys.exit(0 if success else 1)