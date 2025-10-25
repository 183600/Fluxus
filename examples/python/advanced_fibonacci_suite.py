#!/usr/bin/env python3

import sys
import time
import math
from typing import List, Dict, Tuple, Optional, Iterator
from functools import lru_cache, wraps
from decimal import Decimal, getcontext

getcontext().prec = 100

class FibonacciSuite:
    """Comprehensive Fibonacci implementation suite with various algorithms and optimizations."""
    
    def __init__(self):
        self.cache = {}
        self.matrix_cache = {}
    
    @staticmethod
    def time_function(func):
        """Decorator to measure function execution time."""
        @wraps(func)
        def wrapper(*args, **kwargs):
            start = time.perf_counter()
            result = func(*args, **kwargs)
            end = time.perf_counter()
            print(f"{func.__name__} took {end - start:.6f} seconds")
            return result
        return wrapper
    
    @time_function
    def iterative(self, n: int) -> int:
        """Classic iterative approach - O(n) time, O(1) space."""
        if n <= 1:
            return n
        
        a, b = 0, 1
        for _ in range(2, n + 1):
            a, b = b, a + b
        return b
    
    @time_function
    def recursive_naive(self, n: int) -> int:
        """Naive recursive approach - O(2^n) time, O(n) space."""
        if n <= 1:
            return n
        return self.recursive_naive(n - 1) + self.recursive_naive(n - 2)
    
    @time_function
    @lru_cache(maxsize=None)
    def recursive_memoized(self, n: int) -> int:
        """Memoized recursive approach - O(n) time, O(n) space."""
        if n <= 1:
            return n
        return self.recursive_memoized(n - 1) + self.recursive_memoized(n - 2)
    
    @time_function
    def matrix_power(self, n: int) -> int:
        """Matrix exponentiation approach - O(log n) time."""
        if n <= 1:
            return n
        
        def matrix_multiply(a, b):
            return [
                [a[0][0] * b[0][0] + a[0][1] * b[1][0], a[0][0] * b[0][1] + a[0][1] * b[1][1]],
                [a[1][0] * b[0][0] + a[1][1] * b[1][0], a[1][0] * b[0][1] + a[1][1] * b[1][1]]
            ]
        
        def matrix_power(matrix, power):
            if power == 1:
                return matrix
            if power % 2 == 0:
                half = matrix_power(matrix, power // 2)
                return matrix_multiply(half, half)
            else:
                return matrix_multiply(matrix, matrix_power(matrix, power - 1))
        
        base_matrix = [[1, 1], [1, 0]]
        result_matrix = matrix_power(base_matrix, n)
        return result_matrix[0][1]
    
    @time_function
    def binet_formula(self, n: int) -> int:
        """Binet's formula using golden ratio - O(1) time."""
        phi = (1 + math.sqrt(5)) / 2
        psi = (1 - math.sqrt(5)) / 2
        return int((phi**n - psi**n) / math.sqrt(5) + 0.5)
    
    @time_function
    def fast_doubling(self, n: int) -> int:
        """Fast doubling method - O(log n) time."""
        def fib_pair(k):
            if k == 0:
                return (0, 1)
            
            m = k // 2
            c, d = fib_pair(m)
            
            c_squared = c * c
            d_squared = d * d
            
            if k % 2 == 0:
                return (c * (2 * d - c), c_squared + d_squared)
            else:
                return (c_squared + d_squared, d * (2 * c + d))
        
        return fib_pair(n)[0]
    
    @time_function
    def generate_sequence(self, count: int) -> List[int]:
        """Generate fibonacci sequence up to count numbers."""
        if count <= 0:
            return []
        if count == 1:
            return [0]
        if count == 2:
            return [0, 1]
        
        sequence = [0, 1]
        for i in range(2, count):
            sequence.append(sequence[i-1] + sequence[i-2])
        return sequence
    
    @time_function
    def fibonacci_generator(self, limit: int) -> Iterator[int]:
        """Generator for fibonacci numbers up to limit."""
        a, b = 0, 1
        count = 0
        while count < limit:
            yield a
            a, b = b, a + b
            count += 1
    
    @time_function
    def lucas_numbers(self, n: int) -> int:
        """Calculate Lucas numbers (similar to Fibonacci but starts with 2, 1)."""
        if n == 0:
            return 2
        if n == 1:
            return 1
        
        a, b = 2, 1
        for _ in range(2, n + 1):
            a, b = b, a + b
        return b
    
    @time_function
    def tribonacci(self, n: int) -> int:
        """Calculate Tribonacci numbers (sum of previous 3 numbers)."""
        if n <= 1:
            return 0 if n == 0 else 1
        if n == 2:
            return 1
        
        a, b, c = 0, 1, 1
        for _ in range(3, n + 1):
            a, b, c = b, c, a + b + c
        return c
    
    def benchmark_all(self, test_values: List[int]):
        """Benchmark all fibonacci implementations."""
        print("=== Fibonacci Algorithm Benchmarks ===\n")
        
        methods = [
            ('Iterative', self.iterative),
            ('Matrix Power', self.matrix_power),
            ('Fast Doubling', self.fast_doubling),
            ('Binet Formula', self.binet_formula),
            ('Memoized Recursive', self.recursive_memoized)
        ]
        
        for n in test_values:
            print(f"Computing Fibonacci({n}):")
            results = {}
            
            for name, method in methods:
                try:
                    if name == 'Naive Recursive' and n > 35:
                        print(f"  {name}: Skipped (too slow for n={n})")
                        continue
                    
                    result = method(n)
                    results[name] = result
                    print(f"  {name}: {result}")
                except Exception as e:
                    print(f"  {name}: Error - {e}")
            
            # Verify all methods give same result
            unique_results = set(results.values())
            if len(unique_results) == 1:
                print(f"  ✓ All methods agree: {unique_results.pop()}")
            else:
                print(f"  ✗ Methods disagree: {results}")
            print()

def main():
    fib_suite = FibonacciSuite()
    
    print("Advanced Fibonacci Suite")
    print("=" * 50)
    
    # Test small values with all methods
    print("Testing small values (including naive recursive):")
    small_test = [10, 15, 20]
    for n in small_test:
        print(f"\nFibonacci({n}) using naive recursive: {fib_suite.recursive_naive(n)}")
    
    # Benchmark larger values
    test_values = [30, 50, 100, 500, 1000]
    fib_suite.benchmark_all(test_values)
    
    # Demonstrate sequence generation
    print("First 20 Fibonacci numbers:")
    sequence = fib_suite.generate_sequence(20)
    print(sequence)
    
    # Demonstrate generator
    print("\nFirst 15 Fibonacci numbers using generator:")
    fib_gen = fib_suite.fibonacci_generator(15)
    print(list(fib_gen))
    
    # Test related sequences
    print(f"\nLucas(10): {fib_suite.lucas_numbers(10)}")
    print(f"Tribonacci(10): {fib_suite.tribonacci(10)}")
    
    # Large number test
    print(f"\nLarge Fibonacci number test:")
    large_n = 10000
    result = fib_suite.fast_doubling(large_n)
    print(f"Fibonacci({large_n}) has {len(str(result))} digits")
    print(f"First 50 digits: {str(result)[:50]}...")

if __name__ == "__main__":
    main()