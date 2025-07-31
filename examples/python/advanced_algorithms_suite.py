#!/usr/bin/env python3

import sys
import time
import math
from typing import List, Tuple, Optional

def fibonacci_recursive(n: int) -> int:
    if n <= 1:
        return n
    return fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2)

def fibonacci_iterative(n: int) -> int:
    if n <= 1:
        return n
    a, b = 0, 1
    for _ in range(2, n + 1):
        a, b = b, a + b
    return b

def fibonacci_memoized(n: int, memo: dict = None) -> int:
    if memo is None:
        memo = {}
    if n in memo:
        return memo[n]
    if n <= 1:
        return n
    memo[n] = fibonacci_memoized(n - 1, memo) + fibonacci_memoized(n - 2, memo)
    return memo[n]

def quicksort(arr: List[int]) -> List[int]:
    if len(arr) <= 1:
        return arr
    pivot = arr[len(arr) // 2]
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]
    return quicksort(left) + middle + quicksort(right)

def mergesort(arr: List[int]) -> List[int]:
    if len(arr) <= 1:
        return arr
    mid = len(arr) // 2
    left = mergesort(arr[:mid])
    right = mergesort(arr[mid:])
    return merge(left, right)

def merge(left: List[int], right: List[int]) -> List[int]:
    result = []
    i = j = 0
    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            j += 1
    result.extend(left[i:])
    result.extend(right[j:])
    return result

def heapsort(arr: List[int]) -> List[int]:
    def heapify(arr: List[int], n: int, i: int):
        largest = i
        left = 2 * i + 1
        right = 2 * i + 2
        
        if left < n and arr[left] > arr[largest]:
            largest = left
        if right < n and arr[right] > arr[largest]:
            largest = right
        if largest != i:
            arr[i], arr[largest] = arr[largest], arr[i]
            heapify(arr, n, largest)
    
    n = len(arr)
    for i in range(n // 2 - 1, -1, -1):
        heapify(arr, n, i)
    
    for i in range(n - 1, 0, -1):
        arr[0], arr[i] = arr[i], arr[0]
        heapify(arr, i, 0)
    
    return arr

def binary_search(arr: List[int], target: int) -> int:
    left, right = 0, len(arr) - 1
    while left <= right:
        mid = (left + right) // 2
        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1
    return -1

def gcd(a: int, b: int) -> int:
    while b:
        a, b = b, a % b
    return a

def lcm(a: int, b: int) -> int:
    return abs(a * b) // gcd(a, b)

def is_prime(n: int) -> bool:
    if n < 2:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False
    for i in range(3, int(math.sqrt(n)) + 1, 2):
        if n % i == 0:
            return False
    return True

def sieve_of_eratosthenes(limit: int) -> List[int]:
    sieve = [True] * (limit + 1)
    sieve[0] = sieve[1] = False
    
    for i in range(2, int(math.sqrt(limit)) + 1):
        if sieve[i]:
            for j in range(i * i, limit + 1, i):
                sieve[j] = False
    
    return [i for i in range(limit + 1) if sieve[i]]

def factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

def factorial_iterative(n: int) -> int:
    result = 1
    for i in range(2, n + 1):
        result *= i
    return result

def power(base: int, exp: int) -> int:
    if exp == 0:
        return 1
    if exp < 0:
        return 1 / power(base, -exp)
    
    result = 1
    while exp > 0:
        if exp % 2 == 1:
            result *= base
        base *= base
        exp //= 2
    return result

def matrix_multiply(A: List[List[int]], B: List[List[int]]) -> List[List[int]]:
    rows_A, cols_A = len(A), len(A[0])
    rows_B, cols_B = len(B), len(B[0])
    
    if cols_A != rows_B:
        raise ValueError("Cannot multiply matrices: incompatible dimensions")
    
    result = [[0] * cols_B for _ in range(rows_A)]
    
    for i in range(rows_A):
        for j in range(cols_B):
            for k in range(cols_A):
                result[i][j] += A[i][k] * B[k][j]
    
    return result

def longest_common_subsequence(text1: str, text2: str) -> int:
    m, n = len(text1), len(text2)
    dp = [[0] * (n + 1) for _ in range(m + 1)]
    
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            if text1[i - 1] == text2[j - 1]:
                dp[i][j] = dp[i - 1][j - 1] + 1
            else:
                dp[i][j] = max(dp[i - 1][j], dp[i][j - 1])
    
    return dp[m][n]

def knapsack(weights: List[int], values: List[int], capacity: int) -> int:
    n = len(weights)
    dp = [[0] * (capacity + 1) for _ in range(n + 1)]
    
    for i in range(1, n + 1):
        for w in range(capacity + 1):
            if weights[i - 1] <= w:
                dp[i][w] = max(dp[i - 1][w], dp[i - 1][w - weights[i - 1]] + values[i - 1])
            else:
                dp[i][w] = dp[i - 1][w]
    
    return dp[n][capacity]

def dijkstra(graph: dict, start: str) -> dict:
    distances = {node: float('infinity') for node in graph}
    distances[start] = 0
    unvisited = set(graph.keys())
    
    while unvisited:
        current = min(unvisited, key=lambda x: distances[x])
        unvisited.remove(current)
        
        for neighbor, weight in graph[current].items():
            distance = distances[current] + weight
            if distance < distances[neighbor]:
                distances[neighbor] = distance
    
    return distances

def main():
    print("Advanced Python Algorithms Suite")
    print("=" * 40)
    
    # Test Fibonacci implementations
    n = 20
    print(f"\nFibonacci({n}):")
    print(f"Recursive: {fibonacci_recursive(n)}")
    print(f"Iterative: {fibonacci_iterative(n)}")
    print(f"Memoized: {fibonacci_memoized(n)}")
    
    # Test sorting algorithms
    test_array = [64, 34, 25, 12, 22, 11, 90, 5, 77, 30]
    print(f"\nOriginal array: {test_array}")
    print(f"Quicksort: {quicksort(test_array.copy())}")
    print(f"Mergesort: {mergesort(test_array.copy())}")
    print(f"Heapsort: {heapsort(test_array.copy())}")
    
    # Test binary search
    sorted_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
    target = 7
    print(f"\nBinary search for {target} in {sorted_array}: {binary_search(sorted_array, target)}")
    
    # Test mathematical functions
    print(f"\nGCD(48, 18): {gcd(48, 18)}")
    print(f"LCM(12, 15): {lcm(12, 15)}")
    print(f"Is 17 prime? {is_prime(17)}")
    print(f"Primes up to 30: {sieve_of_eratosthenes(30)}")
    print(f"Factorial(5): {factorial(5)}")
    print(f"2^10: {power(2, 10)}")
    
    # Test matrix multiplication
    A = [[1, 2], [3, 4]]
    B = [[5, 6], [7, 8]]
    print(f"\nMatrix A: {A}")
    print(f"Matrix B: {B}")
    print(f"A * B: {matrix_multiply(A, B)}")
    
    # Test dynamic programming
    print(f"\nLCS of 'ABCDGH' and 'AEDFHR': {longest_common_subsequence('ABCDGH', 'AEDFHR')}")
    
    weights = [10, 20, 30]
    values = [60, 100, 120]
    capacity = 50
    print(f"Knapsack (weights={weights}, values={values}, capacity={capacity}): {knapsack(weights, values, capacity)}")
    
    # Test graph algorithm
    graph = {
        'A': {'B': 4, 'C': 2},
        'B': {'C': 1, 'D': 5},
        'C': {'D': 8, 'E': 10},
        'D': {'E': 2},
        'E': {}
    }
    print(f"\nDijkstra from 'A': {dijkstra(graph, 'A')}")

if __name__ == "__main__":
    main()