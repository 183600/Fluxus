#!/usr/bin/env python3

import sys
import time
import random
from typing import List, Tuple, Dict, Optional

class AdvancedAlgorithms:
    def __init__(self):
        self.cache = {}
    
    def fibonacci_memoized(self, n: int) -> int:
        """Memoized Fibonacci calculation"""
        if n in self.cache:
            return self.cache[n]
        
        if n <= 1:
            result = n
        else:
            result = self.fibonacci_memoized(n-1) + self.fibonacci_memoized(n-2)
        
        self.cache[n] = result
        return result
    
    def quicksort(self, arr: List[int]) -> List[int]:
        """Quicksort implementation"""
        if len(arr) <= 1:
            return arr
        
        pivot = arr[len(arr) // 2]
        left = [x for x in arr if x < pivot]
        middle = [x for x in arr if x == pivot]
        right = [x for x in arr if x > pivot]
        
        return self.quicksort(left) + middle + self.quicksort(right)
    
    def merge_sort(self, arr: List[int]) -> List[int]:
        """Merge sort implementation"""
        if len(arr) <= 1:
            return arr
        
        mid = len(arr) // 2
        left = self.merge_sort(arr[:mid])
        right = self.merge_sort(arr[mid:])
        
        return self.merge(left, right)
    
    def merge(self, left: List[int], right: List[int]) -> List[int]:
        """Merge two sorted arrays"""
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
    
    def binary_search(self, arr: List[int], target: int) -> int:
        """Binary search implementation"""
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
    
    def dijkstra(self, graph: Dict[int, List[Tuple[int, int]]], start: int) -> Dict[int, int]:
        """Dijkstra's shortest path algorithm"""
        distances = {node: float('infinity') for node in graph}
        distances[start] = 0
        unvisited = set(graph.keys())
        
        while unvisited:
            current = min(unvisited, key=lambda node: distances[node])
            unvisited.remove(current)
            
            for neighbor, weight in graph[current]:
                distance = distances[current] + weight
                if distance < distances[neighbor]:
                    distances[neighbor] = distance
        
        return distances
    
    def knapsack_dp(self, weights: List[int], values: List[int], capacity: int) -> int:
        """0/1 Knapsack problem using dynamic programming"""
        n = len(weights)
        dp = [[0 for _ in range(capacity + 1)] for _ in range(n + 1)]
        
        for i in range(1, n + 1):
            for w in range(1, capacity + 1):
                if weights[i-1] <= w:
                    dp[i][w] = max(
                        values[i-1] + dp[i-1][w - weights[i-1]],
                        dp[i-1][w]
                    )
                else:
                    dp[i][w] = dp[i-1][w]
        
        return dp[n][capacity]
    
    def longest_common_subsequence(self, text1: str, text2: str) -> int:
        """Find length of longest common subsequence"""
        m, n = len(text1), len(text2)
        dp = [[0] * (n + 1) for _ in range(m + 1)]
        
        for i in range(1, m + 1):
            for j in range(1, n + 1):
                if text1[i-1] == text2[j-1]:
                    dp[i][j] = dp[i-1][j-1] + 1
                else:
                    dp[i][j] = max(dp[i-1][j], dp[i][j-1])
        
        return dp[m][n]
    
    def prime_sieve(self, n: int) -> List[int]:
        """Sieve of Eratosthenes to find all primes up to n"""
        if n < 2:
            return []
        
        sieve = [True] * (n + 1)
        sieve[0] = sieve[1] = False
        
        for i in range(2, int(n**0.5) + 1):
            if sieve[i]:
                for j in range(i*i, n + 1, i):
                    sieve[j] = False
        
        return [i for i in range(2, n + 1) if sieve[i]]
    
    def matrix_multiply(self, A: List[List[int]], B: List[List[int]]) -> List[List[int]]:
        """Matrix multiplication"""
        rows_A, cols_A = len(A), len(A[0])
        rows_B, cols_B = len(B), len(B[0])
        
        if cols_A != rows_B:
            raise ValueError("Cannot multiply matrices")
        
        result = [[0 for _ in range(cols_B)] for _ in range(rows_A)]
        
        for i in range(rows_A):
            for j in range(cols_B):
                for k in range(cols_A):
                    result[i][j] += A[i][k] * B[k][j]
        
        return result

def test_algorithms():
    """Test all algorithm implementations"""
    algo = AdvancedAlgorithms()
    
    print("Testing Advanced Algorithms")
    print("=" * 40)
    
    # Test Fibonacci
    print(f"Fibonacci(20) = {algo.fibonacci_memoized(20)}")
    
    # Test sorting
    test_array = [64, 34, 25, 12, 22, 11, 90]
    print(f"Original array: {test_array}")
    print(f"Quicksort: {algo.quicksort(test_array.copy())}")
    print(f"Merge sort: {algo.merge_sort(test_array.copy())}")
    
    # Test binary search
    sorted_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
    target = 7
    index = algo.binary_search(sorted_array, target)
    print(f"Binary search for {target} in {sorted_array}: index {index}")
    
    # Test Dijkstra's algorithm
    graph = {
        0: [(1, 4), (2, 1)],
        1: [(3, 1)],
        2: [(1, 2), (3, 5)],
        3: []
    }
    distances = algo.dijkstra(graph, 0)
    print(f"Dijkstra distances from node 0: {distances}")
    
    # Test Knapsack
    weights = [1, 3, 4, 5]
    values = [1, 4, 5, 7]
    capacity = 7
    max_value = algo.knapsack_dp(weights, values, capacity)
    print(f"Knapsack max value: {max_value}")
    
    # Test LCS
    text1, text2 = "ABCDGH", "AEDFHR"
    lcs_length = algo.longest_common_subsequence(text1, text2)
    print(f"LCS length of '{text1}' and '{text2}': {lcs_length}")
    
    # Test prime sieve
    primes = algo.prime_sieve(30)
    print(f"Primes up to 30: {primes}")
    
    # Test matrix multiplication
    A = [[1, 2], [3, 4]]
    B = [[5, 6], [7, 8]]
    result = algo.matrix_multiply(A, B)
    print(f"Matrix A × B = {result}")

if __name__ == "__main__":
    test_algorithms()