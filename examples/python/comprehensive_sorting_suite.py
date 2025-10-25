#!/usr/bin/env python3

import random
import time
import sys
import heapq
from typing import List, Tuple, Optional, Callable, Any
from collections import deque, defaultdict

class SortingAlgorithms:
    """Comprehensive collection of sorting algorithms with benchmarking."""
    
    def __init__(self):
        self.comparisons = 0
        self.swaps = 0
    
    def reset_counters(self):
        """Reset performance counters."""
        self.comparisons = 0
        self.swaps = 0
    
    def compare(self, a, b):
        """Compare two elements and increment counter."""
        self.comparisons += 1
        return a < b
    
    def swap(self, arr, i, j):
        """Swap two elements and increment counter."""
        self.swaps += 1
        arr[i], arr[j] = arr[j], arr[i]
    
    def bubble_sort(self, arr: List[int]) -> List[int]:
        """Bubble sort - O(n²) time, O(1) space."""
        arr = arr.copy()
        n = len(arr)
        
        for i in range(n):
            swapped = False
            for j in range(0, n - i - 1):
                if not self.compare(arr[j], arr[j + 1]):
                    self.swap(arr, j, j + 1)
                    swapped = True
            if not swapped:
                break
        
        return arr
    
    def selection_sort(self, arr: List[int]) -> List[int]:
        """Selection sort - O(n²) time, O(1) space."""
        arr = arr.copy()
        n = len(arr)
        
        for i in range(n):
            min_idx = i
            for j in range(i + 1, n):
                if self.compare(arr[j], arr[min_idx]):
                    min_idx = j
            if min_idx != i:
                self.swap(arr, i, min_idx)
        
        return arr
    
    def insertion_sort(self, arr: List[int]) -> List[int]:
        """Insertion sort - O(n²) time, O(1) space."""
        arr = arr.copy()
        
        for i in range(1, len(arr)):
            key = arr[i]
            j = i - 1
            
            while j >= 0 and not self.compare(arr[j], key):
                self.comparisons += 1
                arr[j + 1] = arr[j]
                j -= 1
            
            if j >= 0:
                self.comparisons += 1
            arr[j + 1] = key
        
        return arr
    
    def merge_sort(self, arr: List[int]) -> List[int]:
        """Merge sort - O(n log n) time, O(n) space."""
        if len(arr) <= 1:
            return arr
        
        mid = len(arr) // 2
        left = self.merge_sort(arr[:mid])
        right = self.merge_sort(arr[mid:])
        
        return self._merge(left, right)
    
    def _merge(self, left: List[int], right: List[int]) -> List[int]:
        """Helper function for merge sort."""
        result = []
        i = j = 0
        
        while i < len(left) and j < len(right):
            if self.compare(left[i], right[j]):
                result.append(left[i])
                i += 1
            else:
                result.append(right[j])
                j += 1
        
        result.extend(left[i:])
        result.extend(right[j:])
        return result
    
    def quick_sort(self, arr: List[int]) -> List[int]:
        """Quick sort - O(n log n) average, O(n²) worst case."""
        arr = arr.copy()
        self._quick_sort_helper(arr, 0, len(arr) - 1)
        return arr
    
    def _quick_sort_helper(self, arr: List[int], low: int, high: int):
        """Helper function for quick sort."""
        if low < high:
            pi = self._partition(arr, low, high)
            self._quick_sort_helper(arr, low, pi - 1)
            self._quick_sort_helper(arr, pi + 1, high)
    
    def _partition(self, arr: List[int], low: int, high: int) -> int:
        """Partition function for quick sort."""
        pivot = arr[high]
        i = low - 1
        
        for j in range(low, high):
            if self.compare(arr[j], pivot) or arr[j] == pivot:
                i += 1
                self.swap(arr, i, j)
        
        self.swap(arr, i + 1, high)
        return i + 1
    
    def heap_sort(self, arr: List[int]) -> List[int]:
        """Heap sort - O(n log n) time, O(1) space."""
        arr = arr.copy()
        n = len(arr)
        
        # Build max heap
        for i in range(n // 2 - 1, -1, -1):
            self._heapify(arr, n, i)
        
        # Extract elements from heap one by one
        for i in range(n - 1, 0, -1):
            self.swap(arr, 0, i)
            self._heapify(arr, i, 0)
        
        return arr
    
    def _heapify(self, arr: List[int], n: int, i: int):
        """Heapify helper for heap sort."""
        largest = i
        left = 2 * i + 1
        right = 2 * i + 2
        
        if left < n and not self.compare(arr[left], arr[largest]):
            largest = left
        
        if right < n and not self.compare(arr[right], arr[largest]):
            largest = right
        
        if largest != i:
            self.swap(arr, i, largest)
            self._heapify(arr, n, largest)
    
    def counting_sort(self, arr: List[int]) -> List[int]:
        """Counting sort - O(n + k) time where k is range of input."""
        if not arr:
            return arr
        
        min_val = min(arr)
        max_val = max(arr)
        range_val = max_val - min_val + 1
        
        count = [0] * range_val
        output = [0] * len(arr)
        
        # Count occurrences
        for num in arr:
            count[num - min_val] += 1
        
        # Calculate cumulative count
        for i in range(1, range_val):
            count[i] += count[i - 1]
        
        # Build output array
        for i in range(len(arr) - 1, -1, -1):
            output[count[arr[i] - min_val] - 1] = arr[i]
            count[arr[i] - min_val] -= 1
        
        return output
    
    def radix_sort(self, arr: List[int]) -> List[int]:
        """Radix sort - O(d * (n + b)) time where d is digits, b is base."""
        if not arr:
            return arr
        
        # Handle negative numbers by separating them
        positive = [x for x in arr if x >= 0]
        negative = [-x for x in arr if x < 0]
        
        # Sort positive numbers
        if positive:
            positive = self._radix_sort_positive(positive)
        
        # Sort negative numbers and reverse
        if negative:
            negative = self._radix_sort_positive(negative)
            negative = [-x for x in reversed(negative)]
        
        return negative + positive
    
    def _radix_sort_positive(self, arr: List[int]) -> List[int]:
        """Helper for radix sort with positive numbers."""
        if not arr:
            return arr
        
        max_num = max(arr)
        exp = 1
        
        while max_num // exp > 0:
            arr = self._counting_sort_by_digit(arr, exp)
            exp *= 10
        
        return arr
    
    def _counting_sort_by_digit(self, arr: List[int], exp: int) -> List[int]:
        """Counting sort by specific digit for radix sort."""
        n = len(arr)
        output = [0] * n
        count = [0] * 10
        
        # Count occurrences of each digit
        for num in arr:
            index = (num // exp) % 10
            count[index] += 1
        
        # Calculate cumulative count
        for i in range(1, 10):
            count[i] += count[i - 1]
        
        # Build output array
        for i in range(n - 1, -1, -1):
            index = (arr[i] // exp) % 10
            output[count[index] - 1] = arr[i]
            count[index] -= 1
        
        return output
    
    def bucket_sort(self, arr: List[float], num_buckets: int = 10) -> List[float]:
        """Bucket sort - O(n + k) average time."""
        if not arr:
            return arr
        
        # Create buckets
        buckets = [[] for _ in range(num_buckets)]
        
        # Distribute elements into buckets
        max_val = max(arr)
        min_val = min(arr)
        range_val = max_val - min_val
        
        for num in arr:
            if range_val == 0:
                bucket_idx = 0
            else:
                bucket_idx = int((num - min_val) / range_val * (num_buckets - 1))
            buckets[bucket_idx].append(num)
        
        # Sort individual buckets
        result = []
        for bucket in buckets:
            if bucket:
                bucket.sort()  # Using built-in sort for simplicity
                result.extend(bucket)
        
        return result
    
    def shell_sort(self, arr: List[int]) -> List[int]:
        """Shell sort - O(n^1.5) average time."""
        arr = arr.copy()
        n = len(arr)
        gap = n // 2
        
        while gap > 0:
            for i in range(gap, n):
                temp = arr[i]
                j = i
                
                while j >= gap and not self.compare(arr[j - gap], temp):
                    self.comparisons += 1
                    arr[j] = arr[j - gap]
                    j -= gap
                
                if j >= gap:
                    self.comparisons += 1
                arr[j] = temp
            
            gap //= 2
        
        return arr
    
    def tim_sort(self, arr: List[int]) -> List[int]:
        """Simplified Tim sort implementation."""
        arr = arr.copy()
        min_merge = 32
        
        n = len(arr)
        
        # Sort individual subarrays of size min_merge using insertion sort
        for start in range(0, n, min_merge):
            end = min(start + min_merge - 1, n - 1)
            self._insertion_sort_range(arr, start, end)
        
        # Start merging from size min_merge
        size = min_merge
        while size < n:
            # Pick starting point of left sub array
            for start in range(0, n, size * 2):
                # Calculate mid and end points
                mid = start + size - 1
                end = min(start + size * 2 - 1, n - 1)
                
                # Merge subarrays if mid is smaller than end
                if mid < end:
                    self._merge_range(arr, start, mid, end)
            
            size *= 2
        
        return arr
    
    def _insertion_sort_range(self, arr: List[int], left: int, right: int):
        """Insertion sort for a specific range."""
        for i in range(left + 1, right + 1):
            key = arr[i]
            j = i - 1
            
            while j >= left and not self.compare(arr[j], key):
                arr[j + 1] = arr[j]
                j -= 1
            
            arr[j + 1] = key
    
    def _merge_range(self, arr: List[int], left: int, mid: int, right: int):
        """Merge function for tim sort."""
        left_arr = arr[left:mid + 1]
        right_arr = arr[mid + 1:right + 1]
        
        i = j = 0
        k = left
        
        while i < len(left_arr) and j < len(right_arr):
            if self.compare(left_arr[i], right_arr[j]) or left_arr[i] == right_arr[j]:
                arr[k] = left_arr[i]
                i += 1
            else:
                arr[k] = right_arr[j]
                j += 1
            k += 1
        
        while i < len(left_arr):
            arr[k] = left_arr[i]
            i += 1
            k += 1
        
        while j < len(right_arr):
            arr[k] = right_arr[j]
            j += 1
            k += 1
    
    def benchmark_algorithm(self, algorithm: Callable, arr: List[int], name: str) -> Tuple[List[int], float, int, int]:
        """Benchmark a single sorting algorithm."""
        self.reset_counters()
        start_time = time.perf_counter()
        
        if name == "Bucket Sort":
            # Convert to floats for bucket sort
            float_arr = [float(x) for x in arr]
            result = algorithm(float_arr)
            result = [int(x) for x in result]
        else:
            result = algorithm(arr)
        
        end_time = time.perf_counter()
        execution_time = end_time - start_time
        
        return result, execution_time, self.comparisons, self.swaps
    
    def benchmark_all(self, test_arrays: List[Tuple[str, List[int]]]):
        """Benchmark all sorting algorithms on various test cases."""
        algorithms = [
            ("Bubble Sort", self.bubble_sort),
            ("Selection Sort", self.selection_sort),
            ("Insertion Sort", self.insertion_sort),
            ("Merge Sort", self.merge_sort),
            ("Quick Sort", self.quick_sort),
            ("Heap Sort", self.heap_sort),
            ("Counting Sort", self.counting_sort),
            ("Radix Sort", self.radix_sort),
            ("Shell Sort", self.shell_sort),
            ("Tim Sort", self.tim_sort),
        ]
        
        print("Sorting Algorithm Benchmarks")
        print("=" * 80)
        
        for test_name, test_array in test_arrays:
            print(f"\nTest Case: {test_name} (Size: {len(test_array)})")
            print("-" * 60)
            print(f"{'Algorithm':<15} {'Time (s)':<12} {'Comparisons':<12} {'Swaps':<10} {'Correct':<8}")
            print("-" * 60)
            
            expected_result = sorted(test_array)
            
            for algo_name, algorithm in algorithms:
                try:
                    # Skip slow algorithms for large arrays
                    if len(test_array) > 1000 and algo_name in ["Bubble Sort", "Selection Sort"]:
                        print(f"{algo_name:<15} {'SKIPPED':<12} {'(too slow)':<12} {'':<10} {'':<8}")
                        continue
                    
                    result, exec_time, comparisons, swaps = self.benchmark_algorithm(algorithm, test_array, algo_name)
                    is_correct = result == expected_result
                    
                    print(f"{algo_name:<15} {exec_time:<12.6f} {comparisons:<12} {swaps:<10} {'✓' if is_correct else '✗':<8}")
                    
                    if not is_correct:
                        print(f"  Expected: {expected_result[:10]}{'...' if len(expected_result) > 10 else ''}")
                        print(f"  Got:      {result[:10]}{'...' if len(result) > 10 else ''}")
                
                except Exception as e:
                    print(f"{algo_name:<15} {'ERROR':<12} {str(e)[:20]:<12} {'':<10} {'✗':<8}")

def generate_test_arrays() -> List[Tuple[str, List[int]]]:
    """Generate various test arrays for benchmarking."""
    test_arrays = []
    
    # Small random array
    test_arrays.append(("Small Random", random.sample(range(1, 101), 50)))
    
    # Already sorted
    test_arrays.append(("Already Sorted", list(range(1, 51))))
    
    # Reverse sorted
    test_arrays.append(("Reverse Sorted", list(range(50, 0, -1))))
    
    # Nearly sorted
    nearly_sorted = list(range(1, 51))
    for _ in range(5):
        i, j = random.randint(0, 49), random.randint(0, 49)
        nearly_sorted[i], nearly_sorted[j] = nearly_sorted[j], nearly_sorted[i]
    test_arrays.append(("Nearly Sorted", nearly_sorted))
    
    # Duplicates
    test_arrays.append(("With Duplicates", [1, 3, 2, 3, 1, 2, 1, 3, 2] * 5))
    
    # All same elements
    test_arrays.append(("All Same", [5] * 30))
    
    # Large random (for efficient algorithms only)
    test_arrays.append(("Large Random", random.sample(range(1, 5001), 2000)))
    
    return test_arrays

def main():
    sorter = SortingAlgorithms()
    
    print("Comprehensive Sorting Algorithm Suite")
    print("=" * 50)
    
    # Generate test cases
    test_arrays = generate_test_arrays()
    
    # Run benchmarks
    sorter.benchmark_all(test_arrays)
    
    # Demonstrate special features
    print("\n\nSpecial Demonstrations:")
    print("=" * 30)
    
    # Bucket sort with floats
    float_array = [0.897, 0.565, 0.656, 0.1234, 0.665, 0.3434]
    print(f"Bucket sort with floats: {float_array}")
    sorted_floats = sorter.bucket_sort(float_array.copy())
    print(f"Result: {sorted_floats}")
    
    # Radix sort with negative numbers
    mixed_array = [-5, 3, -2, 8, -1, 4, 0, -7, 6]
    print(f"\nRadix sort with negative numbers: {mixed_array}")
    sorted_mixed = sorter.radix_sort(mixed_array.copy())
    print(f"Result: {sorted_mixed}")

if __name__ == "__main__":
    main()