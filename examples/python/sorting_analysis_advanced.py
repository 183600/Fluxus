import random
import time
import matplotlib.pyplot as plt
import numpy as np
from typing import List, Tuple, Callable
import statistics

class SortingAlgorithms:
    """Advanced sorting algorithms with performance analysis"""
    
    def __init__(self):
        self.comparison_count = 0
        self.swap_count = 0
    
    def reset_counters(self):
        """Reset performance counters"""
        self.comparison_count = 0
        self.swap_count = 0
    
    def bubble_sort(self, arr: List[int]) -> List[int]:
        """Bubble sort with optimization"""
        arr = arr.copy()
        n = len(arr)
        
        for i in range(n):
            swapped = False
            for j in range(0, n - i - 1):
                self.comparison_count += 1
                if arr[j] > arr[j + 1]:
                    arr[j], arr[j + 1] = arr[j + 1], arr[j]
                    self.swap_count += 1
                    swapped = True
            
            # If no swapping occurred, array is sorted
            if not swapped:
                break
        
        return arr
    
    def quick_sort(self, arr: List[int]) -> List[int]:
        """Quick sort with random pivot"""
        arr = arr.copy()
        self._quick_sort_helper(arr, 0, len(arr) - 1)
        return arr
    
    def _quick_sort_helper(self, arr: List[int], low: int, high: int):
        if low < high:
            # Use random pivot for better average performance
            pivot_idx = random.randint(low, high)
            arr[pivot_idx], arr[high] = arr[high], arr[pivot_idx]
            
            pi = self._partition(arr, low, high)
            self._quick_sort_helper(arr, low, pi - 1)
            self._quick_sort_helper(arr, pi + 1, high)
    
    def _partition(self, arr: List[int], low: int, high: int) -> int:
        pivot = arr[high]
        i = low - 1
        
        for j in range(low, high):
            self.comparison_count += 1
            if arr[j] <= pivot:
                i += 1
                if i != j:
                    arr[i], arr[j] = arr[j], arr[i]
                    self.swap_count += 1
        
        if i + 1 != high:
            arr[i + 1], arr[high] = arr[high], arr[i + 1]
            self.swap_count += 1
        
        return i + 1
    
    def merge_sort(self, arr: List[int]) -> List[int]:
        """Merge sort implementation"""
        if len(arr) <= 1:
            return arr
        
        mid = len(arr) // 2
        left = self.merge_sort(arr[:mid])
        right = self.merge_sort(arr[mid:])
        
        return self._merge(left, right)
    
    def _merge(self, left: List[int], right: List[int]) -> List[int]:
        result = []
        i = j = 0
        
        while i < len(left) and j < len(right):
            self.comparison_count += 1
            if left[i] <= right[j]:
                result.append(left[i])
                i += 1
            else:
                result.append(right[j])
                j += 1
        
        result.extend(left[i:])
        result.extend(right[j:])
        return result
    
    def heap_sort(self, arr: List[int]) -> List[int]:
        """Heap sort implementation"""
        arr = arr.copy()
        n = len(arr)
        
        # Build max heap
        for i in range(n // 2 - 1, -1, -1):
            self._heapify(arr, n, i)
        
        # Extract elements from heap one by one
        for i in range(n - 1, 0, -1):
            arr[0], arr[i] = arr[i], arr[0]
            self.swap_count += 1
            self._heapify(arr, i, 0)
        
        return arr
    
    def _heapify(self, arr: List[int], n: int, i: int):
        largest = i
        left = 2 * i + 1
        right = 2 * i + 2
        
        if left < n:
            self.comparison_count += 1
            if arr[left] > arr[largest]:
                largest = left
        
        if right < n:
            self.comparison_count += 1
            if arr[right] > arr[largest]:
                largest = right
        
        if largest != i:
            arr[i], arr[largest] = arr[largest], arr[i]
            self.swap_count += 1
            self._heapify(arr, n, largest)
    
    def counting_sort(self, arr: List[int]) -> List[int]:
        """Counting sort for integers"""
        if not arr:
            return arr
        
        min_val = min(arr)
        max_val = max(arr)
        range_size = max_val - min_val + 1
        
        # Count occurrences
        count = [0] * range_size
        for num in arr:
            count[num - min_val] += 1
        
        # Reconstruct sorted array
        result = []
        for i, freq in enumerate(count):
            result.extend([i + min_val] * freq)
        
        return result
    
    def radix_sort(self, arr: List[int]) -> List[int]:
        """Radix sort for non-negative integers"""
        if not arr or min(arr) < 0:
            return arr
        
        # Find maximum number to determine number of digits
        max_num = max(arr)
        exp = 1
        
        while max_num // exp > 0:
            arr = self._counting_sort_by_digit(arr, exp)
            exp *= 10
        
        return arr
    
    def _counting_sort_by_digit(self, arr: List[int], exp: int) -> List[int]:
        n = len(arr)
        output = [0] * n
        count = [0] * 10
        
        # Count occurrences of each digit
        for num in arr:
            index = (num // exp) % 10
            count[index] += 1
        
        # Change count[i] to actual position
        for i in range(1, 10):
            count[i] += count[i - 1]
        
        # Build output array
        for i in range(n - 1, -1, -1):
            index = (arr[i] // exp) % 10
            output[count[index] - 1] = arr[i]
            count[index] -= 1
        
        return output

class PerformanceBenchmark:
    """Performance benchmarking for sorting algorithms"""
    
    def __init__(self):
        self.sorter = SortingAlgorithms()
    
    def generate_test_data(self, size: int, data_type: str = "random") -> List[int]:
        """Generate test data of different types"""
        if data_type == "random":
            return [random.randint(1, 1000) for _ in range(size)]
        elif data_type == "sorted":
            return list(range(1, size + 1))
        elif data_type == "reverse":
            return list(range(size, 0, -1))
        elif data_type == "nearly_sorted":
            arr = list(range(1, size + 1))
            # Swap 10% of elements randomly
            for _ in range(size // 10):
                i, j = random.randint(0, size-1), random.randint(0, size-1)
                arr[i], arr[j] = arr[j], arr[i]
            return arr
        else:
            return [random.randint(1, 1000) for _ in range(size)]
    
    def benchmark_algorithm(self, algorithm: Callable, data: List[int]) -> Tuple[float, int, int]:
        """Benchmark a single algorithm"""
        self.sorter.reset_counters()
        start_time = time.time()
        result = algorithm(data)
        end_time = time.time()
        
        return (
            end_time - start_time,
            self.sorter.comparison_count,
            self.sorter.swap_count
        )
    
    def run_comprehensive_benchmark(self, sizes: List[int] = None, iterations: int = 5):
        """Run comprehensive benchmark across multiple algorithms and data sizes"""
        if sizes is None:
            sizes = [100, 500, 1000, 2000, 5000]
        
        algorithms = {
            "Bubble Sort": self.sorter.bubble_sort,
            "Quick Sort": self.sorter.quick_sort,
            "Merge Sort": self.sorter.merge_sort,
            "Heap Sort": self.sorter.heap_sort,
            "Counting Sort": self.sorter.counting_sort,
        }
        
        results = {}
        
        for size in sizes:
            print(f"\nBenchmarking with array size: {size}")
            print("-" * 40)
            
            results[size] = {}
            
            for algo_name, algo_func in algorithms.items():
                times = []
                comparisons = []
                swaps = []
                
                for _ in range(iterations):
                    data = self.generate_test_data(size, "random")
                    time_taken, comp_count, swap_count = self.benchmark_algorithm(algo_func, data)
                    times.append(time_taken)
                    comparisons.append(comp_count)
                    swaps.append(swap_count)
                
                avg_time = statistics.mean(times)
                avg_comparisons = statistics.mean(comparisons) if comparisons[0] > 0 else 0
                avg_swaps = statistics.mean(swaps) if swaps[0] > 0 else 0
                
                results[size][algo_name] = {
                    "time": avg_time,
                    "comparisons": avg_comparisons,
                    "swaps": avg_swaps
                }
                
                print(f"{algo_name:15}: {avg_time:.6f}s, {avg_comparisons:.0f} comparisons, {avg_swaps:.0f} swaps")
        
        return results

def main():
    print("Advanced Sorting Algorithms with Performance Analysis")
    print("====================================================")
    
    # Create benchmark instance
    benchmark = PerformanceBenchmark()
    
    # Test with small dataset first
    print("\nTesting correctness with small dataset:")
    test_data = [64, 34, 25, 12, 22, 11, 90, 5]
    print(f"Original: {test_data}")
    
    algorithms = {
        "Bubble Sort": benchmark.sorter.bubble_sort,
        "Quick Sort": benchmark.sorter.quick_sort,
        "Merge Sort": benchmark.sorter.merge_sort,
        "Heap Sort": benchmark.sorter.heap_sort,
        "Counting Sort": benchmark.sorter.counting_sort,
        "Radix Sort": benchmark.sorter.radix_sort,
    }
    
    for name, func in algorithms.items():
        benchmark.sorter.reset_counters()
        sorted_data = func(test_data)
        print(f"{name:15}: {sorted_data}")
    
    # Run comprehensive benchmark
    print("\nRunning comprehensive performance benchmark...")
    results = benchmark.run_comprehensive_benchmark([100, 500, 1000], iterations=3)
    
    # Performance analysis
    print("\nPerformance Summary:")
    print("===================")
    
    for size in sorted(results.keys()):
        print(f"\nArray size: {size}")
        # Sort algorithms by time
        sorted_algos = sorted(results[size].items(), key=lambda x: x[1]["time"])
        
        for i, (algo_name, metrics) in enumerate(sorted_algos):
            rank = i + 1
            print(f"{rank}. {algo_name:15}: {metrics['time']:.6f}s")
    
    # Test different data patterns
    print("\nTesting with different data patterns (size=1000):")
    print("=" * 50)
    
    patterns = ["random", "sorted", "reverse", "nearly_sorted"]
    test_algorithms = ["Quick Sort", "Merge Sort", "Heap Sort"]
    
    for pattern in patterns:
        print(f"\nData pattern: {pattern.title()}")
        print("-" * 30)
        test_data = benchmark.generate_test_data(1000, pattern)
        
        for algo_name in test_algorithms:
            algo_func = algorithms[algo_name]
            time_taken, comparisons, swaps = benchmark.benchmark_algorithm(algo_func, test_data)
            print(f"{algo_name:15}: {time_taken:.6f}s")

if __name__ == "__main__":
    main()