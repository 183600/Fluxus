import time
import threading
from concurrent.futures import ThreadPoolExecutor
from typing import List, Dict, Any, Callable
from dataclasses import dataclass
from datetime import datetime
import random
import json
import math

@dataclass
class SortResult:
    algorithm: str
    array_size: int
    execution_time: float
    comparisons: int
    swaps: int
    memory_usage: str
    is_stable: bool
    is_adaptive: bool

class SortingAnalyzer:
    def __init__(self):
        self.results: List[SortResult] = []
        
    def bubble_sort(self, arr: List[int]) -> tuple:
        """Bubble sort with metrics tracking"""
        arr = arr.copy()
        n = len(arr)
        comparisons = 0
        swaps = 0
        
        for i in range(n):
            swapped = False
            for j in range(0, n - i - 1):
                comparisons += 1
                if arr[j] > arr[j + 1]:
                    arr[j], arr[j + 1] = arr[j + 1], arr[j]
                    swaps += 1
                    swapped = True
            if not swapped:
                break
                
        return arr, comparisons, swaps
    
    def insertion_sort(self, arr: List[int]) -> tuple:
        """Insertion sort with metrics tracking"""
        arr = arr.copy()
        n = len(arr)
        comparisons = 0
        swaps = 0
        
        for i in range(1, n):
            key = arr[i]
            j = i - 1
            
            while j >= 0:
                comparisons += 1
                if arr[j] > key:
                    arr[j + 1] = arr[j]
                    swaps += 1
                    j -= 1
                else:
                    break
            arr[j + 1] = key
            
        return arr, comparisons, swaps
    
    def selection_sort(self, arr: List[int]) -> tuple:
        """Selection sort with metrics tracking"""
        arr = arr.copy()
        n = len(arr)
        comparisons = 0
        swaps = 0
        
        for i in range(n):
            min_idx = i
            for j in range(i + 1, n):
                comparisons += 1
                if arr[j] < arr[min_idx]:
                    min_idx = j
            
            if min_idx != i:
                arr[i], arr[min_idx] = arr[min_idx], arr[i]
                swaps += 1
                
        return arr, comparisons, swaps
    
    def merge_sort(self, arr: List[int]) -> tuple:
        """Merge sort with metrics tracking"""
        self.merge_comparisons = 0
        self.merge_operations = 0
        
        def merge_sort_helper(arr):
            if len(arr) <= 1:
                return arr
            
            mid = len(arr) // 2
            left = merge_sort_helper(arr[:mid])
            right = merge_sort_helper(arr[mid:])
            
            return merge(left, right)
        
        def merge(left, right):
            result = []
            i = j = 0
            
            while i < len(left) and j < len(right):
                self.merge_comparisons += 1
                if left[i] <= right[j]:
                    result.append(left[i])
                    i += 1
                else:
                    result.append(right[j])
                    j += 1
                self.merge_operations += 1
            
            result.extend(left[i:])
            result.extend(right[j:])
            self.merge_operations += len(left[i:]) + len(right[j:])
            
            return result
        
        sorted_arr = merge_sort_helper(arr.copy())
        return sorted_arr, self.merge_comparisons, self.merge_operations
    
    def quick_sort(self, arr: List[int]) -> tuple:
        """Quick sort with metrics tracking"""
        self.quick_comparisons = 0
        self.quick_swaps = 0
        arr = arr.copy()
        
        def quick_sort_helper(arr, low, high):
            if low < high:
                pi = partition(arr, low, high)
                quick_sort_helper(arr, low, pi - 1)
                quick_sort_helper(arr, pi + 1, high)
        
        def partition(arr, low, high):
            pivot = arr[high]
            i = low - 1
            
            for j in range(low, high):
                self.quick_comparisons += 1
                if arr[j] <= pivot:
                    i += 1
                    arr[i], arr[j] = arr[j], arr[i]
                    self.quick_swaps += 1
            
            arr[i + 1], arr[high] = arr[high], arr[i + 1]
            self.quick_swaps += 1
            return i + 1
        
        quick_sort_helper(arr, 0, len(arr) - 1)
        return arr, self.quick_comparisons, self.quick_swaps
    
    def heap_sort(self, arr: List[int]) -> tuple:
        """Heap sort with metrics tracking"""
        arr = arr.copy()
        n = len(arr)
        self.heap_comparisons = 0
        self.heap_swaps = 0
        
        def heapify(arr, n, i):
            largest = i
            left = 2 * i + 1
            right = 2 * i + 2
            
            if left < n:
                self.heap_comparisons += 1
                if arr[left] > arr[largest]:
                    largest = left
            
            if right < n:
                self.heap_comparisons += 1
                if arr[right] > arr[largest]:
                    largest = right
            
            if largest != i:
                arr[i], arr[largest] = arr[largest], arr[i]
                self.heap_swaps += 1
                heapify(arr, n, largest)
        
        # Build max heap
        for i in range(n // 2 - 1, -1, -1):
            heapify(arr, n, i)
        
        # Extract elements from heap
        for i in range(n - 1, 0, -1):
            arr[0], arr[i] = arr[i], arr[0]
            self.heap_swaps += 1
            heapify(arr, i, 0)
        
        return arr, self.heap_comparisons, self.heap_swaps
    
    def counting_sort(self, arr: List[int]) -> tuple:
        """Counting sort with metrics tracking"""
        if not arr:
            return arr, 0, 0
            
        arr = arr.copy()
        max_val = max(arr)
        min_val = min(arr)
        range_val = max_val - min_val + 1
        
        count = [0] * range_val
        output = [0] * len(arr)
        
        # Count occurrences
        for num in arr:
            count[num - min_val] += 1
        
        # Modify count array to store actual positions
        for i in range(1, range_val):
            count[i] += count[i - 1]
        
        # Build output array
        operations = 0
        for i in range(len(arr) - 1, -1, -1):
            output[count[arr[i] - min_val] - 1] = arr[i]
            count[arr[i] - min_val] -= 1
            operations += 1
        
        return output, len(arr), operations  # comparisons = array length for counting
    
    def radix_sort(self, arr: List[int]) -> tuple:
        """Radix sort with metrics tracking"""
        if not arr:
            return arr, 0, 0
            
        arr = arr.copy()
        max_val = max(arr)
        operations = 0
        passes = 0
        
        exp = 1
        while max_val // exp > 0:
            passes += 1
            n = len(arr)
            output = [0] * n
            count = [0] * 10
            
            # Count occurrences of each digit
            for i in range(n):
                index = arr[i] // exp
                count[index % 10] += 1
                operations += 1
            
            # Change count[i] to actual position
            for i in range(1, 10):
                count[i] += count[i - 1]
            
            # Build output array
            i = n - 1
            while i >= 0:
                index = arr[i] // exp
                output[count[index % 10] - 1] = arr[i]
                count[index % 10] -= 1
                operations += 1
                i -= 1
            
            # Copy output array to arr
            for i in range(n):
                arr[i] = output[i]
            
            exp *= 10
        
        return arr, operations, passes
    
    def benchmark_algorithm(self, algorithm_name: str, sort_func: Callable, 
                          test_array: List[int]) -> SortResult:
        """Benchmark a single sorting algorithm"""
        
        # Algorithm properties
        properties = {
            'bubble_sort': {'stable': True, 'adaptive': True, 'memory': 'O(1)'},
            'insertion_sort': {'stable': True, 'adaptive': True, 'memory': 'O(1)'},
            'selection_sort': {'stable': False, 'adaptive': False, 'memory': 'O(1)'},
            'merge_sort': {'stable': True, 'adaptive': False, 'memory': 'O(n)'},
            'quick_sort': {'stable': False, 'adaptive': False, 'memory': 'O(log n)'},
            'heap_sort': {'stable': False, 'adaptive': False, 'memory': 'O(1)'},
            'counting_sort': {'stable': True, 'adaptive': False, 'memory': 'O(k)'},
            'radix_sort': {'stable': True, 'adaptive': False, 'memory': 'O(k)'}
        }
        
        start_time = time.time()
        sorted_array, comparisons, operations = sort_func(test_array)
        end_time = time.time()
        
        execution_time = end_time - start_time
        
        props = properties.get(algorithm_name, {'stable': False, 'adaptive': False, 'memory': 'Unknown'})
        
        return SortResult(
            algorithm=algorithm_name.replace('_', ' ').title(),
            array_size=len(test_array),
            execution_time=execution_time,
            comparisons=comparisons,
            swaps=operations,
            memory_usage=props['memory'],
            is_stable=props['stable'],
            is_adaptive=props['adaptive']
        )
    
    def run_comprehensive_analysis(self, array_sizes: List[int] = None, 
                                 num_trials: int = 3) -> Dict[str, Any]:
        """Run comprehensive sorting analysis"""
        if array_sizes is None:
            array_sizes = [100, 500, 1000, 5000]
        
        algorithms = {
            'bubble_sort': self.bubble_sort,
            'insertion_sort': self.insertion_sort,
            'selection_sort': self.selection_sort,
            'merge_sort': self.merge_sort,
            'quick_sort': self.quick_sort,
            'heap_sort': self.heap_sort,
            'counting_sort': self.counting_sort,
            'radix_sort': self.radix_sort
        }
        
        print("Running Comprehensive Sorting Analysis")
        print("=" * 50)
        
        results = {}
        
        for size in array_sizes:
            print(f"\nTesting with array size: {size}")
            results[size] = {}
            
            # Generate test arrays
            random_array = [random.randint(1, size) for _ in range(size)]
            nearly_sorted = list(range(size))
            for _ in range(size // 10):  # Add some disorder
                i, j = random.randint(0, size-1), random.randint(0, size-1)
                nearly_sorted[i], nearly_sorted[j] = nearly_sorted[j], nearly_sorted[i]
            
            reverse_sorted = list(range(size, 0, -1))
            
            test_cases = {
                'random': random_array,
                'nearly_sorted': nearly_sorted,
                'reverse_sorted': reverse_sorted
            }
            
            for case_name, test_array in test_cases.items():
                print(f"  Testing {case_name} array...")
                results[size][case_name] = {}
                
                for algo_name, algo_func in algorithms.items():
                    try:
                        # Run multiple trials and average
                        trial_results = []
                        for _ in range(num_trials):
                            result = self.benchmark_algorithm(algo_name, algo_func, test_array)
                            trial_results.append(result)
                        
                        # Average the results
                        avg_time = sum(r.execution_time for r in trial_results) / num_trials
                        avg_comparisons = sum(r.comparisons for r in trial_results) / num_trials
                        avg_swaps = sum(r.swaps for r in trial_results) / num_trials
                        
                        results[size][case_name][algo_name] = {
                            'time': avg_time,
                            'comparisons': int(avg_comparisons),
                            'swaps': int(avg_swaps),
                            'memory': trial_results[0].memory_usage,
                            'stable': trial_results[0].is_stable,
                            'adaptive': trial_results[0].is_adaptive
                        }
                        
                        print(f"    {algo_name}: {avg_time:.4f}s")
                        
                    except Exception as e:
                        print(f"    {algo_name}: Failed - {e}")
                        results[size][case_name][algo_name] = {'error': str(e)}
        
        return results
    
    def generate_report(self, results: Dict[str, Any]) -> str:
        """Generate detailed analysis report"""
        report = []
        report.append("ADVANCED SORTING ALGORITHMS ANALYSIS REPORT")
        report.append("=" * 60)
        report.append(f"Analysis Date: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        report.append("")
        
        # Summary by algorithm
        report.append("ALGORITHM CHARACTERISTICS")
        report.append("-" * 30)
        
        characteristics = {
            'bubble_sort': 'O(n²) worst case, adaptive, stable',
            'insertion_sort': 'O(n²) worst case, adaptive, stable, good for small arrays',
            'selection_sort': 'O(n²) always, not adaptive, not stable',
            'merge_sort': 'O(n log n) always, stable, requires O(n) extra space',
            'quick_sort': 'O(n log n) average, O(n²) worst case, in-place',
            'heap_sort': 'O(n log n) always, in-place, not stable',
            'counting_sort': 'O(n + k) where k is range, stable, requires extra space',
            'radix_sort': 'O(d × (n + k)) where d is digits, stable'
        }
        
        for algo, desc in characteristics.items():
            report.append(f"{algo.replace('_', ' ').title()}: {desc}")
        
        report.append("")
        
        # Performance analysis by array size
        for size in sorted(results.keys()):
            report.append(f"ARRAY SIZE: {size}")
            report.append("-" * 20)
            
            for case_type in ['random', 'nearly_sorted', 'reverse_sorted']:
                if case_type not in results[size]:
                    continue
                    
                report.append(f"\n{case_type.replace('_', ' ').title()} Array:")
                
                # Sort algorithms by performance
                algo_times = []
                for algo_name, metrics in results[size][case_type].items():
                    if 'error' not in metrics:
                        algo_times.append((algo_name, metrics['time']))
                
                algo_times.sort(key=lambda x: x[1])
                
                for i, (algo_name, time_taken) in enumerate(algo_times):
                    metrics = results[size][case_type][algo_name]
                    rank_indicator = "🥇" if i == 0 else "🥈" if i == 1 else "🥉" if i == 2 else "  "
                    
                    report.append(f"  {rank_indicator} {algo_name.replace('_', ' ').title()}: "
                                f"{time_taken:.4f}s, {metrics['comparisons']} comparisons, "
                                f"{metrics['swaps']} operations")
            
            report.append("")
        
        # Best algorithm recommendations
        report.append("RECOMMENDATIONS")
        report.append("-" * 20)
        report.append("• Small arrays (< 50): Insertion Sort")
        report.append("• Large arrays, need stability: Merge Sort")
        report.append("• Large arrays, memory constrained: Heap Sort")
        report.append("• Nearly sorted data: Insertion Sort (adaptive)")
        report.append("• Integer data with small range: Counting Sort")
        report.append("• General purpose: Quick Sort or Merge Sort")
        
        return "\n".join(report)
    
    def parallel_benchmark(self, array_sizes: List[int], max_workers: int = 4):
        """Run benchmarks in parallel for faster execution"""
        print("Running Parallel Sorting Benchmarks")
        print("=" * 40)
        
        algorithms = {
            'merge_sort': self.merge_sort,
            'quick_sort': self.quick_sort,
            'heap_sort': self.heap_sort,
        }
        
        def benchmark_task(args):
            algo_name, algo_func, test_array, size = args
            start_time = time.time()
            try:
                result = self.benchmark_algorithm(algo_name, algo_func, test_array)
                return (algo_name, size, result.execution_time, None)
            except Exception as e:
                return (algo_name, size, 0, str(e))
        
        results = {}
        
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            for size in array_sizes:
                print(f"Benchmarking array size {size}...")
                test_array = [random.randint(1, size) for _ in range(size)]
                
                tasks = [
                    (algo_name, algo_func, test_array, size)
                    for algo_name, algo_func in algorithms.items()
                ]
                
                future_results = executor.map(benchmark_task, tasks)
                
                results[size] = {}
                for algo_name, array_size, exec_time, error in future_results:
                    if error:
                        print(f"  {algo_name}: Error - {error}")
                    else:
                        print(f"  {algo_name}: {exec_time:.4f}s")
                        results[size][algo_name] = exec_time
        
        return results

def main():
    """Main execution function for sorting analysis"""
    analyzer = SortingAnalyzer()
    
    # Run comprehensive analysis
    print("Starting comprehensive sorting analysis...")
    
    # Test with different array sizes
    test_sizes = [100, 500, 1000, 2000]
    
    results = analyzer.run_comprehensive_analysis(test_sizes, num_trials=3)
    
    # Generate and display report
    report = analyzer.generate_report(results)
    print("\n" + report)
    
    # Save results to file
    with open('sorting_analysis_results.json', 'w') as f:
        json.dump(results, f, indent=2)
    
    print(f"\nDetailed results saved to 'sorting_analysis_results.json'")
    
    # Run parallel benchmark for comparison
    print("\n" + "="*50)
    parallel_results = analyzer.parallel_benchmark([1000, 5000, 10000])
    
    print("\nParallel Benchmark Results:")
    for size, algos in parallel_results.items():
        print(f"\nArray size {size}:")
        for algo, time_taken in sorted(algos.items(), key=lambda x: x[1]):
            print(f"  {algo}: {time_taken:.4f}s")

if __name__ == "__main__":
    main()