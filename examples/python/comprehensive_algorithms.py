#!/usr/bin/env python3
"""
Comprehensive Python Algorithms and Data Structures Demo
"""

import json
import math
import random
import time
from collections import defaultdict, deque
from typing import List, Dict, Any, Optional, Tuple
import threading
import queue
import heapq


class TreeNode:
    def __init__(self, val: int = 0):
        self.val = val
        self.left: Optional['TreeNode'] = None
        self.right: Optional['TreeNode'] = None


class BinarySearchTree:
    def __init__(self):
        self.root: Optional[TreeNode] = None
    
    def insert(self, val: int) -> None:
        self.root = self._insert_recursive(self.root, val)
    
    def _insert_recursive(self, node: Optional[TreeNode], val: int) -> TreeNode:
        if not node:
            return TreeNode(val)
        
        if val < node.val:
            node.left = self._insert_recursive(node.left, val)
        else:
            node.right = self._insert_recursive(node.right, val)
        
        return node
    
    def search(self, val: int) -> bool:
        return self._search_recursive(self.root, val)
    
    def _search_recursive(self, node: Optional[TreeNode], val: int) -> bool:
        if not node:
            return False
        
        if node.val == val:
            return True
        elif val < node.val:
            return self._search_recursive(node.left, val)
        else:
            return self._search_recursive(node.right, val)
    
    def inorder_traversal(self) -> List[int]:
        result = []
        self._inorder_recursive(self.root, result)
        return result
    
    def _inorder_recursive(self, node: Optional[TreeNode], result: List[int]) -> None:
        if node:
            self._inorder_recursive(node.left, result)
            result.append(node.val)
            self._inorder_recursive(node.right, result)


class Graph:
    def __init__(self):
        self.vertices: Dict[int, List[int]] = defaultdict(list)
        self.lock = threading.RLock()
    
    def add_vertex(self, v: int) -> None:
        with self.lock:
            if v not in self.vertices:
                self.vertices[v] = []
    
    def add_edge(self, from_v: int, to_v: int) -> None:
        with self.lock:
            self.vertices[from_v].append(to_v)
    
    def dfs(self, start: int) -> List[int]:
        with self.lock:
            visited = set()
            result = []
            self._dfs_recursive(start, visited, result)
            return result
    
    def _dfs_recursive(self, vertex: int, visited: set, result: List[int]) -> None:
        visited.add(vertex)
        result.append(vertex)
        
        for neighbor in self.vertices[vertex]:
            if neighbor not in visited:
                self._dfs_recursive(neighbor, visited, result)
    
    def bfs(self, start: int) -> List[int]:
        with self.lock:
            visited = set()
            queue_obj = deque([start])
            result = []
            
            while queue_obj:
                vertex = queue_obj.popleft()
                if vertex not in visited:
                    visited.add(vertex)
                    result.append(vertex)
                    
                    for neighbor in self.vertices[vertex]:
                        if neighbor not in visited:
                            queue_obj.append(neighbor)
            
            return result


class LRUCache:
    class Node:
        def __init__(self, key: int = 0, value: int = 0):
            self.key = key
            self.value = value
            self.prev: Optional['LRUCache.Node'] = None
            self.next: Optional['LRUCache.Node'] = None
    
    def __init__(self, capacity: int):
        self.capacity = capacity
        self.cache: Dict[int, 'LRUCache.Node'] = {}
        self.lock = threading.RLock()
        
        # Create dummy head and tail
        self.head = self.Node()
        self.tail = self.Node()
        self.head.next = self.tail
        self.tail.prev = self.head
    
    def get(self, key: int) -> int:
        with self.lock:
            if key in self.cache:
                node = self.cache[key]
                self._move_to_head(node)
                return node.value
            return -1
    
    def put(self, key: int, value: int) -> None:
        with self.lock:
            if key in self.cache:
                node = self.cache[key]
                node.value = value
                self._move_to_head(node)
            else:
                new_node = self.Node(key, value)
                
                if len(self.cache) >= self.capacity:
                    # Remove least recently used
                    tail = self._remove_tail()
                    del self.cache[tail.key]
                
                self.cache[key] = new_node
                self._add_to_head(new_node)
    
    def _add_to_head(self, node: 'LRUCache.Node') -> None:
        node.prev = self.head
        node.next = self.head.next
        self.head.next.prev = node
        self.head.next = node
    
    def _remove_node(self, node: 'LRUCache.Node') -> None:
        node.prev.next = node.next
        node.next.prev = node.prev
    
    def _move_to_head(self, node: 'LRUCache.Node') -> None:
        self._remove_node(node)
        self._add_to_head(node)
    
    def _remove_tail(self) -> 'LRUCache.Node':
        tail = self.tail.prev
        self._remove_node(tail)
        return tail


class Trie:
    class TrieNode:
        def __init__(self):
            self.children: Dict[str, 'Trie.TrieNode'] = {}
            self.is_end = False
    
    def __init__(self):
        self.root = self.TrieNode()
        self.lock = threading.RLock()
    
    def insert(self, word: str) -> None:
        with self.lock:
            node = self.root
            for char in word:
                if char not in node.children:
                    node.children[char] = self.TrieNode()
                node = node.children[char]
            node.is_end = True
    
    def search(self, word: str) -> bool:
        with self.lock:
            node = self.root
            for char in word:
                if char not in node.children:
                    return False
                node = node.children[char]
            return node.is_end
    
    def starts_with(self, prefix: str) -> bool:
        with self.lock:
            node = self.root
            for char in prefix:
                if char not in node.children:
                    return False
                node = node.children[char]
            return True


def quick_sort(arr: List[int]) -> List[int]:
    if len(arr) <= 1:
        return arr
    
    pivot = arr[len(arr) // 2]
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]
    
    return quick_sort(left) + middle + quick_sort(right)


def merge_sort(arr: List[int]) -> List[int]:
    if len(arr) <= 1:
        return arr
    
    mid = len(arr) // 2
    left = merge_sort(arr[:mid])
    right = merge_sort(arr[mid:])
    
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


def is_prime(n: int) -> bool:
    if n <= 1:
        return False
    if n <= 3:
        return True
    if n % 2 == 0 or n % 3 == 0:
        return False
    
    i = 5
    while i * i <= n:
        if n % i == 0 or n % (i + 2) == 0:
            return False
        i += 6
    
    return True


def fibonacci_recursive(n: int) -> int:
    if n <= 1:
        return n
    return fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2)


def fibonacci_dp(n: int) -> int:
    if n <= 1:
        return n
    
    dp = [0] * (n + 1)
    dp[1] = 1
    
    for i in range(2, n + 1):
        dp[i] = dp[i - 1] + dp[i - 2]
    
    return dp[n]


def fibonacci_iterative(n: int) -> int:
    if n <= 1:
        return n
    
    a, b = 0, 1
    for _ in range(2, n + 1):
        a, b = b, a + b
    
    return b


def gcd(a: int, b: int) -> int:
    while b:
        a, b = b, a % b
    return a


def lcm(a: int, b: int) -> int:
    return abs(a * b) // gcd(a, b)


def binary_search(arr: List[int], target: int) -> int:
    left, right = 0, len(arr) - 1
    
    while left <= right:
        mid = left + (right - left) // 2
        
        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1
    
    return -1


def dijkstra(graph: Dict[int, List[Tuple[int, int]]], start: int) -> Dict[int, int]:
    """Dijkstra's shortest path algorithm"""
    distances = defaultdict(lambda: float('inf'))
    distances[start] = 0
    pq = [(0, start)]
    visited = set()
    
    while pq:
        current_distance, current = heapq.heappop(pq)
        
        if current in visited:
            continue
        
        visited.add(current)
        
        for neighbor, weight in graph[current]:
            distance = current_distance + weight
            
            if distance < distances[neighbor]:
                distances[neighbor] = distance
                heapq.heappush(pq, (distance, neighbor))
    
    return dict(distances)


def matrix_multiply(a: List[List[int]], b: List[List[int]]) -> List[List[int]]:
    rows_a, cols_a = len(a), len(a[0])
    rows_b, cols_b = len(b), len(b[0])
    
    if cols_a != rows_b:
        raise ValueError("Cannot multiply matrices with incompatible dimensions")
    
    result = [[0 for _ in range(cols_b)] for _ in range(rows_a)]
    
    for i in range(rows_a):
        for j in range(cols_b):
            for k in range(cols_a):
                result[i][j] += a[i][k] * b[k][j]
    
    return result


def calculate_statistics(numbers: List[float]) -> Dict[str, float]:
    if not numbers:
        return {"mean": 0, "median": 0, "mode": 0, "variance": 0, "std_dev": 0}
    
    # Mean
    mean = sum(numbers) / len(numbers)
    
    # Variance and standard deviation
    variance = sum((x - mean) ** 2 for x in numbers) / len(numbers)
    std_dev = math.sqrt(variance)
    
    # Median
    sorted_nums = sorted(numbers)
    n = len(sorted_nums)
    if n % 2 == 0:
        median = (sorted_nums[n // 2 - 1] + sorted_nums[n // 2]) / 2
    else:
        median = sorted_nums[n // 2]
    
    # Mode (most frequent value)
    frequency = {}
    for num in numbers:
        frequency[num] = frequency.get(num, 0) + 1
    
    mode = max(frequency.keys(), key=frequency.get)
    
    return {
        "mean": mean,
        "median": median,
        "mode": mode,
        "variance": variance,
        "std_dev": std_dev
    }


def worker_function(worker_id: int, task_queue: queue.Queue, result_queue: queue.Queue):
    """Worker function for demonstrating threading"""
    while True:
        try:
            task = task_queue.get(timeout=1)
            if task is None:  # Poison pill
                break
            
            # Simulate work
            result = task ** 2
            time.sleep(0.01)  # Simulate processing time
            
            result_queue.put((worker_id, task, result))
            task_queue.task_done()
        
        except queue.Empty:
            break


def demonstrate_threading():
    """Demonstrate multi-threading with a worker pool"""
    task_queue = queue.Queue()
    result_queue = queue.Queue()
    
    # Add tasks
    for i in range(1, 21):
        task_queue.put(i)
    
    # Create and start workers
    workers = []
    for i in range(4):
        worker = threading.Thread(target=worker_function, args=(i, task_queue, result_queue))
        worker.start()
        workers.append(worker)
    
    # Wait for all tasks to complete
    task_queue.join()
    
    # Stop workers
    for _ in workers:
        task_queue.put(None)
    
    for worker in workers:
        worker.join()
    
    # Collect results
    results = []
    while not result_queue.empty():
        results.append(result_queue.get())
    
    return results


def main():
    print("=== Comprehensive Python Algorithms and Data Structures Demo ===")
    
    # 1. Binary Search Tree
    print("\n1. Binary Search Tree Operations:")
    bst = BinarySearchTree()
    values = [50, 30, 70, 20, 40, 60, 80]
    
    for val in values:
        bst.insert(val)
    
    print(f"Inorder traversal: {bst.inorder_traversal()}")
    print(f"Search 40: {bst.search(40)}")
    print(f"Search 90: {bst.search(90)}")
    
    # 2. Graph Operations
    print("\n2. Graph Operations:")
    graph = Graph()
    vertices = [1, 2, 3, 4, 5]
    
    for v in vertices:
        graph.add_vertex(v)
    
    edges = [(1, 2), (1, 3), (2, 4), (3, 4), (4, 5)]
    for from_v, to_v in edges:
        graph.add_edge(from_v, to_v)
    
    print(f"DFS from vertex 1: {graph.dfs(1)}")
    print(f"BFS from vertex 1: {graph.bfs(1)}")
    
    # 3. Sorting Algorithms
    print("\n3. Sorting Algorithms:")
    original_array = [64, 34, 25, 12, 22, 11, 90]
    print(f"Original array: {original_array}")
    
    quick_sorted = quick_sort(original_array.copy())
    print(f"Quick sort result: {quick_sorted}")
    
    merge_sorted = merge_sort(original_array.copy())
    print(f"Merge sort result: {merge_sorted}")
    
    # 4. Mathematical Operations
    print("\n4. Mathematical Operations:")
    print(f"Is 17 prime? {is_prime(17)}")
    print(f"Is 15 prime? {is_prime(15)}")
    
    print(f"Fibonacci(10) recursive: {fibonacci_recursive(10)}")
    print(f"Fibonacci(10) DP: {fibonacci_dp(10)}")
    print(f"Fibonacci(10) iterative: {fibonacci_iterative(10)}")
    
    print(f"GCD(48, 18): {gcd(48, 18)}")
    print(f"LCM(12, 8): {lcm(12, 8)}")
    
    # 5. LRU Cache
    print("\n5. LRU Cache Operations:")
    cache = LRUCache(3)
    
    cache.put(1, 100)
    cache.put(2, 200)
    cache.put(3, 300)
    
    print(f"Get key 1: {cache.get(1)}")
    print(f"Get key 2: {cache.get(2)}")
    
    cache.put(4, 400)  # This should evict key 3
    print(f"Get key 3 (should be -1): {cache.get(3)}")
    print(f"Get key 4: {cache.get(4)}")
    
    # 6. Trie Operations
    print("\n6. Trie (Prefix Tree) Operations:")
    trie = Trie()
    words = ["apple", "app", "application", "apply", "banana", "band"]
    
    for word in words:
        trie.insert(word)
    
    test_words = ["app", "apple", "appl", "application", "banana", "orange"]
    for word in test_words:
        print(f"Search '{word}': {trie.search(word)}")
    
    prefixes = ["app", "ban", "or"]
    for prefix in prefixes:
        print(f"Starts with '{prefix}': {trie.starts_with(prefix)}")
    
    # 7. Binary Search
    print("\n7. Binary Search:")
    sorted_array = [2, 3, 4, 10, 40]
    target = 10
    result = binary_search(sorted_array, target)
    print(f"Binary search for {target} in {sorted_array}: index {result}")
    
    # 8. Matrix Operations
    print("\n8. Matrix Operations:")
    matrix1 = [[1, 2], [3, 4]]
    matrix2 = [[5, 6], [7, 8]]
    product = matrix_multiply(matrix1, matrix2)
    print(f"Matrix multiplication result: {product}")
    
    # 9. Statistical Analysis
    print("\n9. Statistical Analysis:")
    numbers = [1.5, 2.3, 3.7, 2.3, 4.1, 5.2, 2.3, 6.8, 7.9, 3.7]
    stats = calculate_statistics(numbers)
    print(f"Data: {numbers}")
    print(f"Statistics: {stats}")
    
    # 10. Dijkstra's Algorithm
    print("\n10. Dijkstra's Shortest Path:")
    weighted_graph = {
        0: [(1, 4), (2, 1)],
        1: [(3, 1)],
        2: [(1, 2), (3, 5)],
        3: []
    }
    distances = dijkstra(weighted_graph, 0)
    print(f"Shortest distances from vertex 0: {dict(distances)}")
    
    # 11. Threading Demo
    print("\n11. Multi-threading Demo:")
    thread_results = demonstrate_threading()
    print(f"Processed {len(thread_results)} tasks using 4 worker threads")
    print("Sample results (worker_id, task, result):")
    for result in thread_results[:5]:
        print(f"  {result}")
    
    # 12. Performance Comparison
    print("\n12. Performance Comparison:")
    
    # Compare fibonacci implementations
    n = 25
    
    start_time = time.time()
    fib_iterative = fibonacci_iterative(n)
    iterative_time = time.time() - start_time
    
    start_time = time.time()
    fib_dp = fibonacci_dp(n)
    dp_time = time.time() - start_time
    
    print(f"Fibonacci({n}):")
    print(f"  Iterative: {fib_iterative} (Time: {iterative_time:.6f}s)")
    print(f"  DP: {fib_dp} (Time: {dp_time:.6f}s)")
    
    # 13. JSON Processing
    print("\n13. JSON Processing:")
    data = {
        "users": [
            {"id": 1, "name": "Alice", "age": 30, "skills": ["Python", "Go"]},
            {"id": 2, "name": "Bob", "age": 25, "skills": ["JavaScript", "Python"]},
            {"id": 3, "name": "Charlie", "age": 35, "skills": ["Go", "Rust"]}
        ],
        "timestamp": time.time()
    }
    
    json_string = json.dumps(data, indent=2)
    print("JSON data:")
    print(json_string[:200] + "..." if len(json_string) > 200 else json_string)
    
    parsed_data = json.loads(json_string)
    print(f"Parsed {len(parsed_data['users'])} users from JSON")
    
    print("\n=== Comprehensive Demo Complete ===")


if __name__ == "__main__":
    main()