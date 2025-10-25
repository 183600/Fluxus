#!/usr/bin/env python3
"""
Advanced Data Structures and Algorithms Implementation in Python
"""

import heapq
import bisect
import random
import time
import math
from typing import List, Dict, Optional, Any, Tuple, Generic, TypeVar
from collections import defaultdict, deque
from dataclasses import dataclass
from abc import ABC, abstractmethod

T = TypeVar('T')
K = TypeVar('K')
V = TypeVar('V')


class Stack(Generic[T]):
    """Generic stack implementation."""
    
    def __init__(self):
        self._items: List[T] = []
    
    def push(self, item: T) -> None:
        self._items.append(item)
    
    def pop(self) -> T:
        if self.is_empty():
            raise IndexError("Stack is empty")
        return self._items.pop()
    
    def peek(self) -> T:
        if self.is_empty():
            raise IndexError("Stack is empty")
        return self._items[-1]
    
    def is_empty(self) -> bool:
        return len(self._items) == 0
    
    def size(self) -> int:
        return len(self._items)
    
    def __str__(self) -> str:
        return f"Stack({self._items})"


class Queue(Generic[T]):
    """Generic queue implementation using deque for efficiency."""
    
    def __init__(self):
        self._items = deque()
    
    def enqueue(self, item: T) -> None:
        self._items.append(item)
    
    def dequeue(self) -> T:
        if self.is_empty():
            raise IndexError("Queue is empty")
        return self._items.popleft()
    
    def front(self) -> T:
        if self.is_empty():
            raise IndexError("Queue is empty")
        return self._items[0]
    
    def is_empty(self) -> bool:
        return len(self._items) == 0
    
    def size(self) -> int:
        return len(self._items)
    
    def __str__(self) -> str:
        return f"Queue({list(self._items)})"


class PriorityQueue(Generic[T]):
    """Priority queue implementation using heapq."""
    
    def __init__(self):
        self._heap = []
        self._index = 0
    
    def push(self, item: T, priority: int) -> None:
        heapq.heappush(self._heap, (priority, self._index, item))
        self._index += 1
    
    def pop(self) -> T:
        if self.is_empty():
            raise IndexError("Priority queue is empty")
        return heapq.heappop(self._heap)[2]
    
    def peek(self) -> T:
        if self.is_empty():
            raise IndexError("Priority queue is empty")
        return self._heap[0][2]
    
    def is_empty(self) -> bool:
        return len(self._heap) == 0
    
    def size(self) -> int:
        return len(self._heap)


class TreeNode(Generic[T]):
    """Binary tree node."""
    
    def __init__(self, value: T):
        self.value = value
        self.left: Optional['TreeNode[T]'] = None
        self.right: Optional['TreeNode[T]'] = None


class BinarySearchTree(Generic[T]):
    """Binary Search Tree implementation."""
    
    def __init__(self):
        self.root: Optional[TreeNode[T]] = None
        self._size = 0
    
    def insert(self, value: T) -> None:
        self.root = self._insert_recursive(self.root, value)
        self._size += 1
    
    def _insert_recursive(self, node: Optional[TreeNode[T]], value: T) -> TreeNode[T]:
        if node is None:
            return TreeNode(value)
        
        if value < node.value:
            node.left = self._insert_recursive(node.left, value)
        elif value > node.value:
            node.right = self._insert_recursive(node.right, value)
        
        return node
    
    def search(self, value: T) -> bool:
        return self._search_recursive(self.root, value)
    
    def _search_recursive(self, node: Optional[TreeNode[T]], value: T) -> bool:
        if node is None:
            return False
        
        if value == node.value:
            return True
        elif value < node.value:
            return self._search_recursive(node.left, value)
        else:
            return self._search_recursive(node.right, value)
    
    def inorder_traversal(self) -> List[T]:
        result = []
        self._inorder_recursive(self.root, result)
        return result
    
    def _inorder_recursive(self, node: Optional[TreeNode[T]], result: List[T]) -> None:
        if node:
            self._inorder_recursive(node.left, result)
            result.append(node.value)
            self._inorder_recursive(node.right, result)
    
    def size(self) -> int:
        return self._size


class HashTable(Generic[K, V]):
    """Hash table implementation with chaining."""
    
    def __init__(self, initial_capacity: int = 16):
        self._capacity = initial_capacity
        self._size = 0
        self._buckets: List[List[Tuple[K, V]]] = [[] for _ in range(self._capacity)]
    
    def _hash(self, key: K) -> int:
        return hash(key) % self._capacity
    
    def put(self, key: K, value: V) -> None:
        index = self._hash(key)
        bucket = self._buckets[index]
        
        # Update existing key
        for i, (k, v) in enumerate(bucket):
            if k == key:
                bucket[i] = (key, value)
                return
        
        # Add new key-value pair
        bucket.append((key, value))
        self._size += 1
        
        # Resize if load factor > 0.75
        if self._size > 0.75 * self._capacity:
            self._resize()
    
    def get(self, key: K) -> V:
        index = self._hash(key)
        bucket = self._buckets[index]
        
        for k, v in bucket:
            if k == key:
                return v
        
        raise KeyError(key)
    
    def remove(self, key: K) -> None:
        index = self._hash(key)
        bucket = self._buckets[index]
        
        for i, (k, v) in enumerate(bucket):
            if k == key:
                bucket.pop(i)
                self._size -= 1
                return
        
        raise KeyError(key)
    
    def _resize(self) -> None:
        old_buckets = self._buckets
        self._capacity *= 2
        self._size = 0
        self._buckets = [[] for _ in range(self._capacity)]
        
        for bucket in old_buckets:
            for key, value in bucket:
                self.put(key, value)
    
    def size(self) -> int:
        return self._size


class Graph:
    """Graph implementation using adjacency list."""
    
    def __init__(self, directed: bool = False):
        self.directed = directed
        self.vertices: Dict[Any, List[Any]] = defaultdict(list)
        self.weights: Dict[Tuple[Any, Any], float] = {}
    
    def add_vertex(self, vertex: Any) -> None:
        if vertex not in self.vertices:
            self.vertices[vertex] = []
    
    def add_edge(self, from_vertex: Any, to_vertex: Any, weight: float = 1.0) -> None:
        self.add_vertex(from_vertex)
        self.add_vertex(to_vertex)
        
        self.vertices[from_vertex].append(to_vertex)
        self.weights[(from_vertex, to_vertex)] = weight
        
        if not self.directed:
            self.vertices[to_vertex].append(from_vertex)
            self.weights[(to_vertex, from_vertex)] = weight
    
    def bfs(self, start: Any) -> List[Any]:
        """Breadth-first search."""
        visited = set()
        queue = deque([start])
        result = []
        
        while queue:
            vertex = queue.popleft()
            if vertex not in visited:
                visited.add(vertex)
                result.append(vertex)
                
                for neighbor in self.vertices[vertex]:
                    if neighbor not in visited:
                        queue.append(neighbor)
        
        return result
    
    def dfs(self, start: Any) -> List[Any]:
        """Depth-first search."""
        visited = set()
        result = []
        
        def dfs_recursive(vertex):
            visited.add(vertex)
            result.append(vertex)
            
            for neighbor in self.vertices[vertex]:
                if neighbor not in visited:
                    dfs_recursive(neighbor)
        
        dfs_recursive(start)
        return result
    
    def dijkstra(self, start: Any) -> Dict[Any, float]:
        """Dijkstra's shortest path algorithm."""
        distances = {vertex: float('infinity') for vertex in self.vertices}
        distances[start] = 0
        
        priority_queue = [(0, start)]
        visited = set()
        
        while priority_queue:
            current_distance, current_vertex = heapq.heappop(priority_queue)
            
            if current_vertex in visited:
                continue
            
            visited.add(current_vertex)
            
            for neighbor in self.vertices[current_vertex]:
                weight = self.weights.get((current_vertex, neighbor), 1.0)
                distance = current_distance + weight
                
                if distance < distances[neighbor]:
                    distances[neighbor] = distance
                    heapq.heappush(priority_queue, (distance, neighbor))
        
        return distances


class TrieNode:
    """Trie node for string operations."""
    
    def __init__(self):
        self.children: Dict[str, 'TrieNode'] = {}
        self.is_end_word = False


class Trie:
    """Trie (prefix tree) implementation."""
    
    def __init__(self):
        self.root = TrieNode()
    
    def insert(self, word: str) -> None:
        node = self.root
        for char in word:
            if char not in node.children:
                node.children[char] = TrieNode()
            node = node.children[char]
        node.is_end_word = True
    
    def search(self, word: str) -> bool:
        node = self.root
        for char in word:
            if char not in node.children:
                return False
            node = node.children[char]
        return node.is_end_word
    
    def starts_with(self, prefix: str) -> bool:
        node = self.root
        for char in prefix:
            if char not in node.children:
                return False
            node = node.children[char]
        return True
    
    def get_words_with_prefix(self, prefix: str) -> List[str]:
        """Get all words with given prefix."""
        node = self.root
        for char in prefix:
            if char not in node.children:
                return []
            node = node.children[char]
        
        words = []
        self._collect_words(node, prefix, words)
        return words
    
    def _collect_words(self, node: TrieNode, current_word: str, words: List[str]) -> None:
        if node.is_end_word:
            words.append(current_word)
        
        for char, child_node in node.children.items():
            self._collect_words(child_node, current_word + char, words)


# Sorting Algorithms
class SortingAlgorithms:
    """Collection of sorting algorithms."""
    
    @staticmethod
    def bubble_sort(arr: List[T]) -> List[T]:
        """Bubble sort algorithm."""
        arr = arr.copy()
        n = len(arr)
        
        for i in range(n):
            for j in range(0, n - i - 1):
                if arr[j] > arr[j + 1]:
                    arr[j], arr[j + 1] = arr[j + 1], arr[j]
        
        return arr
    
    @staticmethod
    def selection_sort(arr: List[T]) -> List[T]:
        """Selection sort algorithm."""
        arr = arr.copy()
        n = len(arr)
        
        for i in range(n):
            min_idx = i
            for j in range(i + 1, n):
                if arr[j] < arr[min_idx]:
                    min_idx = j
            arr[i], arr[min_idx] = arr[min_idx], arr[i]
        
        return arr
    
    @staticmethod
    def insertion_sort(arr: List[T]) -> List[T]:
        """Insertion sort algorithm."""
        arr = arr.copy()
        
        for i in range(1, len(arr)):
            key = arr[i]
            j = i - 1
            
            while j >= 0 and arr[j] > key:
                arr[j + 1] = arr[j]
                j -= 1
            
            arr[j + 1] = key
        
        return arr
    
    @staticmethod
    def quick_sort(arr: List[T]) -> List[T]:
        """Quick sort algorithm."""
        if len(arr) <= 1:
            return arr
        
        pivot = arr[len(arr) // 2]
        left = [x for x in arr if x < pivot]
        middle = [x for x in arr if x == pivot]
        right = [x for x in arr if x > pivot]
        
        return SortingAlgorithms.quick_sort(left) + middle + SortingAlgorithms.quick_sort(right)
    
    @staticmethod
    def merge_sort(arr: List[T]) -> List[T]:
        """Merge sort algorithm."""
        if len(arr) <= 1:
            return arr
        
        mid = len(arr) // 2
        left = SortingAlgorithms.merge_sort(arr[:mid])
        right = SortingAlgorithms.merge_sort(arr[mid:])
        
        return SortingAlgorithms._merge(left, right)
    
    @staticmethod
    def _merge(left: List[T], right: List[T]) -> List[T]:
        """Merge two sorted arrays."""
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


# Advanced Algorithms
class AdvancedAlgorithms:
    """Collection of advanced algorithms."""
    
    @staticmethod
    def binary_search(arr: List[T], target: T) -> int:
        """Binary search algorithm."""
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
    
    @staticmethod
    def longest_common_subsequence(s1: str, s2: str) -> str:
        """Find longest common subsequence using dynamic programming."""
        m, n = len(s1), len(s2)
        dp = [[0] * (n + 1) for _ in range(m + 1)]
        
        # Fill DP table
        for i in range(1, m + 1):
            for j in range(1, n + 1):
                if s1[i - 1] == s2[j - 1]:
                    dp[i][j] = dp[i - 1][j - 1] + 1
                else:
                    dp[i][j] = max(dp[i - 1][j], dp[i][j - 1])
        
        # Reconstruct LCS
        lcs = []
        i, j = m, n
        while i > 0 and j > 0:
            if s1[i - 1] == s2[j - 1]:
                lcs.append(s1[i - 1])
                i -= 1
                j -= 1
            elif dp[i - 1][j] > dp[i][j - 1]:
                i -= 1
            else:
                j -= 1
        
        return ''.join(reversed(lcs))
    
    @staticmethod
    def knapsack_01(weights: List[int], values: List[int], capacity: int) -> int:
        """0/1 Knapsack problem using dynamic programming."""
        n = len(weights)
        dp = [[0] * (capacity + 1) for _ in range(n + 1)]
        
        for i in range(1, n + 1):
            for w in range(capacity + 1):
                if weights[i - 1] <= w:
                    dp[i][w] = max(
                        values[i - 1] + dp[i - 1][w - weights[i - 1]],
                        dp[i - 1][w]
                    )
                else:
                    dp[i][w] = dp[i - 1][w]
        
        return dp[n][capacity]
    
    @staticmethod
    def edit_distance(s1: str, s2: str) -> int:
        """Calculate edit distance (Levenshtein distance)."""
        m, n = len(s1), len(s2)
        dp = [[0] * (n + 1) for _ in range(m + 1)]
        
        # Initialize base cases
        for i in range(m + 1):
            dp[i][0] = i
        for j in range(n + 1):
            dp[0][j] = j
        
        # Fill DP table
        for i in range(1, m + 1):
            for j in range(1, n + 1):
                if s1[i - 1] == s2[j - 1]:
                    dp[i][j] = dp[i - 1][j - 1]
                else:
                    dp[i][j] = 1 + min(
                        dp[i - 1][j],      # deletion
                        dp[i][j - 1],      # insertion
                        dp[i - 1][j - 1]   # substitution
                    )
        
        return dp[m][n]


def benchmark_sorting_algorithms():
    """Benchmark different sorting algorithms."""
    print("\n=== Sorting Algorithms Benchmark ===")
    
    algorithms = [
        ("Bubble Sort", SortingAlgorithms.bubble_sort),
        ("Selection Sort", SortingAlgorithms.selection_sort),
        ("Insertion Sort", SortingAlgorithms.insertion_sort),
        ("Quick Sort", SortingAlgorithms.quick_sort),
        ("Merge Sort", SortingAlgorithms.merge_sort),
        ("Python's Built-in", sorted)
    ]
    
    test_sizes = [100, 500, 1000]
    
    for size in test_sizes:
        print(f"\nArray size: {size}")
        test_data = [random.randint(1, 1000) for _ in range(size)]
        
        for name, algorithm in algorithms:
            start_time = time.time()
            sorted_data = algorithm(test_data)
            end_time = time.time()
            
            # Verify correctness
            is_sorted = all(sorted_data[i] <= sorted_data[i + 1] 
                          for i in range(len(sorted_data) - 1))
            
            print(f"  {name:20} | {end_time - start_time:8.4f}s | {'✓' if is_sorted else '✗'}")


def main():
    """Main function demonstrating data structures and algorithms."""
    print("=== Advanced Data Structures and Algorithms Demo ===")
    
    # 1. Stack Demo
    print("\n1. Stack Operations:")
    stack = Stack[int]()
    for i in range(5):
        stack.push(i)
        print(f"  Pushed {i}, stack: {stack}")
    
    while not stack.is_empty():
        item = stack.pop()
        print(f"  Popped {item}, stack: {stack}")
    
    # 2. Queue Demo
    print("\n2. Queue Operations:")
    queue = Queue[str]()
    for item in ["first", "second", "third"]:
        queue.enqueue(item)
        print(f"  Enqueued '{item}', queue: {queue}")
    
    while not queue.is_empty():
        item = queue.dequeue()
        print(f"  Dequeued '{item}', queue: {queue}")
    
    # 3. Priority Queue Demo
    print("\n3. Priority Queue Operations:")
    pq = PriorityQueue[str]()
    tasks = [("Low priority task", 3), ("High priority task", 1), ("Medium priority task", 2)]
    
    for task, priority in tasks:
        pq.push(task, priority)
        print(f"  Added '{task}' with priority {priority}")
    
    print("  Processing tasks by priority:")
    while not pq.is_empty():
        task = pq.pop()
        print(f"    {task}")
    
    # 4. Binary Search Tree Demo
    print("\n4. Binary Search Tree:")
    bst = BinarySearchTree[int]()
    values = [50, 30, 70, 20, 40, 60, 80]
    
    for value in values:
        bst.insert(value)
    
    print(f"  Inserted values: {values}")
    print(f"  In-order traversal: {bst.inorder_traversal()}")
    print(f"  Search 40: {bst.search(40)}")
    print(f"  Search 25: {bst.search(25)}")
    
    # 5. Hash Table Demo
    print("\n5. Hash Table:")
    ht = HashTable[str, int]()
    
    items = [("apple", 5), ("banana", 3), ("orange", 8), ("grape", 12)]
    for key, value in items:
        ht.put(key, value)
        print(f"  Put '{key}': {value}")
    
    for key, _ in items:
        print(f"  Get '{key}': {ht.get(key)}")
    
    # 6. Graph Demo
    print("\n6. Graph Algorithms:")
    graph = Graph()
    
    # Create a simple graph
    edges = [('A', 'B', 1), ('A', 'C', 4), ('B', 'C', 2), ('B', 'D', 5), ('C', 'D', 1)]
    for from_v, to_v, weight in edges:
        graph.add_edge(from_v, to_v, weight)
    
    print(f"  BFS from A: {graph.bfs('A')}")
    print(f"  DFS from A: {graph.dfs('A')}")
    
    distances = graph.dijkstra('A')
    print(f"  Shortest distances from A: {distances}")
    
    # 7. Trie Demo
    print("\n7. Trie (Prefix Tree):")
    trie = Trie()
    
    words = ["apple", "app", "application", "apply", "banana", "band"]
    for word in words:
        trie.insert(word)
    
    print(f"  Inserted words: {words}")
    print(f"  Search 'app': {trie.search('app')}")
    print(f"  Search 'appl': {trie.search('appl')}")
    print(f"  Starts with 'app': {trie.starts_with('app')}")
    print(f"  Words with prefix 'app': {trie.get_words_with_prefix('app')}")
    
    # 8. Advanced Algorithms Demo
    print("\n8. Advanced Algorithms:")
    
    # Binary search
    sorted_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
    target = 7
    index = AdvancedAlgorithms.binary_search(sorted_array, target)
    print(f"  Binary search for {target} in {sorted_array}: index {index}")
    
    # Longest Common Subsequence
    s1, s2 = "ABCDGH", "AEDFHR"
    lcs = AdvancedAlgorithms.longest_common_subsequence(s1, s2)
    print(f"  LCS of '{s1}' and '{s2}': '{lcs}'")
    
    # 0/1 Knapsack
    weights = [10, 20, 30]
    values = [60, 100, 120]
    capacity = 50
    max_value = AdvancedAlgorithms.knapsack_01(weights, values, capacity)
    print(f"  Knapsack (capacity {capacity}): max value {max_value}")
    
    # Edit distance
    s1, s2 = "kitten", "sitting"
    distance = AdvancedAlgorithms.edit_distance(s1, s2)
    print(f"  Edit distance between '{s1}' and '{s2}': {distance}")
    
    # 9. Sorting Algorithms Benchmark
    benchmark_sorting_algorithms()
    
    print("\n=== Demo completed ===")


if __name__ == "__main__":
    main()