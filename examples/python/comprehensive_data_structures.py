#!/usr/bin/env python3

import sys
import time
import math
import random
from typing import List, Dict, Tuple, Optional, Set, Any, Union
from collections import deque, defaultdict, Counter
from enum import Enum
import heapq
import bisect

class DataStructureShowcase:
    """Comprehensive demonstration of various data structures and their operations."""
    
    def __init__(self):
        self.demo_results = {}
    
    def run_all_demos(self):
        """Run all data structure demonstrations."""
        print("Comprehensive Data Structures Showcase")
        print("=" * 50)
        
        demos = [
            ("Dynamic Array", self.demo_dynamic_array),
            ("Linked List", self.demo_linked_list),
            ("Stack", self.demo_stack),
            ("Queue", self.demo_queue),
            ("Binary Search Tree", self.demo_bst),
            ("AVL Tree", self.demo_avl_tree),
            ("Hash Table", self.demo_hash_table),
            ("Heap", self.demo_heap),
            ("Trie", self.demo_trie),
            ("Graph", self.demo_graph),
            ("Disjoint Set", self.demo_disjoint_set),
            ("LRU Cache", self.demo_lru_cache),
            ("Bloom Filter", self.demo_bloom_filter),
            ("Skip List", self.demo_skip_list)
        ]
        
        for name, demo_func in demos:
            print(f"\n{name} Demo:")
            print("-" * 30)
            start_time = time.perf_counter()
            demo_func()
            end_time = time.perf_counter()
            print(f"Demo completed in {end_time - start_time:.4f} seconds")

class DynamicArray:
    """Dynamic array implementation with automatic resizing."""
    
    def __init__(self, initial_capacity: int = 4):
        self.capacity = initial_capacity
        self.size = 0
        self.data = [None] * self.capacity
    
    def _resize(self, new_capacity: int):
        """Resize the internal array."""
        old_data = self.data
        self.capacity = new_capacity
        self.data = [None] * self.capacity
        
        for i in range(self.size):
            self.data[i] = old_data[i]
    
    def append(self, item):
        """Add item to end of array."""
        if self.size == self.capacity:
            self._resize(2 * self.capacity)
        
        self.data[self.size] = item
        self.size += 1
    
    def get(self, index: int):
        """Get item at index."""
        if 0 <= index < self.size:
            return self.data[index]
        raise IndexError("Index out of range")
    
    def set(self, index: int, item):
        """Set item at index."""
        if 0 <= index < self.size:
            self.data[index] = item
        else:
            raise IndexError("Index out of range")
    
    def insert(self, index: int, item):
        """Insert item at index."""
        if index < 0 or index > self.size:
            raise IndexError("Index out of range")
        
        if self.size == self.capacity:
            self._resize(2 * self.capacity)
        
        # Shift elements to the right
        for i in range(self.size, index, -1):
            self.data[i] = self.data[i - 1]
        
        self.data[index] = item
        self.size += 1
    
    def delete(self, index: int):
        """Delete item at index."""
        if index < 0 or index >= self.size:
            raise IndexError("Index out of range")
        
        # Shift elements to the left
        for i in range(index, self.size - 1):
            self.data[i] = self.data[i + 1]
        
        self.size -= 1
        
        # Shrink if necessary
        if self.size <= self.capacity // 4 and self.capacity > 4:
            self._resize(self.capacity // 2)
    
    def __str__(self):
        return str([self.data[i] for i in range(self.size)])

class Node:
    """Generic node for linked structures."""
    
    def __init__(self, data, next_node=None):
        self.data = data
        self.next = next_node

class LinkedList:
    """Singly linked list implementation."""
    
    def __init__(self):
        self.head = None
        self.size = 0
    
    def prepend(self, data):
        """Add element to the beginning."""
        new_node = Node(data, self.head)
        self.head = new_node
        self.size += 1
    
    def append(self, data):
        """Add element to the end."""
        new_node = Node(data)
        
        if not self.head:
            self.head = new_node
        else:
            current = self.head
            while current.next:
                current = current.next
            current.next = new_node
        
        self.size += 1
    
    def insert(self, index: int, data):
        """Insert element at index."""
        if index < 0 or index > self.size:
            raise IndexError("Index out of range")
        
        if index == 0:
            self.prepend(data)
            return
        
        new_node = Node(data)
        current = self.head
        
        for i in range(index - 1):
            current = current.next
        
        new_node.next = current.next
        current.next = new_node
        self.size += 1
    
    def delete(self, index: int):
        """Delete element at index."""
        if index < 0 or index >= self.size:
            raise IndexError("Index out of range")
        
        if index == 0:
            self.head = self.head.next
            self.size -= 1
            return
        
        current = self.head
        for i in range(index - 1):
            current = current.next
        
        current.next = current.next.next
        self.size -= 1
    
    def find(self, data):
        """Find index of element."""
        current = self.head
        index = 0
        
        while current:
            if current.data == data:
                return index
            current = current.next
            index += 1
        
        return -1
    
    def to_list(self):
        """Convert to Python list."""
        result = []
        current = self.head
        while current:
            result.append(current.data)
            current = current.next
        return result

class Stack:
    """Stack implementation using dynamic array."""
    
    def __init__(self):
        self.items = []
    
    def push(self, item):
        """Push item onto stack."""
        self.items.append(item)
    
    def pop(self):
        """Pop item from stack."""
        if self.is_empty():
            raise IndexError("Pop from empty stack")
        return self.items.pop()
    
    def peek(self):
        """Peek at top item."""
        if self.is_empty():
            raise IndexError("Peek at empty stack")
        return self.items[-1]
    
    def is_empty(self):
        """Check if stack is empty."""
        return len(self.items) == 0
    
    def size(self):
        """Get stack size."""
        return len(self.items)

class Queue:
    """Queue implementation using deque."""
    
    def __init__(self):
        self.items = deque()
    
    def enqueue(self, item):
        """Add item to rear of queue."""
        self.items.append(item)
    
    def dequeue(self):
        """Remove item from front of queue."""
        if self.is_empty():
            raise IndexError("Dequeue from empty queue")
        return self.items.popleft()
    
    def front(self):
        """Peek at front item."""
        if self.is_empty():
            raise IndexError("Peek at empty queue")
        return self.items[0]
    
    def is_empty(self):
        """Check if queue is empty."""
        return len(self.items) == 0
    
    def size(self):
        """Get queue size."""
        return len(self.items)

class TreeNode:
    """Binary tree node."""
    
    def __init__(self, data):
        self.data = data
        self.left = None
        self.right = None
        self.height = 1  # For AVL tree

class BinarySearchTree:
    """Binary Search Tree implementation."""
    
    def __init__(self):
        self.root = None
    
    def insert(self, data):
        """Insert data into BST."""
        self.root = self._insert_recursive(self.root, data)
    
    def _insert_recursive(self, node, data):
        """Recursive helper for insert."""
        if not node:
            return TreeNode(data)
        
        if data < node.data:
            node.left = self._insert_recursive(node.left, data)
        elif data > node.data:
            node.right = self._insert_recursive(node.right, data)
        
        return node
    
    def search(self, data):
        """Search for data in BST."""
        return self._search_recursive(self.root, data)
    
    def _search_recursive(self, node, data):
        """Recursive helper for search."""
        if not node or node.data == data:
            return node is not None
        
        if data < node.data:
            return self._search_recursive(node.left, data)
        else:
            return self._search_recursive(node.right, data)
    
    def inorder_traversal(self):
        """Inorder traversal of BST."""
        result = []
        self._inorder_recursive(self.root, result)
        return result
    
    def _inorder_recursive(self, node, result):
        """Recursive helper for inorder traversal."""
        if node:
            self._inorder_recursive(node.left, result)
            result.append(node.data)
            self._inorder_recursive(node.right, result)
    
    def delete(self, data):
        """Delete data from BST."""
        self.root = self._delete_recursive(self.root, data)
    
    def _delete_recursive(self, node, data):
        """Recursive helper for delete."""
        if not node:
            return node
        
        if data < node.data:
            node.left = self._delete_recursive(node.left, data)
        elif data > node.data:
            node.right = self._delete_recursive(node.right, data)
        else:
            # Node to be deleted found
            if not node.left:
                return node.right
            elif not node.right:
                return node.left
            
            # Node with two children
            successor = self._find_min(node.right)
            node.data = successor.data
            node.right = self._delete_recursive(node.right, successor.data)
        
        return node
    
    def _find_min(self, node):
        """Find minimum node in subtree."""
        current = node
        while current.left:
            current = current.left
        return current

class AVLTree:
    """AVL Tree (self-balancing BST) implementation."""
    
    def __init__(self):
        self.root = None
    
    def _get_height(self, node):
        """Get height of node."""
        if not node:
            return 0
        return node.height
    
    def _get_balance(self, node):
        """Get balance factor of node."""
        if not node:
            return 0
        return self._get_height(node.left) - self._get_height(node.right)
    
    def _rotate_right(self, y):
        """Right rotate."""
        x = y.left
        T2 = x.right
        
        x.right = y
        y.left = T2
        
        y.height = 1 + max(self._get_height(y.left), self._get_height(y.right))
        x.height = 1 + max(self._get_height(x.left), self._get_height(x.right))
        
        return x
    
    def _rotate_left(self, x):
        """Left rotate."""
        y = x.right
        T2 = y.left
        
        y.left = x
        x.right = T2
        
        x.height = 1 + max(self._get_height(x.left), self._get_height(x.right))
        y.height = 1 + max(self._get_height(y.left), self._get_height(y.right))
        
        return y
    
    def insert(self, data):
        """Insert data into AVL tree."""
        self.root = self._insert_recursive(self.root, data)
    
    def _insert_recursive(self, node, data):
        """Recursive helper for insert with balancing."""
        # Standard BST insertion
        if not node:
            return TreeNode(data)
        
        if data < node.data:
            node.left = self._insert_recursive(node.left, data)
        elif data > node.data:
            node.right = self._insert_recursive(node.right, data)
        else:
            return node  # Duplicate keys not allowed
        
        # Update height
        node.height = 1 + max(self._get_height(node.left), self._get_height(node.right))
        
        # Get balance factor
        balance = self._get_balance(node)
        
        # Left Left Case
        if balance > 1 and data < node.left.data:
            return self._rotate_right(node)
        
        # Right Right Case
        if balance < -1 and data > node.right.data:
            return self._rotate_left(node)
        
        # Left Right Case
        if balance > 1 and data > node.left.data:
            node.left = self._rotate_left(node.left)
            return self._rotate_right(node)
        
        # Right Left Case
        if balance < -1 and data < node.right.data:
            node.right = self._rotate_right(node.right)
            return self._rotate_left(node)
        
        return node
    
    def inorder_traversal(self):
        """Inorder traversal of AVL tree."""
        result = []
        self._inorder_recursive(self.root, result)
        return result
    
    def _inorder_recursive(self, node, result):
        """Recursive helper for inorder traversal."""
        if node:
            self._inorder_recursive(node.left, result)
            result.append(node.data)
            self._inorder_recursive(node.right, result)

class HashTable:
    """Hash table implementation with chaining."""
    
    def __init__(self, initial_capacity: int = 16):
        self.capacity = initial_capacity
        self.size = 0
        self.buckets = [[] for _ in range(self.capacity)]
    
    def _hash(self, key):
        """Hash function."""
        return hash(key) % self.capacity
    
    def _resize(self):
        """Resize hash table when load factor exceeds threshold."""
        old_buckets = self.buckets
        self.capacity *= 2
        self.size = 0
        self.buckets = [[] for _ in range(self.capacity)]
        
        for bucket in old_buckets:
            for key, value in bucket:
                self.put(key, value)
    
    def put(self, key, value):
        """Insert/update key-value pair."""
        if self.size >= self.capacity * 0.75:  # Load factor threshold
            self._resize()
        
        bucket_index = self._hash(key)
        bucket = self.buckets[bucket_index]
        
        for i, (k, v) in enumerate(bucket):
            if k == key:
                bucket[i] = (key, value)  # Update existing
                return
        
        bucket.append((key, value))  # Insert new
        self.size += 1
    
    def get(self, key):
        """Get value by key."""
        bucket_index = self._hash(key)
        bucket = self.buckets[bucket_index]
        
        for k, v in bucket:
            if k == key:
                return v
        
        raise KeyError(f"Key '{key}' not found")
    
    def delete(self, key):
        """Delete key-value pair."""
        bucket_index = self._hash(key)
        bucket = self.buckets[bucket_index]
        
        for i, (k, v) in enumerate(bucket):
            if k == key:
                del bucket[i]
                self.size -= 1
                return
        
        raise KeyError(f"Key '{key}' not found")
    
    def keys(self):
        """Get all keys."""
        result = []
        for bucket in self.buckets:
            for key, _ in bucket:
                result.append(key)
        return result
    
    def values(self):
        """Get all values."""
        result = []
        for bucket in self.buckets:
            for _, value in bucket:
                result.append(value)
        return result

class MinHeap:
    """Min heap implementation."""
    
    def __init__(self):
        self.heap = []
    
    def _parent(self, i):
        """Get parent index."""
        return (i - 1) // 2
    
    def _left_child(self, i):
        """Get left child index."""
        return 2 * i + 1
    
    def _right_child(self, i):
        """Get right child index."""
        return 2 * i + 2
    
    def _swap(self, i, j):
        """Swap elements at indices i and j."""
        self.heap[i], self.heap[j] = self.heap[j], self.heap[i]
    
    def insert(self, value):
        """Insert value into heap."""
        self.heap.append(value)
        self._heapify_up(len(self.heap) - 1)
    
    def _heapify_up(self, i):
        """Maintain heap property upward."""
        while i > 0 and self.heap[self._parent(i)] > self.heap[i]:
            self._swap(i, self._parent(i))
            i = self._parent(i)
    
    def extract_min(self):
        """Extract minimum element."""
        if not self.heap:
            raise IndexError("Extract from empty heap")
        
        min_val = self.heap[0]
        self.heap[0] = self.heap[-1]
        self.heap.pop()
        
        if self.heap:
            self._heapify_down(0)
        
        return min_val
    
    def _heapify_down(self, i):
        """Maintain heap property downward."""
        min_index = i
        left = self._left_child(i)
        right = self._right_child(i)
        
        if left < len(self.heap) and self.heap[left] < self.heap[min_index]:
            min_index = left
        
        if right < len(self.heap) and self.heap[right] < self.heap[min_index]:
            min_index = right
        
        if min_index != i:
            self._swap(i, min_index)
            self._heapify_down(min_index)
    
    def peek(self):
        """Peek at minimum element."""
        if not self.heap:
            raise IndexError("Peek at empty heap")
        return self.heap[0]
    
    def size(self):
        """Get heap size."""
        return len(self.heap)

class TrieNode:
    """Trie node."""
    
    def __init__(self):
        self.children = {}
        self.is_end_of_word = False

class Trie:
    """Trie (prefix tree) implementation."""
    
    def __init__(self):
        self.root = TrieNode()
    
    def insert(self, word: str):
        """Insert word into trie."""
        node = self.root
        
        for char in word:
            if char not in node.children:
                node.children[char] = TrieNode()
            node = node.children[char]
        
        node.is_end_of_word = True
    
    def search(self, word: str):
        """Search for word in trie."""
        node = self.root
        
        for char in word:
            if char not in node.children:
                return False
            node = node.children[char]
        
        return node.is_end_of_word
    
    def starts_with(self, prefix: str):
        """Check if any word starts with prefix."""
        node = self.root
        
        for char in prefix:
            if char not in node.children:
                return False
            node = node.children[char]
        
        return True
    
    def get_words_with_prefix(self, prefix: str):
        """Get all words with given prefix."""
        node = self.root
        
        # Navigate to prefix node
        for char in prefix:
            if char not in node.children:
                return []
            node = node.children[char]
        
        # DFS to find all words
        words = []
        self._dfs(node, prefix, words)
        return words
    
    def _dfs(self, node, current_word, words):
        """DFS helper for finding words."""
        if node.is_end_of_word:
            words.append(current_word)
        
        for char, child_node in node.children.items():
            self._dfs(child_node, current_word + char, words)

class Graph:
    """Graph implementation with adjacency list."""
    
    def __init__(self, directed: bool = False):
        self.directed = directed
        self.vertices = set()
        self.adjacency_list = defaultdict(list)
    
    def add_vertex(self, vertex):
        """Add vertex to graph."""
        self.vertices.add(vertex)
    
    def add_edge(self, from_vertex, to_vertex, weight: float = 1.0):
        """Add edge to graph."""
        self.vertices.add(from_vertex)
        self.vertices.add(to_vertex)
        self.adjacency_list[from_vertex].append((to_vertex, weight))
        
        if not self.directed:
            self.adjacency_list[to_vertex].append((from_vertex, weight))
    
    def get_neighbors(self, vertex):
        """Get neighbors of vertex."""
        return self.adjacency_list[vertex]
    
    def bfs(self, start_vertex):
        """Breadth-first search."""
        visited = set()
        queue = deque([start_vertex])
        result = []
        
        while queue:
            vertex = queue.popleft()
            if vertex not in visited:
                visited.add(vertex)
                result.append(vertex)
                
                for neighbor, _ in self.adjacency_list[vertex]:
                    if neighbor not in visited:
                        queue.append(neighbor)
        
        return result
    
    def dfs(self, start_vertex):
        """Depth-first search."""
        visited = set()
        result = []
        
        def dfs_helper(vertex):
            visited.add(vertex)
            result.append(vertex)
            
            for neighbor, _ in self.adjacency_list[vertex]:
                if neighbor not in visited:
                    dfs_helper(neighbor)
        
        dfs_helper(start_vertex)
        return result
    
    def dijkstra(self, start_vertex):
        """Dijkstra's shortest path algorithm."""
        distances = {vertex: float('infinity') for vertex in self.vertices}
        distances[start_vertex] = 0
        pq = [(0, start_vertex)]
        visited = set()
        
        while pq:
            current_distance, current_vertex = heapq.heappop(pq)
            
            if current_vertex in visited:
                continue
            
            visited.add(current_vertex)
            
            for neighbor, weight in self.adjacency_list[current_vertex]:
                distance = current_distance + weight
                
                if distance < distances[neighbor]:
                    distances[neighbor] = distance
                    heapq.heappush(pq, (distance, neighbor))
        
        return distances

class DisjointSet:
    """Disjoint Set (Union-Find) data structure."""
    
    def __init__(self, n: int):
        self.parent = list(range(n))
        self.rank = [0] * n
        self.num_sets = n
    
    def find(self, x: int):
        """Find root of set containing x with path compression."""
        if self.parent[x] != x:
            self.parent[x] = self.find(self.parent[x])
        return self.parent[x]
    
    def union(self, x: int, y: int):
        """Union sets containing x and y."""
        root_x = self.find(x)
        root_y = self.find(y)
        
        if root_x != root_y:
            # Union by rank
            if self.rank[root_x] < self.rank[root_y]:
                self.parent[root_x] = root_y
            elif self.rank[root_x] > self.rank[root_y]:
                self.parent[root_y] = root_x
            else:
                self.parent[root_y] = root_x
                self.rank[root_x] += 1
            
            self.num_sets -= 1
    
    def connected(self, x: int, y: int):
        """Check if x and y are in the same set."""
        return self.find(x) == self.find(y)
    
    def count_sets(self):
        """Count number of disjoint sets."""
        return self.num_sets

class LRUCache:
    """LRU (Least Recently Used) Cache implementation."""
    
    def __init__(self, capacity: int):
        self.capacity = capacity
        self.cache = {}
        self.order = deque()
    
    def get(self, key):
        """Get value by key."""
        if key in self.cache:
            # Move to end (most recently used)
            self.order.remove(key)
            self.order.append(key)
            return self.cache[key]
        return None
    
    def put(self, key, value):
        """Put key-value pair."""
        if key in self.cache:
            # Update existing key
            self.order.remove(key)
            self.order.append(key)
            self.cache[key] = value
        else:
            # Add new key
            if len(self.cache) >= self.capacity:
                # Remove least recently used
                oldest = self.order.popleft()
                del self.cache[oldest]
            
            self.cache[key] = value
            self.order.append(key)
    
    def keys(self):
        """Get all keys in order of usage."""
        return list(self.order)

class BloomFilter:
    """Bloom filter implementation."""
    
    def __init__(self, size: int, hash_functions: int):
        self.size = size
        self.hash_functions = hash_functions
        self.bit_array = [False] * size
    
    def _hash(self, item, seed):
        """Hash function with seed."""
        return (hash(str(item) + str(seed))) % self.size
    
    def add(self, item):
        """Add item to bloom filter."""
        for i in range(self.hash_functions):
            index = self._hash(item, i)
            self.bit_array[index] = True
    
    def contains(self, item):
        """Check if item might be in set."""
        for i in range(self.hash_functions):
            index = self._hash(item, i)
            if not self.bit_array[index]:
                return False
        return True

class SkipListNode:
    """Skip list node."""
    
    def __init__(self, value, level):
        self.value = value
        self.forward = [None] * (level + 1)

class SkipList:
    """Skip list implementation."""
    
    def __init__(self, max_level: int = 16):
        self.max_level = max_level
        self.header = SkipListNode(None, max_level)
        self.level = 0
    
    def _random_level(self):
        """Generate random level for new node."""
        level = 0
        while random.random() < 0.5 and level < self.max_level:
            level += 1
        return level
    
    def search(self, value):
        """Search for value in skip list."""
        current = self.header
        
        for i in range(self.level, -1, -1):
            while (current.forward[i] and 
                   current.forward[i].value < value):
                current = current.forward[i]
        
        current = current.forward[0]
        return current and current.value == value
    
    def insert(self, value):
        """Insert value into skip list."""
        update = [None] * (self.max_level + 1)
        current = self.header
        
        # Find update positions
        for i in range(self.level, -1, -1):
            while (current.forward[i] and 
                   current.forward[i].value < value):
                current = current.forward[i]
            update[i] = current
        
        current = current.forward[0]
        
        if not current or current.value != value:
            # Generate random level for new node
            new_level = self._random_level()
            
            if new_level > self.level:
                for i in range(self.level + 1, new_level + 1):
                    update[i] = self.header
                self.level = new_level
            
            # Create new node
            new_node = SkipListNode(value, new_level)
            
            # Update forward pointers
            for i in range(new_level + 1):
                new_node.forward[i] = update[i].forward[i]
                update[i].forward[i] = new_node
    
    def to_list(self):
        """Convert skip list to regular list."""
        result = []
        current = self.header.forward[0]
        while current:
            result.append(current.value)
            current = current.forward[0]
        return result

# Demo implementations for DataStructureShowcase
def demo_dynamic_array(self):
    """Demo dynamic array operations."""
    arr = DynamicArray()
    
    print("Dynamic Array Operations:")
    
    # Test append
    for i in range(10):
        arr.append(i)
    print(f"After appending 0-9: {arr}")
    print(f"Capacity: {arr.capacity}, Size: {arr.size}")
    
    # Test insert
    arr.insert(5, 99)
    print(f"After inserting 99 at index 5: {arr}")
    
    # Test delete
    arr.delete(5)
    print(f"After deleting index 5: {arr}")
    
    # Test get/set
    print(f"Element at index 3: {arr.get(3)}")
    arr.set(3, 42)
    print(f"After setting index 3 to 42: {arr}")

def demo_linked_list(self):
    """Demo linked list operations."""
    ll = LinkedList()
    
    print("Linked List Operations:")
    
    # Test append
    for i in range(1, 6):
        ll.append(i)
    print(f"After appending 1-5: {ll.to_list()}")
    
    # Test prepend
    ll.prepend(0)
    print(f"After prepending 0: {ll.to_list()}")
    
    # Test insert
    ll.insert(3, 99)
    print(f"After inserting 99 at index 3: {ll.to_list()}")
    
    # Test find
    print(f"Index of 99: {ll.find(99)}")
    
    # Test delete
    ll.delete(3)
    print(f"After deleting index 3: {ll.to_list()}")

def demo_stack(self):
    """Demo stack operations."""
    stack = Stack()
    
    print("Stack Operations:")
    
    # Test push
    for i in range(1, 6):
        stack.push(i)
        print(f"Pushed {i}, stack size: {stack.size()}")
    
    # Test peek
    print(f"Top element: {stack.peek()}")
    
    # Test pop
    while not stack.is_empty():
        popped = stack.pop()
        print(f"Popped {popped}, remaining size: {stack.size()}")

def demo_queue(self):
    """Demo queue operations."""
    queue = Queue()
    
    print("Queue Operations:")
    
    # Test enqueue
    for i in range(1, 6):
        queue.enqueue(i)
        print(f"Enqueued {i}, queue size: {queue.size()}")
    
    # Test front
    print(f"Front element: {queue.front()}")
    
    # Test dequeue
    while not queue.is_empty():
        dequeued = queue.dequeue()
        print(f"Dequeued {dequeued}, remaining size: {queue.size()}")

def demo_bst(self):
    """Demo binary search tree operations."""
    bst = BinarySearchTree()
    
    print("Binary Search Tree Operations:")
    
    # Test insert
    values = [50, 30, 70, 20, 40, 60, 80]
    for val in values:
        bst.insert(val)
    
    print(f"Inserted values: {values}")
    print(f"Inorder traversal: {bst.inorder_traversal()}")
    
    # Test search
    print(f"Search 40: {bst.search(40)}")
    print(f"Search 90: {bst.search(90)}")
    
    # Test delete
    bst.delete(30)
    print(f"After deleting 30: {bst.inorder_traversal()}")

def demo_avl_tree(self):
    """Demo AVL tree operations."""
    avl = AVLTree()
    
    print("AVL Tree Operations:")
    
    # Test insert (should trigger rotations)
    values = [10, 20, 30, 40, 50, 25]
    for val in values:
        avl.insert(val)
    
    print(f"Inserted values: {values}")
    print(f"Inorder traversal (balanced): {avl.inorder_traversal()}")

def demo_hash_table(self):
    """Demo hash table operations."""
    ht = HashTable()
    
    print("Hash Table Operations:")
    
    # Test put
    ht.put("apple", 5)
    ht.put("banana", 3)
    ht.put("orange", 8)
    ht.put("grape", 12)
    
    print(f"Keys: {ht.keys()}")
    print(f"Values: {ht.values()}")
    
    # Test get
    print(f"apple: {ht.get('apple')}")
    
    # Test update
    ht.put("apple", 10)
    print(f"Updated apple: {ht.get('apple')}")
    
    # Test delete
    ht.delete("banana")
    print(f"After deleting banana, keys: {ht.keys()}")

def demo_heap(self):
    """Demo heap operations."""
    heap = MinHeap()
    
    print("Min Heap Operations:")
    
    # Test insert
    values = [20, 15, 30, 10, 25, 5]
    for val in values:
        heap.insert(val)
        print(f"Inserted {val}, min: {heap.peek()}")
    
    # Test extract_min
    print("Extracting minimums:")
    while heap.size() > 0:
        min_val = heap.extract_min()
        print(f"Extracted {min_val}, remaining size: {heap.size()}")

def demo_trie(self):
    """Demo trie operations."""
    trie = Trie()
    
    print("Trie Operations:")
    
    # Test insert
    words = ["cat", "car", "card", "care", "careful", "cars", "carry"]
    for word in words:
        trie.insert(word)
    
    print(f"Inserted words: {words}")
    
    # Test search
    print(f"Search 'car': {trie.search('car')}")
    print(f"Search 'care': {trie.search('care')}")
    print(f"Search 'caring': {trie.search('caring')}")
    
    # Test starts_with
    print(f"Starts with 'car': {trie.starts_with('car')}")
    print(f"Words with prefix 'car': {trie.get_words_with_prefix('car')}")

def demo_graph(self):
    """Demo graph operations."""
    graph = Graph()
    
    print("Graph Operations:")
    
    # Add edges
    edges = [('A', 'B'), ('B', 'C'), ('C', 'D'), ('A', 'D'), ('B', 'D')]
    for from_v, to_v in edges:
        graph.add_edge(from_v, to_v)
    
    print(f"Added edges: {edges}")
    print(f"Vertices: {sorted(graph.vertices)}")
    
    # Test BFS
    print(f"BFS from A: {graph.bfs('A')}")
    
    # Test DFS
    print(f"DFS from A: {graph.dfs('A')}")
    
    # Test Dijkstra (with weighted edges)
    weighted_graph = Graph()
    weighted_edges = [('A', 'B', 4), ('A', 'C', 2), ('B', 'C', 1), ('B', 'D', 5), ('C', 'D', 8)]
    for from_v, to_v, weight in weighted_edges:
        weighted_graph.add_edge(from_v, to_v, weight)
    
    distances = weighted_graph.dijkstra('A')
    print(f"Shortest distances from A: {distances}")

def demo_disjoint_set(self):
    """Demo disjoint set operations."""
    ds = DisjointSet(6)
    
    print("Disjoint Set Operations:")
    print(f"Initial sets count: {ds.count_sets()}")
    
    # Test union operations
    unions = [(0, 1), (2, 3), (0, 2), (4, 5)]
    for x, y in unions:
        ds.union(x, y)
        print(f"Union({x}, {y}), sets count: {ds.count_sets()}")
    
    # Test connected queries
    queries = [(0, 3), (1, 4), (0, 4)]
    for x, y in queries:
        print(f"Connected({x}, {y}): {ds.connected(x, y)}")

def demo_lru_cache(self):
    """Demo LRU cache operations."""
    cache = LRUCache(3)
    
    print("LRU Cache Operations (capacity=3):")
    
    # Test put and get
    cache.put("a", 1)
    cache.put("b", 2)
    cache.put("c", 3)
    print(f"After putting a,b,c: keys={cache.keys()}")
    
    print(f"Get 'a': {cache.get('a')}")
    print(f"Keys after getting 'a': {cache.keys()}")
    
    cache.put("d", 4)  # Should evict 'b'
    print(f"After putting 'd': keys={cache.keys()}")
    
    print(f"Get 'b': {cache.get('b')}")  # Should be None

def demo_bloom_filter(self):
    """Demo bloom filter operations."""
    bf = BloomFilter(100, 3)
    
    print("Bloom Filter Operations:")
    
    # Add items
    items = ["apple", "banana", "orange", "grape"]
    for item in items:
        bf.add(item)
    
    print(f"Added items: {items}")
    
    # Test contains (should all be True)
    for item in items:
        print(f"Contains '{item}': {bf.contains(item)}")
    
    # Test items not added (may have false positives)
    test_items = ["cherry", "mango", "kiwi"]
    for item in test_items:
        print(f"Contains '{item}': {bf.contains(item)} (may be false positive)")

def demo_skip_list(self):
    """Demo skip list operations."""
    sl = SkipList()
    
    print("Skip List Operations:")
    
    # Insert values
    values = [3, 6, 7, 9, 12, 19, 17, 26, 21, 25]
    for val in values:
        sl.insert(val)
    
    print(f"Inserted values: {values}")
    print(f"Skip list contents: {sl.to_list()}")
    
    # Test search
    search_values = [7, 15, 19, 30]
    for val in search_values:
        print(f"Search {val}: {sl.search(val)}")

# Add methods to DataStructureShowcase class
DataStructureShowcase.demo_dynamic_array = demo_dynamic_array
DataStructureShowcase.demo_linked_list = demo_linked_list
DataStructureShowcase.demo_stack = demo_stack
DataStructureShowcase.demo_queue = demo_queue
DataStructureShowcase.demo_bst = demo_bst
DataStructureShowcase.demo_avl_tree = demo_avl_tree
DataStructureShowcase.demo_hash_table = demo_hash_table
DataStructureShowcase.demo_heap = demo_heap
DataStructureShowcase.demo_trie = demo_trie
DataStructureShowcase.demo_graph = demo_graph
DataStructureShowcase.demo_disjoint_set = demo_disjoint_set
DataStructureShowcase.demo_lru_cache = demo_lru_cache
DataStructureShowcase.demo_bloom_filter = demo_bloom_filter
DataStructureShowcase.demo_skip_list = demo_skip_list

def main():
    showcase = DataStructureShowcase()
    showcase.run_all_demos()

if __name__ == "__main__":
    main()