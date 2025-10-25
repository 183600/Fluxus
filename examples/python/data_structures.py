#!/usr/bin/env python3
"""
Data Structure Implementations and Algorithms
"""

from typing import List, Optional, Any, Iterator
import heapq
from collections import deque


class Node:
    """Basic node structure for linked lists and trees."""
    
    def __init__(self, data: Any):
        self.data = data
        self.next: Optional['Node'] = None
    
    def __str__(self) -> str:
        return str(self.data)


class LinkedList:
    """Implementation of a singly linked list."""
    
    def __init__(self):
        self.head: Optional[Node] = None
        self.size = 0
    
    def append(self, data: Any) -> None:
        """Add element to the end of the list."""
        new_node = Node(data)
        if not self.head:
            self.head = new_node
        else:
            current = self.head
            while current.next:
                current = current.next
            current.next = new_node
        self.size += 1
    
    def prepend(self, data: Any) -> None:
        """Add element to the beginning of the list."""
        new_node = Node(data)
        new_node.next = self.head
        self.head = new_node
        self.size += 1
    
    def delete(self, data: Any) -> bool:
        """Delete first occurrence of data."""
        if not self.head:
            return False
        
        if self.head.data == data:
            self.head = self.head.next
            self.size -= 1
            return True
        
        current = self.head
        while current.next:
            if current.next.data == data:
                current.next = current.next.next
                self.size -= 1
                return True
            current = current.next
        
        return False
    
    def find(self, data: Any) -> bool:
        """Check if data exists in the list."""
        current = self.head
        while current:
            if current.data == data:
                return True
            current = current.next
        return False
    
    def get_at_index(self, index: int) -> Any:
        """Get element at specific index."""
        if index < 0 or index >= self.size:
            raise IndexError("Index out of range")
        
        current = self.head
        for _ in range(index):
            current = current.next
        return current.data
    
    def reverse(self) -> None:
        """Reverse the linked list in place."""
        prev = None
        current = self.head
        
        while current:
            next_node = current.next
            current.next = prev
            prev = current
            current = next_node
        
        self.head = prev
    
    def to_list(self) -> List[Any]:
        """Convert to Python list."""
        result = []
        current = self.head
        while current:
            result.append(current.data)
            current = current.next
        return result
    
    def __len__(self) -> int:
        return self.size
    
    def __str__(self) -> str:
        return " -> ".join(str(data) for data in self.to_list())


class Stack:
    """Implementation of a stack using a list."""
    
    def __init__(self):
        self.items: List[Any] = []
    
    def push(self, item: Any) -> None:
        """Add item to top of stack."""
        self.items.append(item)
    
    def pop(self) -> Any:
        """Remove and return top item."""
        if self.is_empty():
            raise IndexError("Stack is empty")
        return self.items.pop()
    
    def peek(self) -> Any:
        """Return top item without removing it."""
        if self.is_empty():
            raise IndexError("Stack is empty")
        return self.items[-1]
    
    def is_empty(self) -> bool:
        """Check if stack is empty."""
        return len(self.items) == 0
    
    def size(self) -> int:
        """Return size of stack."""
        return len(self.items)
    
    def __str__(self) -> str:
        return f"Stack({self.items})"


class Queue:
    """Implementation of a queue using deque."""
    
    def __init__(self):
        self.items = deque()
    
    def enqueue(self, item: Any) -> None:
        """Add item to rear of queue."""
        self.items.append(item)
    
    def dequeue(self) -> Any:
        """Remove and return front item."""
        if self.is_empty():
            raise IndexError("Queue is empty")
        return self.items.popleft()
    
    def front(self) -> Any:
        """Return front item without removing it."""
        if self.is_empty():
            raise IndexError("Queue is empty")
        return self.items[0]
    
    def is_empty(self) -> bool:
        """Check if queue is empty."""
        return len(self.items) == 0
    
    def size(self) -> int:
        """Return size of queue."""
        return len(self.items)
    
    def __str__(self) -> str:
        return f"Queue({list(self.items)})"


class MinHeap:
    """Implementation of a min heap."""
    
    def __init__(self):
        self.heap: List[Any] = []
    
    def push(self, item: Any) -> None:
        """Add item to heap."""
        heapq.heappush(self.heap, item)
    
    def pop(self) -> Any:
        """Remove and return minimum item."""
        if self.is_empty():
            raise IndexError("Heap is empty")
        return heapq.heappop(self.heap)
    
    def peek(self) -> Any:
        """Return minimum item without removing it."""
        if self.is_empty():
            raise IndexError("Heap is empty")
        return self.heap[0]
    
    def is_empty(self) -> bool:
        """Check if heap is empty."""
        return len(self.heap) == 0
    
    def size(self) -> int:
        """Return size of heap."""
        return len(self.heap)
    
    def __str__(self) -> str:
        return f"MinHeap({self.heap})"


class SortingAlgorithms:
    """Collection of sorting algorithms."""
    
    @staticmethod
    def bubble_sort(arr: List[Any]) -> List[Any]:
        """Bubble sort implementation."""
        arr = arr.copy()
        n = len(arr)
        
        for i in range(n):
            swapped = False
            for j in range(0, n - i - 1):
                if arr[j] > arr[j + 1]:
                    arr[j], arr[j + 1] = arr[j + 1], arr[j]
                    swapped = True
            if not swapped:
                break
        
        return arr
    
    @staticmethod
    def selection_sort(arr: List[Any]) -> List[Any]:
        """Selection sort implementation."""
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
    def insertion_sort(arr: List[Any]) -> List[Any]:
        """Insertion sort implementation."""
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
    def merge_sort(arr: List[Any]) -> List[Any]:
        """Merge sort implementation."""
        if len(arr) <= 1:
            return arr.copy()
        
        mid = len(arr) // 2
        left = SortingAlgorithms.merge_sort(arr[:mid])
        right = SortingAlgorithms.merge_sort(arr[mid:])
        
        return SortingAlgorithms._merge(left, right)
    
    @staticmethod
    def _merge(left: List[Any], right: List[Any]) -> List[Any]:
        """Helper function for merge sort."""
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
    
    @staticmethod
    def quick_sort(arr: List[Any]) -> List[Any]:
        """Quick sort implementation."""
        if len(arr) <= 1:
            return arr.copy()
        
        pivot = arr[len(arr) // 2]
        left = [x for x in arr if x < pivot]
        middle = [x for x in arr if x == pivot]
        right = [x for x in arr if x > pivot]
        
        return (SortingAlgorithms.quick_sort(left) + 
                middle + 
                SortingAlgorithms.quick_sort(right))


class SearchAlgorithms:
    """Collection of search algorithms."""
    
    @staticmethod
    def linear_search(arr: List[Any], target: Any) -> int:
        """Linear search implementation."""
        for i, item in enumerate(arr):
            if item == target:
                return i
        return -1
    
    @staticmethod
    def binary_search(arr: List[Any], target: Any) -> int:
        """Binary search implementation (requires sorted array)."""
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


def demonstrate_data_structures():
    """Demonstrate data structure implementations."""
    print("=== Data Structures and Algorithms Demo ===")
    
    # Linked List
    print("\n--- Linked List ---")
    ll = LinkedList()
    for i in range(1, 6):
        ll.append(i)
    print(f"Original list: {ll}")
    ll.prepend(0)
    print(f"After prepend(0): {ll}")
    ll.delete(3)
    print(f"After delete(3): {ll}")
    ll.reverse()
    print(f"After reverse: {ll}")
    print(f"Element at index 2: {ll.get_at_index(2)}")
    print(f"Contains 4: {ll.find(4)}")
    
    # Stack
    print("\n--- Stack ---")
    stack = Stack()
    for i in range(1, 4):
        stack.push(i)
        print(f"Pushed {i}: {stack}")
    
    while not stack.is_empty():
        popped = stack.pop()
        print(f"Popped {popped}: {stack}")
    
    # Queue
    print("\n--- Queue ---")
    queue = Queue()
    for i in range(1, 4):
        queue.enqueue(i)
        print(f"Enqueued {i}: {queue}")
    
    while not queue.is_empty():
        dequeued = queue.dequeue()
        print(f"Dequeued {dequeued}: {queue}")
    
    # Min Heap
    print("\n--- Min Heap ---")
    heap = MinHeap()
    values = [5, 2, 8, 1, 9, 3]
    for val in values:
        heap.push(val)
        print(f"Pushed {val}: {heap}")
    
    print("Popping all elements:")
    while not heap.is_empty():
        min_val = heap.pop()
        print(f"Popped {min_val}: {heap}")
    
    # Sorting Algorithms
    print("\n--- Sorting Algorithms ---")
    test_array = [64, 34, 25, 12, 22, 11, 90]
    print(f"Original array: {test_array}")
    
    sorters = SortingAlgorithms()
    print(f"Bubble sort: {sorters.bubble_sort(test_array)}")
    print(f"Selection sort: {sorters.selection_sort(test_array)}")
    print(f"Insertion sort: {sorters.insertion_sort(test_array)}")
    print(f"Merge sort: {sorters.merge_sort(test_array)}")
    print(f"Quick sort: {sorters.quick_sort(test_array)}")
    
    # Search Algorithms
    print("\n--- Search Algorithms ---")
    sorted_array = sorted(test_array)
    print(f"Sorted array: {sorted_array}")
    
    target = 25
    linear_result = SearchAlgorithms.linear_search(test_array, target)
    binary_result = SearchAlgorithms.binary_search(sorted_array, target)
    
    print(f"Linear search for {target}: index {linear_result}")
    print(f"Binary search for {target}: index {binary_result}")
    
    # Performance comparison
    import time
    large_array = list(range(10000, 0, -1))  # Reverse sorted array
    
    print("\n--- Performance Test (10,000 elements) ---")
    algorithms = [
        ("Bubble Sort", sorters.bubble_sort),
        ("Selection Sort", sorters.selection_sort),
        ("Insertion Sort", sorters.insertion_sort),
        ("Merge Sort", sorters.merge_sort),
        ("Quick Sort", sorters.quick_sort),
    ]
    
    test_size = 1000  # Smaller test for bubble/selection sort
    small_array = large_array[:test_size]
    
    for name, algorithm in algorithms:
        start_time = time.time()
        if name in ["Bubble Sort", "Selection Sort"]:
            # Test with smaller array for slower algorithms
            algorithm(small_array)
            array_size = test_size
        else:
            algorithm(large_array)
            array_size = len(large_array)
        end_time = time.time()
        
        print(f"{name} ({array_size} elements): {end_time - start_time:.4f} seconds")


if __name__ == "__main__":
    demonstrate_data_structures()