# Python data structures for fluxus compilation

class Node:
    def __init__(self, value):
        self.value = value
        self.next = None

class LinkedList:
    def __init__(self):
        self.head = None
    
    def insert(self, value):
        new_node = Node(value)
        if self.head is None:
            self.head = new_node
            return
        
        current = self.head
        while current.next:
            current = current.next
        current.next = new_node
    
    def display(self):
        current = self.head
        while current:
            print(f"{current.value} -> ", end="")
            current = current.next
        print("nil")

class Stack:
    def __init__(self):
        self.items = []
    
    def push(self, item):
        self.items.append(item)
    
    def pop(self):
        if not self.is_empty():
            return self.items.pop()
        return None
    
    def is_empty(self):
        return len(self.items) == 0
    
    def peek(self):
        if not self.is_empty():
            return self.items[-1]
        return None

def main():
    print("Python Data Structures Test")
    
    # Test linked list
    print("Linked List Test:")
    ll = LinkedList()
    values = [10, 20, 30, 40, 50]
    
    for v in values:
        ll.insert(v)
    
    print("Linked List: ", end="")
    ll.display()
    
    # Test stack
    print("Stack Test:")
    stack = Stack()
    
    print("Pushing elements:")
    for v in values[:3]:
        stack.push(v)
        print(f"  Pushed {v}, Stack: {stack.items}")
    
    print("Popping elements:")
    while not stack.is_empty():
        popped = stack.pop()
        print(f"  Popped {popped}, Stack: {stack.items}")
    
    # Test basic list operations
    print("Basic List Operations:")
    arr = [1, 2, 3, 4, 5]
    print(f"Original list: {arr}")
    print(f"Length: {len(arr)}")
    print(f"First element: {arr[0]}")
    print(f"Last element: {arr[-1]}")
    
    # List slicing
    print(f"First 3 elements: {arr[:3]}")
    print(f"Last 2 elements: {arr[-2:]}")
    
    # List methods
    arr.append(6)
    print(f"After append: {arr}")
    arr.insert(0, 0)
    print(f"After insert: {arr}")
    arr.remove(3)
    print(f"After remove: {arr}")
    
    # Test dictionary operations
    print("Dictionary Test:")
    person = {"name": "Alice", "age": 25, "city": "New York"}
    print(f"Person: {person}")
    print(f"Name: {person['name']}")
    print(f"Age: {person['age']}")
    
    person["job"] = "Engineer"
    print(f"After adding job: {person}")
    
    # Test tuple operations
    print("Tuple Test:")
    coordinates = (10, 20)
    print(f"Coordinates: {coordinates}")
    print(f"X: {coordinates[0]}")
    print(f"Y: {coordinates[1]}")
    
    # Test set operations
    print("Set Test:")
    set1 = {1, 2, 3, 4, 5}
    set2 = {4, 5, 6, 7, 8}
    print(f"Set 1: {set1}")
    print(f"Set 2: {set2}")
    print(f"Union: {set1.union(set2)}")
    print(f"Intersection: {set1.intersection(set2)}")
    print(f"Difference: {set1.difference(set2)}")
    
    print("Data structures test completed successfully")

if __name__ == "__main__":
    main()
