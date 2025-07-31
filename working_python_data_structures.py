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

def main():
    print("Python Data Structures Test")
    
    # Test linked list
    ll = LinkedList()
    values = [10, 20, 30, 40, 50]
    
    for v in values:
        ll.insert(v)
    
    print("Linked List: ", end="")
    ll.display()
    
    # Test stack
    stack = []
    print("Stack Operations:")
    
    # Push
    for v in values[:3]:
        stack.append(v)
        print(f"Pushed {v}, Stack: {stack}")
    
    # Pop
    while stack:
        popped = stack.pop()
        print(f"Popped {popped}, Stack: {stack}")
    
    # Test dictionary
    student_ages = {
        "Alice": 20,
        "Bob": 22,
        "Charlie": 21
    }
    
    print("Dictionary Operations:")
    for name, age in student_ages.items():
        print(f"  {name}: {age}")
    
    student_ages["David"] = 23
    print(f"Added David: {student_ages['David']}")
    
    # Test list operations
    arr = [1, 2, 3, 4, 5]
    print(f"Original list: {arr}")
    print(f"Length: {len(arr)}")
    print(f"First element: {arr[0]}")
    print(f"Last element: {arr[-1]}")
    
    print("Python data structures test completed successfully!")

if __name__ == "__main__":
    main()
