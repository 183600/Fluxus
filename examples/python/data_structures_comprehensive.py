class Node:
    def __init__(self, data):
        self.data = data
        self.next = None

class LinkedList:
    def __init__(self):
        self.head = None
        
    def append(self, data):
        new_node = Node(data)
        if not self.head:
            self.head = new_node
            return
        current = self.head
        while current.next:
            current = current.next
        current.next = new_node
    
    def prepend(self, data):
        new_node = Node(data)
        new_node.next = self.head
        self.head = new_node
    
    def delete(self, data):
        if not self.head:
            return
        if self.head.data == data:
            self.head = self.head.next
            return
        current = self.head
        while current.next:
            if current.next.data == data:
                current.next = current.next.next
                return
            current = current.next
    
    def find(self, data):
        current = self.head
        while current:
            if current.data == data:
                return True
            current = current.next
        return False
    
    def display(self):
        elements = []
        current = self.head
        while current:
            elements.append(current.data)
            current = current.next
        return elements
    
    def reverse(self):
        prev = None
        current = self.head
        while current:
            next_temp = current.next
            current.next = prev
            prev = current
            current = next_temp
        self.head = prev
    
    def get_middle(self):
        if not self.head:
            return None
        slow = self.head
        fast = self.head
        while fast and fast.next:
            slow = slow.next
            fast = fast.next.next
        return slow.data
    
    def has_cycle(self):
        if not self.head:
            return False
        slow = self.head
        fast = self.head
        while fast and fast.next:
            slow = slow.next
            fast = fast.next.next
            if slow == fast:
                return True
        return False

class Stack:
    def __init__(self):
        self.items = []
    
    def push(self, item):
        self.items.append(item)
    
    def pop(self):
        if not self.is_empty():
            return self.items.pop()
        return None
    
    def peek(self):
        if not self.is_empty():
            return self.items[-1]
        return None
    
    def is_empty(self):
        return len(self.items) == 0
    
    def size(self):
        return len(self.items)
    
    def display(self):
        return self.items.copy()

class Queue:
    def __init__(self):
        self.items = []
    
    def enqueue(self, item):
        self.items.append(item)
    
    def dequeue(self):
        if not self.is_empty():
            return self.items.pop(0)
        return None
    
    def front(self):
        if not self.is_empty():
            return self.items[0]
        return None
    
    def is_empty(self):
        return len(self.items) == 0
    
    def size(self):
        return len(self.items)
    
    def display(self):
        return self.items.copy()

class BinaryTreeNode:
    def __init__(self, data):
        self.data = data
        self.left = None
        self.right = None

class BinarySearchTree:
    def __init__(self):
        self.root = None
    
    def insert(self, data):
        if not self.root:
            self.root = BinaryTreeNode(data)
        else:
            self._insert_recursive(self.root, data)
    
    def _insert_recursive(self, node, data):
        if data < node.data:
            if node.left is None:
                node.left = BinaryTreeNode(data)
            else:
                self._insert_recursive(node.left, data)
        elif data > node.data:
            if node.right is None:
                node.right = BinaryTreeNode(data)
            else:
                self._insert_recursive(node.right, data)
    
    def search(self, data):
        return self._search_recursive(self.root, data)
    
    def _search_recursive(self, node, data):
        if node is None or node.data == data:
            return node is not None
        if data < node.data:
            return self._search_recursive(node.left, data)
        return self._search_recursive(node.right, data)
    
    def inorder_traversal(self):
        result = []
        self._inorder_recursive(self.root, result)
        return result
    
    def _inorder_recursive(self, node, result):
        if node:
            self._inorder_recursive(node.left, result)
            result.append(node.data)
            self._inorder_recursive(node.right, result)
    
    def preorder_traversal(self):
        result = []
        self._preorder_recursive(self.root, result)
        return result
    
    def _preorder_recursive(self, node, result):
        if node:
            result.append(node.data)
            self._preorder_recursive(node.left, result)
            self._preorder_recursive(node.right, result)
    
    def postorder_traversal(self):
        result = []
        self._postorder_recursive(self.root, result)
        return result
    
    def _postorder_recursive(self, node, result):
        if node:
            self._postorder_recursive(node.left, result)
            self._postorder_recursive(node.right, result)
            result.append(node.data)
    
    def find_min(self):
        if not self.root:
            return None
        current = self.root
        while current.left:
            current = current.left
        return current.data
    
    def find_max(self):
        if not self.root:
            return None
        current = self.root
        while current.right:
            current = current.right
        return current.data
    
    def height(self):
        return self._height_recursive(self.root)
    
    def _height_recursive(self, node):
        if not node:
            return -1
        left_height = self._height_recursive(node.left)
        right_height = self._height_recursive(node.right)
        return max(left_height, right_height) + 1

class Graph:
    def __init__(self):
        self.vertices = {}
    
    def add_vertex(self, vertex):
        if vertex not in self.vertices:
            self.vertices[vertex] = []
    
    def add_edge(self, vertex1, vertex2):
        if vertex1 not in self.vertices:
            self.add_vertex(vertex1)
        if vertex2 not in self.vertices:
            self.add_vertex(vertex2)
        
        self.vertices[vertex1].append(vertex2)
        self.vertices[vertex2].append(vertex1)
    
    def remove_edge(self, vertex1, vertex2):
        if vertex1 in self.vertices and vertex2 in self.vertices[vertex1]:
            self.vertices[vertex1].remove(vertex2)
        if vertex2 in self.vertices and vertex1 in self.vertices[vertex2]:
            self.vertices[vertex2].remove(vertex1)
    
    def get_vertices(self):
        return list(self.vertices.keys())
    
    def get_edges(self):
        edges = []
        for vertex in self.vertices:
            for neighbor in self.vertices[vertex]:
                if (neighbor, vertex) not in edges:
                    edges.append((vertex, neighbor))
        return edges
    
    def bfs(self, start_vertex):
        visited = set()
        queue = [start_vertex]
        result = []
        
        while queue:
            vertex = queue.pop(0)
            if vertex not in visited:
                visited.add(vertex)
                result.append(vertex)
                for neighbor in self.vertices.get(vertex, []):
                    if neighbor not in visited:
                        queue.append(neighbor)
        
        return result
    
    def dfs(self, start_vertex):
        visited = set()
        result = []
        
        def dfs_recursive(vertex):
            visited.add(vertex)
            result.append(vertex)
            for neighbor in self.vertices.get(vertex, []):
                if neighbor not in visited:
                    dfs_recursive(neighbor)
        
        dfs_recursive(start_vertex)
        return result

def test_data_structures():
    print("Testing LinkedList:")
    ll = LinkedList()
    ll.append(1)
    ll.append(2)
    ll.append(3)
    ll.prepend(0)
    print(f"List: {ll.display()}")
    print(f"Find 2: {ll.find(2)}")
    print(f"Middle element: {ll.get_middle()}")
    ll.reverse()
    print(f"Reversed: {ll.display()}")
    
    print("\nTesting Stack:")
    stack = Stack()
    stack.push(1)
    stack.push(2)
    stack.push(3)
    print(f"Stack: {stack.display()}")
    print(f"Pop: {stack.pop()}")
    print(f"Peek: {stack.peek()}")
    
    print("\nTesting Queue:")
    queue = Queue()
    queue.enqueue(1)
    queue.enqueue(2)
    queue.enqueue(3)
    print(f"Queue: {queue.display()}")
    print(f"Dequeue: {queue.dequeue()}")
    print(f"Front: {queue.front()}")
    
    print("\nTesting Binary Search Tree:")
    bst = BinarySearchTree()
    elements = [50, 30, 70, 20, 40, 60, 80]
    for elem in elements:
        bst.insert(elem)
    print(f"Inorder: {bst.inorder_traversal()}")
    print(f"Preorder: {bst.preorder_traversal()}")
    print(f"Postorder: {bst.postorder_traversal()}")
    print(f"Search 40: {bst.search(40)}")
    print(f"Min: {bst.find_min()}")
    print(f"Max: {bst.find_max()}")
    print(f"Height: {bst.height()}")
    
    print("\nTesting Graph:")
    graph = Graph()
    graph.add_edge('A', 'B')
    graph.add_edge('A', 'C')
    graph.add_edge('B', 'D')
    graph.add_edge('C', 'D')
    graph.add_edge('D', 'E')
    print(f"Vertices: {graph.get_vertices()}")
    print(f"Edges: {graph.get_edges()}")
    print(f"BFS from A: {graph.bfs('A')}")
    print(f"DFS from A: {graph.dfs('A')}")

if __name__ == "__main__":
    test_data_structures()