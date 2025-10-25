package main

import "fmt"

type Node struct {
	data int
	next *Node
}

type LinkedList struct {
	head *Node
}

func NewLinkedList() *LinkedList {
	return &LinkedList{}
}

func (ll *LinkedList) Append(data int) {
	newNode := &Node{data: data}
	if ll.head == nil {
		ll.head = newNode
		return
	}
	
	current := ll.head
	for current.next != nil {
		current = current.next
	}
	current.next = newNode
}

func (ll *LinkedList) Prepend(data int) {
	newNode := &Node{data: data, next: ll.head}
	ll.head = newNode
}

func (ll *LinkedList) Delete(data int) {
	if ll.head == nil {
		return
	}
	
	if ll.head.data == data {
		ll.head = ll.head.next
		return
	}
	
	current := ll.head
	for current.next != nil {
		if current.next.data == data {
			current.next = current.next.next
			return
		}
		current = current.next
	}
}

func (ll *LinkedList) Find(data int) bool {
	current := ll.head
	for current != nil {
		if current.data == data {
			return true
		}
		current = current.next
	}
	return false
}

func (ll *LinkedList) Display() []int {
	var elements []int
	current := ll.head
	for current != nil {
		elements = append(elements, current.data)
		current = current.next
	}
	return elements
}

func (ll *LinkedList) Reverse() {
	var prev *Node
	current := ll.head
	
	for current != nil {
		next := current.next
		current.next = prev
		prev = current
		current = next
	}
	
	ll.head = prev
}

func (ll *LinkedList) GetMiddle() int {
	if ll.head == nil {
		return -1
	}
	
	slow := ll.head
	fast := ll.head
	
	for fast != nil && fast.next != nil {
		slow = slow.next
		fast = fast.next.next
	}
	
	return slow.data
}

type Stack struct {
	items []int
}

func NewStack() *Stack {
	return &Stack{}
}

func (s *Stack) Push(item int) {
	s.items = append(s.items, item)
}

func (s *Stack) Pop() (int, bool) {
	if len(s.items) == 0 {
		return 0, false
	}
	
	index := len(s.items) - 1
	item := s.items[index]
	s.items = s.items[:index]
	return item, true
}

func (s *Stack) Peek() (int, bool) {
	if len(s.items) == 0 {
		return 0, false
	}
	return s.items[len(s.items)-1], true
}

func (s *Stack) IsEmpty() bool {
	return len(s.items) == 0
}

func (s *Stack) Size() int {
	return len(s.items)
}

func (s *Stack) Display() []int {
	result := make([]int, len(s.items))
	copy(result, s.items)
	return result
}

type Queue struct {
	items []int
}

func NewQueue() *Queue {
	return &Queue{}
}

func (q *Queue) Enqueue(item int) {
	q.items = append(q.items, item)
}

func (q *Queue) Dequeue() (int, bool) {
	if len(q.items) == 0 {
		return 0, false
	}
	
	item := q.items[0]
	q.items = q.items[1:]
	return item, true
}

func (q *Queue) Front() (int, bool) {
	if len(q.items) == 0 {
		return 0, false
	}
	return q.items[0], true
}

func (q *Queue) IsEmpty() bool {
	return len(q.items) == 0
}

func (q *Queue) Size() int {
	return len(q.items)
}

func (q *Queue) Display() []int {
	result := make([]int, len(q.items))
	copy(result, q.items)
	return result
}

type BinaryTreeNode struct {
	data  int
	left  *BinaryTreeNode
	right *BinaryTreeNode
}

type BinarySearchTree struct {
	root *BinaryTreeNode
}

func NewBinarySearchTree() *BinarySearchTree {
	return &BinarySearchTree{}
}

func (bst *BinarySearchTree) Insert(data int) {
	if bst.root == nil {
		bst.root = &BinaryTreeNode{data: data}
	} else {
		bst.insertRecursive(bst.root, data)
	}
}

func (bst *BinarySearchTree) insertRecursive(node *BinaryTreeNode, data int) {
	if data < node.data {
		if node.left == nil {
			node.left = &BinaryTreeNode{data: data}
		} else {
			bst.insertRecursive(node.left, data)
		}
	} else if data > node.data {
		if node.right == nil {
			node.right = &BinaryTreeNode{data: data}
		} else {
			bst.insertRecursive(node.right, data)
		}
	}
}

func (bst *BinarySearchTree) Search(data int) bool {
	return bst.searchRecursive(bst.root, data)
}

func (bst *BinarySearchTree) searchRecursive(node *BinaryTreeNode, data int) bool {
	if node == nil {
		return false
	}
	
	if node.data == data {
		return true
	}
	
	if data < node.data {
		return bst.searchRecursive(node.left, data)
	}
	
	return bst.searchRecursive(node.right, data)
}

func (bst *BinarySearchTree) InOrderTraversal() []int {
	var result []int
	bst.inOrderRecursive(bst.root, &result)
	return result
}

func (bst *BinarySearchTree) inOrderRecursive(node *BinaryTreeNode, result *[]int) {
	if node != nil {
		bst.inOrderRecursive(node.left, result)
		*result = append(*result, node.data)
		bst.inOrderRecursive(node.right, result)
	}
}

func (bst *BinarySearchTree) PreOrderTraversal() []int {
	var result []int
	bst.preOrderRecursive(bst.root, &result)
	return result
}

func (bst *BinarySearchTree) preOrderRecursive(node *BinaryTreeNode, result *[]int) {
	if node != nil {
		*result = append(*result, node.data)
		bst.preOrderRecursive(node.left, result)
		bst.preOrderRecursive(node.right, result)
	}
}

func (bst *BinarySearchTree) PostOrderTraversal() []int {
	var result []int
	bst.postOrderRecursive(bst.root, &result)
	return result
}

func (bst *BinarySearchTree) postOrderRecursive(node *BinaryTreeNode, result *[]int) {
	if node != nil {
		bst.postOrderRecursive(node.left, result)
		bst.postOrderRecursive(node.right, result)
		*result = append(*result, node.data)
	}
}

func (bst *BinarySearchTree) FindMin() (int, bool) {
	if bst.root == nil {
		return 0, false
	}
	
	current := bst.root
	for current.left != nil {
		current = current.left
	}
	
	return current.data, true
}

func (bst *BinarySearchTree) FindMax() (int, bool) {
	if bst.root == nil {
		return 0, false
	}
	
	current := bst.root
	for current.right != nil {
		current = current.right
	}
	
	return current.data, true
}

func (bst *BinarySearchTree) Height() int {
	return bst.heightRecursive(bst.root)
}

func (bst *BinarySearchTree) heightRecursive(node *BinaryTreeNode) int {
	if node == nil {
		return -1
	}
	
	leftHeight := bst.heightRecursive(node.left)
	rightHeight := bst.heightRecursive(node.right)
	
	if leftHeight > rightHeight {
		return leftHeight + 1
	}
	return rightHeight + 1
}

type Graph struct {
	vertices map[string][]string
}

func NewGraph() *Graph {
	return &Graph{vertices: make(map[string][]string)}
}

func (g *Graph) AddVertex(vertex string) {
	if _, exists := g.vertices[vertex]; !exists {
		g.vertices[vertex] = []string{}
	}
}

func (g *Graph) AddEdge(vertex1, vertex2 string) {
	g.AddVertex(vertex1)
	g.AddVertex(vertex2)
	
	g.vertices[vertex1] = append(g.vertices[vertex1], vertex2)
	g.vertices[vertex2] = append(g.vertices[vertex2], vertex1)
}

func (g *Graph) RemoveEdge(vertex1, vertex2 string) {
	if neighbors, exists := g.vertices[vertex1]; exists {
		g.vertices[vertex1] = remove(neighbors, vertex2)
	}
	
	if neighbors, exists := g.vertices[vertex2]; exists {
		g.vertices[vertex2] = remove(neighbors, vertex1)
	}
}

func remove(slice []string, item string) []string {
	for i, v := range slice {
		if v == item {
			return append(slice[:i], slice[i+1:]...)
		}
	}
	return slice
}

func (g *Graph) GetVertices() []string {
	var vertices []string
	for vertex := range g.vertices {
		vertices = append(vertices, vertex)
	}
	return vertices
}

func (g *Graph) GetEdges() [][]string {
	var edges [][]string
	visited := make(map[string]bool)
	
	for vertex := range g.vertices {
		for _, neighbor := range g.vertices[vertex] {
			edgeKey := vertex + "-" + neighbor
			reverseKey := neighbor + "-" + vertex
			
			if !visited[edgeKey] && !visited[reverseKey] {
				edges = append(edges, []string{vertex, neighbor})
				visited[edgeKey] = true
				visited[reverseKey] = true
			}
		}
	}
	
	return edges
}

func (g *Graph) BFS(startVertex string) []string {
	visited := make(map[string]bool)
	queue := []string{startVertex}
	var result []string
	
	for len(queue) > 0 {
		vertex := queue[0]
		queue = queue[1:]
		
		if !visited[vertex] {
			visited[vertex] = true
			result = append(result, vertex)
			
			for _, neighbor := range g.vertices[vertex] {
				if !visited[neighbor] {
					queue = append(queue, neighbor)
				}
			}
		}
	}
	
	return result
}

func (g *Graph) DFS(startVertex string) []string {
	visited := make(map[string]bool)
	var result []string
	
	g.dfsRecursive(startVertex, visited, &result)
	return result
}

func (g *Graph) dfsRecursive(vertex string, visited map[string]bool, result *[]string) {
	visited[vertex] = true
	*result = append(*result, vertex)
	
	for _, neighbor := range g.vertices[vertex] {
		if !visited[neighbor] {
			g.dfsRecursive(neighbor, visited, result)
		}
	}
}

func testDataStructures() {
	fmt.Println("Testing LinkedList:")
	ll := NewLinkedList()
	ll.Append(1)
	ll.Append(2)
	ll.Append(3)
	ll.Prepend(0)
	fmt.Printf("List: %v\n", ll.Display())
	fmt.Printf("Find 2: %t\n", ll.Find(2))
	fmt.Printf("Middle element: %d\n", ll.GetMiddle())
	ll.Reverse()
	fmt.Printf("Reversed: %v\n", ll.Display())
	
	fmt.Println("\nTesting Stack:")
	stack := NewStack()
	stack.Push(1)
	stack.Push(2)
	stack.Push(3)
	fmt.Printf("Stack: %v\n", stack.Display())
	if val, ok := stack.Pop(); ok {
		fmt.Printf("Pop: %d\n", val)
	}
	if val, ok := stack.Peek(); ok {
		fmt.Printf("Peek: %d\n", val)
	}
	
	fmt.Println("\nTesting Queue:")
	queue := NewQueue()
	queue.Enqueue(1)
	queue.Enqueue(2)
	queue.Enqueue(3)
	fmt.Printf("Queue: %v\n", queue.Display())
	if val, ok := queue.Dequeue(); ok {
		fmt.Printf("Dequeue: %d\n", val)
	}
	if val, ok := queue.Front(); ok {
		fmt.Printf("Front: %d\n", val)
	}
	
	fmt.Println("\nTesting Binary Search Tree:")
	bst := NewBinarySearchTree()
	elements := []int{50, 30, 70, 20, 40, 60, 80}
	for _, elem := range elements {
		bst.Insert(elem)
	}
	fmt.Printf("Inorder: %v\n", bst.InOrderTraversal())
	fmt.Printf("Preorder: %v\n", bst.PreOrderTraversal())
	fmt.Printf("Postorder: %v\n", bst.PostOrderTraversal())
	fmt.Printf("Search 40: %t\n", bst.Search(40))
	if min, ok := bst.FindMin(); ok {
		fmt.Printf("Min: %d\n", min)
	}
	if max, ok := bst.FindMax(); ok {
		fmt.Printf("Max: %d\n", max)
	}
	fmt.Printf("Height: %d\n", bst.Height())
	
	fmt.Println("\nTesting Graph:")
	graph := NewGraph()
	graph.AddEdge("A", "B")
	graph.AddEdge("A", "C")
	graph.AddEdge("B", "D")
	graph.AddEdge("C", "D")
	graph.AddEdge("D", "E")
	fmt.Printf("Vertices: %v\n", graph.GetVertices())
	fmt.Printf("Edges: %v\n", graph.GetEdges())
	fmt.Printf("BFS from A: %v\n", graph.BFS("A"))
	fmt.Printf("DFS from A: %v\n", graph.DFS("A"))
}

func main() {
	fmt.Println("=== Go Data Structures Demo ===")
	testDataStructures()
}