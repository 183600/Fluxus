package main

import (
	"fmt"
	"sync"
	"time"
	"math/rand"
)

// Stack implementation
type Stack struct {
	items []int
	mutex sync.Mutex
}

func (s *Stack) Push(item int) {
	s.mutex.Lock()
	defer s.mutex.Unlock()
	s.items = append(s.items, item)
}

func (s *Stack) Pop() (int, bool) {
	s.mutex.Lock()
	defer s.mutex.Unlock()
	if len(s.items) == 0 {
		return 0, false
	}
	index := len(s.items) - 1
	item := s.items[index]
	s.items = s.items[:index]
	return item, true
}

func (s *Stack) IsEmpty() bool {
	s.mutex.Lock()
	defer s.mutex.Unlock()
	return len(s.items) == 0
}

// Queue implementation
type Queue struct {
	items []int
	mutex sync.Mutex
}

func (q *Queue) Enqueue(item int) {
	q.mutex.Lock()
	defer q.mutex.Unlock()
	q.items = append(q.items, item)
}

func (q *Queue) Dequeue() (int, bool) {
	q.mutex.Lock()
	defer q.mutex.Unlock()
	if len(q.items) == 0 {
		return 0, false
	}
	item := q.items[0]
	q.items = q.items[1:]
	return item, true
}

// Binary Tree Node
type TreeNode struct {
	Value int
	Left  *TreeNode
	Right *TreeNode
}

func (root *TreeNode) Insert(value int) {
	if value < root.Value {
		if root.Left == nil {
			root.Left = &TreeNode{Value: value}
		} else {
			root.Left.Insert(value)
		}
	} else {
		if root.Right == nil {
			root.Right = &TreeNode{Value: value}
		} else {
			root.Right.Insert(value)
		}
	}
}

func (root *TreeNode) Search(value int) bool {
	if root == nil {
		return false
	}
	if root.Value == value {
		return true
	} else if value < root.Value {
		return root.Left.Search(value)
	} else {
		return root.Right.Search(value)
	}
}

func (root *TreeNode) InorderTraversal() []int {
	if root == nil {
		return []int{}
	}
	result := []int{}
	result = append(result, root.Left.InorderTraversal()...)
	result = append(result, root.Value)
	result = append(result, root.Right.InorderTraversal()...)
	return result
}

// Graph implementation using adjacency list
type Graph struct {
	vertices map[int][]int
}

func NewGraph() *Graph {
	return &Graph{vertices: make(map[int][]int)}
}

func (g *Graph) AddEdge(from, to int) {
	g.vertices[from] = append(g.vertices[from], to)
	g.vertices[to] = append(g.vertices[to], from) // For undirected graph
}

func (g *Graph) BFS(start int) []int {
	if _, exists := g.vertices[start]; !exists {
		return []int{}
	}
	
	visited := make(map[int]bool)
	queue := []int{start}
	result := []int{}
	
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

func (g *Graph) DFS(start int) []int {
	if _, exists := g.vertices[start]; !exists {
		return []int{}
	}
	
	visited := make(map[int]bool)
	result := []int{}
	
	var dfsHelper func(int)
	dfsHelper = func(vertex int) {
		visited[vertex] = true
		result = append(result, vertex)
		
		for _, neighbor := range g.vertices[vertex] {
			if !visited[neighbor] {
				dfsHelper(neighbor)
			}
		}
	}
	
	dfsHelper(start)
	return result
}

// Worker pool pattern
func worker(id int, jobs <-chan int, results chan<- int) {
	for job := range jobs {
		fmt.Printf("Worker %d processing job %d\n", id, job)
		time.Sleep(time.Duration(rand.Intn(1000)) * time.Millisecond)
		results <- job * 2
	}
}

func main() {
	fmt.Println("Advanced Data Structures and Concurrency Demo")
	fmt.Println("=============================================")
	
	// Stack demo
	fmt.Println("\n--- Stack Demo ---")
	stack := &Stack{}
	stack.Push(1)
	stack.Push(2)
	stack.Push(3)
	
	for !stack.IsEmpty() {
		if item, ok := stack.Pop(); ok {
			fmt.Printf("Popped: %d\n", item)
		}
	}
	
	// Queue demo
	fmt.Println("\n--- Queue Demo ---")
	queue := &Queue{}
	queue.Enqueue(10)
	queue.Enqueue(20)
	queue.Enqueue(30)
	
	for len(queue.items) > 0 {
		if item, ok := queue.Dequeue(); ok {
			fmt.Printf("Dequeued: %d\n", item)
		}
	}
	
	// Binary Tree demo
	fmt.Println("\n--- Binary Tree Demo ---")
	root := &TreeNode{Value: 50}
	values := []int{30, 70, 20, 40, 60, 80}
	for _, v := range values {
		root.Insert(v)
	}
	
	fmt.Printf("Inorder traversal: %v\n", root.InorderTraversal())
	fmt.Printf("Search 40: %t\n", root.Search(40))
	fmt.Printf("Search 25: %t\n", root.Search(25))
	
	// Graph demo
	fmt.Println("\n--- Graph Demo ---")
	graph := NewGraph()
	graph.AddEdge(1, 2)
	graph.AddEdge(1, 3)
	graph.AddEdge(2, 4)
	graph.AddEdge(3, 4)
	graph.AddEdge(4, 5)
	
	fmt.Printf("BFS from 1: %v\n", graph.BFS(1))
	fmt.Printf("DFS from 1: %v\n", graph.DFS(1))
	
	// Concurrency demo
	fmt.Println("\n--- Worker Pool Demo ---")
	const numJobs = 5
	const numWorkers = 3
	
	jobs := make(chan int, numJobs)
	results := make(chan int, numJobs)
	
	// Start workers
	for w := 1; w <= numWorkers; w++ {
		go worker(w, jobs, results)
	}
	
	// Send jobs
	for j := 1; j <= numJobs; j++ {
		jobs <- j
	}
	close(jobs)
	
	// Collect results
	for a := 1; a <= numJobs; a++ {
		result := <-results
		fmt.Printf("Result: %d\n", result)
	}
}