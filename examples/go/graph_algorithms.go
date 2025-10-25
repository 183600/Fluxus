package main

import (
	"fmt"
)

type Graph struct {
	vertices int
	edges    [][]int
}

func NewGraph(vertices int) *Graph {
	edges := make([][]int, vertices)
	for i := range edges {
		edges[i] = make([]int, 0)
	}
	return &Graph{
		vertices: vertices,
		edges:    edges,
	}
}

func (g *Graph) AddEdge(from, to int) {
	if from >= 0 && from < g.vertices && to >= 0 && to < g.vertices {
		g.edges[from] = append(g.edges[from], to)
	}
}

func (g *Graph) DFS(start int, visited []bool) {
	visited[start] = true
	fmt.Printf("%d ", start)
	
	for _, neighbor := range g.edges[start] {
		if !visited[neighbor] {
			g.DFS(neighbor, visited)
		}
	}
}

func (g *Graph) BFS(start int) {
	visited := make([]bool, g.vertices)
	queue := []int{start}
	visited[start] = true
	
	fmt.Printf("BFS traversal starting from vertex %d: ", start)
	
	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]
		fmt.Printf("%d ", current)
		
		for _, neighbor := range g.edges[current] {
			if !visited[neighbor] {
				visited[neighbor] = true
				queue = append(queue, neighbor)
			}
		}
	}
	fmt.Println()
}

func (g *Graph) PrintGraph() {
	fmt.Println("\nGraph adjacency list:")
	for i := 0; i < g.vertices; i++ {
		fmt.Printf("Vertex %d: ", i)
		for _, neighbor := range g.edges[i] {
			fmt.Printf("%d ", neighbor)
		}
		fmt.Println()
	}
}

type Stack struct {
	items []int
}

func (s *Stack) Push(item int) {
	s.items = append(s.items, item)
}

func (s *Stack) Pop() int {
	if len(s.items) == 0 {
		return -1
	}
	item := s.items[len(s.items)-1]
	s.items = s.items[:len(s.items)-1]
	return item
}

func (s *Stack) IsEmpty() bool {
	return len(s.items) == 0
}

func (s *Stack) Size() int {
	return len(s.items)
}

type Queue struct {
	items []int
}

func (q *Queue) Enqueue(item int) {
	q.items = append(q.items, item)
}

func (q *Queue) Dequeue() int {
	if len(q.items) == 0 {
		return -1
	}
	item := q.items[0]
	q.items = q.items[1:]
	return item
}

func (q *Queue) IsEmpty() bool {
	return len(q.items) == 0
}

func (q *Queue) Size() int {
	return len(q.items)
}

func demonstrateStackAndQueue() {
	fmt.Println("\n=== Stack Operations ===")
	stack := &Stack{}
	
	for i := 1; i <= 5; i++ {
		stack.Push(i)
		fmt.Printf("Pushed %d, Stack size: %d\n", i, stack.Size())
	}
	
	for !stack.IsEmpty() {
		item := stack.Pop()
		fmt.Printf("Popped %d, Stack size: %d\n", item, stack.Size())
	}
	
	fmt.Println("\n=== Queue Operations ===")
	queue := &Queue{}
	
	for i := 1; i <= 5; i++ {
		queue.Enqueue(i)
		fmt.Printf("Enqueued %d, Queue size: %d\n", i, queue.Size())
	}
	
	for !queue.IsEmpty() {
		item := queue.Dequeue()
		fmt.Printf("Dequeued %d, Queue size: %d\n", item, queue.Size())
	}
}

func main() {
	fmt.Println("Graph and Data Structures Demo")
	
	// Create a graph with 6 vertices
	graph := NewGraph(6)
	
	// Add edges
	graph.AddEdge(0, 1)
	graph.AddEdge(0, 2)
	graph.AddEdge(1, 3)
	graph.AddEdge(1, 4)
	graph.AddEdge(2, 4)
	graph.AddEdge(3, 4)
	graph.AddEdge(3, 5)
	graph.AddEdge(4, 5)
	
	graph.PrintGraph()
	
	// DFS traversal
	fmt.Printf("\nDFS traversal starting from vertex 0: ")
	visited := make([]bool, 6)
	graph.DFS(0, visited)
	fmt.Println()
	
	// BFS traversal
	graph.BFS(0)
	
	// Demonstrate stack and queue
	demonstrateStackAndQueue()
}