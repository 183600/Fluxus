package main

import (
	"fmt"
	"net/http"
	"sync"
	"time"
)

// Graph represents a graph data structure
type Graph struct {
	vertices map[int][]int
	mutex    sync.RWMutex
}

func NewGraph() *Graph {
	return &Graph{
		vertices: make(map[int][]int),
	}
}

func (g *Graph) AddVertex(v int) {
	g.mutex.Lock()
	defer g.mutex.Unlock()
	if _, exists := g.vertices[v]; !exists {
		g.vertices[v] = []int{}
	}
}

func (g *Graph) AddEdge(from, to int) {
	g.mutex.Lock()
	defer g.mutex.Unlock()
	g.vertices[from] = append(g.vertices[from], to)
}

func (g *Graph) DFS(start int, visited map[int]bool, result *[]int) {
	g.mutex.RLock()
	defer g.mutex.RUnlock()
	
	visited[start] = true
	*result = append(*result, start)
	
	for _, neighbor := range g.vertices[start] {
		if !visited[neighbor] {
			g.DFS(neighbor, visited, result)
		}
	}
}

func (g *Graph) BFS(start int) []int {
	g.mutex.RLock()
	defer g.mutex.RUnlock()
	
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

// LRU Cache implementation
type LRUCache struct {
	capacity int
	cache    map[int]*Node
	head     *Node
	tail     *Node
	mutex    sync.RWMutex
}

type Node struct {
	key   int
	value int
	prev  *Node
	next  *Node
}

func NewLRUCache(capacity int) *LRUCache {
	head := &Node{}
	tail := &Node{}
	head.next = tail
	tail.prev = head
	
	return &LRUCache{
		capacity: capacity,
		cache:    make(map[int]*Node),
		head:     head,
		tail:     tail,
	}
}

func (lru *LRUCache) Get(key int) int {
	lru.mutex.Lock()
	defer lru.mutex.Unlock()
	
	if node, exists := lru.cache[key]; exists {
		lru.moveToHead(node)
		return node.value
	}
	return -1
}

func (lru *LRUCache) Put(key, value int) {
	lru.mutex.Lock()
	defer lru.mutex.Unlock()
	
	if node, exists := lru.cache[key]; exists {
		node.value = value
		lru.moveToHead(node)
	} else {
		newNode := &Node{key: key, value: value}
		
		if len(lru.cache) >= lru.capacity {
			// Remove least recently used
			tail := lru.removeTail()
			delete(lru.cache, tail.key)
		}
		
		lru.cache[key] = newNode
		lru.addToHead(newNode)
	}
}

func (lru *LRUCache) addToHead(node *Node) {
	node.prev = lru.head
	node.next = lru.head.next
	lru.head.next.prev = node
	lru.head.next = node
}

func (lru *LRUCache) removeNode(node *Node) {
	node.prev.next = node.next
	node.next.prev = node.prev
}

func (lru *LRUCache) moveToHead(node *Node) {
	lru.removeNode(node)
	lru.addToHead(node)
}

func (lru *LRUCache) removeTail() *Node {
	tail := lru.tail.prev
	lru.removeNode(tail)
	return tail
}

// Trie (Prefix Tree) implementation
type TrieNode struct {
	children map[rune]*TrieNode
	isEnd    bool
}

type Trie struct {
	root  *TrieNode
	mutex sync.RWMutex
}

func NewTrie() *Trie {
	return &Trie{
		root: &TrieNode{
			children: make(map[rune]*TrieNode),
			isEnd:    false,
		},
	}
}

func (t *Trie) Insert(word string) {
	t.mutex.Lock()
	defer t.mutex.Unlock()
	
	node := t.root
	for _, char := range word {
		if _, exists := node.children[char]; !exists {
			node.children[char] = &TrieNode{
				children: make(map[rune]*TrieNode),
				isEnd:    false,
			}
		}
		node = node.children[char]
	}
	node.isEnd = true
}

func (t *Trie) Search(word string) bool {
	t.mutex.RLock()
	defer t.mutex.RUnlock()
	
	node := t.root
	for _, char := range word {
		if _, exists := node.children[char]; !exists {
			return false
		}
		node = node.children[char]
	}
	return node.isEnd
}

func (t *Trie) StartsWith(prefix string) bool {
	t.mutex.RLock()
	defer t.mutex.RUnlock()
	
	node := t.root
	for _, char := range prefix {
		if _, exists := node.children[char]; !exists {
			return false
		}
		node = node.children[char]
	}
	return true
}

// Rate Limiter implementation
type RateLimiter struct {
	requests map[string][]time.Time
	limit    int
	window   time.Duration
	mutex    sync.RWMutex
}

func NewRateLimiter(limit int, window time.Duration) *RateLimiter {
	return &RateLimiter{
		requests: make(map[string][]time.Time),
		limit:    limit,
		window:   window,
	}
}

func (rl *RateLimiter) IsAllowed(identifier string) bool {
	rl.mutex.Lock()
	defer rl.mutex.Unlock()
	
	now := time.Now()
	cutoff := now.Add(-rl.window)
	
	// Clean old requests
	if requests, exists := rl.requests[identifier]; exists {
		validRequests := []time.Time{}
		for _, reqTime := range requests {
			if reqTime.After(cutoff) {
				validRequests = append(validRequests, reqTime)
			}
		}
		rl.requests[identifier] = validRequests
	}
	
	// Check if under limit
	if len(rl.requests[identifier]) < rl.limit {
		rl.requests[identifier] = append(rl.requests[identifier], now)
		return true
	}
	
	return false
}

// Simple HTTP Server handlers
func healthCheckHandler(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(http.StatusOK)
	fmt.Fprintf(w, `{"status": "healthy", "timestamp": "%s"}`, time.Now().Format(time.RFC3339))
}

func algorithmHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	
	// Demonstrate some algorithm
	numbers := []int{5, 2, 8, 1, 9, 3}
	
	// Quick sort
	quickSort := func(arr []int, low, high int) {
		if low < high {
			pi := func(arr []int, low, high int) int {
				pivot := arr[high]
				i := low - 1
				for j := low; j < high; j++ {
					if arr[j] < pivot {
						i++
						arr[i], arr[j] = arr[j], arr[i]
					}
				}
				arr[i+1], arr[high] = arr[high], arr[i+1]
				return i + 1
			}(arr, low, high)
			
			quickSort(arr, low, pi-1)
			quickSort(arr, pi+1, high)
		}
	}
	
	sorted := make([]int, len(numbers))
	copy(sorted, numbers)
	quickSort(sorted, 0, len(sorted)-1)
	
	fmt.Fprintf(w, `{"original": %v, "sorted": %v}`, numbers, sorted)
}

func fibonacciHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	
	// Calculate fibonacci sequence
	fib := func(n int) []int {
		if n <= 0 {
			return []int{}
		}
		if n == 1 {
			return []int{0}
		}
		
		sequence := []int{0, 1}
		for i := 2; i < n; i++ {
			next := sequence[i-1] + sequence[i-2]
			sequence = append(sequence, next)
		}
		return sequence
	}
	
	sequence := fib(15)
	fmt.Fprintf(w, `{"sequence": %v, "length": %d}`, sequence, len(sequence))
}

func startHTTPServer() {
	mux := http.NewServeMux()
	mux.HandleFunc("/health", healthCheckHandler)
	mux.HandleFunc("/algorithm", algorithmHandler)
	mux.HandleFunc("/fibonacci", fibonacciHandler)
	
	server := &http.Server{
		Addr:         ":8080",
		Handler:      mux,
		ReadTimeout:  10 * time.Second,
		WriteTimeout: 10 * time.Second,
	}
	
	fmt.Println("Starting HTTP server on :8080")
	go func() {
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			fmt.Printf("HTTP server error: %v\n", err)
		}
	}()
}

func main() {
	fmt.Println("=== Complex Data Structures and Algorithms Demo ===")

	// 1. Graph Operations
	fmt.Println("\n1. Graph Operations:")
	graph := NewGraph()
	vertices := []int{1, 2, 3, 4, 5}
	for _, v := range vertices {
		graph.AddVertex(v)
	}
	
	edges := [][2]int{{1, 2}, {1, 3}, {2, 4}, {3, 4}, {4, 5}}
	for _, edge := range edges {
		graph.AddEdge(edge[0], edge[1])
	}
	
	// DFS
	visited := make(map[int]bool)
	var dfsResult []int
	graph.DFS(1, visited, &dfsResult)
	fmt.Printf("DFS from vertex 1: %v\n", dfsResult)
	
	// BFS
	bfsResult := graph.BFS(1)
	fmt.Printf("BFS from vertex 1: %v\n", bfsResult)

	// 2. LRU Cache
	fmt.Println("\n2. LRU Cache Operations:")
	cache := NewLRUCache(3)
	
	cache.Put(1, 100)
	cache.Put(2, 200)
	cache.Put(3, 300)
	fmt.Printf("Get key 1: %d\n", cache.Get(1))
	fmt.Printf("Get key 2: %d\n", cache.Get(2))
	
	cache.Put(4, 400) // This should evict key 3
	fmt.Printf("Get key 3 (should be -1): %d\n", cache.Get(3))
	fmt.Printf("Get key 4: %d\n", cache.Get(4))

	// 3. Trie Operations
	fmt.Println("\n3. Trie (Prefix Tree) Operations:")
	trie := NewTrie()
	words := []string{"apple", "app", "application", "apply", "banana", "band"}
	
	for _, word := range words {
		trie.Insert(word)
	}
	
	testWords := []string{"app", "apple", "appl", "application", "banana", "orange"}
	for _, word := range testWords {
		fmt.Printf("Search '%s': %t\n", word, trie.Search(word))
	}
	
	prefixes := []string{"app", "ban", "or"}
	for _, prefix := range prefixes {
		fmt.Printf("Starts with '%s': %t\n", prefix, trie.StartsWith(prefix))
	}

	// 4. Rate Limiter
	fmt.Println("\n4. Rate Limiter (5 requests per 10 seconds):")
	rateLimiter := NewRateLimiter(5, 10*time.Second)
	
	// Simulate requests
	for i := 1; i <= 8; i++ {
		allowed := rateLimiter.IsAllowed("user123")
		fmt.Printf("Request %d: %t\n", i, allowed)
		time.Sleep(time.Millisecond * 100)
	}

	// 5. Complex Algorithm - Finding Strongly Connected Components
	fmt.Println("\n5. Advanced Graph Algorithm - Topological Sort:")
	
	// Create a DAG for topological sort
	dag := NewGraph()
	for i := 0; i <= 5; i++ {
		dag.AddVertex(i)
	}
	
	// Add edges to create dependencies
	dagEdges := [][2]int{{5, 2}, {5, 0}, {4, 0}, {4, 1}, {2, 3}, {3, 1}}
	for _, edge := range dagEdges {
		dag.AddEdge(edge[0], edge[1])
	}
	
	// Simple topological sort using DFS
	topologicalSort := func(g *Graph) []int {
		visited := make(map[int]bool)
		stack := []int{}
		
		var dfsUtil func(int)
		dfsUtil = func(v int) {
			visited[v] = true
			for _, neighbor := range g.vertices[v] {
				if !visited[neighbor] {
					dfsUtil(neighbor)
				}
			}
			stack = append([]int{v}, stack...) // Add to front
		}
		
		for vertex := range g.vertices {
			if !visited[vertex] {
				dfsUtil(vertex)
			}
		}
		
		return stack
	}
	
	topoOrder := topologicalSort(dag)
	fmt.Printf("Topological order: %v\n", topoOrder)

	// 6. Start HTTP Server (non-blocking)
	fmt.Println("\n6. Starting HTTP Server:")
	startHTTPServer()
	
	// Give server time to start
	time.Sleep(time.Second)
	fmt.Println("HTTP server started on port 8080")
	fmt.Println("Endpoints available:")
	fmt.Println("  - GET /health")
	fmt.Println("  - GET /algorithm")
	fmt.Println("  - GET /fibonacci")

	// 7. Concurrent Processing Demo
	fmt.Println("\n7. Concurrent Processing:")
	
	// Fan-out pattern
	input := make(chan int, 10)
	output1 := make(chan int, 10)
	output2 := make(chan int, 10)
	
	// Worker 1: Square numbers
	go func() {
		for num := range input {
			output1 <- num * num
		}
		close(output1)
	}()
	
	// Worker 2: Double numbers  
	go func() {
		for num := range input {
			output2 <- num * 2
		}
		close(output2)
	}()
	
	// Send data
	for i := 1; i <= 5; i++ {
		input <- i
	}
	close(input)
	
	// Collect results
	fmt.Print("Squared results: ")
	for result := range output1 {
		fmt.Printf("%d ", result)
	}
	fmt.Println()
	
	fmt.Print("Doubled results: ")
	for result := range output2 {
		fmt.Printf("%d ", result)
	}
	fmt.Println()

	fmt.Println("\n=== Complex Demo Complete ===")
	fmt.Println("HTTP server is still running. You can test endpoints manually.")
}