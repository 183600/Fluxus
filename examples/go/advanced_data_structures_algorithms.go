package main

import (
    "encoding/json"
    "fmt"
    "hash/fnv"
    "io"
    "math"
    "net/http"
    "strconv"
    "strings"
    "sync"
    "time"
    "unsafe"
)

// Generic data structures

// Generic Stack
type Stack[T any] struct {
    items []T
    mutex sync.RWMutex
}

func NewStack[T any]() *Stack[T] {
    return &Stack[T]{items: make([]T, 0)}
}

func (s *Stack[T]) Push(item T) {
    s.mutex.Lock()
    defer s.mutex.Unlock()
    s.items = append(s.items, item)
}

func (s *Stack[T]) Pop() (T, bool) {
    s.mutex.Lock()
    defer s.mutex.Unlock()
    var zero T
    if len(s.items) == 0 {
        return zero, false
    }
    item := s.items[len(s.items)-1]
    s.items = s.items[:len(s.items)-1]
    return item, true
}

func (s *Stack[T]) Peek() (T, bool) {
    s.mutex.RLock()
    defer s.mutex.RUnlock()
    var zero T
    if len(s.items) == 0 {
        return zero, false
    }
    return s.items[len(s.items)-1], true
}

func (s *Stack[T]) Size() int {
    s.mutex.RLock()
    defer s.mutex.RUnlock()
    return len(s.items)
}

// Generic Queue
type Queue[T any] struct {
    items []T
    mutex sync.RWMutex
}

func NewQueue[T any]() *Queue[T] {
    return &Queue[T]{items: make([]T, 0)}
}

func (q *Queue[T]) Enqueue(item T) {
    q.mutex.Lock()
    defer q.mutex.Unlock()
    q.items = append(q.items, item)
}

func (q *Queue[T]) Dequeue() (T, bool) {
    q.mutex.Lock()
    defer q.mutex.Unlock()
    var zero T
    if len(q.items) == 0 {
        return zero, false
    }
    item := q.items[0]
    q.items = q.items[1:]
    return item, true
}

func (q *Queue[T]) Size() int {
    q.mutex.RLock()
    defer q.mutex.RUnlock()
    return len(q.items)
}

// Binary Search Tree
type TreeNode[T comparable] struct {
    Value T
    Left  *TreeNode[T]
    Right *TreeNode[T]
}

type BST[T comparable] struct {
    root *TreeNode[T]
    size int
    compare func(a, b T) int
}

func NewBST[T comparable](compare func(a, b T) int) *BST[T] {
    return &BST[T]{compare: compare}
}

func (bst *BST[T]) Insert(value T) {
    bst.root = bst.insertNode(bst.root, value)
    bst.size++
}

func (bst *BST[T]) insertNode(node *TreeNode[T], value T) *TreeNode[T] {
    if node == nil {
        return &TreeNode[T]{Value: value}
    }
    
    cmp := bst.compare(value, node.Value)
    if cmp < 0 {
        node.Left = bst.insertNode(node.Left, value)
    } else if cmp > 0 {
        node.Right = bst.insertNode(node.Right, value)
    }
    return node
}

func (bst *BST[T]) InOrderTraversal() []T {
    var result []T
    bst.inOrder(bst.root, &result)
    return result
}

func (bst *BST[T]) inOrder(node *TreeNode[T], result *[]T) {
    if node != nil {
        bst.inOrder(node.Left, result)
        *result = append(*result, node.Value)
        bst.inOrder(node.Right, result)
    }
}

// Hash Map with chaining
type KeyValue[K comparable, V any] struct {
    Key   K
    Value V
    Next  *KeyValue[K, V]
}

type HashMap[K comparable, V any] struct {
    buckets []*KeyValue[K, V]
    size    int
    capacity int
}

func NewHashMap[K comparable, V any](capacity int) *HashMap[K, V] {
    return &HashMap[K, V]{
        buckets:  make([]*KeyValue[K, V], capacity),
        capacity: capacity,
    }
}

func (hm *HashMap[K, V]) hash(key K) int {
    h := fnv.New32a()
    keyBytes := (*[unsafe.Sizeof(key)]byte)(unsafe.Pointer(&key))[:]
    h.Write(keyBytes)
    return int(h.Sum32()) % hm.capacity
}

func (hm *HashMap[K, V]) Put(key K, value V) {
    index := hm.hash(key)
    if hm.buckets[index] == nil {
        hm.buckets[index] = &KeyValue[K, V]{Key: key, Value: value}
        hm.size++
        return
    }
    
    current := hm.buckets[index]
    for current != nil {
        if current.Key == key {
            current.Value = value
            return
        }
        if current.Next == nil {
            break
        }
        current = current.Next
    }
    
    current.Next = &KeyValue[K, V]{Key: key, Value: value}
    hm.size++
}

func (hm *HashMap[K, V]) Get(key K) (V, bool) {
    var zero V
    index := hm.hash(key)
    current := hm.buckets[index]
    
    for current != nil {
        if current.Key == key {
            return current.Value, true
        }
        current = current.Next
    }
    return zero, false
}

// Graph implementation
type Graph struct {
    vertices map[string][]string
    mutex    sync.RWMutex
}

func NewGraph() *Graph {
    return &Graph{vertices: make(map[string][]string)}
}

func (g *Graph) AddVertex(vertex string) {
    g.mutex.Lock()
    defer g.mutex.Unlock()
    if _, exists := g.vertices[vertex]; !exists {
        g.vertices[vertex] = make([]string, 0)
    }
}

func (g *Graph) AddEdge(from, to string) {
    g.mutex.Lock()
    defer g.mutex.Unlock()
    g.vertices[from] = append(g.vertices[from], to)
}

func (g *Graph) BFS(start string) []string {
    g.mutex.RLock()
    defer g.mutex.RUnlock()
    
    visited := make(map[string]bool)
    queue := []string{start}
    result := make([]string, 0)
    
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

func (g *Graph) DFS(start string) []string {
    g.mutex.RLock()
    defer g.mutex.RUnlock()
    
    visited := make(map[string]bool)
    result := make([]string, 0)
    g.dfsRecursive(start, visited, &result)
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

// Advanced algorithms

// Dijkstra's shortest path
func (g *Graph) Dijkstra(start, end string) ([]string, int) {
    g.mutex.RLock()
    defer g.mutex.RUnlock()
    
    distances := make(map[string]int)
    previous := make(map[string]string)
    unvisited := make(map[string]bool)
    
    // Initialize distances
    for vertex := range g.vertices {
        distances[vertex] = math.MaxInt32
        unvisited[vertex] = true
    }
    distances[start] = 0
    
    for len(unvisited) > 0 {
        // Find unvisited vertex with minimum distance
        current := ""
        minDist := math.MaxInt32
        for vertex := range unvisited {
            if distances[vertex] < minDist {
                minDist = distances[vertex]
                current = vertex
            }
        }
        
        if current == "" || distances[current] == math.MaxInt32 {
            break
        }
        
        delete(unvisited, current)
        
        if current == end {
            break
        }
        
        // Update distances to neighbors
        for _, neighbor := range g.vertices[current] {
            if unvisited[neighbor] {
                alt := distances[current] + 1 // Assuming edge weight of 1
                if alt < distances[neighbor] {
                    distances[neighbor] = alt
                    previous[neighbor] = current
                }
            }
        }
    }
    
    // Reconstruct path
    path := make([]string, 0)
    current := end
    for current != "" {
        path = append([]string{current}, path...)
        current = previous[current]
    }
    
    return path, distances[end]
}

// Sorting algorithms
func QuickSort[T any](arr []T, compare func(a, b T) int) {
    if len(arr) < 2 {
        return
    }
    quickSortRecursive(arr, 0, len(arr)-1, compare)
}

func quickSortRecursive[T any](arr []T, low, high int, compare func(a, b T) int) {
    if low < high {
        pi := partition(arr, low, high, compare)
        quickSortRecursive(arr, low, pi-1, compare)
        quickSortRecursive(arr, pi+1, high, compare)
    }
}

func partition[T any](arr []T, low, high int, compare func(a, b T) int) int {
    pivot := arr[high]
    i := low - 1
    
    for j := low; j < high; j++ {
        if compare(arr[j], pivot) <= 0 {
            i++
            arr[i], arr[j] = arr[j], arr[i]
        }
    }
    arr[i+1], arr[high] = arr[high], arr[i+1]
    return i + 1
}

func MergeSort[T any](arr []T, compare func(a, b T) int) []T {
    if len(arr) <= 1 {
        return arr
    }
    
    mid := len(arr) / 2
    left := MergeSort(arr[:mid], compare)
    right := MergeSort(arr[mid:], compare)
    
    return merge(left, right, compare)
}

func merge[T any](left, right []T, compare func(a, b T) int) []T {
    result := make([]T, 0, len(left)+len(right))
    i, j := 0, 0
    
    for i < len(left) && j < len(right) {
        if compare(left[i], right[j]) <= 0 {
            result = append(result, left[i])
            i++
        } else {
            result = append(result, right[j])
            j++
        }
    }
    
    result = append(result, left[i:]...)
    result = append(result, right[j:]...)
    
    return result
}

// Simple HTTP client for testing
func fetchData(url string) (string, error) {
    resp, err := http.Get(url)
    if err != nil {
        return "", err
    }
    defer resp.Body.Close()
    
    body, err := io.ReadAll(resp.Body)
    if err != nil {
        return "", err
    }
    
    return string(body), nil
}

// JSON processing
type Person struct {
    Name    string `json:"name"`
    Age     int    `json:"age"`
    Email   string `json:"email"`
    Address struct {
        Street string `json:"street"`
        City   string `json:"city"`
    } `json:"address"`
}

func processJSON(jsonStr string) (*Person, error) {
    var person Person
    err := json.Unmarshal([]byte(jsonStr), &person)
    if err != nil {
        return nil, err
    }
    return &person, nil
}

// Mathematical functions
func fibonacci(n int) []int {
    if n <= 0 {
        return []int{}
    }
    if n == 1 {
        return []int{0}
    }
    
    fib := make([]int, n)
    fib[0], fib[1] = 0, 1
    
    for i := 2; i < n; i++ {
        fib[i] = fib[i-1] + fib[i-2]
    }
    
    return fib
}

func isPrime(n int) bool {
    if n < 2 {
        return false
    }
    if n == 2 {
        return true
    }
    if n%2 == 0 {
        return false
    }
    
    for i := 3; i*i <= n; i += 2 {
        if n%i == 0 {
            return false
        }
    }
    return true
}

func sieveOfEratosthenes(limit int) []int {
    if limit < 2 {
        return []int{}
    }
    
    sieve := make([]bool, limit+1)
    for i := 2; i <= limit; i++ {
        sieve[i] = true
    }
    
    for i := 2; i*i <= limit; i++ {
        if sieve[i] {
            for j := i * i; j <= limit; j += i {
                sieve[j] = false
            }
        }
    }
    
    primes := make([]int, 0)
    for i := 2; i <= limit; i++ {
        if sieve[i] {
            primes = append(primes, i)
        }
    }
    
    return primes
}

// String processing algorithms
func longestCommonSubsequence(s1, s2 string) string {
    m, n := len(s1), len(s2)
    dp := make([][]int, m+1)
    for i := range dp {
        dp[i] = make([]int, n+1)
    }
    
    for i := 1; i <= m; i++ {
        for j := 1; j <= n; j++ {
            if s1[i-1] == s2[j-1] {
                dp[i][j] = dp[i-1][j-1] + 1
            } else {
                dp[i][j] = max(dp[i-1][j], dp[i][j-1])
            }
        }
    }
    
    // Reconstruct LCS
    lcs := make([]byte, 0, dp[m][n])
    i, j := m, n
    for i > 0 && j > 0 {
        if s1[i-1] == s2[j-1] {
            lcs = append([]byte{s1[i-1]}, lcs...)
            i--
            j--
        } else if dp[i-1][j] > dp[i][j-1] {
            i--
        } else {
            j--
        }
    }
    
    return string(lcs)
}

func max(a, b int) int {
    if a > b {
        return a
    }
    return b
}

func main() {
    fmt.Println("=== Advanced Data Structures and Algorithms Demo ===")
    
    // Stack demo
    fmt.Println("\n1. Generic Stack:")
    stack := NewStack[int]()
    for i := 1; i <= 5; i++ {
        stack.Push(i)
    }
    fmt.Printf("  Stack size: %d\n", stack.Size())
    for stack.Size() > 0 {
        if val, ok := stack.Pop(); ok {
            fmt.Printf("  Popped: %d\n", val)
        }
    }
    
    // Queue demo
    fmt.Println("\n2. Generic Queue:")
    queue := NewQueue[string]()
    queue.Enqueue("first")
    queue.Enqueue("second")
    queue.Enqueue("third")
    fmt.Printf("  Queue size: %d\n", queue.Size())
    for queue.Size() > 0 {
        if val, ok := queue.Dequeue(); ok {
            fmt.Printf("  Dequeued: %s\n", val)
        }
    }
    
    // BST demo  
    fmt.Println("\n3. Binary Search Tree:")
    bst := NewBST[int](func(a, b int) int { return a - b })
    values := []int{50, 30, 70, 20, 40, 60, 80}
    for _, val := range values {
        bst.Insert(val)
    }
    fmt.Printf("  In-order traversal: %v\n", bst.InOrderTraversal())
    
    // Graph demo
    fmt.Println("\n4. Graph Algorithms:")
    graph := NewGraph()
    vertices := []string{"A", "B", "C", "D", "E"}
    for _, v := range vertices {
        graph.AddVertex(v)
    }
    graph.AddEdge("A", "B")
    graph.AddEdge("A", "C")
    graph.AddEdge("B", "D")
    graph.AddEdge("C", "E")
    graph.AddEdge("D", "E")
    
    fmt.Printf("  BFS from A: %v\n", graph.BFS("A"))
    fmt.Printf("  DFS from A: %v\n", graph.DFS("A"))
    
    path, dist := graph.Dijkstra("A", "E")
    fmt.Printf("  Shortest path A->E: %v (distance: %d)\n", path, dist)
    
    // Sorting demo
    fmt.Println("\n5. Sorting Algorithms:")
    numbers := []int{64, 34, 25, 12, 22, 11, 90}
    fmt.Printf("  Original: %v\n", numbers)
    
    // Quick sort
    quickSorted := make([]int, len(numbers))
    copy(quickSorted, numbers)
    QuickSort(quickSorted, func(a, b int) int { return a - b })
    fmt.Printf("  Quick sorted: %v\n", quickSorted)
    
    // Merge sort
    mergeSorted := MergeSort(numbers, func(a, b int) int { return a - b })
    fmt.Printf("  Merge sorted: %v\n", mergeSorted)
    
    // Mathematical algorithms
    fmt.Println("\n6. Mathematical Algorithms:")
    fmt.Printf("  Fibonacci(10): %v\n", fibonacci(10))
    fmt.Printf("  Prime numbers up to 30: %v\n", sieveOfEratosthenes(30))
    fmt.Printf("  Is 17 prime? %t\n", isPrime(17))
    fmt.Printf("  Is 25 prime? %t\n", isPrime(25))
    
    // String algorithms
    fmt.Println("\n7. String Algorithms:")
    s1, s2 := "ABCDGH", "AEDFHR"
    lcs := longestCommonSubsequence(s1, s2)
    fmt.Printf("  LCS of '%s' and '%s': '%s'\n", s1, s2, lcs)
    
    // JSON processing
    fmt.Println("\n8. JSON Processing:")
    jsonStr := `{
        "name": "John Doe",
        "age": 30,
        "email": "john@example.com",
        "address": {
            "street": "123 Main St",
            "city": "New York"
        }
    }`
    
    if person, err := processJSON(jsonStr); err == nil {
        fmt.Printf("  Parsed person: %s, age %d, lives in %s\n", 
            person.Name, person.Age, person.Address.City)
    }
    
    fmt.Println("\n=== Demo completed ===")
}