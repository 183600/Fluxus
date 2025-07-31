package main

import (
	"container/heap"
	"fmt"
	"math/rand"
	"sync"
	"time"
)

// DataStructure interface for common operations
type DataStructure interface {
	Size() int
	IsEmpty() bool
	Clear()
}

// =============================================================================
// DYNAMIC ARRAY IMPLEMENTATION
// =============================================================================

// DynamicArray implements a resizable array with automatic capacity management
type DynamicArray struct {
	data     []interface{}
	size     int
	capacity int
}

// NewDynamicArray creates a new dynamic array with initial capacity
func NewDynamicArray(initialCapacity int) *DynamicArray {
	if initialCapacity < 4 {
		initialCapacity = 4
	}
	return &DynamicArray{
		data:     make([]interface{}, initialCapacity),
		size:     0,
		capacity: initialCapacity,
	}
}

// resize changes the internal array capacity
func (da *DynamicArray) resize(newCapacity int) {
	newData := make([]interface{}, newCapacity)
	copy(newData, da.data[:da.size])
	da.data = newData
	da.capacity = newCapacity
}

// Append adds an element to the end of the array
func (da *DynamicArray) Append(item interface{}) {
	if da.size == da.capacity {
		da.resize(2 * da.capacity)
	}
	da.data[da.size] = item
	da.size++
}

// Get retrieves element at index
func (da *DynamicArray) Get(index int) interface{} {
	if index < 0 || index >= da.size {
		panic("Index out of range")
	}
	return da.data[index]
}

// Set updates element at index
func (da *DynamicArray) Set(index int, item interface{}) {
	if index < 0 || index >= da.size {
		panic("Index out of range")
	}
	da.data[index] = item
}

// Insert adds element at specific index
func (da *DynamicArray) Insert(index int, item interface{}) {
	if index < 0 || index > da.size {
		panic("Index out of range")
	}
	
	if da.size == da.capacity {
		da.resize(2 * da.capacity)
	}
	
	// Shift elements to the right
	for i := da.size; i > index; i-- {
		da.data[i] = da.data[i-1]
	}
	
	da.data[index] = item
	da.size++
}

// Delete removes element at index
func (da *DynamicArray) Delete(index int) {
	if index < 0 || index >= da.size {
		panic("Index out of range")
	}
	
	// Shift elements to the left
	for i := index; i < da.size-1; i++ {
		da.data[i] = da.data[i+1]
	}
	
	da.size--
	
	// Shrink if necessary
	if da.size <= da.capacity/4 && da.capacity > 4 {
		da.resize(da.capacity / 2)
	}
}

// Size returns the current size
func (da *DynamicArray) Size() int {
	return da.size
}

// IsEmpty checks if array is empty
func (da *DynamicArray) IsEmpty() bool {
	return da.size == 0
}

// Clear removes all elements
func (da *DynamicArray) Clear() {
	da.size = 0
	da.capacity = 4
	da.data = make([]interface{}, da.capacity)
}

// ToSlice returns a copy as a slice
func (da *DynamicArray) ToSlice() []interface{} {
	result := make([]interface{}, da.size)
	copy(result, da.data[:da.size])
	return result
}

// =============================================================================
// LINKED LIST IMPLEMENTATION
// =============================================================================

// ListNode represents a node in the linked list
type ListNode struct {
	Data interface{}
	Next *ListNode
}

// LinkedList implements a singly linked list
type LinkedList struct {
	head *ListNode
	size int
}

// NewLinkedList creates a new linked list
func NewLinkedList() *LinkedList {
	return &LinkedList{head: nil, size: 0}
}

// Prepend adds element to the beginning
func (ll *LinkedList) Prepend(data interface{}) {
	newNode := &ListNode{Data: data, Next: ll.head}
	ll.head = newNode
	ll.size++
}

// Append adds element to the end
func (ll *LinkedList) Append(data interface{}) {
	newNode := &ListNode{Data: data, Next: nil}
	
	if ll.head == nil {
		ll.head = newNode
	} else {
		current := ll.head
		for current.Next != nil {
			current = current.Next
		}
		current.Next = newNode
	}
	ll.size++
}

// Insert adds element at specific index
func (ll *LinkedList) Insert(index int, data interface{}) {
	if index < 0 || index > ll.size {
		panic("Index out of range")
	}
	
	if index == 0 {
		ll.Prepend(data)
		return
	}
	
	newNode := &ListNode{Data: data, Next: nil}
	current := ll.head
	
	for i := 0; i < index-1; i++ {
		current = current.Next
	}
	
	newNode.Next = current.Next
	current.Next = newNode
	ll.size++
}

// Delete removes element at index
func (ll *LinkedList) Delete(index int) {
	if index < 0 || index >= ll.size {
		panic("Index out of range")
	}
	
	if index == 0 {
		ll.head = ll.head.Next
		ll.size--
		return
	}
	
	current := ll.head
	for i := 0; i < index-1; i++ {
		current = current.Next
	}
	
	current.Next = current.Next.Next
	ll.size--
}

// Find returns index of element, -1 if not found
func (ll *LinkedList) Find(data interface{}) int {
	current := ll.head
	index := 0
	
	for current != nil {
		if current.Data == data {
			return index
		}
		current = current.Next
		index++
	}
	
	return -1
}

// Size returns the current size
func (ll *LinkedList) Size() int {
	return ll.size
}

// IsEmpty checks if list is empty
func (ll *LinkedList) IsEmpty() bool {
	return ll.size == 0
}

// Clear removes all elements
func (ll *LinkedList) Clear() {
	ll.head = nil
	ll.size = 0
}

// ToSlice returns a copy as a slice
func (ll *LinkedList) ToSlice() []interface{} {
	result := make([]interface{}, 0, ll.size)
	current := ll.head
	
	for current != nil {
		result = append(result, current.Data)
		current = current.Next
	}
	
	return result
}

// =============================================================================
// STACK IMPLEMENTATION
// =============================================================================

// Stack implements a LIFO data structure
type Stack struct {
	items []interface{}
}

// NewStack creates a new stack
func NewStack() *Stack {
	return &Stack{items: make([]interface{}, 0)}
}

// Push adds item to top of stack
func (s *Stack) Push(item interface{}) {
	s.items = append(s.items, item)
}

// Pop removes and returns top item
func (s *Stack) Pop() interface{} {
	if len(s.items) == 0 {
		panic("Pop from empty stack")
	}
	
	index := len(s.items) - 1
	item := s.items[index]
	s.items = s.items[:index]
	return item
}

// Peek returns top item without removing it
func (s *Stack) Peek() interface{} {
	if len(s.items) == 0 {
		panic("Peek at empty stack")
	}
	return s.items[len(s.items)-1]
}

// Size returns the current size
func (s *Stack) Size() int {
	return len(s.items)
}

// IsEmpty checks if stack is empty
func (s *Stack) IsEmpty() bool {
	return len(s.items) == 0
}

// Clear removes all elements
func (s *Stack) Clear() {
	s.items = s.items[:0]
}

// =============================================================================
// QUEUE IMPLEMENTATION
// =============================================================================

// Queue implements a FIFO data structure using a circular buffer
type Queue struct {
	items []interface{}
	front int
	rear  int
	size  int
}

// NewQueue creates a new queue with initial capacity
func NewQueue(capacity int) *Queue {
	if capacity < 4 {
		capacity = 4
	}
	return &Queue{
		items: make([]interface{}, capacity),
		front: 0,
		rear:  0,
		size:  0,
	}
}

// resize increases queue capacity
func (q *Queue) resize() {
	newCapacity := len(q.items) * 2
	newItems := make([]interface{}, newCapacity)
	
	for i := 0; i < q.size; i++ {
		newItems[i] = q.items[(q.front+i)%len(q.items)]
	}
	
	q.items = newItems
	q.front = 0
	q.rear = q.size
}

// Enqueue adds item to rear of queue
func (q *Queue) Enqueue(item interface{}) {
	if q.size == len(q.items) {
		q.resize()
	}
	
	q.items[q.rear] = item
	q.rear = (q.rear + 1) % len(q.items)
	q.size++
}

// Dequeue removes and returns front item
func (q *Queue) Dequeue() interface{} {
	if q.size == 0 {
		panic("Dequeue from empty queue")
	}
	
	item := q.items[q.front]
	q.front = (q.front + 1) % len(q.items)
	q.size--
	return item
}

// Front returns front item without removing it
func (q *Queue) Front() interface{} {
	if q.size == 0 {
		panic("Front of empty queue")
	}
	return q.items[q.front]
}

// Size returns the current size
func (q *Queue) Size() int {
	return q.size
}

// IsEmpty checks if queue is empty
func (q *Queue) IsEmpty() bool {
	return q.size == 0
}

// Clear removes all elements
func (q *Queue) Clear() {
	q.front = 0
	q.rear = 0
	q.size = 0
}

// =============================================================================
// BINARY SEARCH TREE IMPLEMENTATION
// =============================================================================

// TreeNode represents a node in the binary search tree
type TreeNode struct {
	Data   int
	Left   *TreeNode
	Right  *TreeNode
	Height int // For AVL tree
}

// BinarySearchTree implements a BST
type BinarySearchTree struct {
	root *TreeNode
	size int
}

// NewBST creates a new binary search tree
func NewBST() *BinarySearchTree {
	return &BinarySearchTree{root: nil, size: 0}
}

// Insert adds a value to the BST
func (bst *BinarySearchTree) Insert(data int) {
	bst.root = bst.insertRecursive(bst.root, data)
}

// insertRecursive helper for insert
func (bst *BinarySearchTree) insertRecursive(node *TreeNode, data int) *TreeNode {
	if node == nil {
		bst.size++
		return &TreeNode{Data: data, Left: nil, Right: nil, Height: 1}
	}
	
	if data < node.Data {
		node.Left = bst.insertRecursive(node.Left, data)
	} else if data > node.Data {
		node.Right = bst.insertRecursive(node.Right, data)
	}
	
	return node
}

// Search looks for a value in the BST
func (bst *BinarySearchTree) Search(data int) bool {
	return bst.searchRecursive(bst.root, data)
}

// searchRecursive helper for search
func (bst *BinarySearchTree) searchRecursive(node *TreeNode, data int) bool {
	if node == nil {
		return false
	}
	
	if data == node.Data {
		return true
	} else if data < node.Data {
		return bst.searchRecursive(node.Left, data)
	} else {
		return bst.searchRecursive(node.Right, data)
	}
}

// InorderTraversal returns values in sorted order
func (bst *BinarySearchTree) InorderTraversal() []int {
	var result []int
	bst.inorderRecursive(bst.root, &result)
	return result
}

// inorderRecursive helper for inorder traversal
func (bst *BinarySearchTree) inorderRecursive(node *TreeNode, result *[]int) {
	if node != nil {
		bst.inorderRecursive(node.Left, result)
		*result = append(*result, node.Data)
		bst.inorderRecursive(node.Right, result)
	}
}

// Delete removes a value from the BST
func (bst *BinarySearchTree) Delete(data int) {
	bst.root = bst.deleteRecursive(bst.root, data)
}

// deleteRecursive helper for delete
func (bst *BinarySearchTree) deleteRecursive(node *TreeNode, data int) *TreeNode {
	if node == nil {
		return node
	}
	
	if data < node.Data {
		node.Left = bst.deleteRecursive(node.Left, data)
	} else if data > node.Data {
		node.Right = bst.deleteRecursive(node.Right, data)
	} else {
		bst.size--
		// Node to be deleted found
		if node.Left == nil {
			return node.Right
		} else if node.Right == nil {
			return node.Left
		}
		
		// Node with two children
		successor := bst.findMin(node.Right)
		node.Data = successor.Data
		node.Right = bst.deleteRecursive(node.Right, successor.Data)
		bst.size++ // Compensate for the decrement above
	}
	
	return node
}

// findMin finds the minimum node in a subtree
func (bst *BinarySearchTree) findMin(node *TreeNode) *TreeNode {
	current := node
	for current.Left != nil {
		current = current.Left
	}
	return current
}

// Size returns the current size
func (bst *BinarySearchTree) Size() int {
	return bst.size
}

// IsEmpty checks if BST is empty
func (bst *BinarySearchTree) IsEmpty() bool {
	return bst.size == 0
}

// Clear removes all elements
func (bst *BinarySearchTree) Clear() {
	bst.root = nil
	bst.size = 0
}

// =============================================================================
// HASH TABLE IMPLEMENTATION
// =============================================================================

// HashEntry represents a key-value pair in the hash table
type HashEntry struct {
	Key   string
	Value interface{}
	Next  *HashEntry
}

// HashTable implements a hash table with chaining
type HashTable struct {
	buckets  []*HashEntry
	capacity int
	size     int
}

// NewHashTable creates a new hash table
func NewHashTable(capacity int) *HashTable {
	if capacity < 16 {
		capacity = 16
	}
	return &HashTable{
		buckets:  make([]*HashEntry, capacity),
		capacity: capacity,
		size:     0,
	}
}

// hash function using djb2 algorithm
func (ht *HashTable) hash(key string) int {
	hash := 5381
	for _, c := range key {
		hash = ((hash << 5) + hash) + int(c)
	}
	return hash % ht.capacity
}

// resize doubles the hash table capacity
func (ht *HashTable) resize() {
	oldBuckets := ht.buckets
	oldCapacity := ht.capacity
	
	ht.capacity *= 2
	ht.buckets = make([]*HashEntry, ht.capacity)
	ht.size = 0
	
	// Rehash all existing entries
	for i := 0; i < oldCapacity; i++ {
		entry := oldBuckets[i]
		for entry != nil {
			ht.Put(entry.Key, entry.Value)
			entry = entry.Next
		}
	}
}

// Put inserts or updates a key-value pair
func (ht *HashTable) Put(key string, value interface{}) {
	if float64(ht.size) >= float64(ht.capacity)*0.75 {
		ht.resize()
	}
	
	bucketIndex := ht.hash(key)
	entry := ht.buckets[bucketIndex]
	
	// Check if key already exists
	for entry != nil {
		if entry.Key == key {
			entry.Value = value
			return
		}
		entry = entry.Next
	}
	
	// Insert new entry at beginning of chain
	newEntry := &HashEntry{Key: key, Value: value, Next: ht.buckets[bucketIndex]}
	ht.buckets[bucketIndex] = newEntry
	ht.size++
}

// Get retrieves value by key
func (ht *HashTable) Get(key string) (interface{}, bool) {
	bucketIndex := ht.hash(key)
	entry := ht.buckets[bucketIndex]
	
	for entry != nil {
		if entry.Key == key {
			return entry.Value, true
		}
		entry = entry.Next
	}
	
	return nil, false
}

// Delete removes a key-value pair
func (ht *HashTable) Delete(key string) bool {
	bucketIndex := ht.hash(key)
	entry := ht.buckets[bucketIndex]
	
	if entry == nil {
		return false
	}
	
	// Check if first entry matches
	if entry.Key == key {
		ht.buckets[bucketIndex] = entry.Next
		ht.size--
		return true
	}
	
	// Search through chain
	for entry.Next != nil {
		if entry.Next.Key == key {
			entry.Next = entry.Next.Next
			ht.size--
			return true
		}
		entry = entry.Next
	}
	
	return false
}

// Keys returns all keys
func (ht *HashTable) Keys() []string {
	keys := make([]string, 0, ht.size)
	
	for i := 0; i < ht.capacity; i++ {
		entry := ht.buckets[i]
		for entry != nil {
			keys = append(keys, entry.Key)
			entry = entry.Next
		}
	}
	
	return keys
}

// Size returns the current size
func (ht *HashTable) Size() int {
	return ht.size
}

// IsEmpty checks if hash table is empty
func (ht *HashTable) IsEmpty() bool {
	return ht.size == 0
}

// Clear removes all elements
func (ht *HashTable) Clear() {
	ht.buckets = make([]*HashEntry, ht.capacity)
	ht.size = 0
}

// =============================================================================
// MIN HEAP IMPLEMENTATION
// =============================================================================

// MinHeap implements a binary min heap
type MinHeap struct {
	items []int
}

// NewMinHeap creates a new min heap
func NewMinHeap() *MinHeap {
	return &MinHeap{items: make([]int, 0)}
}

// parent returns parent index
func (h *MinHeap) parent(i int) int {
	return (i - 1) / 2
}

// leftChild returns left child index
func (h *MinHeap) leftChild(i int) int {
	return 2*i + 1
}

// rightChild returns right child index
func (h *MinHeap) rightChild(i int) int {
	return 2*i + 2
}

// swap swaps elements at indices i and j
func (h *MinHeap) swap(i, j int) {
	h.items[i], h.items[j] = h.items[j], h.items[i]
}

// Insert adds a value to the heap
func (h *MinHeap) Insert(value int) {
	h.items = append(h.items, value)
	h.heapifyUp(len(h.items) - 1)
}

// heapifyUp maintains heap property upward
func (h *MinHeap) heapifyUp(i int) {
	for i > 0 && h.items[h.parent(i)] > h.items[i] {
		h.swap(i, h.parent(i))
		i = h.parent(i)
	}
}

// ExtractMin removes and returns minimum element
func (h *MinHeap) ExtractMin() int {
	if len(h.items) == 0 {
		panic("Extract from empty heap")
	}
	
	min := h.items[0]
	h.items[0] = h.items[len(h.items)-1]
	h.items = h.items[:len(h.items)-1]
	
	if len(h.items) > 0 {
		h.heapifyDown(0)
	}
	
	return min
}

// heapifyDown maintains heap property downward
func (h *MinHeap) heapifyDown(i int) {
	minIndex := i
	left := h.leftChild(i)
	right := h.rightChild(i)
	
	if left < len(h.items) && h.items[left] < h.items[minIndex] {
		minIndex = left
	}
	
	if right < len(h.items) && h.items[right] < h.items[minIndex] {
		minIndex = right
	}
	
	if minIndex != i {
		h.swap(i, minIndex)
		h.heapifyDown(minIndex)
	}
}

// Peek returns minimum element without removing it
func (h *MinHeap) Peek() int {
	if len(h.items) == 0 {
		panic("Peek at empty heap")
	}
	return h.items[0]
}

// Size returns the current size
func (h *MinHeap) Size() int {
	return len(h.items)
}

// IsEmpty checks if heap is empty
func (h *MinHeap) IsEmpty() bool {
	return len(h.items) == 0
}

// Clear removes all elements
func (h *MinHeap) Clear() {
	h.items = h.items[:0]
}

// =============================================================================
// TRIE IMPLEMENTATION
// =============================================================================

// TrieNode represents a node in the trie
type TrieNode struct {
	Children    map[rune]*TrieNode
	IsEndOfWord bool
}

// Trie implements a prefix tree
type Trie struct {
	root *TrieNode
	size int
}

// NewTrie creates a new trie
func NewTrie() *Trie {
	return &Trie{
		root: &TrieNode{
			Children:    make(map[rune]*TrieNode),
			IsEndOfWord: false,
		},
		size: 0,
	}
}

// Insert adds a word to the trie
func (t *Trie) Insert(word string) {
	node := t.root
	
	for _, char := range word {
		if _, exists := node.Children[char]; !exists {
			node.Children[char] = &TrieNode{
				Children:    make(map[rune]*TrieNode),
				IsEndOfWord: false,
			}
		}
		node = node.Children[char]
	}
	
	if !node.IsEndOfWord {
		node.IsEndOfWord = true
		t.size++
	}
}

// Search looks for a word in the trie
func (t *Trie) Search(word string) bool {
	node := t.root
	
	for _, char := range word {
		if _, exists := node.Children[char]; !exists {
			return false
		}
		node = node.Children[char]
	}
	
	return node.IsEndOfWord
}

// StartsWith checks if any word starts with prefix
func (t *Trie) StartsWith(prefix string) bool {
	node := t.root
	
	for _, char := range prefix {
		if _, exists := node.Children[char]; !exists {
			return false
		}
		node = node.Children[char]
	}
	
	return true
}

// GetWordsWithPrefix returns all words with given prefix
func (t *Trie) GetWordsWithPrefix(prefix string) []string {
	node := t.root
	
	// Navigate to prefix node
	for _, char := range prefix {
		if _, exists := node.Children[char]; !exists {
			return []string{}
		}
		node = node.Children[char]
	}
	
	// DFS to find all words
	var words []string
	t.dfs(node, prefix, &words)
	return words
}

// dfs helper for finding words
func (t *Trie) dfs(node *TrieNode, currentWord string, words *[]string) {
	if node.IsEndOfWord {
		*words = append(*words, currentWord)
	}
	
	for char, childNode := range node.Children {
		t.dfs(childNode, currentWord+string(char), words)
	}
}

// Size returns the current size
func (t *Trie) Size() int {
	return t.size
}

// IsEmpty checks if trie is empty
func (t *Trie) IsEmpty() bool {
	return t.size == 0
}

// Clear removes all elements
func (t *Trie) Clear() {
	t.root = &TrieNode{
		Children:    make(map[rune]*TrieNode),
		IsEndOfWord: false,
	}
	t.size = 0
}

// =============================================================================
// GRAPH IMPLEMENTATION
// =============================================================================

// Graph implements a graph using adjacency list
type Graph struct {
	vertices   map[string]bool
	edges      map[string][]Edge
	isDirected bool
}

// Edge represents a weighted edge
type Edge struct {
	To     string
	Weight int
}

// NewGraph creates a new graph
func NewGraph(isDirected bool) *Graph {
	return &Graph{
		vertices:   make(map[string]bool),
		edges:      make(map[string][]Edge),
		isDirected: isDirected,
	}
}

// AddVertex adds a vertex to the graph
func (g *Graph) AddVertex(vertex string) {
	g.vertices[vertex] = true
	if _, exists := g.edges[vertex]; !exists {
		g.edges[vertex] = make([]Edge, 0)
	}
}

// AddEdge adds an edge to the graph
func (g *Graph) AddEdge(from, to string, weight int) {
	g.AddVertex(from)
	g.AddVertex(to)
	
	g.edges[from] = append(g.edges[from], Edge{To: to, Weight: weight})
	
	if !g.isDirected {
		g.edges[to] = append(g.edges[to], Edge{To: from, Weight: weight})
	}
}

// GetNeighbors returns neighbors of a vertex
func (g *Graph) GetNeighbors(vertex string) []Edge {
	return g.edges[vertex]
}

// BFS performs breadth-first search
func (g *Graph) BFS(start string) []string {
	if !g.vertices[start] {
		return []string{}
	}
	
	visited := make(map[string]bool)
	queue := []string{start}
	result := []string{}
	
	for len(queue) > 0 {
		vertex := queue[0]
		queue = queue[1:]
		
		if !visited[vertex] {
			visited[vertex] = true
			result = append(result, vertex)
			
			for _, edge := range g.edges[vertex] {
				if !visited[edge.To] {
					queue = append(queue, edge.To)
				}
			}
		}
	}
	
	return result
}

// DFS performs depth-first search
func (g *Graph) DFS(start string) []string {
	if !g.vertices[start] {
		return []string{}
	}
	
	visited := make(map[string]bool)
	result := []string{}
	
	g.dfsHelper(start, visited, &result)
	return result
}

// dfsHelper recursive helper for DFS
func (g *Graph) dfsHelper(vertex string, visited map[string]bool, result *[]string) {
	visited[vertex] = true
	*result = append(*result, vertex)
	
	for _, edge := range g.edges[vertex] {
		if !visited[edge.To] {
			g.dfsHelper(edge.To, visited, result)
		}
	}
}

// GetVertices returns all vertices
func (g *Graph) GetVertices() []string {
	vertices := make([]string, 0, len(g.vertices))
	for vertex := range g.vertices {
		vertices = append(vertices, vertex)
	}
	return vertices
}

// Size returns number of vertices
func (g *Graph) Size() int {
	return len(g.vertices)
}

// IsEmpty checks if graph is empty
func (g *Graph) IsEmpty() bool {
	return len(g.vertices) == 0
}

// Clear removes all vertices and edges
func (g *Graph) Clear() {
	g.vertices = make(map[string]bool)
	g.edges = make(map[string][]Edge)
}

// =============================================================================
// LRU CACHE IMPLEMENTATION
// =============================================================================

// LRUNode represents a node in the LRU cache
type LRUNode struct {
	Key   string
	Value interface{}
	Prev  *LRUNode
	Next  *LRUNode
}

// LRUCache implements a Least Recently Used cache
type LRUCache struct {
	capacity int
	cache    map[string]*LRUNode
	head     *LRUNode
	tail     *LRUNode
	mutex    sync.RWMutex
}

// NewLRUCache creates a new LRU cache
func NewLRUCache(capacity int) *LRUCache {
	lru := &LRUCache{
		capacity: capacity,
		cache:    make(map[string]*LRUNode),
		head:     &LRUNode{},
		tail:     &LRUNode{},
	}
	
	lru.head.Next = lru.tail
	lru.tail.Prev = lru.head
	
	return lru
}

// addToHead adds node right after head
func (lru *LRUCache) addToHead(node *LRUNode) {
	node.Prev = lru.head
	node.Next = lru.head.Next
	
	lru.head.Next.Prev = node
	lru.head.Next = node
}

// removeNode removes an existing node
func (lru *LRUCache) removeNode(node *LRUNode) {
	node.Prev.Next = node.Next
	node.Next.Prev = node.Prev
}

// moveToHead moves node to head
func (lru *LRUCache) moveToHead(node *LRUNode) {
	lru.removeNode(node)
	lru.addToHead(node)
}

// popTail removes the last node
func (lru *LRUCache) popTail() *LRUNode {
	lastNode := lru.tail.Prev
	lru.removeNode(lastNode)
	return lastNode
}

// Get retrieves value by key
func (lru *LRUCache) Get(key string) (interface{}, bool) {
	lru.mutex.Lock()
	defer lru.mutex.Unlock()
	
	if node, exists := lru.cache[key]; exists {
		lru.moveToHead(node)
		return node.Value, true
	}
	
	return nil, false
}

// Put inserts or updates a key-value pair
func (lru *LRUCache) Put(key string, value interface{}) {
	lru.mutex.Lock()
	defer lru.mutex.Unlock()
	
	if node, exists := lru.cache[key]; exists {
		node.Value = value
		lru.moveToHead(node)
	} else {
		newNode := &LRUNode{Key: key, Value: value}
		
		if len(lru.cache) >= lru.capacity {
			tail := lru.popTail()
			delete(lru.cache, tail.Key)
		}
		
		lru.cache[key] = newNode
		lru.addToHead(newNode)
	}
}

// Keys returns all keys in order of usage (most recent first)
func (lru *LRUCache) Keys() []string {
	lru.mutex.RLock()
	defer lru.mutex.RUnlock()
	
	keys := make([]string, 0, len(lru.cache))
	current := lru.head.Next
	
	for current != lru.tail {
		keys = append(keys, current.Key)
		current = current.Next
	}
	
	return keys
}

// Size returns the current size
func (lru *LRUCache) Size() int {
	lru.mutex.RLock()
	defer lru.mutex.RUnlock()
	return len(lru.cache)
}

// IsEmpty checks if cache is empty
func (lru *LRUCache) IsEmpty() bool {
	lru.mutex.RLock()
	defer lru.mutex.RUnlock()
	return len(lru.cache) == 0
}

// Clear removes all elements
func (lru *LRUCache) Clear() {
	lru.mutex.Lock()
	defer lru.mutex.Unlock()
	
	lru.cache = make(map[string]*LRUNode)
	lru.head.Next = lru.tail
	lru.tail.Prev = lru.head
}

// =============================================================================
// DEMONSTRATION FUNCTIONS
// =============================================================================

func demonstrateDynamicArray() {
	fmt.Println("=== Dynamic Array Demo ===")
	
	arr := NewDynamicArray(4)
	
	// Test append
	for i := 0; i < 10; i++ {
		arr.Append(i)
	}
	fmt.Printf("After appending 0-9: %v\n", arr.ToSlice())
	fmt.Printf("Size: %d, Capacity: %d\n", arr.Size(), arr.capacity)
	
	// Test insert
	arr.Insert(5, 99)
	fmt.Printf("After inserting 99 at index 5: %v\n", arr.ToSlice())
	
	// Test delete
	arr.Delete(5)
	fmt.Printf("After deleting index 5: %v\n", arr.ToSlice())
	
	// Test get/set
	fmt.Printf("Element at index 3: %v\n", arr.Get(3))
	arr.Set(3, 42)
	fmt.Printf("After setting index 3 to 42: %v\n", arr.ToSlice())
}

func demonstrateLinkedList() {
	fmt.Println("\n=== Linked List Demo ===")
	
	ll := NewLinkedList()
	
	// Test append
	for i := 1; i <= 5; i++ {
		ll.Append(i)
	}
	fmt.Printf("After appending 1-5: %v\n", ll.ToSlice())
	
	// Test prepend
	ll.Prepend(0)
	fmt.Printf("After prepending 0: %v\n", ll.ToSlice())
	
	// Test insert
	ll.Insert(3, 99)
	fmt.Printf("After inserting 99 at index 3: %v\n", ll.ToSlice())
	
	// Test find
	fmt.Printf("Index of 99: %d\n", ll.Find(99))
	
	// Test delete
	ll.Delete(3)
	fmt.Printf("After deleting index 3: %v\n", ll.ToSlice())
}

func demonstrateStack() {
	fmt.Println("\n=== Stack Demo ===")
	
	stack := NewStack()
	
	// Test push
	for i := 1; i <= 5; i++ {
		stack.Push(i)
		fmt.Printf("Pushed %d, stack size: %d\n", i, stack.Size())
	}
	
	// Test peek
	fmt.Printf("Top element: %v\n", stack.Peek())
	
	// Test pop
	for !stack.IsEmpty() {
		popped := stack.Pop()
		fmt.Printf("Popped %v, remaining size: %d\n", popped, stack.Size())
	}
}

func demonstrateQueue() {
	fmt.Println("\n=== Queue Demo ===")
	
	queue := NewQueue(4)
	
	// Test enqueue
	for i := 1; i <= 5; i++ {
		queue.Enqueue(i)
		fmt.Printf("Enqueued %d, queue size: %d\n", i, queue.Size())
	}
	
	// Test front
	fmt.Printf("Front element: %v\n", queue.Front())
	
	// Test dequeue
	for !queue.IsEmpty() {
		dequeued := queue.Dequeue()
		fmt.Printf("Dequeued %v, remaining size: %d\n", dequeued, queue.Size())
	}
}

func demonstrateBST() {
	fmt.Println("\n=== Binary Search Tree Demo ===")
	
	bst := NewBST()
	
	// Test insert
	values := []int{50, 30, 70, 20, 40, 60, 80}
	for _, val := range values {
		bst.Insert(val)
	}
	
	fmt.Printf("Inserted values: %v\n", values)
	fmt.Printf("Inorder traversal: %v\n", bst.InorderTraversal())
	
	// Test search
	fmt.Printf("Search 40: %v\n", bst.Search(40))
	fmt.Printf("Search 90: %v\n", bst.Search(90))
	
	// Test delete
	bst.Delete(30)
	fmt.Printf("After deleting 30: %v\n", bst.InorderTraversal())
}

func demonstrateHashTable() {
	fmt.Println("\n=== Hash Table Demo ===")
	
	ht := NewHashTable(16)
	
	// Test put
	ht.Put("apple", 5)
	ht.Put("banana", 3)
	ht.Put("orange", 8)
	ht.Put("grape", 12)
	
	fmt.Printf("Keys: %v\n", ht.Keys())
	
	// Test get
	if value, exists := ht.Get("apple"); exists {
		fmt.Printf("apple: %v\n", value)
	}
	
	// Test update
	ht.Put("apple", 10)
	if value, exists := ht.Get("apple"); exists {
		fmt.Printf("Updated apple: %v\n", value)
	}
	
	// Test delete
	ht.Delete("banana")
	fmt.Printf("After deleting banana, keys: %v\n", ht.Keys())
}

func demonstrateHeap() {
	fmt.Println("\n=== Min Heap Demo ===")
	
	heap := NewMinHeap()
	
	// Test insert
	values := []int{20, 15, 30, 10, 25, 5}
	for _, val := range values {
		heap.Insert(val)
		fmt.Printf("Inserted %d, min: %d\n", val, heap.Peek())
	}
	
	// Test extract min
	fmt.Println("Extracting minimums:")
	for !heap.IsEmpty() {
		min := heap.ExtractMin()
		fmt.Printf("Extracted %d, remaining size: %d\n", min, heap.Size())
	}
}

func demonstrateTrie() {
	fmt.Println("\n=== Trie Demo ===")
	
	trie := NewTrie()
	
	// Test insert
	words := []string{"cat", "car", "card", "care", "careful", "cars", "carry"}
	for _, word := range words {
		trie.Insert(word)
	}
	
	fmt.Printf("Inserted words: %v\n", words)
	
	// Test search
	fmt.Printf("Search 'car': %v\n", trie.Search("car"))
	fmt.Printf("Search 'care': %v\n", trie.Search("care"))
	fmt.Printf("Search 'caring': %v\n", trie.Search("caring"))
	
	// Test starts with
	fmt.Printf("Starts with 'car': %v\n", trie.StartsWith("car"))
	fmt.Printf("Words with prefix 'car': %v\n", trie.GetWordsWithPrefix("car"))
}

func demonstrateGraph() {
	fmt.Println("\n=== Graph Demo ===")
	
	graph := NewGraph(false) // Undirected graph
	
	// Add edges
	edges := [][3]interface{}{
		{"A", "B", 1}, {"B", "C", 1}, {"C", "D", 1}, {"A", "D", 1}, {"B", "D", 1},
	}
	for _, edge := range edges {
		graph.AddEdge(edge[0].(string), edge[1].(string), edge[2].(int))
	}
	
	fmt.Printf("Vertices: %v\n", graph.GetVertices())
	
	// Test BFS
	fmt.Printf("BFS from A: %v\n", graph.BFS("A"))
	
	// Test DFS
	fmt.Printf("DFS from A: %v\n", graph.DFS("A"))
}

func demonstrateLRUCache() {
	fmt.Println("\n=== LRU Cache Demo ===")
	
	cache := NewLRUCache(3)
	
	// Test put and get
	cache.Put("a", 1)
	cache.Put("b", 2)
	cache.Put("c", 3)
	fmt.Printf("After putting a,b,c: keys=%v\n", cache.Keys())
	
	if value, exists := cache.Get("a"); exists {
		fmt.Printf("Get 'a': %v\n", value)
	}
	fmt.Printf("Keys after getting 'a': %v\n", cache.Keys())
	
	cache.Put("d", 4) // Should evict 'b'
	fmt.Printf("After putting 'd': keys=%v\n", cache.Keys())
	
	if _, exists := cache.Get("b"); !exists {
		fmt.Printf("Get 'b': not found (evicted)\n")
	}
}

// Benchmark function to test performance
func benchmarkDataStructures() {
	fmt.Println("\n=== Performance Benchmarks ===")
	
	const operations = 10000
	
	// Dynamic Array benchmark
	start := time.Now()
	arr := NewDynamicArray(4)
	for i := 0; i < operations; i++ {
		arr.Append(i)
	}
	fmt.Printf("Dynamic Array %d appends: %v\n", operations, time.Since(start))
	
	// Linked List benchmark
	start = time.Now()
	ll := NewLinkedList()
	for i := 0; i < operations; i++ {
		ll.Append(i)
	}
	fmt.Printf("Linked List %d appends: %v\n", operations, time.Since(start))
	
	// Hash Table benchmark
	start = time.Now()
	ht := NewHashTable(16)
	for i := 0; i < operations; i++ {
		ht.Put(fmt.Sprintf("key%d", i), i)
	}
	fmt.Printf("Hash Table %d puts: %v\n", operations, time.Since(start))
	
	// BST benchmark
	start = time.Now()
	bst := NewBST()
	for i := 0; i < operations; i++ {
		bst.Insert(rand.Intn(operations * 2))
	}
	fmt.Printf("BST %d inserts: %v\n", operations, time.Since(start))
	
	// Min Heap benchmark
	start = time.Now()
	heap := NewMinHeap()
	for i := 0; i < operations; i++ {
		heap.Insert(rand.Intn(operations * 2))
	}
	fmt.Printf("Min Heap %d inserts: %v\n", operations, time.Since(start))
}

func main() {
	fmt.Println("Comprehensive Data Structures Suite in Go")
	fmt.Println("=" + fmt.Sprintf("%50s", "").Replace(" ", "=", -1))
	
	rand.Seed(time.Now().UnixNano())
	
	// Run all demonstrations
	demonstrateDynamicArray()
	demonstrateLinkedList()
	demonstrateStack()
	demonstrateQueue()
	demonstrateBST()
	demonstrateHashTable()
	demonstrateHeap()
	demonstrateTrie()
	demonstrateGraph()
	demonstrateLRUCache()
	
	// Run performance benchmarks
	benchmarkDataStructures()
	
	fmt.Println("\nData structures demonstration completed!")
}