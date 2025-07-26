package main

import (
	"fmt"
	"math"
	"sort"
)

// Generic data structures

type Stack struct {
	items []interface{}
}

func NewStack() *Stack {
	return &Stack{items: make([]interface{}, 0)}
}

func (s *Stack) Push(item interface{}) {
	s.items = append(s.items, item)
}

func (s *Stack) Pop() (interface{}, error) {
	if len(s.items) == 0 {
		return nil, fmt.Errorf("stack is empty")
	}
	item := s.items[len(s.items)-1]
	s.items = s.items[:len(s.items)-1]
	return item, nil
}

func (s *Stack) Peek() (interface{}, error) {
	if len(s.items) == 0 {
		return nil, fmt.Errorf("stack is empty")
	}
	return s.items[len(s.items)-1], nil
}

func (s *Stack) Size() int {
	return len(s.items)
}

func (s *Stack) IsEmpty() bool {
	return len(s.items) == 0
}

type Queue struct {
	items []interface{}
}

func NewQueue() *Queue {
	return &Queue{items: make([]interface{}, 0)}
}

func (q *Queue) Enqueue(item interface{}) {
	q.items = append(q.items, item)
}

func (q *Queue) Dequeue() (interface{}, error) {
	if len(q.items) == 0 {
		return nil, fmt.Errorf("queue is empty")
	}
	item := q.items[0]
	q.items = q.items[1:]
	return item, nil
}

func (q *Queue) Front() (interface{}, error) {
	if len(q.items) == 0 {
		return nil, fmt.Errorf("queue is empty")
	}
	return q.items[0], nil
}

func (q *Queue) Size() int {
	return len(q.items)
}

func (q *Queue) IsEmpty() bool {
	return len(q.items) == 0
}

// Binary Tree implementation
type TreeNode struct {
	Value int
	Left  *TreeNode
	Right *TreeNode
}

type BinaryTree struct {
	Root *TreeNode
}

func NewBinaryTree() *BinaryTree {
	return &BinaryTree{Root: nil}
}

func (bt *BinaryTree) Insert(value int) {
	bt.Root = bt.insertNode(bt.Root, value)
}

func (bt *BinaryTree) insertNode(node *TreeNode, value int) *TreeNode {
	if node == nil {
		return &TreeNode{Value: value}
	}
	
	if value < node.Value {
		node.Left = bt.insertNode(node.Left, value)
	} else {
		node.Right = bt.insertNode(node.Right, value)
	}
	return node
}

func (bt *BinaryTree) Search(value int) bool {
	return bt.searchNode(bt.Root, value)
}

func (bt *BinaryTree) searchNode(node *TreeNode, value int) bool {
	if node == nil {
		return false
	}
	
	if value == node.Value {
		return true
	} else if value < node.Value {
		return bt.searchNode(node.Left, value)
	} else {
		return bt.searchNode(node.Right, value)
	}
}

func (bt *BinaryTree) InorderTraversal() []int {
	var result []int
	bt.inorder(bt.Root, &result)
	return result
}

func (bt *BinaryTree) inorder(node *TreeNode, result *[]int) {
	if node != nil {
		bt.inorder(node.Left, result)
		*result = append(*result, node.Value)
		bt.inorder(node.Right, result)
	}
}

func (bt *BinaryTree) PreorderTraversal() []int {
	var result []int
	bt.preorder(bt.Root, &result)
	return result
}

func (bt *BinaryTree) preorder(node *TreeNode, result *[]int) {
	if node != nil {
		*result = append(*result, node.Value)
		bt.preorder(node.Left, result)
		bt.preorder(node.Right, result)
	}
}

func (bt *BinaryTree) PostorderTraversal() []int {
	var result []int
	bt.postorder(bt.Root, &result)
	return result
}

func (bt *BinaryTree) postorder(node *TreeNode, result *[]int) {
	if node != nil {
		bt.postorder(node.Left, result)
		bt.postorder(node.Right, result)
		*result = append(*result, node.Value)
	}
}

// HashTable implementation
type HashTable struct {
	buckets [][]KeyValue
	size    int
}

type KeyValue struct {
	Key   string
	Value interface{}
}

func NewHashTable(size int) *HashTable {
	return &HashTable{
		buckets: make([][]KeyValue, size),
		size:    size,
	}
}

func (ht *HashTable) hash(key string) int {
	hash := 0
	for _, char := range key {
		hash += int(char)
	}
	return hash % ht.size
}

func (ht *HashTable) Set(key string, value interface{}) {
	index := ht.hash(key)
	
	for i, kv := range ht.buckets[index] {
		if kv.Key == key {
			ht.buckets[index][i].Value = value
			return
		}
	}
	
	ht.buckets[index] = append(ht.buckets[index], KeyValue{Key: key, Value: value})
}

func (ht *HashTable) Get(key string) (interface{}, error) {
	index := ht.hash(key)
	
	for _, kv := range ht.buckets[index] {
		if kv.Key == key {
			return kv.Value, nil
		}
	}
	
	return nil, fmt.Errorf("key '%s' not found", key)
}

func (ht *HashTable) Delete(key string) error {
	index := ht.hash(key)
	
	for i, kv := range ht.buckets[index] {
		if kv.Key == key {
			ht.buckets[index] = append(ht.buckets[index][:i], ht.buckets[index][i+1:]...)
			return nil
		}
	}
	
	return fmt.Errorf("key '%s' not found", key)
}

func (ht *HashTable) Keys() []string {
	var keys []string
	for _, bucket := range ht.buckets {
		for _, kv := range bucket {
			keys = append(keys, kv.Key)
		}
	}
	return keys
}

// Sorting algorithms
func BubbleSort(arr []int) []int {
	result := make([]int, len(arr))
	copy(result, arr)
	
	n := len(result)
	for i := 0; i < n-1; i++ {
		for j := 0; j < n-i-1; j++ {
			if result[j] > result[j+1] {
				result[j], result[j+1] = result[j+1], result[j]
			}
		}
	}
	return result
}

func QuickSort(arr []int) []int {
	result := make([]int, len(arr))
	copy(result, arr)
	quickSortHelper(result, 0, len(result)-1)
	return result
}

func quickSortHelper(arr []int, low, high int) {
	if low < high {
		pi := partition(arr, low, high)
		quickSortHelper(arr, low, pi-1)
		quickSortHelper(arr, pi+1, high)
	}
}

func partition(arr []int, low, high int) int {
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
}

func MergeSort(arr []int) []int {
	if len(arr) <= 1 {
		return arr
	}
	
	mid := len(arr) / 2
	left := MergeSort(arr[:mid])
	right := MergeSort(arr[mid:])
	
	return merge(left, right)
}

func merge(left, right []int) []int {
	result := make([]int, 0, len(left)+len(right))
	i, j := 0, 0
	
	for i < len(left) && j < len(right) {
		if left[i] <= right[j] {
			result = append(result, left[i])
			i++
		} else {
			result = append(result, right[j])
			j++
		}
	}
	
	for i < len(left) {
		result = append(result, left[i])
		i++
	}
	
	for j < len(right) {
		result = append(result, right[j])
		j++
	}
	
	return result
}

// Mathematical utilities
func Factorial(n int) int {
	if n <= 1 {
		return 1
	}
	return n * Factorial(n-1)
}

func Fibonacci(n int) int {
	if n <= 1 {
		return n
	}
	return Fibonacci(n-1) + Fibonacci(n-2)
}

func IsPrime(n int) bool {
	if n <= 1 {
		return false
	}
	if n <= 3 {
		return true
	}
	if n%2 == 0 || n%3 == 0 {
		return false
	}
	
	for i := 5; i*i <= n; i += 6 {
		if n%i == 0 || n%(i+2) == 0 {
			return false
		}
	}
	return true
}

func GCD(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func LCM(a, b int) int {
	return (a * b) / GCD(a, b)
}

func main() {
	fmt.Println("Advanced Data Structures and Algorithms Demo")
	fmt.Println("============================================")
	
	// Stack demo
	fmt.Println("\n1. Stack Operations:")
	stack := NewStack()
	stack.Push(10)
	stack.Push(20)
	stack.Push(30)
	fmt.Printf("Stack size: %d\n", stack.Size())
	
	top, _ := stack.Peek()
	fmt.Printf("Top element: %v\n", top)
	
	popped, _ := stack.Pop()
	fmt.Printf("Popped: %v\n", popped)
	fmt.Printf("Stack size after pop: %d\n", stack.Size())
	
	// Queue demo
	fmt.Println("\n2. Queue Operations:")
	queue := NewQueue()
	queue.Enqueue("A")
	queue.Enqueue("B")
	queue.Enqueue("C")
	fmt.Printf("Queue size: %d\n", queue.Size())
	
	front, _ := queue.Front()
	fmt.Printf("Front element: %v\n", front)
	
	dequeued, _ := queue.Dequeue()
	fmt.Printf("Dequeued: %v\n", dequeued)
	fmt.Printf("Queue size after dequeue: %d\n", queue.Size())
	
	// Binary Tree demo
	fmt.Println("\n3. Binary Tree Operations:")
	tree := NewBinaryTree()
	values := []int{50, 30, 70, 20, 40, 60, 80}
	for _, v := range values {
		tree.Insert(v)
	}
	
	fmt.Printf("Inorder traversal: %v\n", tree.InorderTraversal())
	fmt.Printf("Preorder traversal: %v\n", tree.PreorderTraversal())
	fmt.Printf("Postorder traversal: %v\n", tree.PostorderTraversal())
	
	fmt.Printf("Search 40: %t\n", tree.Search(40))
	fmt.Printf("Search 90: %t\n", tree.Search(90))
	
	// Hash Table demo
	fmt.Println("\n4. Hash Table Operations:")
	ht := NewHashTable(10)
	ht.Set("name", "John")
	ht.Set("age", 25)
	ht.Set("city", "New York")
	
	name, _ := ht.Get("name")
	fmt.Printf("Name: %v\n", name)
	
	age, _ := ht.Get("age")
	fmt.Printf("Age: %v\n", age)
	
	fmt.Printf("All keys: %v\n", ht.Keys())
	
	// Sorting algorithms demo
	fmt.Println("\n5. Sorting Algorithms:")
	unsorted := []int{64, 34, 25, 12, 22, 11, 90}
	fmt.Printf("Unsorted array: %v\n", unsorted)
	
	bubbleSorted := BubbleSort(unsorted)
	fmt.Printf("Bubble sort: %v\n", bubbleSorted)
	
	quickSorted := QuickSort(unsorted)
	fmt.Printf("Quick sort: %v\n", quickSorted)
	
	mergeSorted := MergeSort(unsorted)
	fmt.Printf("Merge sort: %v\n", mergeSorted)
	
	// Mathematical functions demo
	fmt.Println("\n6. Mathematical Functions:")
	fmt.Printf("Factorial of 5: %d\n", Factorial(5))
	fmt.Printf("Fibonacci of 10: %d\n", Fibonacci(10))
	fmt.Printf("Is 17 prime: %t\n", IsPrime(17))
	fmt.Printf("Is 18 prime: %t\n", IsPrime(18))
	fmt.Printf("GCD of 48 and 18: %d\n", GCD(48, 18))
	fmt.Printf("LCM of 12 and 8: %d\n", LCM(12, 8))
	
	// Performance comparison
	fmt.Println("\n7. Performance Test:")
	largeArray := make([]int, 1000)
	for i := 0; i < 1000; i++ {
		largeArray[i] = 1000 - i
	}
	
	fmt.Printf("Sorting array of size %d\n", len(largeArray))
	
	// Test different sorting algorithms (commented out for brevity)
	// You would measure time here in a real scenario
	sorted1 := BubbleSort(largeArray[:100]) // Only first 100 elements for bubble sort
	fmt.Printf("Bubble sort (100 elements): %d...%d\n", sorted1[0], sorted1[len(sorted1)-1])
	
	sorted2 := QuickSort(largeArray)
	fmt.Printf("Quick sort (1000 elements): %d...%d\n", sorted2[0], sorted2[len(sorted2)-1])
	
	sorted3 := MergeSort(largeArray)
	fmt.Printf("Merge sort (1000 elements): %d...%d\n", sorted3[0], sorted3[len(sorted3)-1])
}