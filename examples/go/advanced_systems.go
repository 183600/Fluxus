package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"
)

type User struct {
	ID       int    `json:"id"`
	Name     string `json:"name"`
	Email    string `json:"email"`
	Age      int    `json:"age"`
	Balance  float64 `json:"balance"`
}

type BankAccount struct {
	mu      sync.RWMutex
	balance float64
	owner   string
}

func (ba *BankAccount) Deposit(amount float64) {
	ba.mu.Lock()
	defer ba.mu.Unlock()
	ba.balance += amount
}

func (ba *BankAccount) Withdraw(amount float64) bool {
	ba.mu.Lock()
	defer ba.mu.Unlock()
	if ba.balance >= amount {
		ba.balance -= amount
		return true
	}
	return false
}

func (ba *BankAccount) GetBalance() float64 {
	ba.mu.RLock()
	defer ba.mu.RUnlock()
	return ba.balance
}

type Stack struct {
	items []interface{}
	mutex sync.Mutex
}

func (s *Stack) Push(item interface{}) {
	s.mutex.Lock()
	defer s.mutex.Unlock()
	s.items = append(s.items, item)
}

func (s *Stack) Pop() interface{} {
	s.mutex.Lock()
	defer s.mutex.Unlock()
	if len(s.items) == 0 {
		return nil
	}
	item := s.items[len(s.items)-1]
	s.items = s.items[:len(s.items)-1]
	return item
}

func (s *Stack) IsEmpty() bool {
	s.mutex.Lock()
	defer s.mutex.Unlock()
	return len(s.items) == 0
}

type Queue struct {
	items []interface{}
	mutex sync.Mutex
}

func (q *Queue) Enqueue(item interface{}) {
	q.mutex.Lock()
	defer q.mutex.Unlock()
	q.items = append(q.items, item)
}

func (q *Queue) Dequeue() interface{} {
	q.mutex.Lock()
	defer q.mutex.Unlock()
	if len(q.items) == 0 {
		return nil
	}
	item := q.items[0]
	q.items = q.items[1:]
	return item
}

func (q *Queue) IsEmpty() bool {
	q.mutex.Lock()
	defer q.mutex.Unlock()
	return len(q.items) == 0
}

func workerPool(jobs <-chan int, results chan<- int, wg *sync.WaitGroup) {
	defer wg.Done()
	for job := range jobs {
		// Simulate work
		time.Sleep(time.Millisecond * 10)
		results <- job * job
	}
}

func calculateStatistics(numbers []float64) (mean, median, mode float64, variance float64) {
	if len(numbers) == 0 {
		return 0, 0, 0, 0
	}

	// Mean
	sum := 0.0
	for _, num := range numbers {
		sum += num
	}
	mean = sum / float64(len(numbers))

	// Variance
	sumSquaredDiff := 0.0
	for _, num := range numbers {
		diff := num - mean
		sumSquaredDiff += diff * diff
	}
	variance = sumSquaredDiff / float64(len(numbers))

	// Median (simplified)
	sortedNums := make([]float64, len(numbers))
	copy(sortedNums, numbers)
	for i := 0; i < len(sortedNums); i++ {
		for j := i + 1; j < len(sortedNums); j++ {
			if sortedNums[i] > sortedNums[j] {
				sortedNums[i], sortedNums[j] = sortedNums[j], sortedNums[i]
			}
		}
	}
	
	if len(sortedNums)%2 == 0 {
		median = (sortedNums[len(sortedNums)/2-1] + sortedNums[len(sortedNums)/2]) / 2
	} else {
		median = sortedNums[len(sortedNums)/2]
	}

	// Mode (simplified - returns first most frequent)
	frequency := make(map[float64]int)
	maxCount := 0
	for _, num := range numbers {
		frequency[num]++
		if frequency[num] > maxCount {
			maxCount = frequency[num]
			mode = num
		}
	}

	return mean, median, mode, variance
}

func processTextFile(filename string) (int, int, map[string]int, error) {
	file, err := os.Open(filename)
	if err != nil {
		return 0, 0, nil, err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	wordCount := make(map[string]int)
	totalLines := 0
	totalWords := 0

	for scanner.Scan() {
		totalLines++
		line := strings.ToLower(scanner.Text())
		words := strings.Fields(line)
		totalWords += len(words)
		
		for _, word := range words {
			// Remove punctuation
			word = strings.Trim(word, ".,!?;:\"'()[]{}\"")
			if word != "" {
				wordCount[word]++
			}
		}
	}

	return totalLines, totalWords, wordCount, scanner.Err()
}

func evaluateExpression(expression string) (float64, error) {
	// Simple expression evaluator for basic arithmetic
	expression = strings.ReplaceAll(expression, " ", "")
	
	// Handle simple addition/subtraction
	if strings.Contains(expression, "+") {
		parts := strings.Split(expression, "+")
		if len(parts) == 2 {
			left, err1 := strconv.ParseFloat(parts[0], 64)
			right, err2 := strconv.ParseFloat(parts[1], 64)
			if err1 == nil && err2 == nil {
				return left + right, nil
			}
		}
	}
	
	if strings.Contains(expression, "-") && !strings.HasPrefix(expression, "-") {
		parts := strings.Split(expression, "-")
		if len(parts) == 2 {
			left, err1 := strconv.ParseFloat(parts[0], 64)
			right, err2 := strconv.ParseFloat(parts[1], 64)
			if err1 == nil && err2 == nil {
				return left - right, nil
			}
		}
	}
	
	if strings.Contains(expression, "*") {
		parts := strings.Split(expression, "*")
		if len(parts) == 2 {
			left, err1 := strconv.ParseFloat(parts[0], 64)
			right, err2 := strconv.ParseFloat(parts[1], 64)
			if err1 == nil && err2 == nil {
				return left * right, nil
			}
		}
	}
	
	if strings.Contains(expression, "/") {
		parts := strings.Split(expression, "/")
		if len(parts) == 2 {
			left, err1 := strconv.ParseFloat(parts[0], 64)
			right, err2 := strconv.ParseFloat(parts[1], 64)
			if err1 == nil && err2 == nil && right != 0 {
				return left / right, nil
			}
		}
	}
	
	// Try to parse as single number
	return strconv.ParseFloat(expression, 64)
}

func main() {
	fmt.Println("=== Advanced Go Programming Features Demo ===")

	// 1. JSON Processing with User Data
	fmt.Println("\n1. JSON Processing:")
	users := []User{
		{1, "Alice", "alice@example.com", 30, 1000.50},
		{2, "Bob", "bob@example.com", 25, 750.25},
		{3, "Charlie", "charlie@example.com", 35, 2000.00},
	}

	jsonData, _ := json.MarshalIndent(users, "", "  ")
	fmt.Printf("JSON data:\n%s\n", jsonData)

	var parsedUsers []User
	json.Unmarshal(jsonData, &parsedUsers)
	fmt.Printf("Parsed %d users from JSON\n", len(parsedUsers))

	// 2. Concurrent Banking System
	fmt.Println("\n2. Concurrent Banking System:")
	account := &BankAccount{balance: 1000.0, owner: "John Doe"}
	
	var wg sync.WaitGroup
	
	// Simulate concurrent deposits and withdrawals
	for i := 0; i < 5; i++ {
		wg.Add(2)
		
		go func(id int) {
			defer wg.Done()
			account.Deposit(float64(id * 10))
			fmt.Printf("Deposited %d, balance: %.2f\n", id*10, account.GetBalance())
		}(i)
		
		go func(id int) {
			defer wg.Done()
			success := account.Withdraw(float64(id * 5))
			fmt.Printf("Withdrawal %d success: %t, balance: %.2f\n", id*5, success, account.GetBalance())
		}(i)
	}
	
	wg.Wait()
	fmt.Printf("Final balance: %.2f\n", account.GetBalance())

	// 3. Data Structures (Stack and Queue)
	fmt.Println("\n3. Data Structures:")
	stack := &Stack{}
	queue := &Queue{}

	// Stack operations
	for i := 1; i <= 5; i++ {
		stack.Push(i)
	}
	fmt.Print("Stack pop order: ")
	for !stack.IsEmpty() {
		fmt.Printf("%v ", stack.Pop())
	}
	fmt.Println()

	// Queue operations
	for i := 1; i <= 5; i++ {
		queue.Enqueue(i)
	}
	fmt.Print("Queue dequeue order: ")
	for !queue.IsEmpty() {
		fmt.Printf("%v ", queue.Dequeue())
	}
	fmt.Println()

	// 4. Worker Pool Pattern
	fmt.Println("\n4. Worker Pool Pattern:")
	jobs := make(chan int, 10)
	results := make(chan int, 10)

	var workerWg sync.WaitGroup
	
	// Start 3 workers
	for w := 1; w <= 3; w++ {
		workerWg.Add(1)
		go workerPool(jobs, results, &workerWg)
	}

	// Send jobs
	for j := 1; j <= 10; j++ {
		jobs <- j
	}
	close(jobs)

	// Wait for workers to finish
	go func() {
		workerWg.Wait()
		close(results)
	}()

	// Collect results
	fmt.Print("Worker results: ")
	for result := range results {
		fmt.Printf("%d ", result)
	}
	fmt.Println()

	// 5. Statistical Analysis
	fmt.Println("\n5. Statistical Analysis:")
	numbers := []float64{1.5, 2.3, 3.7, 2.3, 4.1, 5.2, 2.3, 6.8, 7.9, 3.7}
	mean, median, mode, variance := calculateStatistics(numbers)
	fmt.Printf("Data: %v\n", numbers)
	fmt.Printf("Mean: %.2f, Median: %.2f, Mode: %.2f, Variance: %.2f\n", mean, median, mode, variance)

	// 6. Expression Evaluator
	fmt.Println("\n6. Expression Evaluator:")
	expressions := []string{"10+5", "20-3", "4*7", "100/4", "3.14*2"}
	for _, expr := range expressions {
		result, err := evaluateExpression(expr)
		if err == nil {
			fmt.Printf("%s = %.2f\n", expr, result)
		} else {
			fmt.Printf("Error evaluating %s: %v\n", expr, err)
		}
	}

	// 7. Create a temporary text file for processing
	fmt.Println("\n7. Text File Processing:")
	tempFile := "/tmp/sample_text.txt"
	sampleText := `This is a sample text file.
It contains multiple lines of text.
Some words appear multiple times.
This text will be analyzed for word frequency.
The analysis will count lines, words, and word frequency.`

	err := os.WriteFile(tempFile, []byte(sampleText), 0644)
	if err == nil {
		lines, words, wordFreq, err := processTextFile(tempFile)
		if err == nil {
			fmt.Printf("Lines: %d, Words: %d\n", lines, words)
			fmt.Println("Most frequent words:")
			count := 0
			for word, freq := range wordFreq {
				if freq > 1 && count < 5 {
					fmt.Printf("  %s: %d\n", word, freq)
					count++
				}
			}
		}
		os.Remove(tempFile) // Clean up
	}

	fmt.Println("\n=== Advanced Demo Complete ===")
}