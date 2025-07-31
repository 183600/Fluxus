package main

import (
	"fmt"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"
)

type Config struct {
	Host     string
	Port     int
	Database string
	Timeout  time.Duration
}

func NewConfig() *Config {
	return &Config{
		Host:     "localhost",
		Port:     8080,
		Database: "myapp",
		Timeout:  30 * time.Second,
	}
}

func (c *Config) SetHost(host string) *Config {
	c.Host = host
	return c
}

func (c *Config) SetPort(port int) *Config {
	c.Port = port
	return c
}

func (c *Config) SetDatabase(db string) *Config {
	c.Database = db
	return c
}

func (c *Config) SetTimeout(timeout time.Duration) *Config {
	c.Timeout = timeout
	return c
}

func (c *Config) String() string {
	return fmt.Sprintf("Config{Host: %s, Port: %d, Database: %s, Timeout: %v}",
		c.Host, c.Port, c.Database, c.Timeout)
}

type Employee struct {
	ID         int
	Name       string
	Department string
	Salary     float64
	HireDate   time.Time
}

func NewEmployee(id int, name, department string, salary float64) *Employee {
	return &Employee{
		ID:         id,
		Name:       name,
		Department: department,
		Salary:     salary,
		HireDate:   time.Now(),
	}
}

func (e *Employee) String() string {
	return fmt.Sprintf("Employee{ID: %d, Name: %s, Dept: %s, Salary: %.2f}",
		e.ID, e.Name, e.Department, e.Salary)
}

func (e *Employee) GiveRaise(percentage float64) {
	e.Salary *= (1 + percentage/100)
}

func (e *Employee) YearsOfService() int {
	return int(time.Since(e.HireDate).Hours() / (24 * 365))
}

type EmployeeManager struct {
	employees []Employee
	mutex     sync.RWMutex
}

func NewEmployeeManager() *EmployeeManager {
	return &EmployeeManager{
		employees: make([]Employee, 0),
	}
}

func (em *EmployeeManager) AddEmployee(employee Employee) {
	em.mutex.Lock()
	defer em.mutex.Unlock()
	em.employees = append(em.employees, employee)
}

func (em *EmployeeManager) GetEmployee(id int) (*Employee, bool) {
	em.mutex.RLock()
	defer em.mutex.RUnlock()
	
	for i := range em.employees {
		if em.employees[i].ID == id {
			return &em.employees[i], true
		}
	}
	return nil, false
}

func (em *EmployeeManager) GetAllEmployees() []Employee {
	em.mutex.RLock()
	defer em.mutex.RUnlock()
	
	result := make([]Employee, len(em.employees))
	copy(result, em.employees)
	return result
}

func (em *EmployeeManager) GetEmployeesByDepartment(department string) []Employee {
	em.mutex.RLock()
	defer em.mutex.RUnlock()
	
	var result []Employee
	for _, emp := range em.employees {
		if emp.Department == department {
			result = append(result, emp)
		}
	}
	return result
}

func (em *EmployeeManager) UpdateSalary(id int, newSalary float64) bool {
	em.mutex.Lock()
	defer em.mutex.Unlock()
	
	for i := range em.employees {
		if em.employees[i].ID == id {
			em.employees[i].Salary = newSalary
			return true
		}
	}
	return false
}

func (em *EmployeeManager) RemoveEmployee(id int) bool {
	em.mutex.Lock()
	defer em.mutex.Unlock()
	
	for i, emp := range em.employees {
		if emp.ID == id {
			em.employees = append(em.employees[:i], em.employees[i+1:]...)
			return true
		}
	}
	return false
}

func (em *EmployeeManager) GetStatistics() map[string]interface{} {
	em.mutex.RLock()
	defer em.mutex.RUnlock()
	
	if len(em.employees) == 0 {
		return map[string]interface{}{
			"total_employees": 0,
			"departments":     0,
			"average_salary":  0.0,
		}
	}
	
	departments := make(map[string]int)
	totalSalary := 0.0
	
	for _, emp := range em.employees {
		departments[emp.Department]++
		totalSalary += emp.Salary
	}
	
	return map[string]interface{}{
		"total_employees": len(em.employees),
		"departments":     len(departments),
		"average_salary":  totalSalary / float64(len(em.employees)),
		"dept_breakdown":  departments,
	}
}

type Cache struct {
	data  map[string]interface{}
	mutex sync.RWMutex
	ttl   map[string]time.Time
}

func NewCache() *Cache {
	cache := &Cache{
		data: make(map[string]interface{}),
		ttl:  make(map[string]time.Time),
	}
	
	go cache.cleanupExpired()
	return cache
}

func (c *Cache) Set(key string, value interface{}, duration time.Duration) {
	c.mutex.Lock()
	defer c.mutex.Unlock()
	
	c.data[key] = value
	c.ttl[key] = time.Now().Add(duration)
}

func (c *Cache) Get(key string) (interface{}, bool) {
	c.mutex.RLock()
	defer c.mutex.RUnlock()
	
	expiry, exists := c.ttl[key]
	if !exists || time.Now().After(expiry) {
		return nil, false
	}
	
	value, exists := c.data[key]
	return value, exists
}

func (c *Cache) Delete(key string) {
	c.mutex.Lock()
	defer c.mutex.Unlock()
	
	delete(c.data, key)
	delete(c.ttl, key)
}

func (c *Cache) Clear() {
	c.mutex.Lock()
	defer c.mutex.Unlock()
	
	c.data = make(map[string]interface{})
	c.ttl = make(map[string]time.Time)
}

func (c *Cache) Size() int {
	c.mutex.RLock()
	defer c.mutex.RUnlock()
	
	return len(c.data)
}

func (c *Cache) Keys() []string {
	c.mutex.RLock()
	defer c.mutex.RUnlock()
	
	keys := make([]string, 0, len(c.data))
	for key, expiry := range c.ttl {
		if !time.Now().After(expiry) {
			keys = append(keys, key)
		}
	}
	
	return keys
}

func (c *Cache) cleanupExpired() {
	ticker := time.NewTicker(1 * time.Minute)
	defer ticker.Stop()
	
	for range ticker.C {
		c.mutex.Lock()
		now := time.Now()
		
		for key, expiry := range c.ttl {
			if now.After(expiry) {
				delete(c.data, key)
				delete(c.ttl, key)
			}
		}
		
		c.mutex.Unlock()
	}
}

type TaskScheduler struct {
	tasks chan func()
	quit  chan bool
	wg    sync.WaitGroup
}

func NewTaskScheduler(workers int) *TaskScheduler {
	scheduler := &TaskScheduler{
		tasks: make(chan func(), 100),
		quit:  make(chan bool),
	}
	
	for i := 0; i < workers; i++ {
		scheduler.wg.Add(1)
		go scheduler.worker()
	}
	
	return scheduler
}

func (ts *TaskScheduler) worker() {
	defer ts.wg.Done()
	
	for {
		select {
		case task := <-ts.tasks:
			task()
		case <-ts.quit:
			return
		}
	}
}

func (ts *TaskScheduler) Schedule(task func()) {
	select {
	case ts.tasks <- task:
	default:
		fmt.Println("Task queue is full, dropping task")
	}
}

func (ts *TaskScheduler) ScheduleDelayed(task func(), delay time.Duration) {
	go func() {
		time.Sleep(delay)
		ts.Schedule(task)
	}()
}

func (ts *TaskScheduler) Stop() {
	close(ts.quit)
	ts.wg.Wait()
}

type DataProcessor struct {
	processingTime time.Duration
	results        []interface{}
	mutex          sync.Mutex
}

func NewDataProcessor() *DataProcessor {
	return &DataProcessor{
		processingTime: 100 * time.Millisecond,
		results:        make([]interface{}, 0),
	}
}

func (dp *DataProcessor) ProcessItems(items []interface{}) []interface{} {
	numWorkers := runtime.NumCPU()
	itemChan := make(chan interface{}, len(items))
	resultChan := make(chan interface{}, len(items))
	
	for _, item := range items {
		itemChan <- item
	}
	close(itemChan)
	
	var wg sync.WaitGroup
	
	for i := 0; i < numWorkers; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for item := range itemChan {
				result := dp.processItem(item)
				resultChan <- result
			}
		}()
	}
	
	go func() {
		wg.Wait()
		close(resultChan)
	}()
	
	var results []interface{}
	for result := range resultChan {
		results = append(results, result)
	}
	
	return results
}

func (dp *DataProcessor) processItem(item interface{}) interface{} {
	time.Sleep(dp.processingTime)
	
	switch v := item.(type) {
	case int:
		return v * v
	case string:
		return strings.ToUpper(v)
	case float64:
		return v * 2
	default:
		return fmt.Sprintf("processed_%v", v)
	}
}

func (dp *DataProcessor) GetStatistics() map[string]interface{} {
	dp.mutex.Lock()
	defer dp.mutex.Unlock()
	
	return map[string]interface{}{
		"total_processed":  len(dp.results),
		"processing_time":  dp.processingTime,
		"worker_count":     runtime.NumCPU(),
		"last_processed":   time.Now(),
	}
}

type MetricsCollector struct {
	counters map[string]int64
	gauges   map[string]float64
	mutex    sync.RWMutex
}

func NewMetricsCollector() *MetricsCollector {
	return &MetricsCollector{
		counters: make(map[string]int64),
		gauges:   make(map[string]float64),
	}
}

func (mc *MetricsCollector) IncrementCounter(name string, value int64) {
	mc.mutex.Lock()
	defer mc.mutex.Unlock()
	mc.counters[name] += value
}

func (mc *MetricsCollector) SetGauge(name string, value float64) {
	mc.mutex.Lock()
	defer mc.mutex.Unlock()
	mc.gauges[name] = value
}

func (mc *MetricsCollector) GetCounter(name string) int64 {
	mc.mutex.RLock()
	defer mc.mutex.RUnlock()
	return mc.counters[name]
}

func (mc *MetricsCollector) GetGauge(name string) float64 {
	mc.mutex.RLock()
	defer mc.mutex.RUnlock()
	return mc.gauges[name]
}

func (mc *MetricsCollector) GetAllMetrics() map[string]interface{} {
	mc.mutex.RLock()
	defer mc.mutex.RUnlock()
	
	result := make(map[string]interface{})
	
	counters := make(map[string]int64)
	for k, v := range mc.counters {
		counters[k] = v
	}
	
	gauges := make(map[string]float64)
	for k, v := range mc.gauges {
		gauges[k] = v
	}
	
	result["counters"] = counters
	result["gauges"] = gauges
	result["timestamp"] = time.Now()
	
	return result
}

func (mc *MetricsCollector) Reset() {
	mc.mutex.Lock()
	defer mc.mutex.Unlock()
	
	mc.counters = make(map[string]int64)
	mc.gauges = make(map[string]float64)
}

type StringUtilities struct{}

func (su *StringUtilities) Reverse(s string) string {
	runes := []rune(s)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

func (su *StringUtilities) IsPalindrome(s string) bool {
	cleaned := strings.ToLower(strings.ReplaceAll(s, " ", ""))
	return cleaned == su.Reverse(cleaned)
}

func (su *StringUtilities) WordCount(s string) map[string]int {
	words := strings.Fields(strings.ToLower(s))
	count := make(map[string]int)
	
	for _, word := range words {
		word = strings.Trim(word, ".,!?;:")
		if word != "" {
			count[word]++
		}
	}
	
	return count
}

func (su *StringUtilities) MostFrequentWords(s string, n int) []string {
	wordCount := su.WordCount(s)
	
	type wordFreq struct {
		word  string
		count int
	}
	
	var frequencies []wordFreq
	for word, count := range wordCount {
		frequencies = append(frequencies, wordFreq{word, count})
	}
	
	sort.Slice(frequencies, func(i, j int) bool {
		return frequencies[i].count > frequencies[j].count
	})
	
	var result []string
	for i := 0; i < n && i < len(frequencies); i++ {
		result = append(result, frequencies[i].word)
	}
	
	return result
}

func (su *StringUtilities) Capitalize(s string) string {
	if len(s) == 0 {
		return s
	}
	
	words := strings.Fields(s)
	var result []string
	
	for _, word := range words {
		if len(word) > 0 {
			capitalized := strings.ToUpper(word[:1]) + strings.ToLower(word[1:])
			result = append(result, capitalized)
		}
	}
	
	return strings.Join(result, " ")
}

func (su *StringUtilities) RemoveDuplicates(s string) string {
	seen := make(map[rune]bool)
	var result []rune
	
	for _, char := range s {
		if !seen[char] {
			seen[char] = true
			result = append(result, char)
		}
	}
	
	return string(result)
}

func (su *StringUtilities) CompressString(s string) string {
	if len(s) == 0 {
		return s
	}
	
	var result strings.Builder
	count := 1
	current := rune(s[0])
	
	for i := 1; i < len(s); i++ {
		if rune(s[i]) == current {
			count++
		} else {
			result.WriteRune(current)
			if count > 1 {
				result.WriteString(strconv.Itoa(count))
			}
			current = rune(s[i])
			count = 1
		}
	}
	
	result.WriteRune(current)
	if count > 1 {
		result.WriteString(strconv.Itoa(count))
	}
	
	compressed := result.String()
	if len(compressed) < len(s) {
		return compressed
	}
	return s
}

func demonstrateAdvancedFeatures() {
	fmt.Println("=== Advanced Go Features Demo ===\n")
	
	fmt.Println("1. Configuration Management:")
	config := NewConfig().
		SetHost("production.example.com").
		SetPort(443).
		SetDatabase("prod_db").
		SetTimeout(60 * time.Second)
	fmt.Printf("   %s\n", config.String())
	
	fmt.Println("\n2. Employee Management System:")
	manager := NewEmployeeManager()
	
	employees := []*Employee{
		NewEmployee(1, "Alice Johnson", "Engineering", 85000),
		NewEmployee(2, "Bob Smith", "Marketing", 65000),
		NewEmployee(3, "Carol Davis", "Engineering", 92000),
		NewEmployee(4, "David Wilson", "Sales", 70000),
		NewEmployee(5, "Eve Brown", "Marketing", 68000),
	}
	
	for _, emp := range employees {
		manager.AddEmployee(*emp)
	}
	
	fmt.Printf("   Total employees: %d\n", len(manager.GetAllEmployees()))
	
	engEmployees := manager.GetEmployeesByDepartment("Engineering")
	fmt.Printf("   Engineering employees: %d\n", len(engEmployees))
	
	stats := manager.GetStatistics()
	fmt.Printf("   Statistics: %+v\n", stats)
	
	fmt.Println("\n3. Caching System:")
	cache := NewCache()
	
	cache.Set("user:1", "Alice", 5*time.Second)
	cache.Set("user:2", "Bob", 10*time.Second)
	cache.Set("config:timeout", 30, 1*time.Minute)
	
	fmt.Printf("   Cache size: %d\n", cache.Size())
	fmt.Printf("   Keys: %v\n", cache.Keys())
	
	if value, found := cache.Get("user:1"); found {
		fmt.Printf("   Found user:1 = %v\n", value)
	}
	
	fmt.Println("\n4. Task Scheduling:")
	scheduler := NewTaskScheduler(3)
	
	for i := 0; i < 5; i++ {
		taskNum := i
		scheduler.Schedule(func() {
			fmt.Printf("   Executing task %d\n", taskNum)
			time.Sleep(100 * time.Millisecond)
		})
	}
	
	scheduler.ScheduleDelayed(func() {
		fmt.Println("   Delayed task executed!")
	}, 500*time.Millisecond)
	
	time.Sleep(1 * time.Second)
	scheduler.Stop()
	
	fmt.Println("\n5. Data Processing:")
	processor := NewDataProcessor()
	
	items := []interface{}{1, 2, 3, "hello", "world", 4.5, 6.7}
	
	start := time.Now()
	results := processor.ProcessItems(items)
	duration := time.Since(start)
	
	fmt.Printf("   Processed %d items in %v\n", len(items), duration)
	fmt.Printf("   Results: %v\n", results)
	
	fmt.Println("\n6. Metrics Collection:")
	metrics := NewMetricsCollector()
	
	metrics.IncrementCounter("requests", 1)
	metrics.IncrementCounter("requests", 5)
	metrics.IncrementCounter("errors", 2)
	
	metrics.SetGauge("cpu_usage", 65.5)
	metrics.SetGauge("memory_usage", 78.2)
	
	fmt.Printf("   Requests: %d\n", metrics.GetCounter("requests"))
	fmt.Printf("   Errors: %d\n", metrics.GetCounter("errors"))
	fmt.Printf("   CPU Usage: %.1f%%\n", metrics.GetGauge("cpu_usage"))
	
	allMetrics := metrics.GetAllMetrics()
	fmt.Printf("   All metrics: %+v\n", allMetrics)
	
	fmt.Println("\n7. String Utilities:")
	stringUtils := &StringUtilities{}
	
	testString := "Hello World! This is a test string with some repeated words. Hello again!"
	
	fmt.Printf("   Original: %s\n", testString)
	fmt.Printf("   Reversed: %s\n", stringUtils.Reverse(testString))
	fmt.Printf("   Is palindrome: %t\n", stringUtils.IsPalindrome("A man a plan a canal Panama"))
	fmt.Printf("   Word count: %v\n", stringUtils.WordCount(testString))
	fmt.Printf("   Most frequent words: %v\n", stringUtils.MostFrequentWords(testString, 3))
	fmt.Printf("   Capitalized: %s\n", stringUtils.Capitalize("hello world from go"))
	fmt.Printf("   Remove duplicates: %s\n", stringUtils.RemoveDuplicates("programming"))
	fmt.Printf("   Compressed: %s\n", stringUtils.CompressString("aabbcccdddd"))
	
	fmt.Println("\n8. Concurrent Processing Demo:")
	start = time.Now()
	
	var wg sync.WaitGroup
	results2 := make([]int, 10)
	
	for i := 0; i < 10; i++ {
		wg.Add(1)
		go func(index int) {
			defer wg.Done()
			time.Sleep(100 * time.Millisecond)
			results2[index] = index * index
		}(i)
	}
	
	wg.Wait()
	duration = time.Since(start)
	
	fmt.Printf("   Concurrent processing completed in %v\n", duration)
	fmt.Printf("   Results: %v\n", results2)
	
	fmt.Printf("\n   Runtime info: GOMAXPROCS=%d, NumGoroutine=%d\n", 
		runtime.GOMAXPROCS(0), runtime.NumGoroutine())
}

func main() {
	fmt.Println("=== Go Advanced Systems Demo ===")
	demonstrateAdvancedFeatures()
}