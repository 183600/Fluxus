package main

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"time"
)

// File system operations
type FileManager struct {
	rootPath string
}

func NewFileManager(rootPath string) *FileManager {
	return &FileManager{rootPath: rootPath}
}

func (fm *FileManager) ListFiles(dir string) ([]string, error) {
	files, err := ioutil.ReadDir(filepath.Join(fm.rootPath, dir))
	if err != nil {
		return nil, err
	}
	
	var fileNames []string
	for _, file := range files {
		fileNames = append(fileNames, file.Name())
	}
	return fileNames, nil
}

func (fm *FileManager) ReadFile(filename string) (string, error) {
	content, err := ioutil.ReadFile(filepath.Join(fm.rootPath, filename))
	if err != nil {
		return "", err
	}
	return string(content), nil
}

func (fm *FileManager) WriteFile(filename, content string) error {
	return ioutil.WriteFile(filepath.Join(fm.rootPath, filename), []byte(content), 0644)
}

func (fm *FileManager) GetFileStats(filename string) (os.FileInfo, error) {
	return os.Stat(filepath.Join(fm.rootPath, filename))
}

// Log analyzer
type LogEntry struct {
	Timestamp time.Time
	Level     string
	Message   string
	IP        string
}

func parseLogLine(line string) (*LogEntry, error) {
	parts := strings.Split(line, " - ")
	if len(parts) < 4 {
		return nil, fmt.Errorf("invalid log format")
	}
	
	timestamp, err := time.Parse("2006-01-02 15:04:05", parts[0])
	if err != nil {
		return nil, err
	}
	
	return &LogEntry{
		Timestamp: timestamp,
		Level:     parts[1],
		Message:   parts[2],
		IP:        parts[3],
	}, nil
}

func analyzeLogFile(filename string) (map[string]int, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()
	
	stats := make(map[string]int)
	scanner := bufio.NewScanner(file)
	
	for scanner.Scan() {
		entry, err := parseLogLine(scanner.Text())
		if err != nil {
			continue
		}
		stats[entry.Level]++
	}
	
	return stats, scanner.Err()
}

// Simple web server
type WebServer struct {
	port     int
	handlers map[string]http.HandlerFunc
}

func NewWebServer(port int) *WebServer {
	return &WebServer{
		port:     port,
		handlers: make(map[string]http.HandlerFunc),
	}
}

func (ws *WebServer) AddHandler(path string, handler http.HandlerFunc) {
	ws.handlers[path] = handler
}

func (ws *WebServer) Start() error {
	mux := http.NewServeMux()
	
	for path, handler := range ws.handlers {
		mux.HandleFunc(path, handler)
	}
	
	server := &http.Server{
		Addr:    fmt.Sprintf(":%d", ws.port),
		Handler: mux,
	}
	
	fmt.Printf("Starting web server on port %d\n", ws.port)
	return server.ListenAndServe()
}

// TCP server
type TCPServer struct {
	port    int
	clients map[net.Conn]bool
	mutex   sync.Mutex
}

func NewTCPServer(port int) *TCPServer {
	return &TCPServer{
		port:    port,
		clients: make(map[net.Conn]bool),
	}
}

func (ts *TCPServer) handleConnection(conn net.Conn) {
	defer conn.Close()
	
	ts.mutex.Lock()
	ts.clients[conn] = true
	ts.mutex.Unlock()
	
	reader := bufio.NewReader(conn)
	for {
		message, err := reader.ReadString('\n')
		if err != nil {
			break
		}
		
		response := fmt.Sprintf("Echo: %s", message)
		conn.Write([]byte(response))
	}
	
	ts.mutex.Lock()
	delete(ts.clients, conn)
	ts.mutex.Unlock()
}

func (ts *TCPServer) Start() error {
	listener, err := net.Listen("tcp", fmt.Sprintf(":%d", ts.port))
	if err != nil {
		return err
	}
	defer listener.Close()
	
	fmt.Printf("TCP server listening on port %d\n", ts.port)
	
	for {
		conn, err := listener.Accept()
		if err != nil {
			continue
		}
		
		go ts.handleConnection(conn)
	}
}

// Configuration manager
type Config struct {
	settings map[string]string
	mutex    sync.RWMutex
}

func NewConfig() *Config {
	return &Config{
		settings: make(map[string]string),
	}
}

func (c *Config) Set(key, value string) {
	c.mutex.Lock()
	defer c.mutex.Unlock()
	c.settings[key] = value
}

func (c *Config) Get(key string) (string, bool) {
	c.mutex.RLock()
	defer c.mutex.RUnlock()
	value, exists := c.settings[key]
	return value, exists
}

func (c *Config) GetInt(key string) (int, error) {
	c.mutex.RLock()
	defer c.mutex.RUnlock()
	
	value, exists := c.settings[key]
	if !exists {
		return 0, fmt.Errorf("key not found: %s", key)
	}
	
	return strconv.Atoi(value)
}

func (c *Config) LoadFromFile(filename string) error {
	file, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer file.Close()
	
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		
		parts := strings.SplitN(line, "=", 2)
		if len(parts) != 2 {
			continue
		}
		
		key := strings.TrimSpace(parts[0])
		value := strings.TrimSpace(parts[1])
		c.Set(key, value)
	}
	
	return scanner.Err()
}

// Cache implementation
type Cache struct {
	data   map[string]CacheItem
	mutex  sync.RWMutex
	maxTTL time.Duration
}

type CacheItem struct {
	Value     interface{}
	ExpiresAt time.Time
}

func NewCache(maxTTL time.Duration) *Cache {
	cache := &Cache{
		data:   make(map[string]CacheItem),
		maxTTL: maxTTL,
	}
	
	go cache.cleanup()
	return cache
}

func (c *Cache) Set(key string, value interface{}) {
	c.mutex.Lock()
	defer c.mutex.Unlock()
	
	c.data[key] = CacheItem{
		Value:     value,
		ExpiresAt: time.Now().Add(c.maxTTL),
	}
}

func (c *Cache) Get(key string) (interface{}, bool) {
	c.mutex.RLock()
	defer c.mutex.RUnlock()
	
	item, exists := c.data[key]
	if !exists || time.Now().After(item.ExpiresAt) {
		return nil, false
	}
	
	return item.Value, true
}

func (c *Cache) cleanup() {
	ticker := time.NewTicker(time.Minute)
	defer ticker.Stop()
	
	for range ticker.C {
		c.mutex.Lock()
		now := time.Now()
		for key, item := range c.data {
			if now.After(item.ExpiresAt) {
				delete(c.data, key)
			}
		}
		c.mutex.Unlock()
	}
}

// Process monitor
type ProcessInfo struct {
	PID    int
	Name   string
	Status string
	Memory int64
}

func getProcessList() ([]ProcessInfo, error) {
	processes := []ProcessInfo{
		{PID: 1234, Name: "demo-process-1", Status: "running", Memory: 1024000},
		{PID: 5678, Name: "demo-process-2", Status: "sleeping", Memory: 2048000},
		{PID: 9012, Name: "demo-process-3", Status: "running", Memory: 512000},
	}
	return processes, nil
}

func monitorProcesses() {
	ticker := time.NewTicker(5 * time.Second)
	defer ticker.Stop()
	
	for range ticker.C {
		processes, err := getProcessList()
		if err != nil {
			fmt.Printf("Error getting process list: %v\n", err)
			continue
		}
		
		fmt.Println("\n--- Process Monitor ---")
		fmt.Printf("%-6s %-20s %-10s %-10s\n", "PID", "Name", "Status", "Memory")
		for _, proc := range processes {
			fmt.Printf("%-6d %-20s %-10s %-10d\n", proc.PID, proc.Name, proc.Status, proc.Memory)
		}
	}
}

func main() {
	fmt.Println("Comprehensive Systems Demo")
	fmt.Println("==========================")
	
	// File manager demo
	fmt.Println("\n--- File Manager Demo ---")
	fm := NewFileManager(".")
	
	// Create a test file
	testContent := "This is a test file created by the systems demo."
	err := fm.WriteFile("test_file.txt", testContent)
	if err != nil {
		fmt.Printf("Error creating file: %v\n", err)
	} else {
		fmt.Println("Test file created successfully")
	}
	
	// Read the file back
	content, err := fm.ReadFile("test_file.txt")
	if err != nil {
		fmt.Printf("Error reading file: %v\n", err)
	} else {
		fmt.Printf("File content: %s\n", content)
	}
	
	// Get file stats
	stats, err := fm.GetFileStats("test_file.txt")
	if err != nil {
		fmt.Printf("Error getting file stats: %v\n", err)
	} else {
		fmt.Printf("File size: %d bytes, Modified: %v\n", stats.Size(), stats.ModTime())
	}
	
	// Configuration demo
	fmt.Println("\n--- Configuration Manager Demo ---")
	config := NewConfig()
	config.Set("server_port", "8080")
	config.Set("max_connections", "100")
	config.Set("debug_mode", "true")
	
	if port, exists := config.Get("server_port"); exists {
		fmt.Printf("Server port: %s\n", port)
	}
	
	if maxConn, err := config.GetInt("max_connections"); err == nil {
		fmt.Printf("Max connections: %d\n", maxConn)
	}
	
	// Cache demo
	fmt.Println("\n--- Cache Demo ---")
	cache := NewCache(30 * time.Second)
	
	cache.Set("user_1", "John Doe")
	cache.Set("user_2", "Jane Smith")
	cache.Set("config_value", 42)
	
	if value, exists := cache.Get("user_1"); exists {
		fmt.Printf("Cached user: %v\n", value)
	}
	
	if value, exists := cache.Get("config_value"); exists {
		fmt.Printf("Cached config: %v\n", value)
	}
	
	// Create sample log entries
	fmt.Println("\n--- Log Analysis Demo ---")
	logContent := `2024-01-15 10:30:00 - INFO - Application started - 192.168.1.100
2024-01-15 10:30:15 - DEBUG - Database connection established - 192.168.1.100
2024-01-15 10:31:00 - ERROR - Failed to process request - 192.168.1.50
2024-01-15 10:31:30 - INFO - User login successful - 192.168.1.75
2024-01-15 10:32:00 - WARN - High memory usage detected - 192.168.1.100`
	
	err = ioutil.WriteFile("sample.log", []byte(logContent), 0644)
	if err != nil {
		fmt.Printf("Error creating log file: %v\n", err)
	} else {
		stats, err := analyzeLogFile("sample.log")
		if err != nil {
			fmt.Printf("Error analyzing log: %v\n", err)
		} else {
			fmt.Println("Log analysis results:")
			for level, count := range stats {
				fmt.Printf("  %s: %d entries\n", level, count)
			}
		}
	}
	
	// Web server demo (commented out to avoid blocking)
	fmt.Println("\n--- Web Server Demo (Setup) ---")
	webServer := NewWebServer(8080)
	
	webServer.AddHandler("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hello from Go Systems Demo!")
	})
	
	webServer.AddHandler("/status", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Server is running fine!")
	})
	
	fmt.Println("Web server configured (not started to avoid blocking)")
	
	// Process monitor demo (run for a short time)
	fmt.Println("\n--- Process Monitor Demo ---")
	go func() {
		time.Sleep(3 * time.Second)
		processes, _ := getProcessList()
		fmt.Println("Sample process list:")
		fmt.Printf("%-6s %-20s %-10s %-10s\n", "PID", "Name", "Status", "Memory")
		for _, proc := range processes {
			fmt.Printf("%-6d %-20s %-10s %-10d\n", proc.PID, proc.Name, proc.Status, proc.Memory)
		}
	}()
	
	// Clean up
	time.Sleep(4 * time.Second)
	os.Remove("test_file.txt")
	os.Remove("sample.log")
	
	fmt.Println("\nSystems demo completed successfully!")
}