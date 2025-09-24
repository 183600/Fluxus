package main

import (
	"fmt"
	"log"
	"net/http"
	"time"
)

// 测试HTTP服务器模式
func testHTTPServerPatterns() {
	fmt.Println("=== Testing HTTP Server Patterns ===")
	
	// 模式1: 基本HTTP服务器
	fmt.Println("\n--- Pattern 1: Basic HTTP Server ---")
	testBasicHTTPServer()
	
	// 模式2: 中间件模式
	fmt.Println("\n--- Pattern 2: Middleware Pattern ---")
	testMiddlewarePattern()
	
	// 模式3: 路由处理
	fmt.Println("\n--- Pattern 3: Route Handling ---")
	testRouteHandling()
	
	// 模式4: 优雅关闭
	fmt.Println("\n--- Pattern 4: Graceful Shutdown ---")
	testGracefulShutdown()
}

func testBasicHTTPServer() {
	// 简单的处理器函数
	helloHandler := func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hello, World! Request path: %s\n", r.URL.Path)
	}
	
	// 注册处理器
	http.HandleFunc("/hello", helloHandler)
	
	// 启动服务器（在后台goroutine中）
	server := &http.Server{
		Addr:    ":8082",
		Handler: nil, // 使用默认的ServeMux
	}
	
	go func() {
		fmt.Println("Starting HTTP server on :8082")
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Printf("Server error: %v", err)
		}
	}()
	
	// 等待服务器启动
	time.Sleep(100 * time.Millisecond)
	
	// 测试请求
	resp, err := http.Get("http://localhost:8082/hello")
	if err != nil {
		fmt.Printf("Request failed: %v\n", err)
		return
	}
	defer resp.Body.Close()
	
	fmt.Printf("Response status: %s\n", resp.Status)
	
	// 关闭服务器
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	server.Shutdown(ctx)
}

// 日志中间件
func loggingMiddleware(next http.HandlerFunc) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		start := time.Now()
		
		// 创建自定义ResponseWriter来捕获状态码
		wrapper := &responseWrapper{ResponseWriter: w, statusCode: http.StatusOK}
		
		next(wrapper, r)
		
		duration := time.Since(start)
		fmt.Printf("[%s] %s %s - %d (%v)\n", 
			time.Now().Format("15:04:05"), 
			r.Method, 
			r.URL.Path, 
			wrapper.statusCode, 
			duration)
	}
}

// 自定义ResponseWriter包装器
type responseWrapper struct {
	http.ResponseWriter
	statusCode int
}

func (rw *responseWrapper) WriteHeader(code int) {
	rw.statusCode = code
	rw.ResponseWriter.WriteHeader(code)
}

// 认证中间件
func authMiddleware(next http.HandlerFunc) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		// 简单的认证检查
		token := r.Header.Get("Authorization")
		if token == "" {
			http.Error(w, "Authorization header required", http.StatusUnauthorized)
			return
		}
		
		// 这里应该有实际的token验证逻辑
		if token != "Bearer valid-token" {
			http.Error(w, "Invalid token", http.StatusUnauthorized)
			return
		}
		
		next(w, r)
	}
}

func testMiddlewarePattern() {
	// 创建带有中间件的处理器
	protectedHandler := loggingMiddleware(authMiddleware(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Protected resource accessed successfully!\n")
	}))
	
	// 创建新的ServeMux
	mux := http.NewServeMux()
	mux.HandleFunc("/protected", protectedHandler)
	
	server := &http.Server{
		Addr:    ":8083",
		Handler: mux,
	}
	
	go func() {
		fmt.Println("Starting middleware server on :8083")
		server.ListenAndServe()
	}()
	
	time.Sleep(100 * time.Millisecond)
	
	// 测试无token的请求
	fmt.Println("Testing without token:")
	resp, _ := http.Get("http://localhost:8083/protected")
	fmt.Printf("Response status: %s\n", resp.Status)
	
	// 测试有token的请求
	fmt.Println("Testing with valid token:")
	req, _ := http.NewRequest("GET", "http://localhost:8083/protected", nil)
	req.Header.Set("Authorization", "Bearer valid-token")
	
	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		fmt.Printf("Request failed: %v\n", err)
	} else {
		fmt.Printf("Response status: %s\n", resp.Status)
		resp.Body.Close()
	}
	
	server.Shutdown(context.Background())
}

func testRouteHandling() {
	mux := http.NewServeMux()
	
	// 静态路由
	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Home page\n")
	})
	
	mux.HandleFunc("/about", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "About page\n")
	})
	
	// 参数化路由（简单实现）
	mux.HandleFunc("/users/", func(w http.ResponseWriter, r *http.Request) {
		// 从URL路径中提取用户ID
		path := r.URL.Path
		if len(path) > len("/users/") {
			userID := path[len("/users/"):]
			fmt.Fprintf(w, "User profile: ID=%s\n", userID)
		} else {
			fmt.Fprintf(w, "Users list\n")
		}
	})
	
	// RESTful路由
	mux.HandleFunc("/api/users", func(w http.ResponseWriter, r *http.Request) {
		switch r.Method {
		case "GET":
			fmt.Fprintf(w, "List users\n")
		case "POST":
			fmt.Fprintf(w, "Create user\n")
		default:
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		}
	})
	
	server := &http.Server{
		Addr:    ":8084",
		Handler: mux,
	}
	
	go func() {
		fmt.Println("Starting route server on :8084")
		server.ListenAndServe()
	}()
	
	time.Sleep(100 * time.Millisecond)
	
	// 测试不同路由
	routes := []string{
		"/",
		"/about",
		"/users/123",
		"/api/users",
	}
	
	for _, route := range routes {
		fmt.Printf("Testing route %s:\n", route)
		resp, err := http.Get("http://localhost:8084" + route)
		if err != nil {
			fmt.Printf("  Error: %v\n", err)
			continue
		}
		fmt.Printf("  Status: %s\n", resp.Status)
		resp.Body.Close()
	}
	
	server.Shutdown(context.Background())
}

func testGracefulShutdown() {
	fmt.Println("Testing graceful shutdown...")
	
	server := &http.Server{
		Addr: ":8085",
		Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// 模拟长时间运行的请求
			if r.URL.Path == "/slow" {
				time.Sleep(2 * time.Second)
				fmt.Fprintf(w, "Slow response completed\n")
			} else {
				fmt.Fprintf(w, "Normal response\n")
			}
		}),
	}
	
	// 启动服务器
	go func() {
		fmt.Println("Starting graceful shutdown server on :8085")
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Printf("Server error: %v", err)
		}
	}()
	
	time.Sleep(100 * time.Millisecond)
	
	// 启动一个慢请求
	go func() {
		fmt.Println("Starting slow request...")
		start := time.Now()
		resp, err := http.Get("http://localhost:8085/slow")
		if err != nil {
			fmt.Printf("Slow request failed: %v\n", err)
			return
		}
		defer resp.Body.Close()
		duration := time.Since(start)
		fmt.Printf("Slow request completed in %v, status: %s\n", duration, resp.Status)
	}()
	
	// 等待一下让慢请求开始
	time.Sleep(500 * time.Millisecond)
	
	// 优雅关闭
	fmt.Println("Initiating graceful shutdown...")
	startShutdown := time.Now()
	
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	
	if err := server.Shutdown(ctx); err != nil {
		fmt.Printf("Graceful shutdown failed: %v\n", err)
	} else {
		shutdownDuration := time.Since(startShutdown)
		fmt.Printf("Graceful shutdown completed in %v\n", shutdownDuration)
	}
}

// 引入context包
import "context"

func main() {
	testHTTPServerPatterns()
	fmt.Println("\n=== HTTP Server patterns tests completed ===")
}