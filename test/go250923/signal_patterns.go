package main

import (
	"fmt"
	"os"
	"os/signal"
	"syscall"
	"time"
)

// 测试信号处理和进程管理
func testSignalHandling() {
	fmt.Println("=== Testing Signal Handling and Process Management ===")
	
	// 模式1: 基本信号处理
	fmt.Println("\n--- Pattern 1: Basic Signal Handling ---")
	testBasicSignalHandling()
	
	// 模式2: 优雅关闭
	fmt.Println("\n--- Pattern 2: Graceful Shutdown with Signals ---")
	testGracefulShutdownWithSignals()
	
	// 模式3: 环境变量
	fmt.Println("\n--- Pattern 3: Environment Variables ---")
	testEnvironmentVariables()
	
	// 模式4: 命令行参数
	fmt.Println("\n--- Pattern 4: Command Line Arguments ---")
	testCommandLineArguments()
}

func testBasicSignalHandling() {
	// 创建信号通道
	sigChan := make(chan os.Signal, 1)
	
	// 注册要捕获的信号
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
	
	// 启动一个goroutine来处理信号
	go func() {
		fmt.Println("Signal handler started. Waiting for signals...")
		
		select {
		case sig := <-sigChan:
			fmt.Printf("Received signal: %v\n", sig)
			
			// 根据信号类型执行不同的操作
			switch sig {
			case syscall.SIGINT:
				fmt.Println("Handling SIGINT (Ctrl+C)")
			case syscall.SIGTERM:
				fmt.Println("Handling SIGTERM (termination signal)")
			}
			
			// 模拟清理操作
			fmt.Println("Performing cleanup operations...")
			time.Sleep(500 * time.Millisecond)
			fmt.Println("Cleanup completed")
			
		case <-time.After(2 * time.Second):
			fmt.Println("Signal handling timeout - no signals received")
		}
	}()
	
	// 等待信号处理完成
	time.Sleep(3 * time.Second)
	
	// 停止信号通知
	signal.Stop(sigChan)
	close(sigChan)
	
	fmt.Println("Basic signal handling test completed")
}

func testGracefulShutdownWithSignals() {
	fmt.Println("Starting graceful shutdown test...")
	
	// 创建退出通道
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	
	// 创建完成通道
	done := make(chan bool, 1)
	
	// 启动工作goroutine
	go func() {
		fmt.Println("Worker: Starting long-running task...")
		
		ticker := time.NewTicker(500 * time.Millisecond)
		defer ticker.Stop()
		
		workCount := 0
		
		for {
			select {
			case <-ticker.C:
				workCount++
				fmt.Printf("Worker: Processing work item %d\n", workCount)
				
				// 模拟工作
				time.Sleep(200 * time.Millisecond)
				
				if workCount >= 10 {
					fmt.Println("Worker: Maximum work count reached")
					done <- true
					return
				}
				
			case <-quit:
				fmt.Println("Worker: Shutdown signal received")
				fmt.Println("Worker: Finishing current work...")
				
				// 完成当前工作
				if workCount > 0 {
					fmt.Printf("Worker: Completed %d work items before shutdown\n", workCount)
				}
				
				// 执行清理
				fmt.Println("Worker: Performing cleanup...")
				time.Sleep(300 * time.Millisecond)
				fmt.Println("Worker: Cleanup completed")
				
				done <- true
				return
			}
		}
	}()
	
	// 等待工作完成或超时
	select {
	case <-done:
		fmt.Println("Main: Worker completed successfully")
	case <-time.After(5 * time.Second):
		fmt.Println("Main: Timeout reached, forcing shutdown")
	}
	
	signal.Stop(quit)
	close(quit)
	
	fmt.Println("Graceful shutdown test completed")
}

func testEnvironmentVariables() {
	fmt.Println("Testing environment variables...")
	
	// 设置一些环境变量
	os.Setenv("APP_NAME", "SignalHandlingTest")
	os.Setenv("APP_VERSION", "1.0.0")
	os.Setenv("LOG_LEVEL", "info")
	os.Setenv("MAX_WORKERS", "10")
	
	// 读取环境变量
	fmt.Printf("APP_NAME: %s\n", os.Getenv("APP_NAME"))
	fmt.Printf("APP_VERSION: %s\n", os.Getenv("APP_VERSION"))
	fmt.Printf("LOG_LEVEL: %s\n", os.Getenv("LOG_LEVEL"))
	fmt.Printf("MAX_WORKERS: %s\n", os.Getenv("MAX_WORKERS"))
	
	// 获取所有环境变量
	fmt.Println("\nAll environment variables:")
	for _, env := range os.Environ() {
		if len(env) > 20 {
			fmt.Printf("  %s...\n", env[:20])
		} else {
			fmt.Printf("  %s\n", env)
		}
	}
	
	// 检查环境变量是否存在
	if value, exists := os.LookupEnv("NON_EXISTENT_VAR"); exists {
		fmt.Printf("\nNON_EXISTENT_VAR exists: %s\n", value)
	} else {
		fmt.Println("\nNON_EXISTENT_VAR does not exist")
	}
	
	// 清理环境变量
	os.Unsetenv("APP_NAME")
	os.Unsetenv("APP_VERSION")
	os.Unsetenv("LOG_LEVEL")
	os.Unsetenv("MAX_WORKERS")
}

func testCommandLineArguments() {
	fmt.Println("Testing command line arguments...")
	
	// 获取当前程序的命令行参数
	fmt.Printf("Program name: %s\n", os.Args[0])
	fmt.Printf("Number of arguments: %d\n", len(os.Args)-1)
	
	if len(os.Args) > 1 {
		fmt.Println("Arguments:")
		for i, arg := range os.Args[1:] {
			fmt.Printf("  [%d]: %s\n", i+1, arg)
		}
	} else {
		fmt.Println("No command line arguments provided")
		fmt.Println("Example usage: go run signal_patterns.go arg1 arg2 arg3")
	}
	
	// 模拟参数解析
	if len(os.Args) > 1 {
		fmt.Println("\nParsed arguments:")
		
		// 查找特定参数
		verbose := false
		configFile := ""
		
		for i, arg := range os.Args[1:] {
			switch arg {
			case "-v", "--verbose":
				verbose = true
			case "-c", "--config":
				if i+2 < len(os.Args) {
					configFile = os.Args[i+2]
				}
			}
		}
		
		fmt.Printf("  Verbose mode: %t\n", verbose)
		fmt.Printf("  Config file: %s\n", configFile)
	}
}

// 测试配置管理
func testConfigurationManagement() {
	fmt.Println("\n--- Pattern 5: Configuration Management ---")
	
	type Config struct {
		AppName     string
		AppVersion  string
		Debug       bool
		MaxWorkers  int
		Timeout     time.Duration
		AllowedIPs  []string
	}
	
	// 从环境变量加载配置
	loadConfigFromEnv := func() Config {
		config := Config{
			AppName:    getEnvOrDefault("APP_NAME", "DefaultApp"),
			AppVersion: getEnvOrDefault("APP_VERSION", "1.0.0"),
			Debug:      getEnvOrDefault("DEBUG", "false") == "true",
			MaxWorkers: 10, // 默认值
			Timeout:    30 * time.Second,
			AllowedIPs: []string{"127.0.0.1", "::1"},
		}
		
		// 解析数字配置
		if workers := getEnvOrDefault("MAX_WORKERS", ""); workers != "" {
			if w, err := time.ParseDuration(workers); err == nil {
				config.MaxWorkers = int(w.Seconds())
			}
		}
		
		if timeout := getEnvOrDefault("TIMEOUT", ""); timeout != "" {
			if t, err := time.ParseDuration(timeout); err == nil {
				config.Timeout = t
			}
		}
		
		return config
	}
	
	// 设置一些测试环境变量
	os.Setenv("APP_NAME", "ConfigTestApp")
	os.Setenv("APP_VERSION", "2.1.0")
	os.Setenv("DEBUG", "true")
	os.Setenv("MAX_WORKERS", "20")
	os.Setenv("TIMEOUT", "45s")
	
	// 加载配置
	config := loadConfigFromEnv()
	
	fmt.Println("Loaded configuration:")
	fmt.Printf("  App Name: %s\n", config.AppName)
	fmt.Printf("  App Version: %s\n", config.AppVersion)
	fmt.Printf("  Debug Mode: %t\n", config.Debug)
	fmt.Printf("  Max Workers: %d\n", config.MaxWorkers)
	fmt.Printf("  Timeout: %v\n", config.Timeout)
	fmt.Printf("  Allowed IPs: %v\n", config.AllowedIPs)
	
	// 清理环境变量
	os.Unsetenv("APP_NAME")
	os.Unsetenv("APP_VERSION")
	os.Unsetenv("DEBUG")
	os.Unsetenv("MAX_WORKERS")
	os.Unsetenv("TIMEOUT")
}

// 辅助函数：获取环境变量或默认值
func getEnvOrDefault(key, defaultValue string) string {
	if value, exists := os.LookupEnv(key); exists {
		return value
	}
	return defaultValue
}

func main() {
	testSignalHandling()
	testConfigurationManagement()
	fmt.Println("\n=== Signal handling and process management tests completed ===")
}