#!/bin/bash

echo "=== 编程语言编译完整性测试套件 ==="
echo "测试目标：确保 Go 和 Python 代码正常编译和运行"
echo ""

# 测试计数器
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# 测试函数
test_compilation() {
    local lang=$1
    local source_file=$2
    local test_name=$3
    
    echo "[$lang] 测试: $test_name"
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    if [ "$lang" == "go" ]; then
        if go run "$source_file" > /dev/null 2>&1; then
            echo "  ✅ Go 运行成功"
            
            # 测试编译
            exec_name="${source_file%.*}_exec"
            if go build -o "$exec_name" "$source_file" 2>/dev/null; then
                echo "  ✅ Go 编译成功"
                
                # 测试执行
                if ./"$exec_name" > /dev/null 2>&1; then
                    echo "  ✅ 可执行文件运行成功"
                    PASSED_TESTS=$((PASSED_TESTS + 1))
                else
                    echo "  ❌ 可执行文件运行失败"
                    FAILED_TESTS=$((FAILED_TESTS + 1))
                fi
                rm -f "$exec_name"
            else
                echo "  ❌ Go 编译失败"
                FAILED_TESTS=$((FAILED_TESTS + 1))
            fi
        else
            echo "  ❌ Go 运行失败"
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    elif [ "$lang" == "python" ]; then
        if python3 "$source_file" > /dev/null 2>&1; then
            echo "  ✅ Python 运行成功"
            
            # 测试编译为字节码
            if python3 -m py_compile "$source_file" 2>/dev/null; then
                echo "  ✅ Python 字节码编译成功"
                PASSED_TESTS=$((PASSED_TESTS + 1))
            else
                echo "  ❌ Python 字节码编译失败"
                FAILED_TESTS=$((FAILED_TESTS + 1))
            fi
        else
            echo "  ❌ Python 运行失败"
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    fi
    echo ""
}

# 创建综合性测试文件
echo "=== 生成测试文件 ==="

# Go 综合测试
cat > stress_go_comprehensive.go << 'EOF'
package main

import (
    "fmt"
    "time"
    "sync"
)

// 数据结构测试
type Person struct {
    Name string
    Age  int
}

// 接口测试
type Shape interface {
    Area() float64
}

type Rectangle struct {
    Width, Height float64
}

func (r Rectangle) Area() float64 {
    return r.Width * r.Height
}

// 并发测试
func worker(id int, wg *sync.WaitGroup, ch chan string) {
    defer wg.Done()
    ch <- fmt.Sprintf("Worker %d finished", id)
}

// 错误处理测试
func divide(a, b float64) (float64, error) {
    if b == 0 {
        return 0, fmt.Errorf("division by zero")
    }
    return a / b, nil
}

func main() {
    fmt.Println("=== Go 综合功能测试 ===")
    
    // 基本数据类型
    var num int = 42
    var pi float64 = 3.14159
    var name string = "Comprehensive Test"
    var isActive bool = true
    
    fmt.Printf("数字: %d, 浮点: %.2f, 字符串: %s, 布尔: %t\n", num, pi, name, isActive)
    
    // 数组和切片
    numbers := []int{1, 2, 3, 4, 5}
    for i, v := range numbers {
        fmt.Printf("Index %d: Value %d\n", i, v)
    }
    
    // 映射
    ages := map[string]int{
        "Alice":   25,
        "Bob":     30,
        "Charlie": 35,
    }
    
    for name, age := range ages {
        fmt.Printf("%s 的年龄是 %d\n", name, age)
    }
    
    // 结构体
    person := Person{Name: "John", Age: 28}
    fmt.Printf("Person: %+v\n", person)
    
    // 接口
    rect := Rectangle{Width: 10, Height: 5}
    fmt.Printf("Rectangle area: %.2f\n", rect.Area())
    
    // 错误处理
    result, err := divide(10, 2)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
    } else {
        fmt.Printf("Division result: %.2f\n", result)
    }
    
    // 并发测试
    var wg sync.WaitGroup
    ch := make(chan string, 3)
    
    for i := 1; i <= 3; i++ {
        wg.Add(1)
        go worker(i, &wg, ch)
    }
    
    wg.Wait()
    close(ch)
    
    for msg := range ch {
        fmt.Println(msg)
    }
    
    // 时间测试
    fmt.Printf("Current time: %s\n", time.Now().Format("2006-01-02 15:04:05"))
    
    fmt.Println("=== Go 测试完成 ===")
}
EOF

# Python 综合测试
cat > stress_python_comprehensive.py << 'EOF'
#!/usr/bin/env python3
"""Python 综合功能测试"""

import sys
import os
import time
import threading
import json
from collections import defaultdict
from typing import List, Dict, Optional

class Person:
    """测试类定义"""
    def __init__(self, name: str, age: int):
        self.name = name
        self.age = age
    
    def greet(self) -> str:
        return f"Hello, I'm {self.name} and I'm {self.age} years old"
    
    def __str__(self) -> str:
        return f"Person(name='{self.name}', age={self.age})"

def fibonacci(n: int) -> int:
    """递归斐波那契"""
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

def factorial(n: int) -> int:
    """阶乘计算"""
    if n <= 1:
        return 1
    return n * factorial(n - 1)

def worker_function(worker_id: int, results: List[str]) -> None:
    """线程工作函数"""
    time.sleep(0.1)  # 模拟工作
    results.append(f"Worker {worker_id} completed")

def test_data_structures():
    """测试数据结构"""
    print("=== 数据结构测试 ===")
    
    # 列表
    numbers = [1, 2, 3, 4, 5]
    squares = [x**2 for x in numbers]
    print(f"Numbers: {numbers}")
    print(f"Squares: {squares}")
    
    # 字典
    person_ages = {
        "Alice": 25,
        "Bob": 30,
        "Charlie": 35
    }
    
    for name, age in person_ages.items():
        print(f"{name} 的年龄是 {age}")
    
    # 集合
    unique_numbers = {1, 2, 3, 3, 4, 4, 5}
    print(f"Unique numbers: {unique_numbers}")
    
    # 元组
    coordinates = (10, 20)
    x, y = coordinates
    print(f"坐标: x={x}, y={y}")

def test_functions():
    """测试函数功能"""
    print("=== 函数测试 ===")
    
    # 斐波那契
    fib_result = fibonacci(8)
    print(f"fibonacci(8) = {fib_result}")
    
    # 阶乘
    fact_result = factorial(5)
    print(f"factorial(5) = {fact_result}")
    
    # Lambda 函数
    multiply = lambda x, y: x * y
    print(f"Lambda multiplication: {multiply(6, 7)}")

def test_classes():
    """测试类和对象"""
    print("=== 类和对象测试 ===")
    
    person1 = Person("Alice", 25)
    person2 = Person("Bob", 30)
    
    print(person1)
    print(person2)
    print(person1.greet())
    print(person2.greet())

def test_file_operations():
    """测试文件操作"""
    print("=== 文件操作测试 ===")
    
    # 写入文件
    test_file = "test_output.txt"
    try:
        with open(test_file, "w", encoding="utf-8") as f:
            f.write("Python 综合测试\n")
            f.write("文件操作正常\n")
        
        # 读取文件
        with open(test_file, "r", encoding="utf-8") as f:
            content = f.read()
            print("File content:")
            print(content.strip())
        
        # 清理
        os.remove(test_file)
        print("文件操作测试完成")
    except Exception as e:
        print(f"文件操作错误: {e}")

def test_threading():
    """测试多线程"""
    print("=== 多线程测试 ===")
    
    results = []
    threads = []
    
    # 创建线程
    for i in range(3):
        thread = threading.Thread(target=worker_function, args=(i, results))
        threads.append(thread)
        thread.start()
    
    # 等待完成
    for thread in threads:
        thread.join()
    
    for result in results:
        print(result)

def test_json_operations():
    """测试 JSON 操作"""
    print("=== JSON 操作测试 ===")
    
    data = {
        "name": "测试数据",
        "version": "1.0",
        "features": ["parsing", "serialization", "validation"],
        "config": {
            "debug": True,
            "max_connections": 100
        }
    }
    
    # 序列化
    json_str = json.dumps(data, ensure_ascii=False, indent=2)
    print("JSON 序列化:")
    print(json_str)
    
    # 反序列化
    parsed_data = json.loads(json_str)
    print(f"反序列化后的名称: {parsed_data['name']}")

def main():
    """主函数"""
    print("=== Python 综合功能测试开始 ===")
    print(f"Python 版本: {sys.version}")
    print(f"当前时间: {time.strftime('%Y-%m-%d %H:%M:%S')}")
    print("")
    
    try:
        test_data_structures()
        print("")
        
        test_functions()
        print("")
        
        test_classes()
        print("")
        
        test_file_operations()
        print("")
        
        test_threading()
        print("")
        
        test_json_operations()
        print("")
        
        print("=== Python 综合功能测试完成 ===")
        
    except Exception as e:
        print(f"测试过程中出现错误: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
EOF

# 运行测试
echo "=== 开始执行测试 ==="
echo ""

test_compilation "go" "stress_go_comprehensive.go" "Go 综合功能测试"
test_compilation "python" "stress_python_comprehensive.py" "Python 综合功能测试"

# 测试现有示例
if [ -f "examples/go/fibonacci.go" ]; then
    test_compilation "go" "examples/go/fibonacci.go" "Go 斐波那契示例"
fi

if [ -f "examples/python/fibonacci.py" ]; then
    test_compilation "python" "examples/python/fibonacci.py" "Python 斐波那契示例"
fi

# 清理测试文件
echo "=== 清理测试文件 ==="
rm -f stress_go_comprehensive.go stress_python_comprehensive.py
rm -f *.pyc __pycache__/*
echo ""

# 报告结果
echo "=== 测试结果总结 ==="
echo "总测试数: $TOTAL_TESTS"
echo "通过测试: $PASSED_TESTS"
echo "失败测试: $FAILED_TESTS"

if [ $TOTAL_TESTS -gt 0 ]; then
    success_rate=$((PASSED_TESTS * 100 / TOTAL_TESTS))
    echo "成功率: ${success_rate}%"
    
    if [ $FAILED_TESTS -eq 0 ]; then
        echo ""
        echo "🎉 所有测试都通过了！编译系统运行正常。"
        exit 0
    else
        echo ""
        echo "⚠️  有 $FAILED_TESTS 个测试失败，需要检查编译系统。"
        exit 1
    fi
else
    echo "❌ 没有执行任何测试"
    exit 1
fi