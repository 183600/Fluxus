#\!/bin/bash

echo "=== 修复验证测试套件 ==="

FLUXUS="dist-newstyle/build/x86_64-linux/ghc-9.6.7/fluxus-0.1.0.0/x/fluxus/build/fluxus/fluxus"
PASSED=0
TOTAL=0

test_fix() {
    local test_name="$1"
    local source_file="$2"
    local expected_output="$3"
    
    echo "测试: $test_name"
    ((TOTAL++))
    
    if $FLUXUS "$source_file" -o "${source_file%.*}_compiled" 2>/dev/null; then
        if [ -x "${source_file%.*}_compiled" ]; then
            actual_output=$(./"${source_file%.*}_compiled" 2>&1)
            if [ "$actual_output" = "$expected_output" ]; then
                echo "✅ 修复成功: $test_name"
                ((PASSED++))
            else
                echo "❌ 仍有问题: $test_name"
                echo "期望: '$expected_output'"
                echo "实际: '$actual_output'"
            fi
        else
            echo "❌ 编译成功但不可执行: $test_name"
        fi
    else
        echo "❌ 编译失败: $test_name"
    fi
    echo
}

# 1. 测试Python基础print
cat > test_py_basic_compiled.py << 'PYEOF'
print("Hello Python")
print(42)
PYEOF

test_fix "Python基础print" "test_py_basic_compiled.py" "Hello Python
42"

# 2. 测试Python递归函数
cat > test_py_recursive.py << 'PYEOF'
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

for i in range(5):
    result = fibonacci(i)
    print(f"fib({i}) = {result}")
PYEOF

test_fix "Python递归斐波那契" "test_py_recursive.py" "fib(0) = 0
fib(1) = 1
fib(2) = 1
fib(3) = 2
fib(4) = 3"

# 3. 测试Go基础功能  
cat > test_go_basic_fixed.go << 'GOEOF'
package main

import "fmt"

func main() {
    fmt.Println("Hello Go")
    fmt.Printf("Pi = %.2f\n", 3.14159)
}
GOEOF

test_fix "Go基础print和printf" "test_go_basic_fixed.go" "Hello Go
Pi = 3.14"

# 4. 测试Go函数调用
cat > test_go_func_fixed.go << 'GOEOF'
package main

import "fmt"

func add(a int, b int) int {
    return a + b
}

func main() {
    result := add(5, 3)
    fmt.Printf("5 + 3 = %d\n", result)
}
GOEOF

test_fix "Go函数调用和printf" "test_go_func_fixed.go" "5 + 3 = 8"

# 5. 测试Go条件语句
cat > test_go_conditions_fixed.go << 'GOEOF'
package main

import "fmt"

func main() {
    x := 5
    if x > 0 {
        fmt.Println("positive")
    } else {
        fmt.Println("not positive")
    }
}
GOEOF

test_fix "Go简单条件语句" "test_go_conditions_fixed.go" "positive"

echo "=== 修复效果总结 ==="
echo "测试总数: $TOTAL"
echo "修复成功: $PASSED"
echo "修复率: $(( (PASSED * 100) / TOTAL ))%"

if [ $PASSED -gt 3 ]; then
    echo "🎉 主要功能修复成功\!"
else
    echo "⚠️ 还需要更多修复"
fi
EOF < /dev/null
