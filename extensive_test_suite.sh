#\!/bin/bash

# 完整的Fluxus编译器测试套件
echo "=== Fluxus编译器全面测试套件 ==="

FLUXUS="dist-newstyle/build/x86_64-linux/ghc-9.6.7/fluxus-0.1.0.0/x/fluxus/build/fluxus/fluxus"
PASSED=0
FAILED=0
TOTAL=0

# 测试结果数组
declare -a FAILED_TESTS=()

test_compilation() {
    local test_name="$1"
    local source_file="$2"
    local expected_output="$3"
    local should_compile="$4" # true/false
    
    echo "测试: $test_name"
    ((TOTAL++))
    
    # 清理之前的文件
    rm -f "${source_file%.*}_compiled" "${source_file%.*}.cpp"
    
    # 编译
    if $FLUXUS "$source_file" -o "${source_file%.*}_compiled" 2>/dev/null; then
        if [ "$should_compile" = "false" ]; then
            echo "❌ 失败: $test_name (应该编译失败但成功了)"
            FAILED_TESTS+=("$test_name")
            ((FAILED++))
            return
        fi
        
        # 运行并检查输出
        if [ -x "${source_file%.*}_compiled" ]; then
            actual_output=$(./"${source_file%.*}_compiled" 2>&1)
            if [ "$actual_output" = "$expected_output" ]; then
                echo "✅ 通过: $test_name"
                ((PASSED++))
            else
                echo "❌ 失败: $test_name (输出错误)"
                echo "期望: '$expected_output'"
                echo "实际: '$actual_output'"
                FAILED_TESTS+=("$test_name")
                ((FAILED++))
            fi
        else
            echo "❌ 失败: $test_name (不可执行)"
            FAILED_TESTS+=("$test_name")
            ((FAILED++))
        fi
    else
        if [ "$should_compile" = "true" ]; then
            echo "❌ 失败: $test_name (编译失败)"
            FAILED_TESTS+=("$test_name")
            ((FAILED++))
        else
            echo "✅ 通过: $test_name (正确地编译失败)"
            ((PASSED++))
        fi
    fi
    echo
}

# 构建编译器
echo "构建Fluxus编译器..."
cabal build -v0
echo

echo "开始运行测试..."
echo "================================================="

# === Python基础功能测试 ===
echo "📝 Python基础功能测试"

# 1. 基本输出
cat > test_py_print.py << 'PYEOF'
print("Hello Python")
print(42)
print(3.14)
PYEOF

test_compilation "Python基本print" "test_py_print.py" "Hello Python
42
3.14" "true"

# 2. 变量和运算
cat > test_py_variables.py << 'PYEOF'
x = 10
y = 20
z = x + y
print(f"x = {x}")
print(f"y = {y}")
print(f"x + y = {z}")
PYEOF

test_compilation "Python变量和运算" "test_py_variables.py" "x = 10
y = 20
x + y = 30" "true"

# 3. 函数定义和调用
cat > test_py_functions.py << 'PYEOF'
def add(a, b):
    return a + b

def multiply(x, y):
    return x * y

result1 = add(5, 3)
result2 = multiply(4, 6)
print(f"add(5, 3) = {result1}")
print(f"multiply(4, 6) = {result2}")
PYEOF

test_compilation "Python函数" "test_py_functions.py" "add(5, 3) = 8
multiply(4, 6) = 24" "true"

# 4. 条件语句
cat > test_py_conditions.py << 'PYEOF'
def check_number(n):
    if n > 0:
        return "positive"
    elif n < 0:
        return "negative"
    else:
        return "zero"

print(check_number(5))
print(check_number(-3))
print(check_number(0))
PYEOF

test_compilation "Python条件语句" "test_py_conditions.py" "positive
negative
zero" "true"

# 5. 循环结构
cat > test_py_loops.py << 'PYEOF'
# for循环
for i in range(3):
    print(f"i = {i}")

# 嵌套循环
total = 0
for i in range(3):
    for j in range(2):
        total += 1
print(f"total = {total}")
PYEOF

test_compilation "Python循环" "test_py_loops.py" "i = 0
i = 1
i = 2
total = 6" "true"

# === Go基础功能测试 ===
echo "📝 Go基础功能测试"

# 6. 基本输出
cat > test_go_print.go << 'GOEOF'
package main

import "fmt"

func main() {
    fmt.Println("Hello Go")
    fmt.Println(42)
    fmt.Printf("Pi = %.2f\n", 3.14159)
}
GOEOF

test_compilation "Go基本print" "test_go_print.go" "Hello Go
42
Pi = 3.14" "true"

# 7. 变量和运算
cat > test_go_variables.go << 'GOEOF'
package main

import "fmt"

func main() {
    x := 10
    y := 20
    z := x + y
    fmt.Printf("x = %d\n", x)
    fmt.Printf("y = %d\n", y)
    fmt.Printf("x + y = %d\n", z)
}
GOEOF

test_compilation "Go变量和运算" "test_go_variables.go" "x = 10
y = 20
x + y = 30" "true"

# 8. 函数定义和调用
cat > test_go_functions.go << 'GOEOF'
package main

import "fmt"

func add(a int, b int) int {
    return a + b
}

func multiply(x int, y int) int {
    return x * y
}

func main() {
    result1 := add(5, 3)
    result2 := multiply(4, 6)
    fmt.Printf("add(5, 3) = %d\n", result1)
    fmt.Printf("multiply(4, 6) = %d\n", result2)
}
GOEOF

test_compilation "Go函数" "test_go_functions.go" "add(5, 3) = 8
multiply(4, 6) = 24" "true"

# 9. 条件语句
cat > test_go_conditions.go << 'GOEOF'
package main

import "fmt"

func checkNumber(n int) string {
    if n > 0 {
        return "positive"
    } else if n < 0 {
        return "negative"
    } else {
        return "zero"
    }
}

func main() {
    fmt.Println(checkNumber(5))
    fmt.Println(checkNumber(-3))
    fmt.Println(checkNumber(0))
}
GOEOF

test_compilation "Go条件语句" "test_go_conditions.go" "positive
negative
zero" "true"

# === 复杂算法测试 ===
echo "📝 复杂算法测试"

# 10. 递归斐波那契数列
cat > test_py_fibonacci.py << 'PYEOF'
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

for i in range(6):
    result = fibonacci(i)
    print(f"fib({i}) = {result}")
PYEOF

test_compilation "Python递归斐波那契" "test_py_fibonacci.py" "fib(0) = 0
fib(1) = 1
fib(2) = 1
fib(3) = 2
fib(4) = 3
fib(5) = 5" "true"

# 11. Go递归阶乘
cat > test_go_factorial.go << 'GOEOF'
package main

import "fmt"

func factorial(n int) int {
    if n <= 1 {
        return 1
    }
    return n * factorial(n-1)
}

func main() {
    for i := 1; i <= 5; i++ {
        result := factorial(i)
        fmt.Printf("factorial(%d) = %d\n", i, result)
    }
}
GOEOF

test_compilation "Go递归阶乘" "test_go_factorial.go" "factorial(1) = 1
factorial(2) = 2
factorial(3) = 6
factorial(4) = 24
factorial(5) = 120" "true"

# === 数据结构测试 ===
echo "📝 数据结构测试"

# 12. Python列表操作
cat > test_py_lists.py << 'PYEOF'
numbers = [1, 2, 3, 4, 5]
total = 0
for num in numbers:
    total += num
print(f"sum = {total}")

# 列表推导
squares = [x*x for x in range(4)]
for i, sq in enumerate(squares):
    print(f"squares[{i}] = {sq}")
PYEOF

test_compilation "Python列表操作" "test_py_lists.py" "sum = 15
squares[0] = 0
squares[1] = 1
squares[2] = 4
squares[3] = 9" "true"

# === 错误处理测试 ===
echo "📝 错误处理测试"

# 13. 语法错误应该编译失败
cat > test_syntax_error.py << 'PYEOF'
def broken_function(
    print("This should fail")
PYEOF

test_compilation "Python语法错误" "test_syntax_error.py" "" "false"

# === 性能和复杂度测试 ===
echo "📝 性能测试"

# 14. 复杂计算
cat > test_performance.py << 'PYEOF'
def is_prime(n):
    if n < 2:
        return False
    for i in range(2, int(n**0.5) + 1):
        if n % i == 0:
            return False
    return True

count = 0
for i in range(2, 20):
    if is_prime(i):
        count += 1
        print(f"{i} is prime")

print(f"Found {count} primes")
PYEOF

test_compilation "Python质数计算" "test_performance.py" "2 is prime
3 is prime
5 is prime
7 is prime
11 is prime
13 is prime
17 is prime
19 is prime
Found 8 primes" "true"

# === 总结 ===
echo "================================================="
echo "=== 测试结果总结 ==="
echo "总测试数: $TOTAL"
echo "通过: $PASSED"
echo "失败: $FAILED"
echo "通过率: $(( (PASSED * 100) / TOTAL ))%"

if [ $FAILED -gt 0 ]; then
    echo ""
    echo "❌ 失败的测试:"
    for test in "${FAILED_TESTS[@]}"; do
        echo "  - $test"
    done
    echo ""
    echo "需要修复这些问题\!"
    exit 1
else
    echo ""
    echo "🎉 所有测试都通过了\!"
    exit 0
fi
EOF < /dev/null
