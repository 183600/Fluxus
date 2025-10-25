#!/bin/bash

# 全面的Python到C++编译验证脚本
# 确保编译后的C++代码没有语法错误且功能正确

set -e

echo "=========================================="
echo "Python到C++编译全面验证测试"
echo "=========================================="
echo ""

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 统计变量
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
SKIPPED_TESTS=0

# 测试结果数组
declare -a FAILED_TEST_NAMES
declare -a PASSED_TEST_NAMES

# 创建临时测试目录
TEST_DIR="temp_verification_tests"
mkdir -p "$TEST_DIR"

# 测试函数
test_python_to_cpp() {
    local test_name="$1"
    local py_code="$2"
    local expected_output="$3"
    local description="$4"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    echo "=========================================="
    echo -e "${BLUE}测试 #$TOTAL_TESTS: $test_name${NC}"
    echo "描述: $description"
    echo "=========================================="
    
    local py_file="$TEST_DIR/${test_name}.py"
    local cpp_file="$TEST_DIR/${test_name}.cpp"
    local exe_file="$TEST_DIR/${test_name}_exe"
    
    # 创建Python测试文件
    echo "$py_code" > "$py_file"
    echo "✓ 创建Python测试文件: $py_file"
    
    # 步骤1: 验证Python代码可以运行
    echo ""
    echo "步骤1: 验证Python代码可以运行"
    local py_output
    if py_output=$(python3 "$py_file" 2>&1); then
        echo -e "${GREEN}✓ Python代码运行成功${NC}"
        echo "Python输出: $py_output"
    else
        echo -e "${RED}✗ Python代码运行失败${NC}"
        echo "错误: $py_output"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$test_name: Python代码运行失败")
        return 1
    fi
    
    # 步骤2: 编译Python到C++
    echo ""
    echo "步骤2: 编译Python到C++"
    if cabal run fluxus -- --python "$py_file" > "$cpp_file" 2>&1; then
        echo -e "${GREEN}✓ Python到C++编译成功${NC}"
    else
        echo -e "${RED}✗ Python到C++编译失败${NC}"
        cat "$cpp_file"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$test_name: Python到C++编译失败")
        return 1
    fi
    
    # 检查C++文件是否生成
    if [ ! -f "$cpp_file" ] || [ ! -s "$cpp_file" ]; then
        echo -e "${RED}✗ C++文件未生成或为空${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$test_name: C++文件未生成")
        return 1
    fi
    
    echo "生成的C++代码行数: $(wc -l < "$cpp_file")"
    
    # 步骤3: 检查C++语法
    echo ""
    echo "步骤3: 检查C++语法"
    local syntax_check
    if syntax_check=$(g++ -std=c++20 -fsyntax-only "$cpp_file" 2>&1); then
        echo -e "${GREEN}✓ C++语法检查通过${NC}"
    else
        echo -e "${RED}✗ C++语法检查失败${NC}"
        echo "语法错误:"
        echo "$syntax_check"
        echo ""
        echo "生成的C++代码:"
        cat "$cpp_file"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$test_name: C++语法错误")
        return 1
    fi
    
    # 步骤4: 编译C++到可执行文件
    echo ""
    echo "步骤4: 编译C++到可执行文件"
    local compile_output
    if compile_output=$(g++ -std=c++20 -O2 -Wall "$cpp_file" -o "$exe_file" 2>&1); then
        echo -e "${GREEN}✓ C++编译成功${NC}"
    else
        echo -e "${RED}✗ C++编译失败${NC}"
        echo "编译错误:"
        echo "$compile_output"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$test_name: C++编译失败")
        return 1
    fi
    
    # 步骤5: 运行编译后的C++程序
    echo ""
    echo "步骤5: 运行编译后的C++程序"
    local cpp_output
    if cpp_output=$("$exe_file" 2>&1); then
        echo -e "${GREEN}✓ C++程序运行成功${NC}"
        echo "C++输出: $cpp_output"
    else
        echo -e "${RED}✗ C++程序运行失败${NC}"
        echo "运行错误: $cpp_output"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$test_name: C++程序运行失败")
        return 1
    fi
    
    # 步骤6: 比较输出
    echo ""
    echo "步骤6: 比较输出结果"
    
    # 规范化输出（去除首尾空白）
    py_output=$(echo "$py_output" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
    cpp_output=$(echo "$cpp_output" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
    
    if [ "$py_output" = "$cpp_output" ]; then
        echo -e "${GREEN}✓✓✓ 测试通过！输出完全一致${NC}"
        echo "输出: $py_output"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        PASSED_TEST_NAMES+=("$test_name")
        return 0
    else
        echo -e "${RED}✗ 输出不一致${NC}"
        echo "Python输出: [$py_output]"
        echo "C++输出:    [$cpp_output]"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$test_name: 输出不一致")
        return 1
    fi
}

# 构建项目
echo "=========================================="
echo "构建Fluxus编译器..."
echo "=========================================="
if cabal build 2>&1 | tail -10; then
    echo -e "${GREEN}✓ 构建成功${NC}"
else
    echo -e "${RED}✗ 构建失败！${NC}"
    exit 1
fi
echo ""

# ==================== 测试用例 ====================

# 测试1: 最简单的打印
test_python_to_cpp "test_01_simple_print" \
'print(42)' \
'42' \
"最简单的整数打印"

# 测试2: 变量赋值和打印
test_python_to_cpp "test_02_variable" \
'x = 100
print(x)' \
'100' \
"变量赋值和打印"

# 测试3: 简单算术运算
test_python_to_cpp "test_03_arithmetic" \
'a = 10
b = 5
c = a + b
print(c)' \
'15' \
"简单加法运算"

# 测试4: 多个算术运算
test_python_to_cpp "test_04_multi_arithmetic" \
'a = 10
b = 3
print(a + b)
print(a - b)
print(a * b)' \
'13
7
30' \
"多种算术运算"

# 测试5: 条件语句 - if
test_python_to_cpp "test_05_if_statement" \
'x = 10
if x > 5:
    print(1)
else:
    print(0)' \
'1' \
"简单if-else语句"

# 测试6: 条件语句 - else
test_python_to_cpp "test_06_if_else" \
'x = 3
if x > 5:
    print(1)
else:
    print(0)' \
'0' \
"if-else语句（else分支）"

# 测试7: while循环
test_python_to_cpp "test_07_while_loop" \
'i = 0
while i < 3:
    print(i)
    i = i + 1' \
'0
1
2' \
"简单while循环"

# 测试8: for循环
test_python_to_cpp "test_08_for_loop" \
'for i in range(3):
    print(i)' \
'0
1
2' \
"简单for循环"

# 测试9: 函数定义和调用
test_python_to_cpp "test_09_function" \
'def add(a, b):
    return a + b

result = add(5, 3)
print(result)' \
'8' \
"函数定义和调用"

# 测试10: 递归函数 - 阶乘
test_python_to_cpp "test_10_factorial" \
'def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))' \
'120' \
"递归函数计算阶乘"

# 测试11: 递归函数 - 斐波那契
test_python_to_cpp "test_11_fibonacci" \
'def fib(n):
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)

print(fib(6))' \
'8' \
"递归函数计算斐波那契数"

# 测试12: 字符串打印
test_python_to_cpp "test_12_string" \
'print("Hello")' \
'Hello' \
"字符串打印"

# 测试13: 字符串变量
test_python_to_cpp "test_13_string_var" \
's = "World"
print(s)' \
'World' \
"字符串变量"

# 测试14: 多个变量
test_python_to_cpp "test_14_multiple_vars" \
'x = 10
y = 20
z = x + y
print(z)' \
'30' \
"多个变量操作"

# 测试15: 嵌套if语句
test_python_to_cpp "test_15_nested_if" \
'x = 15
if x > 10:
    if x > 20:
        print(2)
    else:
        print(1)
else:
    print(0)' \
'1' \
"嵌套if语句"

# 测试16: 比较运算符
test_python_to_cpp "test_16_comparison" \
'a = 10
b = 5
print(a > b)
print(a < b)
print(a == b)' \
'True
False
False' \
"比较运算符"

# 测试17: 逻辑运算符
test_python_to_cpp "test_17_logical" \
'a = True
b = False
print(a and b)
print(a or b)' \
'False
True' \
"逻辑运算符"

# 测试18: 列表基础
test_python_to_cpp "test_18_list_basic" \
'lst = [1, 2, 3]
print(lst[0])
print(lst[1])' \
'1
2' \
"列表基础操作"

# 测试19: 列表长度
test_python_to_cpp "test_19_list_len" \
'lst = [1, 2, 3, 4, 5]
print(len(lst))' \
'5' \
"列表长度"

# 测试20: 字符串长度
test_python_to_cpp "test_20_string_len" \
's = "Hello"
print(len(s))' \
'5' \
"字符串长度"

# ==================== 测试总结 ====================

echo ""
echo "=========================================="
echo "测试总结"
echo "=========================================="
echo "总测试数: $TOTAL_TESTS"
echo -e "${GREEN}通过: $PASSED_TESTS${NC}"
echo -e "${RED}失败: $FAILED_TESTS${NC}"
echo -e "${YELLOW}跳过: $SKIPPED_TESTS${NC}"
echo ""

if [ $PASSED_TESTS -gt 0 ]; then
    echo -e "${GREEN}通过的测试:${NC}"
    for test_name in "${PASSED_TEST_NAMES[@]}"; do
        echo -e "${GREEN}  ✓ $test_name${NC}"
    done
    echo ""
fi

if [ $FAILED_TESTS -gt 0 ]; then
    echo -e "${RED}失败的测试:${NC}"
    for test_name in "${FAILED_TEST_NAMES[@]}"; do
        echo -e "${RED}  ✗ $test_name${NC}"
    done
    echo ""
fi

# 计算成功率
if [ $TOTAL_TESTS -gt 0 ]; then
    success_rate=$((PASSED_TESTS * 100 / TOTAL_TESTS))
    echo "成功率: ${success_rate}%"
fi

echo ""
if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "${GREEN}=========================================="
    echo "所有测试通过！✓✓✓"
    echo "==========================================${NC}"
    # 清理测试目录
    rm -rf "$TEST_DIR"
    exit 0
else
    echo -e "${RED}=========================================="
    echo "有 $FAILED_TESTS 个测试失败 ✗"
    echo "==========================================${NC}"
    echo "测试文件保留在: $TEST_DIR"
    exit 1
fi
