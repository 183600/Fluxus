#!/bin/bash

# 端到端测试脚本 - 测试Python到C++的完整流程

echo "======================================"
echo "Python到C++端到端测试"
echo "======================================"
echo ""

# 颜色
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

PASSED=0
FAILED=0

# 测试函数
run_test() {
    local name=$1
    local py_code=$2
    local expected=$3
    
    echo "测试: $name"
    
    # 创建Python文件
    echo "$py_code" > test_temp.py
    
    # 运行Python获取期望输出
    py_out=$(python3 test_temp.py 2>&1)
    
    # 编译到C++
    if ! cabal run fluxus -- --python test_temp.py > test_temp.cpp 2>&1; then
        echo -e "${RED}✗ 编译失败${NC}"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    # 检查C++文件
    if [ ! -f test_temp.cpp ]; then
        echo -e "${RED}✗ C++文件未生成${NC}"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    # 编译C++
    if ! g++ -std=c++20 test_temp.cpp -o test_temp_exe 2>&1; then
        echo -e "${RED}✗ C++编译失败${NC}"
        echo "生成的C++代码:"
        cat test_temp.cpp
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    # 运行C++程序
    cpp_out=$(./test_temp_exe 2>&1)
    
    # 比较输出
    if [ "$py_out" = "$cpp_out" ]; then
        echo -e "${GREEN}✓ 通过${NC}"
        PASSED=$((PASSED + 1))
        return 0
    else
        echo -e "${RED}✗ 输出不匹配${NC}"
        echo "Python: $py_out"
        echo "C++:    $cpp_out"
        FAILED=$((FAILED + 1))
        return 1
    fi
}

# 清理函数
cleanup() {
    rm -f test_temp.py test_temp.cpp test_temp_exe
}

# 运行测试
echo "1. 简单打印"
run_test "simple_print" "print(42)" "42"
echo ""

echo "2. 变量"
run_test "variable" "x = 100
print(x)" "100"
echo ""

echo "3. 算术"
run_test "arithmetic" "x = 10
y = 5
z = x + y
print(z)" "15"
echo ""

echo "4. 函数"
run_test "function" "def add(a, b):
    return a + b

print(add(3, 7))" "10"
echo ""

echo "5. 条件"
run_test "condition" "x = 10
if x > 5:
    print(1)
else:
    print(0)" "1"
echo ""

# 清理
cleanup

# 总结
echo ""
echo "======================================"
echo "测试总结"
echo "======================================"
echo -e "${GREEN}通过: $PASSED${NC}"
echo -e "${RED}失败: $FAILED${NC}"

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}所有测试通过！${NC}"
    exit 0
else
    echo -e "${RED}有测试失败${NC}"
    exit 1
fi
