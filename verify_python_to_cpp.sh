#!/bin/bash

# Python到C++编译验证脚本
# 确保编译后的C++代码没有语法错误且功能正确

set -e

echo "=========================================="
echo "Python到C++编译验证测试"
echo "=========================================="
echo ""

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# 统计变量
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# 测试结果数组
declare -a FAILED_TEST_NAMES

# 测试函数
test_python_file() {
    local py_file=$1
    local test_name=$(basename "$py_file" .py)
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    echo "----------------------------------------"
    echo "测试 #$TOTAL_TESTS: $test_name"
    echo "源文件: $py_file"
    
    # 检查Python文件是否存在
    if [ ! -f "$py_file" ]; then
        echo -e "${RED}✗ Python文件不存在${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$test_name: Python文件不存在")
        return 1
    fi
    
    # 生成C++文件名
    local cpp_file="${test_name}.cpp"
    local exe_file="${test_name}_test"
    
    # 步骤1: 编译Python到C++
    echo "步骤1: 编译 $py_file 到 C++"
    if ! cabal run fluxus -- --python "$py_file" > "$cpp_file" 2>&1; then
        echo -e "${RED}✗ Python到C++编译失败${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$test_name: Python到C++编译失败")
        return 1
    fi
    
    # 检查C++文件是否生成
    if [ ! -f "$cpp_file" ]; then
        echo -e "${RED}✗ C++文件未生成${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$test_name: C++文件未生成")
        return 1
    fi
    
    echo -e "${GREEN}✓ C++代码生成成功${NC}"
    
    # 步骤2: 检查C++语法
    echo "步骤2: 检查C++语法"
    if ! g++ -std=c++20 -fsyntax-only "$cpp_file" 2>&1; then
        echo -e "${RED}✗ C++语法检查失败${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$test_name: C++语法错误")
        return 1
    fi
    
    echo -e "${GREEN}✓ C++语法正确${NC}"
    
    # 步骤3: 编译C++到可执行文件
    echo "步骤3: 编译C++到可执行文件"
    if ! g++ -std=c++20 -O2 "$cpp_file" -o "$exe_file" 2>&1; then
        echo -e "${RED}✗ C++编译失败${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$test_name: C++编译失败")
        return 1
    fi
    
    echo -e "${GREEN}✓ C++编译成功${NC}"
    
    # 步骤4: 运行Python原始代码
    echo "步骤4: 运行Python原始代码"
    local py_output=$(python3 "$py_file" 2>&1 || echo "PYTHON_ERROR")
    echo "Python输出: $py_output"
    
    # 步骤5: 运行编译后的C++程序
    echo "步骤5: 运行编译后的C++程序"
    local cpp_output=$(./"$exe_file" 2>&1 || echo "CPP_ERROR")
    echo "C++输出: $cpp_output"
    
    # 步骤6: 比较输出
    echo "步骤6: 比较输出结果"
    if [ "$py_output" = "$cpp_output" ]; then
        echo -e "${GREEN}✓ 输出一致！测试通过${NC}"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        
        # 清理临时文件
        rm -f "$cpp_file" "$exe_file"
        return 0
    else
        echo -e "${RED}✗ 输出不一致！${NC}"
        echo "Python输出: $py_output"
        echo "C++输出: $cpp_output"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$test_name: 输出不一致")
        return 1
    fi
}

# 构建项目
echo "构建Fluxus编译器..."
if ! cabal build 2>&1 | tail -5; then
    echo -e "${RED}构建失败！${NC}"
    exit 1
fi
echo -e "${GREEN}构建成功${NC}"
echo ""

# 测试用例列表
TEST_FILES=(
    "simple_test.py"
    "fibonacci.py"
    "factorial.py"
    "simple_math.py"
    "simple_function.py"
    "simple_loop.py"
    "print_test.py"
)

# 运行所有测试
for test_file in "${TEST_FILES[@]}"; do
    if [ -f "$test_file" ]; then
        test_python_file "$test_file"
    else
        echo -e "${YELLOW}跳过不存在的文件: $test_file${NC}"
    fi
    echo ""
done

# 输出总结
echo "=========================================="
echo "测试总结"
echo "=========================================="
echo "总测试数: $TOTAL_TESTS"
echo -e "${GREEN}通过: $PASSED_TESTS${NC}"
echo -e "${RED}失败: $FAILED_TESTS${NC}"

if [ $FAILED_TESTS -gt 0 ]; then
    echo ""
    echo "失败的测试:"
    for failed_test in "${FAILED_TEST_NAMES[@]}"; do
        echo -e "${RED}  - $failed_test${NC}"
    done
fi

echo ""
if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "${GREEN}所有测试通过！✓${NC}"
    exit 0
else
    echo -e "${RED}有测试失败 ✗${NC}"
    exit 1
fi
