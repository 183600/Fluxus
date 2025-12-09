#!/bin/bash

# Python到C++验证脚本
# 用于验证生成的C++代码是否正确

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TEST_DIR="extended_python_tests"

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

# 测试函数
test_python_to_cpp() {
    local test_name="$1"
    local py_file="$2"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    echo -e "${BLUE}测试 #${TOTAL_TESTS}: ${test_name}${NC}"
    
    local cpp_file="${py_file}.cpp"
    local exe_file="${py_file}_exe"
    
    # 步骤1: 验证Python代码可以运行
    echo "  步骤1: 验证Python代码"
    if ! python3 "${py_file}" > /dev/null 2>&1; then
        echo -e "  ${RED}✗ Python代码运行失败${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
    echo -e "  ${GREEN}✓ Python代码运行成功${NC}"
    
    # 步骤2: 编译Python到C++
    echo "  步骤2: 编译到C++"
    if ! cabal run fluxus -- --python "${py_file}" > "${cpp_file}" 2>&1; then
        echo -e "  ${RED}✗ 编译失败${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
    echo -e "  ${GREEN}✓ 编译成功${NC}"
    
    # 步骤3: 检查C++语法
    echo "  步骤3: 检查C++语法"
    if ! g++ -std=c++20 -fsyntax-only "${cpp_file}" 2>&1; then
        echo -e "  ${RED}✗ C++语法错误${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
    echo -e "  ${GREEN}✓ C++语法正确${NC}"
    
    # 步骤4: 编译C++
    echo "  步骤4: 编译C++"
    if ! g++ -std=c++20 -O2 "${cpp_file}" -o "${exe_file}" 2>&1; then
        echo -e "  ${RED}✗ C++编译失败${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
    echo -e "  ${GREEN}✓ C++编译成功${NC}"
    
    PASSED_TESTS=$((PASSED_TESTS + 1))
    return 0
}

# 主测试循环
echo "开始Python到C++验证测试..."
echo "=========================================="

# 查找所有Python测试文件
find "${TEST_DIR}" -name "test_*.py" | while read -r py_file; do
    test_name=$(basename "${py_file}" .py)
    test_python_to_cpp "${test_name}" "${py_file}"
    echo ""
done

echo "=========================================="
echo "测试总结"
echo "总测试数: ${TOTAL_TESTS}"
echo -e "${GREEN}通过: ${PASSED_TESTS}${NC}"
echo -e "${RED}失败: ${FAILED_TESTS}${NC}"

if [ ${FAILED_TESTS} -eq 0 ]; then
    echo -e "${GREEN}所有测试通过！${NC}"
    exit 0
else
    echo -e "${RED}有 ${FAILED_TESTS} 个测试失败${NC}"
    exit 1
fi
