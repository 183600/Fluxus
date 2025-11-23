#!/bin/bash
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$PATH"

ensure_haskell_toolchain() {
  if command -v cabal >/dev/null 2>&1 && command -v ghc >/dev/null 2>&1; then
    return
  fi

  if [ -x "$SCRIPT_DIR/ensure_haskell_toolchain.sh" ]; then
    echo "未检测到完整的 Haskell 工具链，正在运行 ensure_haskell_toolchain.sh ..." >&2
    bash "$SCRIPT_DIR/ensure_haskell_toolchain.sh"
  else
    echo "未检测到 cabal/ghc，且无法在 $SCRIPT_DIR 找到 ensure_haskell_toolchain.sh。请先安装 Haskell 工具链。" >&2
    exit 1
  fi
}

ensure_haskell_toolchain

echo "=== HyperStatic2 编译器大规模测试 ==="

# 测试结果统计
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# 函数：运行测试
run_test() {
    local file=$1
    local lang=$2
    echo "测试文件: $file"
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    if cabal exec fluxus -- --$lang $file > /dev/null 2>&1; then
        echo "  ✅ 编译成功"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        
        # 检查是否生成了C++文件
        cpp_file="${file%.*}.cpp"
        if [ -f "$cpp_file" ]; then
            echo "  ✅ 生成C++代码"
            
            # 尝试编译C++
            executable="${file%.*}_exec"
            if g++ -o "$executable" "$cpp_file" 2>/dev/null; then
                echo "  ✅ C++编译成功"
                
                # 尝试运行
                if ./"$executable" > /dev/null 2>&1; then
                    echo "  ✅ 程序运行成功"
                else
                    echo "  ⚠️  程序运行失败"
                fi
                rm -f "$executable"
            else
                echo "  ⚠️  C++编译失败"
            fi
        else
            echo "  ⚠️  未生成C++代码"
        fi
    else
        echo "  ❌ 编译失败"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    echo ""
}

# Go 测试用例
echo "=== Go 语言测试 ==="

# 测试1：基本包声明
cat > go_test_basic_package.go << 'EOF'
package main
EOF
run_test "go_test_basic_package.go" "go"

# 测试2：变量声明
cat > go_test_variables.go << 'EOF'
package main

var x int
var y string
var z bool
var a float64
var b []int
EOF
run_test "go_test_variables.go" "go"

# 测试3：多种类型变量
cat > go_test_complex_vars.go << 'EOF'
package main

var counter int
var name string
var active bool
var ratio float64
var items []string
var config map[string]int
EOF
run_test "go_test_complex_vars.go" "go"

# Python 测试用例
echo "=== Python 语言测试 ==="

# 测试1：单个导入
cat > python_test_single_import.py << 'EOF'
import os
EOF
run_test "python_test_single_import.py" "python"

# 测试2：多个导入
cat > python_test_multiple_imports.py << 'EOF'
import os
import sys
import json
import time
import re
import math
EOF
run_test "python_test_multiple_imports.py" "python"

# 测试3：常用库导入
cat > python_test_stdlib_imports.py << 'EOF'
import os
import sys
import json
import time
import datetime
import collections
import itertools
import functools
EOF
run_test "python_test_stdlib_imports.py" "python"

# 清理测试文件
echo "清理测试文件..."
rm -f go_test_*.go python_test_*.py *.cpp *_exec

# 显示测试结果
echo "=== 测试结果总结 ==="
echo "总测试数: $TOTAL_TESTS"
echo "通过测试: $PASSED_TESTS"  
echo "失败测试: $FAILED_TESTS"
echo "成功率: $((PASSED_TESTS * 100 / TOTAL_TESTS))%"

if [ $FAILED_TESTS -eq 0 ]; then
    echo "🎉 所有测试都通过了！"
    exit 0
else
    echo "⚠️  有 $FAILED_TESTS 个测试失败"
    exit 1
fi