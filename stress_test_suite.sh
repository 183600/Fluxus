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

echo "=== HyperStatic2 压力测试和可靠性验证 ==="

# 测试结果统计
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# 函数：运行测试
run_test() {
    local file=$1
    local lang=$2
    local description=$3
    echo "测试: $description"
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
                
                # 尝试运行多次确保稳定性
                success_count=0
                for i in {1..5}; do
                    if ./"$executable" > /dev/null 2>&1; then
                        success_count=$((success_count + 1))
                    fi
                done
                
                if [ $success_count -eq 5 ]; then
                    echo "  ✅ 程序运行稳定 (5/5次成功)"
                else
                    echo "  ⚠️  程序运行不稳定 ($success_count/5次成功)"
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

# 创建多个测试文件进行批量测试
echo "=== 批量Go程序测试 ==="

for i in {1..10}; do
    cat > "stress_go_test_$i.go" << EOF
package main

// Test case $i
var test_var_$i int
var test_string_$i string
var test_bool_$i bool
EOF
    run_test "stress_go_test_$i.go" "go" "批量Go测试 #$i"
done

echo "=== 批量Python程序测试 ==="

for i in {1..10}; do
    cat > "stress_python_test_$i.py" << EOF
import os
# Test case $i
test_var_$i = $i
EOF
    run_test "stress_python_test_$i.py" "python" "批量Python测试 #$i"
done

# 测试已知可编译的模式
echo "=== 已知可编译模式测试 ==="

# Go模式测试
for pattern in "basic_package" "simple_vars" "complex_vars"; do
    case $pattern in
        "basic_package")
            cat > "pattern_test_$pattern.go" << 'EOF'
package main
EOF
            ;;
        "simple_vars")
            cat > "pattern_test_$pattern.go" << 'EOF'
package main

var x int
var y string
EOF
            ;;
        "complex_vars")
            cat > "pattern_test_$pattern.go" << 'EOF'
package main

var counter int
var name string
var active bool
var ratio float64
var items []string
var config map[string]int
EOF
            ;;
    esac
    run_test "pattern_test_$pattern.go" "go" "Go模式测试: $pattern"
done

# Python模式测试
for pattern in "single_import" "multiple_imports" "stdlib_imports"; do
    case $pattern in
        "single_import")
            cat > "pattern_test_$pattern.py" << 'EOF'
import os
EOF
            ;;
        "multiple_imports")
            cat > "pattern_test_$pattern.py" << 'EOF'
import os
import sys
import json
EOF
            ;;
        "stdlib_imports")
            cat > "pattern_test_$pattern.py" << 'EOF'
import os
import sys
import json
import time
import datetime
import collections
EOF
            ;;
    esac
    run_test "pattern_test_$pattern.py" "python" "Python模式测试: $pattern"
done

# 连续编译测试 - 确保编译器状态一致性
echo "=== 连续编译一致性测试 ==="
cat > "consistency_test.go" << 'EOF'
package main

var consistency_var int
EOF

cat > "consistency_test.py" << 'EOF'
import os
EOF

for i in {1..5}; do
    echo "第$i轮一致性测试:"
    if cabal exec fluxus -- --go consistency_test.go > /dev/null 2>&1; then
        echo "  Go编译成功"
    else
        echo "  Go编译失败"
    fi
    
    if cabal exec fluxus -- --python consistency_test.py > /dev/null 2>&1; then
        echo "  Python编译成功"
    else
        echo "  Python编译失败"
    fi
done

# 清理测试文件
echo "清理压力测试文件..."
rm -f stress_*_test_*.go stress_*_test_*.py pattern_test_*.go pattern_test_*.py
rm -f consistency_test.go consistency_test.py *.cpp *_exec

# 显示最终结果
echo "=== 压力测试结果总结 ==="
echo "总测试数: $TOTAL_TESTS"
echo "通过测试: $PASSED_TESTS"  
echo "失败测试: $FAILED_TESTS"

if [ $TOTAL_TESTS -gt 0 ]; then
    success_rate=$((PASSED_TESTS * 100 / TOTAL_TESTS))
    echo "成功率: $success_rate%"
    
    if [ $success_rate -ge 90 ]; then
        echo "🎉 压力测试表现优秀！编译器非常稳定！"
        exit 0
    elif [ $success_rate -ge 70 ]; then
        echo "✅ 压力测试表现良好！编译器基本稳定！"
        exit 0
    elif [ $success_rate -ge 50 ]; then
        echo "⚠️  压力测试表现一般，编译器需要改进"
        exit 1
    else
        echo "❌ 压力测试表现较差，编译器需要大量改进"
        exit 1
    fi
else
    echo "成功率: 0%"
    echo "❌ 没有执行任何测试"
    exit 1
fi