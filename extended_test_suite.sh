#!/bin/bash
echo "=== HyperStatic2 扩展测试套件 ==="

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
    echo "文件: $file"
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

# Go 语言扩展测试
echo "=== Go 语言扩展测试 ==="

# 测试1：更复杂的包声明
cat > go_extended_test1.go << 'EOF'
package main

// 多行注释测试
/* This is a multi-line comment
   that spans multiple lines */

// 变量声明组
var (
    globalInt    int
    globalString string
    globalBool   bool
)
EOF
run_test "go_extended_test1.go" "go" "复杂包声明和变量组"

# 测试2：多类型变量声明
cat > go_extended_test2.go << 'EOF'
package main

// 各种类型变量
var intVar int
var int8Var int8
var int16Var int16
var int32Var int32
var int64Var int64
var uintVar uint
var uint8Var uint8
var uint16Var uint16
var uint32Var uint32
var uint64Var uint64
var float32Var float32
var float64Var float64
var stringVar string
var boolVar bool
var byteVar byte
var runeVar rune
EOF
run_test "go_extended_test2.go" "go" "所有基本数据类型"

# 测试3：数组和切片声明
cat > go_extended_test3.go << 'EOF'
package main

// 数组声明
var arr1 [5]int
var arr2 [10]string
var arr3 [3]bool

// 切片声明
var slice1 []int
var slice2 []string
var slice3 []float64

// 映射声明
var map1 map[string]int
var map2 map[int]string
EOF
run_test "go_extended_test3.go" "go" "数组切片和映射"

# 测试4：常量声明
cat > go_extended_test4.go << 'EOF'
package main

// 常量声明
const PI = 3.14159
const MAX_SIZE = 100
const APP_NAME = "TestApp"
const DEBUG_MODE = true

// 常量组
const (
    RED   = 0
    GREEN = 1
    BLUE  = 2
)
EOF
run_test "go_extended_test4.go" "go" "常量声明"

# Python 语言扩展测试
echo "=== Python 语言扩展测试 ==="

# 测试1：更多模块导入
cat > python_extended_test1.py << 'EOF'
import sys
import os
import time
import datetime
import json
import re
import math
import random
import collections
import itertools
EOF
run_test "python_extended_test1.py" "python" "标准库导入测试"

# 测试2：变量赋值
cat > python_extended_test2.py << 'EOF'
# 基本变量类型
integer_var = 42
float_var = 3.14159
string_var = "Hello World"
boolean_var = True
none_var = None

# 列表和元组
list_var = [1, 2, 3, 4, 5]
tuple_var = (1, 2, 3)
dict_var = {"key": "value", "number": 42}
set_var = {1, 2, 3, 4, 5}
EOF
run_test "python_extended_test2.py" "python" "变量赋值测试"

# 测试3：多种导入语法
cat > python_extended_test3.py << 'EOF'
import os
import sys as system
from json import loads, dumps
from datetime import datetime, timedelta
EOF
run_test "python_extended_test3.py" "python" "多种导入语法"

# 测试4：注释和字符串
cat > python_extended_test4.py << 'EOF'
# 这是单行注释
"""
这是多行注释
可以跨越多行
"""

# 不同类型的字符串
single_quote = 'Single quote string'
double_quote = "Double quote string"
triple_single = '''Triple single quote'''
triple_double = """Triple double quote"""

# 原始字符串
raw_string = r"This is a raw string \n \t"
EOF
run_test "python_extended_test4.py" "python" "注释和字符串测试"

# 边界情况测试
echo "=== 边界情况测试 ==="

# 空Go文件（只有包声明）
cat > go_empty_test.go << 'EOF'
package main
EOF
run_test "go_empty_test.go" "go" "最小Go程序"

# 空Python文件
cat > python_empty_test.py << 'EOF'
# Empty Python file with just a comment
EOF
run_test "python_empty_test.py" "python" "空Python文件"

# 清理测试文件
echo "清理扩展测试文件..."
rm -f go_extended_test*.go python_extended_test*.py *.cpp *_exec go_empty_test.go python_empty_test.py

# 显示测试结果
echo "=== 扩展测试结果总结 ==="
echo "总测试数: $TOTAL_TESTS"
echo "通过测试: $PASSED_TESTS"  
echo "失败测试: $FAILED_TESTS"
if [ $TOTAL_TESTS -gt 0 ]; then
    echo "成功率: $((PASSED_TESTS * 100 / TOTAL_TESTS))%"
else
    echo "成功率: 0%"
fi

if [ $FAILED_TESTS -eq 0 ]; then
    echo "🎉 所有扩展测试都通过了！"
    exit 0
else
    echo "⚠️  有 $FAILED_TESTS 个扩展测试失败"
    exit 1
fi