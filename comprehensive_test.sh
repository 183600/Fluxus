#!/bin/bash

echo "====================================="
echo "Fluxus编译器全面测试报告"
echo "====================================="
echo ""

OUTPUT_DIR="test_outputs"
mkdir -p "$OUTPUT_DIR"

SUCCESS=0
FAIL=0
RUN_SUCCESS=0
RUN_FAIL=0

# 所有测试文件
test_files=(
    "test_empty.py"
    "test_just_number.py"
    "test_simple_add.py"
    "test_print.py"
    "test_functions.py"
    "test/python-tests/basic_arithmetic.py"
    "test/python-tests/feature_list_comprehension.py"
    "test/python-tests/feature_exception.py"
    "test/python-tests/test_loops.py"
    "test/python-tests/test_functions.py"
    "test/python-tests/test_classes.py"
)

echo "测试1: 编译所有文件"
echo "-------------------------------------"

for file in "${test_files[@]}"; do
    if [ ! -f "$file" ]; then
        echo "SKIP: $file (不存在)"
        continue
    fi
    
    basename=$(basename "$file" .py)
    output="$OUTPUT_DIR/${basename}_out"
    
    echo -n "编译 $file ... "
    
    if timeout 15 stack exec fluxus -- --python -O2 "$file" -o "$output" > /dev/null 2>&1; then
        echo "✓ 编译成功"
        ((SUCCESS++))
        
        # 尝试运行
        if [ -f "$output" ]; then
            echo -n "  运行 $output ... "
            if timeout 5 "./$output" > "$OUTPUT_DIR/${basename}_output.txt" 2>&1; then
                echo "✓ 运行成功"
                ((RUN_SUCCESS++))
            else
                echo "✗ 运行失败"
                ((RUN_FAIL++))
            fi
        fi
    else
        echo "✗ 编译失败"
        ((FAIL++))
    fi
done

echo ""
echo "====================================="
echo "测试结果汇总"
echo "====================================="
echo "编译成功: $SUCCESS"
echo "编译失败: $FAIL"
echo "成功率: $(( SUCCESS * 100 / (SUCCESS + FAIL) ))%"
echo ""
echo "运行成功: $RUN_SUCCESS"
echo "运行失败: $RUN_FAIL"
if [ $RUN_SUCCESS -gt 0 ]; then
    echo "运行成功率: $(( RUN_SUCCESS * 100 / (RUN_SUCCESS + RUN_FAIL) ))%"
fi
echo ""

echo "查看输出结果："
echo "  ls $OUTPUT_DIR/"
