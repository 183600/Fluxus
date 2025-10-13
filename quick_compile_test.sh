#!/bin/bash

# 快速批量测试（使用timeout）
OUTPUT="compile_results.txt"
> "$OUTPUT"

echo "快速测试部分Python文件..." | tee -a "$OUTPUT"

test_files=(
"test_empty.py"
"test_just_number.py"
"test_simple_add.py"
"test_print.py"
"test_functions.py"
"test_classes.py"
"test/python-tests/basic_arithmetic.py"
"test/python-tests/feature_list_comprehension.py"
"test/python-tests/feature_exception.py"
"test/python-tests/test_loops.py"
"test/python-tests/test_functions.py"
"test/python-tests/test_classes.py"
)

SUCCESS=0
FAIL=0
TIMEOUT=0

for file in "${test_files[@]}"; do
    if [ ! -f "$file" ]; then
        echo "SKIP: $file (不存在)" | tee -a "$OUTPUT"
        continue
    fi
    
    output_name="test_out_$(basename "$file" .py)"
    echo -n "测试 $file ... "
    
    # 使用timeout命令，15秒超时
    timeout 15 stack exec fluxus -- --python -O2 "$file" -o "$output_name" > /tmp/compile_tmp.txt 2>&1
    status=$?
    
    if [ $status -eq 0 ]; then
        echo "✓" | tee -a "$OUTPUT"
        ((SUCCESS++))
        rm -f "$output_name" "${output_name}.cpp"
    elif [ $status -eq 124 ]; then
        echo "TIMEOUT" | tee -a "$OUTPUT"
        cat /tmp/compile_tmp.txt >> "$OUTPUT"
        ((TIMEOUT++))
    else
        echo "✗" | tee -a "$OUTPUT"
        cat /tmp/compile_tmp.txt >> "$OUTPUT"
        ((FAIL++))
    fi
done

echo "" | tee -a "$OUTPUT"
echo "结果: 成功=$SUCCESS, 失败=$FAIL, 超时=$TIMEOUT" | tee -a "$OUTPUT"
