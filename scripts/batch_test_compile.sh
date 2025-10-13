#!/bin/bash

# 批量测试编译所有Python文件
SUCCESS=0
FAILED=0
TOTAL=0

echo "开始批量编译测试..."

# 查找所有.py文件
find . -name "*.py" -not -path "*/__pycache__/*" -not -path "*/dist-newstyle/*" | while read pyfile; do
    TOTAL=$((TOTAL + 1))
    basename=$(basename "$pyfile" .py)
    output_name="fibonacci"
    
    echo "[$TOTAL] 测试: $pyfile"
    
    # 尝试编译
    if stack exec fluxus -- --python -O2 "$pyfile" -o "$output_name" >/dev/null 2>&1; then
        echo "✓ 成功"
        SUCCESS=$((SUCCESS + 1))
        rm -f "$output_name"
    else
        echo "✗ 失败"
        FAILED=$((FAILED + 1))
    fi
done

echo "===========================================" 
echo "测试完成"
echo "成功: $SUCCESS"
echo "失败: $FAILED"
echo "总数: $TOTAL"
