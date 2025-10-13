#!/bin/bash

# 批量测试所有Python文件的编译
LOG_FILE="compilation_results.log"
ERROR_FILE="compilation_errors.log"
SUCCESS_FILE="compilation_success.log"

# 清空日志文件
> "$LOG_FILE"
> "$ERROR_FILE"
> "$SUCCESS_FILE"

echo "开始批量编译测试..." | tee -a "$LOG_FILE"
echo "===========================================" | tee -a "$LOG_FILE"

total=0
success=0
failed=0

# 查找所有.py文件
while IFS= read -r pyfile; do
    total=$((total + 1))
    basename=$(basename "$pyfile" .py)
    output_name="test_compile_${total}"
    
    echo "" | tee -a "$LOG_FILE"
    echo "[$total] 编译: $pyfile" | tee -a "$LOG_FILE"
    
    # 尝试编译
    if stack exec fluxus -- --python -O2 "$pyfile" -o "$output_name" 2>&1 | tee -a "$LOG_FILE"; then
        if [ -f "$output_name" ]; then
            echo "✓ 成功: $pyfile" | tee -a "$SUCCESS_FILE"
            success=$((success + 1))
            # 清理生成的文件
            rm -f "$output_name"
        else
            echo "✗ 失败: $pyfile (没有生成可执行文件)" | tee -a "$ERROR_FILE"
            failed=$((failed + 1))
        fi
    else
        echo "✗ 失败: $pyfile" | tee -a "$ERROR_FILE"
        failed=$((failed + 1))
    fi
    
done < <(find . -name "*.py" -not -path "*/__pycache__/*" -not -path "*/dist-newstyle/*" -not -path "*/.git/*")

echo "" | tee -a "$LOG_FILE"
echo "===========================================" | tee -a "$LOG_FILE"
echo "编译完成统计:" | tee -a "$LOG_FILE"
echo "总数: $total" | tee -a "$LOG_FILE"
echo "成功: $success" | tee -a "$LOG_FILE"
echo "失败: $failed" | tee -a "$LOG_FILE"
echo "===========================================" | tee -a "$LOG_FILE"
