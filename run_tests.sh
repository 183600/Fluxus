#!/bin/bash

# 获取test/python-tests目录下所有的.py文件
test_files=$(find test/python-tests -name "*.py" -type f)

# 遍历每个文件并运行
for file in $test_files; do
    # 使用python运行文件，只捕获错误输出
    python "$file" 2>/tmp/error_$$.log >/dev/null

    # 检查退出状态
    if [ $? -ne 0 ]; then
        # 如果有错误，显示错误信息
        echo "Error in $file:"
        cat /tmp/error_$$.log
        echo "---"
    fi

    # 清理临时文件
    rm -f /tmp/error_$$.log
done