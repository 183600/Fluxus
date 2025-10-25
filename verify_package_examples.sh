#!/bin/bash

echo "=========================================="
echo "验证 Go 包声明示例"
echo "=========================================="
echo ""

EXAMPLES_DIR="examples/package_examples"
SUCCESS=0
TOTAL=0

# 检查目录是否存在
if [ ! -d "$EXAMPLES_DIR" ]; then
    echo "❌ 错误: 目录 $EXAMPLES_DIR 不存在"
    exit 1
fi

# 遍历所有 .go 文件
for file in "$EXAMPLES_DIR"/*.go; do
    if [ -f "$file" ]; then
        TOTAL=$((TOTAL + 1))
        filename=$(basename "$file")
        echo "测试文件: $filename"
        
        # 检查语法
        if go fmt "$file" > /dev/null 2>&1; then
            echo "  ✅ 语法正确"
            SUCCESS=$((SUCCESS + 1))
            
            # 提取包名
            package_name=$(grep -m 1 "^package " "$file" | awk '{print $2}')
            echo "  📦 包名: $package_name"
            
            # 检查是否有 main 函数（仅对 package main）
            if [ "$package_name" = "main" ]; then
                if grep -q "func main()" "$file"; then
                    echo "  ✅ 包含 main() 函数"
                else
                    echo "  ⚠️  警告: package main 但没有 main() 函数"
                fi
            fi
        else
            echo "  ❌ 语法错误"
        fi
        echo ""
    fi
done

echo "=========================================="
echo "测试总结"
echo "=========================================="
echo "通过: $SUCCESS/$TOTAL"

if [ $SUCCESS -eq $TOTAL ]; then
    echo "✅ 所有示例文件语法正确！"
    exit 0
else
    echo "❌ 部分示例文件有问题"
    exit 1
fi
