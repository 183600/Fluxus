#!/bin/bash

echo "======================================"
echo "Python到C++编译手动测试"
echo "======================================"

# 测试1: 简单打印
echo -e "\n测试1: 简单打印"
cat > test_manual.py << 'EOF'
print(42)
EOF

echo "Python输出:"
python3 test_manual.py

echo "编译到C++..."
cabal run fluxus -- --python test_manual.py > test_manual.cpp 2>&1

if [ -f test_manual.cpp ]; then
    echo "✓ C++代码已生成"
    
    echo "编译C++..."
    if g++ -std=c++20 test_manual.cpp -o test_manual_exe 2>&1; then
        echo "✓ C++编译成功"
        
        echo "C++输出:"
        ./test_manual_exe
    else
        echo "✗ C++编译失败"
        cat test_manual.cpp
    fi
else
    echo "✗ C++代码未生成"
fi

# 测试2: 比较运算符
echo -e "\n测试2: 比较运算符"
cat > test_manual2.py << 'EOF'
x = 10
if x > 5:
    print(1)
else:
    print(0)
EOF

echo "Python输出:"
python3 test_manual2.py

echo "编译到C++..."
cabal run fluxus -- --python test_manual2.py > test_manual2.cpp 2>&1

if [ -f test_manual2.cpp ]; then
    echo "✓ C++代码已生成"
    
    # 检查TODO
    if grep -q "TODO.*PyComparison" test_manual2.cpp; then
        echo "✗ 发现未实现的比较运算符"
        cat test_manual2.cpp
    else
        echo "✓ 比较运算符已实现"
        
        echo "编译C++..."
        if g++ -std=c++20 test_manual2.cpp -o test_manual2_exe 2>&1; then
            echo "✓ C++编译成功"
            
            echo "C++输出:"
            ./test_manual2_exe
        else
            echo "✗ C++编译失败"
            cat test_manual2.cpp
        fi
    fi
else
    echo "✗ C++代码未生成"
fi

# 清理
rm -f test_manual*.py test_manual*.cpp test_manual*_exe

echo -e "\n======================================"
echo "测试完成"
echo "======================================"
