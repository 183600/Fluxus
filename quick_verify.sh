#!/bin/bash

# 快速验证Python到C++编译功能

echo "=== 快速验证Python到C++编译 ==="
echo ""

# 创建测试目录
mkdir -p test_verify
cd test_verify || exit 1

# 测试1: 简单打印
echo "测试1: 简单打印"
cat > test1.py << 'EOF'
print(42)
EOF

echo "  编译Python到C++..."
cabal run fluxus -- --python test1.py > test1.cpp 2>&1

if [ -f test1.cpp ]; then
    echo "  ✓ C++代码生成成功"
    echo "  检查C++语法..."
    if g++ -std=c++20 -fsyntax-only test1.cpp 2>&1; then
        echo "  ✓ C++语法正确"
        echo "  编译C++..."
        if g++ -std=c++20 test1.cpp -o test1_exe 2>&1; then
            echo "  ✓ C++编译成功"
            echo "  运行测试..."
            py_out=$(python3 test1.py)
            cpp_out=$(./test1_exe)
            if [ "$py_out" = "$cpp_out" ]; then
                echo "  ✓✓✓ 测试1通过！"
            else
                echo "  ✗ 输出不一致"
                echo "    Python: $py_out"
                echo "    C++: $cpp_out"
            fi
        else
            echo "  ✗ C++编译失败"
        fi
    else
        echo "  ✗ C++语法错误"
        cat test1.cpp
    fi
else
    echo "  ✗ C++代码未生成"
fi

echo ""

# 测试2: 变量和算术
echo "测试2: 变量和算术"
cat > test2.py << 'EOF'
x = 10
y = 5
z = x + y
print(z)
EOF

echo "  编译Python到C++..."
cabal run fluxus -- --python test2.py > test2.cpp 2>&1

if [ -f test2.cpp ]; then
    echo "  ✓ C++代码生成成功"
    if g++ -std=c++20 -fsyntax-only test2.cpp 2>&1; then
        echo "  ✓ C++语法正确"
        if g++ -std=c++20 test2.cpp -o test2_exe 2>&1; then
            echo "  ✓ C++编译成功"
            py_out=$(python3 test2.py)
            cpp_out=$(./test2_exe)
            if [ "$py_out" = "$cpp_out" ]; then
                echo "  ✓✓✓ 测试2通过！"
            else
                echo "  ✗ 输出不一致"
                echo "    Python: $py_out"
                echo "    C++: $cpp_out"
            fi
        else
            echo "  ✗ C++编译失败"
        fi
    else
        echo "  ✗ C++语法错误"
    fi
else
    echo "  ✗ C++代码未生成"
fi

echo ""

# 测试3: 函数
echo "测试3: 函数定义和调用"
cat > test3.py << 'EOF'
def add(a, b):
    return a + b

result = add(10, 20)
print(result)
EOF

echo "  编译Python到C++..."
cabal run fluxus -- --python test3.py > test3.cpp 2>&1

if [ -f test3.cpp ]; then
    echo "  ✓ C++代码生成成功"
    if g++ -std=c++20 -fsyntax-only test3.cpp 2>&1; then
        echo "  ✓ C++语法正确"
        if g++ -std=c++20 test3.cpp -o test3_exe 2>&1; then
            echo "  ✓ C++编译成功"
            py_out=$(python3 test3.py)
            cpp_out=$(./test3_exe)
            if [ "$py_out" = "$cpp_out" ]; then
                echo "  ✓✓✓ 测试3通过！"
            else
                echo "  ✗ 输出不一致"
                echo "    Python: $py_out"
                echo "    C++: $cpp_out"
            fi
        else
            echo "  ✗ C++编译失败"
        fi
    else
        echo "  ✗ C++语法错误"
    fi
else
    echo "  ✗ C++代码未生成"
fi

cd ..
echo ""
echo "=== 验证完成 ==="
