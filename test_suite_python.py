#!/usr/bin/env python3
"""
全面的Python测试套件
用于验证Fluxus编译器的Python到C++转换功能
"""

# 测试1: 基本变量赋值
def test_basic_assignment():
    x = 42
    return x

# 测试2: 算术运算
def test_arithmetic():
    a = 10
    b = 5
    c = a + b
    d = a - b
    e = a * b
    f = a / b
    return c + d + e + f

# 测试3: 条件语句
def test_if_else():
    x = 10
    if x > 5:
        return 1
    else:
        return 0

# 测试4: 循环
def test_loop():
    sum = 0
    i = 0
    while i < 5:
        sum = sum + i
        i = i + 1
    return sum

# 测试5: 函数调用
def helper(x):
    return x * 2

def test_function_call():
    result = helper(21)
    return result

# 测试6: 递归
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

def test_recursion():
    return factorial(5)

# 测试7: 字符串操作
def test_strings():
    s = "Hello"
    return s

# 测试8: 列表操作
def test_lists():
    lst = [1, 2, 3]
    return lst[0]

# 主测试运行器
if __name__ == "__main__":
    print(test_basic_assignment())
    print(test_arithmetic())
    print(test_if_else())
    print(test_loop())
    print(test_function_call())
    print(test_recursion())
    print(test_strings())
    print(test_lists())
