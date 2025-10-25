#!/usr/bin/env python3
"""
自动测试和修复Python到C++编译
"""

import subprocess
import os
import sys

# 测试用例
TEST_CASES = [
    {
        "name": "简单打印",
        "code": "print(42)",
        "expected": "42"
    },
    {
        "name": "变量赋值",
        "code": "x = 100\nprint(x)",
        "expected": "100"
    },
    {
        "name": "简单算术",
        "code": "a = 10\nb = 5\nc = a + b\nprint(c)",
        "expected": "15"
    },
    {
        "name": "多个打印",
        "code": "print(1)\nprint(2)\nprint(3)",
        "expected": "1\n2\n3"
    },
    {
        "name": "函数定义",
        "code": "def add(a, b):\n    return a + b\n\nresult = add(5, 3)\nprint(result)",
        "expected": "8"
    },
    {
        "name": "条件语句",
        "code": "x = 10\nif x > 5:\n    print(1)\nelse:\n    print(0)",
        "expected": "1"
    },
    {
        "name": "递归-阶乘",
        "code": "def factorial(n):\n    if n <= 1:\n        return 1\n    return n * factorial(n - 1)\n\nprint(factorial(5))",
        "expected": "120"
    },
]

def run_command(cmd, capture=True):
    """运行命令"""
    try:
        if capture:
            result = subprocess.run(cmd, shell=True, capture_output=True, text=True, timeout=10)
            return result.returncode, result.stdout, result.stderr
        else:
            result = subprocess.run(cmd, shell=True, timeout=10)
            return result.returncode, "", ""
    except subprocess.TimeoutExpired:
        return -1, "", "Timeout"
    except Exception as e:
        return -1, "", str(e)

def test_case(test):
    """测试单个用例"""
    name = test["name"]
    code = test["code"]
    expected = test["expected"]
    
    print(f"\n{'='*60}")
    print(f"测试: {name}")
    print(f"{'='*60}")
    
    # 1. 创建Python文件
    with open("test_temp.py", "w") as f:
        f.write(code)
    
    # 2. 运行Python获取实际输出
    ret, py_out, py_err = run_command("python3 test_temp.py")
    if ret != 0:
        print(f"❌ Python运行失败: {py_err}")
        return False
    
    py_out = py_out.strip()
    print(f"Python输出: {py_out}")
    
    # 3. 编译Python到C++
    print("编译到C++...")
    ret, cpp_code, err = run_command("cabal run fluxus -- --python test_temp.py")
    
    if ret != 0:
        print(f"❌ Python到C++编译失败")
        print(f"错误: {err}")
        return False
    
    # 保存C++代码
    with open("test_temp.cpp", "w") as f:
        f.write(cpp_code)
    
    if not os.path.exists("test_temp.cpp") or os.path.getsize("test_temp.cpp") == 0:
        print("❌ C++文件未生成或为空")
        return False
    
    print(f"✓ C++代码生成成功 ({os.path.getsize('test_temp.cpp')} 字节)")
    
    # 4. 检查C++语法
    print("检查C++语法...")
    ret, _, err = run_command("g++ -std=c++20 -fsyntax-only test_temp.cpp")
    
    if ret != 0:
        print(f"❌ C++语法错误:")
        print(err)
        print("\n生成的C++代码:")
        with open("test_temp.cpp", "r") as f:
            print(f.read())
        return False
    
    print("✓ C++语法正确")
    
    # 5. 编译C++
    print("编译C++...")
    ret, _, err = run_command("g++ -std=c++20 -O2 test_temp.cpp -o test_temp_exe")
    
    if ret != 0:
        print(f"❌ C++编译失败:")
        print(err)
        return False
    
    print("✓ C++编译成功")
    
    # 6. 运行C++程序
    print("运行C++程序...")
    ret, cpp_out, err = run_command("./test_temp_exe")
    
    if ret != 0:
        print(f"❌ C++程序运行失败:")
        print(err)
        return False
    
    cpp_out = cpp_out.strip()
    print(f"C++输出: {cpp_out}")
    
    # 7. 比较输出
    if py_out == cpp_out:
        print(f"✅ 测试通过！")
        return True
    else:
        print(f"❌ 输出不匹配")
        print(f"  Python: [{py_out}]")
        print(f"  C++:    [{cpp_out}]")
        return False

def main():
    """主函数"""
    print("="*60)
    print("Python到C++编译自动测试")
    print("="*60)
    
    # 构建项目
    print("\n构建Fluxus编译器...")
    ret, out, err = run_command("cabal build")
    if ret != 0:
        print(f"❌ 构建失败: {err}")
        return 1
    print("✓ 构建成功")
    
    # 运行测试
    passed = 0
    failed = 0
    failed_tests = []
    
    for test in TEST_CASES:
        if test_case(test):
            passed += 1
        else:
            failed += 1
            failed_tests.append(test["name"])
    
    # 清理
    for f in ["test_temp.py", "test_temp.cpp", "test_temp_exe"]:
        if os.path.exists(f):
            os.remove(f)
    
    # 总结
    print("\n" + "="*60)
    print("测试总结")
    print("="*60)
    print(f"总计: {passed + failed}")
    print(f"✅ 通过: {passed}")
    print(f"❌ 失败: {failed}")
    
    if failed > 0:
        print(f"\n失败的测试:")
        for name in failed_tests:
            print(f"  - {name}")
    
    print(f"\n成功率: {passed * 100 // (passed + failed)}%")
    
    return 0 if failed == 0 else 1

if __name__ == "__main__":
    sys.exit(main())
