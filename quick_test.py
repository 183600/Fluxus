#!/usr/bin/env python3
"""快速测试Python到C++编译"""

import subprocess
import os
import tempfile

def test_simple_comparison():
    """测试比较运算符"""
    print("测试: 比较运算符")
    
    py_code = """x = 10
if x > 5:
    print(1)
else:
    print(0)
"""
    
    with tempfile.TemporaryDirectory() as tmpdir:
        # 写入Python文件
        py_file = os.path.join(tmpdir, "test.py")
        with open(py_file, 'w') as f:
            f.write(py_code)
        
        # 运行Python
        py_result = subprocess.run(['python3', py_file], capture_output=True, text=True)
        print(f"  Python输出: {repr(py_result.stdout)}")
        
        # 编译到C++
        cpp_result = subprocess.run(
            ['cabal', 'run', 'fluxus', '--', '--python', py_file],
            capture_output=True, text=True
        )
        
        if cpp_result.returncode != 0:
            print(f"  ✗ 编译失败: {cpp_result.stderr}")
            return False
        
        cpp_code = cpp_result.stdout
        
        # 检查是否有TODO
        if 'TODO' in cpp_code and 'PyComparison' in cpp_code:
            print(f"  ✗ 发现未实现的比较运算符")
            return False
        
        # 保存C++代码
        cpp_file = os.path.join(tmpdir, "test.cpp")
        with open(cpp_file, 'w') as f:
            f.write(cpp_code)
        
        # 编译C++
        compile_result = subprocess.run(
            ['g++', '-std=c++20', '-o', os.path.join(tmpdir, 'test'), cpp_file],
            capture_output=True, text=True
        )
        
        if compile_result.returncode != 0:
            print(f"  ✗ C++编译失败: {compile_result.stderr}")
            print(f"  C++代码:\n{cpp_code}")
            return False
        
        # 运行C++
        cpp_run_result = subprocess.run(
            [os.path.join(tmpdir, 'test')],
            capture_output=True, text=True
        )
        
        print(f"  C++输出: {repr(cpp_run_result.stdout)}")
        
        if py_result.stdout == cpp_run_result.stdout:
            print(f"  ✓ 测试通过！")
            return True
        else:
            print(f"  ✗ 输出不匹配")
            return False

def test_simple_function():
    """测试简单函数"""
    print("\n测试: 简单函数")
    
    py_code = """def add(a, b):
    return a + b

result = add(10, 20)
print(result)
"""
    
    with tempfile.TemporaryDirectory() as tmpdir:
        py_file = os.path.join(tmpdir, "test.py")
        with open(py_file, 'w') as f:
            f.write(py_code)
        
        # 运行Python
        py_result = subprocess.run(['python3', py_file], capture_output=True, text=True)
        print(f"  Python输出: {repr(py_result.stdout)}")
        
        # 编译到C++
        cpp_result = subprocess.run(
            ['cabal', 'run', 'fluxus', '--', '--python', py_file],
            capture_output=True, text=True
        )
        
        if cpp_result.returncode != 0:
            print(f"  ✗ 编译失败")
            return False
        
        cpp_code = cpp_result.stdout
        cpp_file = os.path.join(tmpdir, "test.cpp")
        with open(cpp_file, 'w') as f:
            f.write(cpp_code)
        
        # 编译C++
        compile_result = subprocess.run(
            ['g++', '-std=c++20', '-o', os.path.join(tmpdir, 'test'), cpp_file],
            capture_output=True, text=True
        )
        
        if compile_result.returncode != 0:
            print(f"  ✗ C++编译失败")
            return False
        
        # 运行C++
        cpp_run_result = subprocess.run(
            [os.path.join(tmpdir, 'test')],
            capture_output=True, text=True
        )
        
        print(f"  C++输出: {repr(cpp_run_result.stdout)}")
        
        if py_result.stdout == cpp_run_result.stdout:
            print(f"  ✓ 测试通过！")
            return True
        else:
            print(f"  ✗ 输出不匹配")
            return False

def main():
    print("="*60)
    print("Fluxus Python到C++编译快速测试")
    print("="*60)
    
    tests = [
        test_simple_comparison,
        test_simple_function
    ]
    
    passed = 0
    for test in tests:
        try:
            if test():
                passed += 1
        except Exception as e:
            print(f"  ✗ 测试异常: {e}")
    
    print("\n" + "="*60)
    print(f"结果: {passed}/{len(tests)} 测试通过")
    print("="*60)
    
    return 0 if passed == len(tests) else 1

if __name__ == '__main__':
    exit(main())
