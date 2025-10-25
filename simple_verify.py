#!/usr/bin/env python3
"""简单验证脚本"""

import subprocess
import os

def test():
    # 创建测试文件
    with open("t.py", "w") as f:
        f.write("print(42)\n")
    
    # 运行Python
    result = subprocess.run(["python3", "t.py"], capture_output=True, text=True)
    py_output = result.stdout.strip()
    print(f"Python输出: {py_output}")
    
    # 编译到C++
    result = subprocess.run(["cabal", "run", "fluxus", "--", "--python", "t.py"], 
                          capture_output=True, text=True)
    
    if result.returncode != 0:
        print(f"编译失败: {result.stderr}")
        return False
    
    cpp_code = result.stdout
    
    # 保存C++代码
    with open("t.cpp", "w") as f:
        f.write(cpp_code)
    
    print(f"C++代码已生成 ({len(cpp_code)} 字节)")
    print("C++代码内容:")
    print(cpp_code)
    
    # 编译C++
    result = subprocess.run(["g++", "-std=c++20", "t.cpp", "-o", "t_exe"],
                          capture_output=True, text=True)
    
    if result.returncode != 0:
        print(f"C++编译失败: {result.stderr}")
        return False
    
    print("C++编译成功")
    
    # 运行C++
    result = subprocess.run(["./t_exe"], capture_output=True, text=True)
    cpp_output = result.stdout.strip()
    print(f"C++输出: {cpp_output}")
    
    # 比较
    if py_output == cpp_output:
        print("✓ 测试通过！")
        return True
    else:
        print(f"✗ 输出不匹配: Python={py_output}, C++={cpp_output}")
        return False

if __name__ == "__main__":
    test()
