#!/usr/bin/env python3
import os
import subprocess
import sys

def run_test(py_file, expected_output):
    """运行单个测试"""
    try:
        # 运行Python代码
        result = subprocess.run(['python3', py_file], 
                              capture_output=True, text=True)
        if result.returncode != 0:
            return False, f"Python运行错误: {result.stderr}"
        
        output = result.stdout.strip()
        if output == expected_output:
            return True, output
        else:
            return False, f"输出不匹配: 期望 '{expected_output}', 实际 '{output}'"
    
    except Exception as e:
        return False, f"测试错误: {str(e)}"

def main():
    test_dir = "extended_python_tests"
    total_tests = 0
    passed_tests = 0
    failed_tests = []
    
    print("运行扩展Python测试套件...")
    print("=" * 50)
    
    for root, dirs, files in os.walk(test_dir):
        for file in files:
            if file.endswith('.py') and file.startswith('test_'):
                test_path = os.path.join(root, file)
                test_name = file[:-3]  # 移除.py后缀
                
                # 从文件名获取期望输出（这里简化处理）
                # 实际应该从测试用例数据中获取
                try:
                    with open(test_path, 'r') as f:
                        code = f.read().strip()
                    
                    success, message = run_test(test_path, "")
                    total_tests += 1
                    
                    if success:
                        print(f"✓ {test_name}: 通过")
                        passed_tests += 1
                    else:
                        print(f"✗ {test_name}: 失败 - {message}")
                        failed_tests.append(test_name)
                
                except Exception as e:
                    print(f"✗ {test_name}: 错误 - {str(e)}")
                    failed_tests.append(test_name)
                    total_tests += 1
    
    print("=" * 50)
    print(f"测试结果: {passed_tests}/{total_tests} 通过")
    
    if failed_tests:
        print(f"失败的测试: {', '.join(failed_tests)}")
        return 1
    else:
        print("所有测试通过！")
        return 0

if __name__ == "__main__":
    sys.exit(main())
