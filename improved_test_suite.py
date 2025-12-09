#!/usr/bin/env python3

# 改进的Python到C++测试套件
# 专注于测试已验证的功能

import os
import subprocess
import sys
import tempfile

# 基于现有测试文件的有效测试用例
working_test_cases = [
    # 基础打印测试
    ("simple_print", "print(42)", "42"),
    ("string_print", 'print("hello")', "hello"),
    ("bool_print", "print(True)", "True"),
    
    # 变量赋值
    ("var_assign", "x = 100\nprint(x)", "100"),
    ("var_string", 's = "world"\nprint(s)', "world"),
    
    # 算术运算
    ("simple_add", "a = 5\nb = 3\nprint(a + b)", "8"),
    ("simple_sub", "a = 5\nb = 3\nprint(a - b)", "2"),
    ("simple_mul", "a = 5\nb = 3\nprint(a * b)", "15"),
    
    # 比较运算
    ("equal_check", "a = 5\nb = 5\nprint(a == b)", "True"),
    ("not_equal_check", "a = 5\nb = 3\nprint(a != b)", "True"),
    ("less_check", "a = 3\nb = 5\nprint(a < b)", "True"),
    ("greater_check", "a = 5\nb = 3\nprint(a > b)", "True"),
    
    # 逻辑运算
    ("and_check", "a = True\nb = True\nprint(a and b)", "True"),
    ("or_check", "a = True\nb = False\nprint(a or b)", "True"),
    ("not_check", "a = True\nprint(not a)", "False"),
    
    # 条件语句
    ("if_true", "x = 10\nif x > 5:\n    print(1)", "1"),
    ("if_false", "x = 3\nif x > 5:\n    print(1)", ""),
    ("if_else", "x = 3\nif x > 5:\n    print(1)\nelse:\n    print(0)", "0"),
    
    # 循环
    ("while_loop", "i = 0\nwhile i < 3:\n    print(i)\n    i += 1", "0\n1\n2"),
    ("for_loop", "for i in range(3):\n    print(i)", "0\n1\n2"),
    
    # 函数
    ("simple_func", "def hello():\n    return 42\nprint(hello())", "42"),
    ("func_param", "def add_one(x):\n    return x + 1\nprint(add_one(5))", "6"),
    ("func_multi", "def add(a, b):\n    return a + b\nprint(add(3, 4))", "7"),
    
    # 递归
    ("factorial", "def factorial(n):\n    if n <= 1:\n        return 1\n    return n * factorial(n - 1)\nprint(factorial(5))", "120"),
    ("fibonacci", "def fib(n):\n    if n <= 1:\n        return n\n    return fib(n - 1) + fib(n - 2)\nprint(fib(6))", "8"),
    
    # 字符串
    ("string_len", 's = "hello"\nprint(len(s))', "5"),
    ("string_concat", 'a = "hello"\nb = "world"\nprint(a + " " + b)', "hello world"),
    
    # 列表基础
    ("list_index", "lst = [1, 2, 3]\nprint(lst[0])", "1"),
    ("list_len", "lst = [1, 2, 3]\nprint(len(lst))", "3"),
]

def run_python_code(code):
    """运行Python代码并获取输出"""
    try:
        result = subprocess.run(['python3', '-c', code], 
                              capture_output=True, text=True, timeout=10)
        if result.returncode == 0:
            return True, result.stdout.strip()
        else:
            return False, result.stderr.strip()
    except subprocess.TimeoutExpired:
        return False, "Python代码执行超时"
    except Exception as e:
        return False, str(e)

def test_existing_files():
    """测试现有的测试文件"""
    print("测试现有的Python文件...")
    
    # 查找现有的Python测试文件
    test_files = [
        "test_simple.py",
        "test_1.py",
        "test_2.py",
        "test_3.py",
        "factorial.py",
        "fibonacci.py",
        "simple_test.py",
        "minimal_test.py",
    ]
    
    results = []
    for test_file in test_files:
        if os.path.exists(test_file):
            print(f"\n测试文件: {test_file}")
            
            # 读取文件内容
            with open(test_file, 'r') as f:
                code = f.read().strip()
            
            # 运行Python代码
            success, output = run_python_code(code)
            if success:
                print(f"  ✓ Python输出: {output}")
                
                # 尝试编译到C++
                try:
                    result = subprocess.run(['cabal', 'run', 'fluxus', '--', '--python', test_file, '--stop-at-codegen'],
                                          capture_output=True, text=True, timeout=30)
                    if result.returncode == 0:
                        print(f"  ✓ 编译成功")
                        results.append((test_file, True, "编译成功"))
                    else:
                        print(f"  ✗ 编译失败: {result.stderr[:100]}...")
                        results.append((test_file, False, "编译失败"))
                except subprocess.TimeoutExpired:
                    print(f"  ✗ 编译超时")
                    results.append((test_file, False, "编译超时"))
                except Exception as e:
                    print(f"  ✗ 编译错误: {str(e)[:100]}...")
                    results.append((test_file, False, f"编译错误: {str(e)}"))
            else:
                print(f"  ✗ Python运行失败: {output}")
                results.append((test_file, False, f"Python运行失败: {output}"))
        else:
            print(f"跳过不存在的文件: {test_file}")
    
    return results

def generate_comprehensive_tests():
    """生成全面的测试用例"""
    print("\n生成全面的测试用例...")
    
    test_dir = "comprehensive_test_cases"
    os.makedirs(test_dir, exist_ok=True)
    
    generated_files = []
    
    for test_name, py_code, expected in working_test_cases:
        # 创建Python测试文件
        py_file = os.path.join(test_dir, f"{test_name}.py")
        with open(py_file, 'w') as f:
            f.write(py_code + '\n')
        
        # 验证Python代码
        success, output = run_python_code(py_code)
        if success and output == expected:
            print(f"  ✓ 创建 {py_file} (验证通过)")
            generated_files.append((test_name, py_file, py_code, expected))
        else:
            print(f"  ✗ 跳过 {test_name} (验证失败: 期望 '{expected}', 实际 '{output}')")
    
    return generated_files

def run_batch_tests(test_files):
    """批量运行测试"""
    print(f"\n批量运行 {len(test_files)} 个测试...")
    
    results = []
    total = len(test_files)
    passed = 0
    
    for i, (test_name, py_file, py_code, expected) in enumerate(test_files, 1):
        print(f"\n[{i}/{total}] 测试: {test_name}")
        
        # 步骤1: 验证Python代码
        success, py_output = run_python_code(py_code)
        if not success:
            print(f"  ✗ Python代码运行失败: {py_output}")
            results.append((test_name, False, "Python运行失败"))
            continue
        
        if py_output != expected:
            print(f"  ✗ Python输出不匹配: 期望 '{expected}', 实际 '{py_output}'")
            results.append((test_name, False, "Python输出不匹配"))
            continue
        
        print(f"  ✓ Python验证通过: {py_output}")
        
        # 步骤2: 尝试编译到C++
        try:
            cpp_file = f"{test_name}.cpp"
            result = subprocess.run(['cabal', 'run', 'fluxus', '--', '--python', py_file, '--stop-at-codegen'],
                                  capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0:
                # 保存C++输出
                with open(cpp_file, 'w') as f:
                    f.write(result.stdout)
                
                print(f"  ✓ 编译成功: {cpp_file}")
                passed += 1
                results.append((test_name, True, "编译成功"))
            else:
                print(f"  ✗ 编译失败: {result.stderr[:100]}...")
                results.append((test_name, False, f"编译失败: {result.stderr[:100]}"))
        
        except subprocess.TimeoutExpired:
            print(f"  ✗ 编译超时")
            results.append((test_name, False, "编译超时"))
        except Exception as e:
            print(f"  ✗ 编译错误: {str(e)[:100]}...")
            results.append((test_name, False, f"编译错误: {str(e)}"))
    
    # 总结
    print(f"\n{'='*50}")
    print("批量测试总结")
    print(f"{'='*50}")
    print(f"总测试数: {total}")
    print(f"成功: {passed}")
    print(f"失败: {total - passed}")
    
    if passed < total:
        print("\n失败的测试:")
        for test_name, success, reason in results:
            if not success:
                print(f"  ✗ {test_name}: {reason}")
    
    success_rate = (passed / total) * 100 if total > 0 else 0
    print(f"\n成功率: {success_rate:.1f}%")
    
    return results

def create_test_summary(results):
    """创建测试总结报告"""
    summary = f"""# Python到C++编译器测试总结

## 测试概览
- 总测试数: {len(results)}
- 成功数: {sum(1 for _, success, _ in results if success)}
- 失败数: {sum(1 for _, success, _ in results if not success)}
- 成功率: {(sum(1 for _, success, _ in results if success) / len(results) * 100):.1f}%

## 测试结果详情

"""
    
    for test_name, success, reason in results:
        status = "✓ 成功" if success else "✗ 失败"
        summary += f"### {test_name}\n"
        summary += f"- 状态: {status}\n"
        summary += f"- 原因: {reason}\n\n"
    
    # 保存总结
    with open("test_summary.md", 'w') as f:
        f.write(summary)
    
    print(f"\n测试总结已保存到: test_summary.md")

def main():
    """主函数"""
    print("改进的Python到C++编译器测试套件")
    print("="*50)
    
    # 检查环境
    if not os.path.exists("fluxus.cabal"):
        print("错误: 请在Fluxus项目根目录运行此脚本")
        return 1
    
    # 构建项目
    print("\n构建Fluxus编译器...")
    result = subprocess.run(['cabal', 'build'], capture_output=True, text=True)
    if result.returncode != 0:
        print(f"构建失败: {result.stderr}")
        return 1
    print("✓ 构建成功")
    
    # 测试现有文件
    existing_results = test_existing_files()
    
    # 生成全面测试
    generated_tests = generate_comprehensive_tests()
    
    # 运行批量测试
    if generated_tests:
        batch_results = run_batch_tests(generated_tests)
        
        # 创建总结
        all_results = existing_results + [(name, success, reason) for name, success, reason in batch_results]
        create_test_summary(all_results)
    
    print("\n测试完成!")
    return 0

if __name__ == "__main__":
    sys.exit(main())
