#!/usr/bin/env python3

# 针对Python到C++编译器的目标测试套件
# 专注于测试已支持的功能和边界情况

import os
import subprocess
import sys

# 核心支持的Python功能测试用例
core_test_cases = [
    # 基础语法和打印
    ("basic_print", "print(42)", "42"),
    ("print_string", 'print("hello")', "hello"),
    ("print_true", "print(True)", "True"),
    ("print_false", "print(False)", "False"),
    
    # 变量赋值
    ("var_int", "x = 100\nprint(x)", "100"),
    ("var_string", 'x = "world"\nprint(x)', "world"),
    ("var_bool", "x = True\nprint(x)", "True"),
    ("var_reassign", "x = 10\nx = 20\nprint(x)", "20"),
    
    # 算术运算
    ("addition", "a = 5\nb = 3\nprint(a + b)", "8"),
    ("subtraction", "a = 5\nb = 3\nprint(a - b)", "2"),
    ("multiplication", "a = 5\nb = 3\nprint(a * b)", "15"),
    ("division", "a = 6\nb = 3\nprint(a / b)", "2.0"),
    ("complex_math", "a = 2\nb = 3\nc = 4\nprint(a + b * c - 1)", "13"),
    
    # 比较运算
    ("equal_true", "a = 5\nb = 5\nprint(a == b)", "True"),
    ("equal_false", "a = 5\nb = 3\nprint(a == b)", "False"),
    ("not_equal_true", "a = 5\nb = 3\nprint(a != b)", "True"),
    ("less_than_true", "a = 3\nb = 5\nprint(a < b)", "True"),
    ("less_than_false", "a = 5\nb = 3\nprint(a < b)", "False"),
    ("greater_than_true", "a = 5\nb = 3\nprint(a > b)", "True"),
    ("greater_than_false", "a = 3\nb = 5\nprint(a > b)", "False"),
    ("less_equal_true", "a = 3\nb = 3\nprint(a <= b)", "True"),
    ("less_equal_false", "a = 5\nb = 3\nprint(a <= b)", "False"),
    ("greater_equal_true", "a = 5\nb = 5\nprint(a >= b)", "True"),
    ("greater_equal_false", "a = 3\nb = 5\nprint(a >= b)", "False"),
    
    # 逻辑运算
    ("and_true", "a = True\nb = True\nprint(a and b)", "True"),
    ("and_false", "a = True\nb = False\nprint(a and b)", "False"),
    ("or_true", "a = True\nb = False\nprint(a or b)", "True"),
    ("or_false", "a = False\nb = False\nprint(a or b)", "False"),
    ("not_true", "a = True\nprint(not a)", "False"),
    ("not_false", "a = False\nprint(not a)", "True"),
    
    # 控制流 - if语句
    ("if_true", "x = 10\nif x > 5:\n    print(1)", "1"),
    ("if_false", "x = 3\nif x > 5:\n    print(1)", ""),
    ("if_else_true", "x = 10\nif x > 5:\n    print(1)\nelse:\n    print(0)", "1"),
    ("if_else_false", "x = 3\nif x > 5:\n    print(1)\nelse:\n    print(0)", "0"),
    ("elif_true", "x = 10\nif x > 15:\n    print(2)\nelif x > 5:\n    print(1)\nelse:\n    print(0)", "1"),
    ("elif_false", "x = 3\nif x > 15:\n    print(2)\nelif x > 5:\n    print(1)\nelse:\n    print(0)", "0"),
    ("nested_if", "x = 15\nif x > 10:\n    if x > 20:\n        print(2)\n    else:\n        print(1)", "1"),
    
    # 控制流 - while循环
    ("while_basic", "i = 0\nwhile i < 3:\n    print(i)\n    i += 1", "0\n1\n2"),
    ("while_zero", "i = 0\nwhile i < 0:\n    print(i)", ""),
    
    # 控制流 - for循环
    ("for_range", "for i in range(3):\n    print(i)", "0\n1\n2"),
    ("for_range_start", "for i in range(1, 4):\n    print(i)", "1\n2\n3"),
    ("for_range_step", "for i in range(0, 6, 2):\n    print(i)", "0\n2\n4"),
    
    # 函数定义和调用
    ("func_no_params", "def hello():\n    return 42\nprint(hello())", "42"),
    ("func_one_param", "def add_one(x):\n    return x + 1\nprint(add_one(5))", "6"),
    ("func_multi_params", "def add(a, b):\n    return a + b\nprint(add(3, 4))", "7"),
    ("func_call_nested", "def add(a, b):\n    return a + b\ndef multiply(a, b):\n    return a * b\nprint(multiply(add(2, 3), 4))", "20"),
    
    # 递归函数
    ("recursive_factorial", "def factorial(n):\n    if n <= 1:\n        return 1\n    return n * factorial(n - 1)\nprint(factorial(5))", "120"),
    ("recursive_fibonacci", "def fib(n):\n    if n <= 1:\n        return n\n    return fib(n - 1) + fib(n - 2)\nprint(fib(6))", "8"),
    
    # 函数中的控制流
    ("func_with_if", "def abs_val(x):\n    if x >= 0:\n        return x\n    else:\n        return -x\nprint(abs_val(-5))", "5"),
    ("func_with_loop", "def sum_range(n):\n    total = 0\n    for i in range(n + 1):\n        total += i\n    return total\nprint(sum_range(3))", "6"),
    
    # 字符串操作
    ("string_concat", 'a = "hello"\nb = "world"\nprint(a + " " + b)', "hello world"),
    ("string_len", 's = "hello"\nprint(len(s))', "5"),
    ("string_index", 's = "hello"\nprint(s[0])', "h"),
    
    # 列表操作（基础）
    ("list_create", "lst = [1, 2, 3]\nprint(lst[0])", "1"),
    ("list_len", "lst = [1, 2, 3]\nprint(len(lst))", "3"),
    
    # 多变量操作
    ("multi_var_ops", "x = 10\ny = 20\nz = x + y\nprint(z)", "30"),
    
    # 复合表达式
    ("complex_condition", "x = 10\ny = 20\nif x > 5 and y > 15:\n    print(1)", "1"),
    
    # 嵌套函数调用
    ("nested_calls", "def add(a, b):\n    return a + b\nprint(add(add(1, 2), 3))", "6"),
]

# 边界情况测试用例
edge_case_tests = [
    # 空值和零值
    ("zero_value", "x = 0\nprint(x)", "0"),
    ("empty_string", 's = ""\nprint(len(s))', "0"),
    
    # 极值
    ("large_number", "print(999999)", "999999"),
    ("negative_number", "print(-42)", "-42"),
    
    # 布尔值的数学运算
    ("bool_math", "a = True\nb = False\nprint(int(a + b))", "1"),
    
    # 深度嵌套
    ("deep_nesting", "if True:\n    if True:\n        if True:\n            print('deep')", "deep"),
    
    # 复杂逻辑表达式
    ("complex_logic", "a = True\nb = False\nc = True\nprint(a and b or c)", "True"),
    
    # 多重比较
    ("chain_comparison", "x = 3\nprint(1 < x < 5)", "True"),
    
    # 函数参数边界
    ("func_edge_params", "def test(x):\n    return x * 2\nprint(test(0))", "0"),
    
    # 循环边界
    ("loop_edge", "for i in range(1):\n    print(i)", "0"),
    ("loop_empty", "for i in range(0):\n    print(i)", ""),
    
    # 递归深度边界
    ("recursion_small", "def fact(n):\n    if n <= 1:\n        return 1\n    return n * fact(n - 1)\nprint(fact(2))", "2"),
    
    # 字符串边界
    ("string_single_char", 's = "a"\nprint(s)', "a"),
    ("string_special_chars", 's = "hello\nworld"\nprint(s)', "hello\nworld"),
]

def run_python_code(code):
    """运行Python代码并获取输出"""
    try:
        result = subprocess.run(['python3', '-c', code], 
                              capture_output=True, text=True)
        if result.returncode == 0:
            return True, result.stdout.strip()
        else:
            return False, result.stderr.strip()
    except Exception as e:
        return False, str(e)

def compile_to_cpp(py_code, output_file):
    """将Python代码编译到C++"""
    try:
        # 创建临时Python文件
        temp_py_file = "temp_test.py"
        with open(temp_py_file, 'w') as f:
            f.write(py_code)
        
        # 使用Fluxus编译器编译
        result = subprocess.run(['cabal', 'run', 'fluxus', '--', '--python', temp_py_file],
                              capture_output=True, text=True)
        
        if result.returncode == 0:
            with open(output_file, 'w') as f:
                f.write(result.stdout)
            return True, "编译成功"
        else:
            return False, result.stderr.strip()
    
    except Exception as e:
        return False, str(e)
    finally:
        # 清理临时文件
        if os.path.exists("temp_test.py"):
            os.remove("temp_test.py")

def check_cpp_syntax(cpp_file):
    """检查C++语法"""
    try:
        result = subprocess.run(['g++', '-std=c++20', '-fsyntax-only', cpp_file],
                              capture_output=True, text=True)
        return result.returncode == 0, result.stderr.strip() if result.returncode != 0 else ""
    except Exception as e:
        return False, str(e)

def compile_cpp(cpp_file, exe_file):
    """编译C++代码"""
    try:
        result = subprocess.run(['g++', '-std=c++20', '-O2', cpp_file, '-o', exe_file],
                              capture_output=True, text=True)
        return result.returncode == 0, result.stderr.strip() if result.returncode != 0 else ""
    except Exception as e:
        return False, str(e)

def run_cpp_exe(exe_file):
    """运行C++可执行文件"""
    try:
        result = subprocess.run([f'./{exe_file}'], capture_output=True, text=True)
        if result.returncode == 0:
            return True, result.stdout.strip()
        else:
            return False, result.stderr.strip()
    except Exception as e:
        return False, str(e)

def run_test_case(test_name, py_code, expected_output):
    """运行单个测试用例"""
    print(f"\n{'='*50}")
    print(f"测试用例: {test_name}")
    print(f"{'='*50}")
    
    # 步骤1: 验证Python代码
    print("步骤1: 验证Python代码")
    success, output = run_python_code(py_code)
    if not success:
        print(f"  ✗ Python代码运行失败: {output}")
        return False, "Python代码运行失败"
    
    print(f"  ✓ Python输出: {output}")
    
    # 步骤2: 编译到C++
    print("步骤2: 编译到C++")
    cpp_file = f"{test_name}.cpp"
    success, message = compile_to_cpp(py_code, cpp_file)
    if not success:
        print(f"  ✗ 编译失败: {message}")
        return False, "编译失败"
    
    print(f"  ✓ 编译成功")
    
    # 步骤3: 检查C++语法
    print("步骤3: 检查C++语法")
    success, message = check_cpp_syntax(cpp_file)
    if not success:
        print(f"  ✗ C++语法错误: {message}")
        return False, "C++语法错误"
    
    print(f"  ✓ C++语法正确")
    
    # 步骤4: 编译C++
    print("步骤4: 编译C++")
    exe_file = f"{test_name}_exe"
    success, message = compile_cpp(cpp_file, exe_file)
    if not success:
        print(f"  ✗ C++编译失败: {message}")
        return False, "C++编译失败"
    
    print(f"  ✓ C++编译成功")
    
    # 步骤5: 运行C++程序
    print("步骤5: 运行C++程序")
    success, cpp_output = run_cpp_exe(exe_file)
    if not success:
        print(f"  ✗ C++程序运行失败: {cpp_output}")
        return False, "C++程序运行失败"
    
    print(f"  ✓ C++输出: {cpp_output}")
    
    # 步骤6: 比较输出
    print("步骤6: 比较输出")
    if output == cpp_output:
        print(f"  ✓✓✓ 输出一致!")
        return True, "测试通过"
    else:
        print(f"  ✗ 输出不一致")
        print(f"    Python: [{output}]")
        print(f"    C++:    [{cpp_output}]")
        return False, "输出不一致"
    
    # 清理文件
    for file in [cpp_file, exe_file]:
        if os.path.exists(file):
            os.remove(file)

def run_test_suite(test_cases, suite_name):
    """运行测试套件"""
    print(f"\n{'='*60}")
    print(f"运行 {suite_name} 测试套件")
    print(f"{'='*60}")
    
    total_tests = len(test_cases)
    passed_tests = 0
    failed_tests = []
    
    for test_name, py_code, expected in test_cases:
        success, message = run_test_case(test_name, py_code, expected)
        if success:
            passed_tests += 1
            print(f"  ✓ 测试通过")
        else:
            failed_tests.append((test_name, message))
            print(f"  ✗ 测试失败: {message}")
    
    # 测试总结
    print(f"\n{'='*60}")
    print(f"{suite_name} 测试总结")
    print(f"{'='*60}")
    print(f"总测试数: {total_tests}")
    print(f"通过: {passed_tests}")
    print(f"失败: {len(failed_tests)}")
    
    if failed_tests:
        print("\n失败的测试:")
        for test_name, reason in failed_tests:
            print(f"  ✗ {test_name}: {reason}")
    
    success_rate = (passed_tests / total_tests) * 100 if total_tests > 0 else 0
    print(f"\n成功率: {success_rate:.1f}%")
    
    return passed_tests, len(failed_tests)

def main():
    """主函数"""
    print("Python到C++编译器目标测试套件")
    print("专注于核心功能和边界情况测试")
    
    # 确保在正确的目录
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
    
    # 运行核心功能测试
    core_passed, core_failed = run_test_suite(core_test_cases, "核心功能")
    
    # 运行边界情况测试
    edge_passed, edge_failed = run_test_suite(edge_case_tests, "边界情况")
    
    # 总体总结
    total_passed = core_passed + edge_passed
    total_failed = core_failed + edge_failed
    total_tests = total_passed + total_failed
    
    print(f"\n{'='*60}")
    print("总体测试总结")
    print(f"{'='*60}")
    print(f"总测试数: {total_tests}")
    print(f"通过: {total_passed}")
    print(f"失败: {total_failed}")
    
    if total_failed == 0:
        print(f"\n🎉 所有测试通过! 编译器工作正常!")
        return 0
    else:
        print(f"\n⚠️  有 {total_failed} 个测试失败")
        return 1

if __name__ == "__main__":
    sys.exit(main())