#!/usr/bin/env python3

# 扩展的Python测试用例生成器
# 为Python到C++编译器创建更全面的测试用例

import os

# 测试用例分类
test_categories = {
    "基础语法": [
        # 简单表达式
        ("basic_int", "42", "42"),
        ("basic_float", "3.14", "3.14"),
        ("basic_string", '"hello"', "hello"),
        ("basic_bool_true", "True", "True"),
        ("basic_bool_false", "False", "False"),
        ("basic_none", "None", "None"),
        
        # 简单打印
        ("print_int", "print(42)", "42"),
        ("print_float", "print(3.14)", "3.14"),
        ("print_string", 'print("hello")', "hello"),
        ("print_bool", "print(True)", "True"),
        ("print_none", "print(None)", "None"),
        
        # 变量赋值
        ("var_assign_int", "x = 10\nprint(x)", "10"),
        ("var_assign_float", "x = 3.14\nprint(x)", "3.14"),
        ("var_assign_string", 'x = "hello"\nprint(x)', "hello"),
        ("var_assign_bool", "x = True\nprint(x)", "True"),
        ("var_assign_none", "x = None\nprint(x)", "None"),
        
        # 多变量赋值
        ("multi_var", "a = 1\nb = 2\nc = 3\nprint(a+b+c)", "6"),
        ("var_reassign", "x = 10\nx = 20\nprint(x)", "20"),
    ],
    
    "算术运算": [
        # 基础运算
        ("add", "a = 5\nb = 3\nprint(a + b)", "8"),
        ("sub", "a = 5\nb = 3\nprint(a - b)", "2"),
        ("mul", "a = 5\nb = 3\nprint(a * b)", "15"),
        ("div", "a = 6\nb = 3\nprint(a / b)", "2.0"),
        ("mod", "a = 5\nb = 3\nprint(a % b)", "2"),
        ("pow", "a = 2\nb = 3\nprint(a ** b)", "8"),
        ("floor_div", "a = 5\nb = 2\nprint(a // b)", "2"),
        
        # 复合运算
        ("compound_add", "x = 5\nx += 3\nprint(x)", "8"),
        ("compound_sub", "x = 5\nx -= 3\nprint(x)", "2"),
        ("compound_mul", "x = 5\nx *= 3\nprint(x)", "15"),
        ("compound_div", "x = 6\nx /= 3\nprint(x)", "2.0"),
        
        # 运算优先级
        ("precedence", "print(2 + 3 * 4)", "14"),
        ("parentheses", "print((2 + 3) * 4)", "20"),
        ("complex_expr", "a = 2\nb = 3\nc = 4\nprint(a + b * c - 1)", "13"),
    ],
    
    "比较运算": [
        # 基础比较
        ("eq_true", "a = 5\nb = 5\nprint(a == b)", "True"),
        ("eq_false", "a = 5\nb = 3\nprint(a == b)", "False"),
        ("ne_true", "a = 5\nb = 3\nprint(a != b)", "True"),
        ("ne_false", "a = 5\nb = 5\nprint(a != b)", "False"),
        ("lt_true", "a = 3\nb = 5\nprint(a < b)", "True"),
        ("lt_false", "a = 5\nb = 3\nprint(a < b)", "False"),
        ("le_true", "a = 3\nb = 3\nprint(a <= b)", "True"),
        ("le_false", "a = 5\nb = 3\nprint(a <= b)", "False"),
        ("gt_true", "a = 5\nb = 3\nprint(a > b)", "True"),
        ("gt_false", "a = 3\nb = 5\nprint(a > b)", "False"),
        ("ge_true", "a = 5\nb = 5\nprint(a >= b)", "True"),
        ("ge_false", "a = 3\nb = 5\nprint(a >= b)", "False"),
        
        # 链式比较
        ("chain_comp", "a = 3\nprint(1 < a < 5)", "True"),
        ("chain_comp_false", "a = 6\nprint(1 < a < 5)", "False"),
    ],
    
    "逻辑运算": [
        # 基础逻辑
        ("and_true", "a = True\nb = True\nprint(a and b)", "True"),
        ("and_false", "a = True\nb = False\nprint(a and b)", "False"),
        ("or_true", "a = True\nb = False\nprint(a or b)", "True"),
        ("or_false", "a = False\nb = False\nprint(a or b)", "False"),
        ("not_true", "a = True\nprint(not a)", "False"),
        ("not_false", "a = False\nprint(not a)", "True"),
        
        # 复合逻辑
        ("complex_logic", "a = True\nb = False\nc = True\nprint(a and b or c)", "True"),
        ("logic_paren", "a = True\nb = False\nc = True\nprint(a and (b or c))", "True"),
    ],
    
    "控制流": [
        # if语句
        ("if_true", "x = 10\nif x > 5:\n    print(1)", "1"),
        ("if_false", "x = 3\nif x > 5:\n    print(1)", ""),
        ("if_else_true", "x = 10\nif x > 5:\n    print(1)\nelse:\n    print(0)", "1"),
        ("if_else_false", "x = 3\nif x > 5:\n    print(1)\nelse:\n    print(0)", "0"),
        ("elif_true", "x = 10\nif x > 15:\n    print(2)\nelif x > 5:\n    print(1)\nelse:\n    print(0)", "1"),
        ("elif_false", "x = 3\nif x > 15:\n    print(2)\nelif x > 5:\n    print(1)\nelse:\n    print(0)", "0"),
        
        # 嵌套if
        ("nested_if", "x = 15\nif x > 10:\n    if x > 20:\n        print(2)\n    else:\n        print(1)", "1"),
        
        # while循环
        ("while_basic", "i = 0\nwhile i < 3:\n    print(i)\n    i += 1", "0\n1\n2"),
        ("while_zero", "i = 0\nwhile i < 0:\n    print(i)", ""),
        
        # for循环
        ("for_range", "for i in range(3):\n    print(i)", "0\n1\n2"),
        ("for_range_start", "for i in range(1, 4):\n    print(i)", "1\n2\n3"),
        ("for_range_step", "for i in range(0, 6, 2):\n    print(i)", "0\n2\n4"),
        
        # break和continue
        ("break_loop", "for i in range(5):\n    if i == 3:\n        break\n    print(i)", "0\n1\n2"),
        ("continue_loop", "for i in range(3):\n    if i == 1:\n        continue\n    print(i)", "0\n2"),
    ],
    
    "函数定义": [
        # 简单函数
        ("func_no_params", "def hello():\n    return 42\nprint(hello())", "42"),
        ("func_one_param", "def add_one(x):\n    return x + 1\nprint(add_one(5))", "6"),
        ("func_multi_params", "def add(a, b):\n    return a + b\nprint(add(3, 4))", "7"),
        
        # 函数调用
        ("func_call_nested", "def add(a, b):\n    return a + b\ndef multiply(a, b):\n    return a * b\nprint(multiply(add(2, 3), 4))", "20"),
        
        # 递归函数
        ("recursive_factorial", "def factorial(n):\n    if n <= 1:\n        return 1\n    return n * factorial(n - 1)\nprint(factorial(5))", "120"),
        ("recursive_fibonacci", "def fib(n):\n    if n <= 1:\n        return n\n    return fib(n - 1) + fib(n - 2)\nprint(fib(6))", "8"),
        
        # 函数中的控制流
        ("func_with_if", "def abs_val(x):\n    if x >= 0:\n        return x\n    else:\n        return -x\nprint(abs_val(-5))", "5"),
        ("func_with_loop", "def sum_range(n):\n    total = 0\n    for i in range(n + 1):\n        total += i\n    return total\nprint(sum_range(3))", "6"),
    ],
    
    "字符串操作": [
        # 基础字符串
        ("string_concat", 'a = "hello"\nb = "world"\nprint(a + " " + b)', "hello world"),
        ("string_repeat", 's = "hi"\nprint(s * 3)', "hihihi"),
        ("string_len", 's = "hello"\nprint(len(s))', "5"),
        
        # 字符串索引
        ("string_index", 's = "hello"\nprint(s[0])', "h"),
        ("string_slice", 's = "hello"\nprint(s[1:4])', "ell"),
        
        # f-string
        ("fstring_basic", 'name = "Alice"\nprint(f"Hello {name}")', "Hello Alice"),
        ("fstring_expr", 'a = 5\nb = 3\nprint(f"{a} + {b} = {a + b}")', "5 + 3 = 8"),
        
        # 字符串方法
        ("string_upper", 's = "hello"\nprint(s.upper())', "HELLO"),
        ("string_lower", 's = "HELLO"\nprint(s.lower())', "hello"),
    ],
    
    "列表操作": [
        # 基础列表
        ("list_create", "lst = [1, 2, 3]\nprint(lst)", "[1, 2, 3]"),
        ("list_index", "lst = [1, 2, 3]\nprint(lst[1])", "2"),
        ("list_len", "lst = [1, 2, 3]\nprint(len(lst))", "3"),
        
        # 列表操作
        ("list_append", "lst = [1, 2]\nlst.append(3)\nprint(lst)", "[1, 2, 3]"),
        ("list_concat", "a = [1, 2]\nb = [3, 4]\nprint(a + b)", "[1, 2, 3, 4]"),
        
        # 列表切片
        ("list_slice", "lst = [1, 2, 3, 4, 5]\nprint(lst[1:4])", "[2, 3, 4]"),
        
        # 列表推导式
        ("list_comp_basic", "numbers = [1, 2, 3]\nevens = [n * 2 for n in numbers]\nprint(evens)", "[2, 4, 6]"),
        ("list_comp_cond", "numbers = [1, 2, 3, 4, 5]\nevens = [n for n in numbers if n % 2 == 0]\nprint(evens)", "[2, 4]"),
    ],
    
    "字典操作": [
        # 基础字典
        ("dict_create", "d = {'a': 1, 'b': 2}\nprint(d)", "{\"a\": 1, \"b\": 2}"),
        ("dict_access", "d = {'a': 1, 'b': 2}\nprint(d['a'])", "1"),
        ("dict_len", "d = {'a': 1, 'b': 2}\nprint(len(d))", "2"),
        
        # 字典操作
        ("dict_assign", "d = {}\nd['key'] = 'value'\nprint(d['key'])", "value"),
        ("dict_keys", "d = {'a': 1, 'b': 2}\nprint(list(d.keys()))", "['a', 'b']"),
        ("dict_values", "d = {'a': 1, 'b': 2}\nprint(list(d.values()))", "[1, 2]"),
    ],
    
    "集合操作": [
        # 基础集合
        ("set_create", "s = {1, 2, 3}\nprint(s)", "{1, 2, 3}"),
        ("set_len", "s = {1, 2, 3}\nprint(len(s))", "3"),
        
        # 集合操作
        ("set_add", "s = {1, 2}\ns.add(3)\nprint(s)", "{1, 2, 3}"),
        ("set_union", "a = {1, 2}\nb = {2, 3}\nprint(a | b)", "{1, 2, 3}"),
        ("set_intersection", "a = {1, 2}\nb = {2, 3}\nprint(a & b)", "{2}"),
    ],
    
    "元组操作": [
        # 基础元组
        ("tuple_create", "t = (1, 2, 3)\nprint(t)", "(1, 2, 3)"),
        ("tuple_index", "t = (1, 2, 3)\nprint(t[1])", "2"),
        ("tuple_len", "t = (1, 2, 3)\nprint(len(t))", "3"),
        
        # 元组解包
        ("tuple_unpack", "t = (1, 2)\na, b = t\nprint(a, b)", "1 2"),
    ],
    
    "高级特性": [
        # 列表推导式（高级）
        ("list_comp_nested", "matrix = [[1, 2], [3, 4]]\nflat = [x for row in matrix for x in row]\nprint(flat)", "[1, 2, 3, 4]"),
        
        # 字典推导式
        ("dict_comp", "keys = ['a', 'b']\nvalues = [1, 2]\nd = {k: v for k, v in zip(keys, values)}\nprint(d)", "{\"a\": 1, \"b\": 2}"),
        
        # 集合推导式
        ("set_comp", "numbers = [1, 2, 3, 2, 1]\nunique = {n for n in numbers}\nprint(unique)", "{1, 2, 3}"),
        
        # 生成器表达式
        ("gen_expr", "squares = (x**2 for x in range(3))\nprint(list(squares))", "[0, 1, 4]"),
        
        # lambda函数
        ("lambda_basic", "add = lambda x, y: x + y\nprint(add(3, 4))", "7"),
        ("lambda_map", "numbers = [1, 2, 3]\nsquares = list(map(lambda x: x**2, numbers))\nprint(squares)", "[1, 4, 9]"),
    ],
    
    "错误处理": [
        # try-except
        ("try_except", "try:\n    x = 1 / 0\nexcept:\n    print('error')", "error"),
        ("try_except_specific", "try:\n    x = 1 / 0\nexcept ZeroDivisionError:\n    print('zero error')", "zero error"),
        
        # try-finally
        ("try_finally", "try:\n    print('try')\nfinally:\n    print('finally')", "try\nfinally"),
    ],
    
    "类和对象": [
        # 简单类
        ("class_basic", "class Point:\n    def __init__(self, x, y):\n        self.x = x\n        self.y = y\np = Point(1, 2)\nprint(p.x, p.y)", "1 2"),
        
        # 类方法
        ("class_method", "class Counter:\n    def __init__(self):\n        self.count = 0\n    def increment(self):\n        self.count += 1\nc = Counter()\nc.increment()\nprint(c.count)", "1"),
        
        # 类继承
        ("class_inherit", "class Animal:\n    def speak(self):\n        return 'sound'\nclass Dog(Animal):\n    def speak(self):\n        return 'woof'\nd = Dog()\nprint(d.speak())", "woof"),
    ],
    
    "模块和导入": [
        # 基础导入
        ("import_basic", "import math\nprint(math.sqrt(16))", "4.0"),
        
        # 从模块导入
        ("from_import", "from math import sqrt\nprint(sqrt(25))", "5.0"),
        
        # 导入别名
        ("import_alias", "import math as m\nprint(m.sqrt(36))", "6.0"),
    ],
    
    "内置函数": [
        # 常用内置函数
        ("builtin_abs", "print(abs(-5))", "5"),
        ("builtin_max", "print(max(1, 5, 3))", "5"),
        ("builtin_min", "print(min(1, 5, 3))", "1"),
        ("builtin_sum", "print(sum([1, 2, 3]))", "6"),
        ("builtin_sorted", "print(sorted([3, 1, 2]))", "[1, 2, 3]"),
        ("builtin_range", "print(list(range(3)))", "[0, 1, 2]"),
        ("builtin_enumerate", "print(list(enumerate(['a', 'b'])))", "[(0, 'a'), (1, 'b')]"),
        ("builtin_zip", "print(list(zip([1, 2], ['a', 'b'])))", "[(1, 'a'), (2, 'b')]"),
    ],
    
    "边界情况": [
        # 空数据结构
        ("empty_list", "lst = []\nprint(len(lst))", "0"),
        ("empty_dict", "d = {}\nprint(len(d))", "0"),
        ("empty_string", 's = ""\nprint(len(s))', "0"),
        
        # 极值
        ("large_number", "print(999999999)", "999999999"),
        ("small_number", "print(0.000001)", "0.000001"),
        
        # 特殊值
        ("zero_division_check", "try:\n    x = 1 / 0\nexcept ZeroDivisionError:\n    print('caught')", "caught"),
        
        # 深度嵌套
        ("deep_nesting", "if True:\n    if True:\n        if True:\n            print('deep')", "deep"),
    ],
    
    "性能测试": [
        # 大数据量
        ("large_list", "lst = list(range(1000))\nprint(len(lst))", "1000"),
        ("large_loop", "total = 0\nfor i in range(100):\n    total += i\nprint(total)", "4950"),
        
        # 复杂算法
        ("bubble_sort", "def bubble_sort(arr):\n    n = len(arr)\n    for i in range(n):\n        for j in range(0, n-i-1):\n            if arr[j] > arr[j+1]:\n                arr[j], arr[j+1] = arr[j+1], arr[j]\n    return arr\nprint(bubble_sort([3, 1, 4, 2]))", "[1, 2, 3, 4]"),
    ]
}

def create_test_files():
    """创建测试文件"""
    test_dir = "extended_python_tests"
    os.makedirs(test_dir, exist_ok=True)
    
    total_tests = 0
    for category, tests in test_categories.items():
        category_dir = os.path.join(test_dir, category.lower().replace(" ", "_"))
        os.makedirs(category_dir, exist_ok=True)
        
        print(f"\n创建 {category} 类别的测试用例...")
        
        for test_name, code, expected in tests:
            filename = f"test_{test_name}.py"
            filepath = os.path.join(category_dir, filename)
            
            with open(filepath, 'w') as f:
                f.write(code + '\n')
            
            print(f"  ✓ 创建 {filepath}")
            total_tests += 1
    
    print(f"\n总共创建了 {total_tests} 个测试文件")
    return test_dir

def create_test_runner(test_dir):
    """创建测试运行脚本"""
    runner_script = f"""#!/usr/bin/env python3
import os
import subprocess
import sys

def run_test(py_file, expected_output):
    \"\"\"运行单个测试\"\"\"
    try:
        # 运行Python代码
        result = subprocess.run(['python3', py_file], 
                              capture_output=True, text=True)
        if result.returncode != 0:
            return False, f"Python运行错误: {{result.stderr}}"
        
        output = result.stdout.strip()
        if output == expected_output:
            return True, output
        else:
            return False, f"输出不匹配: 期望 '{{expected_output}}', 实际 '{{output}}'"
    
    except Exception as e:
        return False, f"测试错误: {{str(e)}}"

def main():
    test_dir = "{test_dir}"
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
                        print(f"✓ {{test_name}}: 通过")
                        passed_tests += 1
                    else:
                        print(f"✗ {{test_name}}: 失败 - {{message}}")
                        failed_tests.append(test_name)
                
                except Exception as e:
                    print(f"✗ {{test_name}}: 错误 - {{str(e)}}")
                    failed_tests.append(test_name)
                    total_tests += 1
    
    print("=" * 50)
    print(f"测试结果: {{passed_tests}}/{{total_tests}} 通过")
    
    if failed_tests:
        print(f"失败的测试: {{', '.join(failed_tests)}}")
        return 1
    else:
        print("所有测试通过！")
        return 0

if __name__ == "__main__":
    sys.exit(main())
"""
    
    runner_path = os.path.join(test_dir, "run_tests.py")
    with open(runner_path, 'w') as f:
        f.write(runner_script)
    
    os.chmod(runner_path, 0o755)
    print(f"创建测试运行脚本: {runner_path}")
    return runner_path

def create_cpp_verification_script(test_dir):
    """创建C++验证脚本"""
    cpp_script = f"""#!/bin/bash

# Python到C++验证脚本
# 用于验证生成的C++代码是否正确

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TEST_DIR="{test_dir}"

# 颜色定义
RED='\\033[0;31m'
GREEN='\\033[0;32m'
YELLOW='\\033[1;33m'
BLUE='\\033[0;34m'
NC='\\033[0m' # No Color

# 统计变量
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# 测试函数
test_python_to_cpp() {{
    local test_name="$1"
    local py_file="$2"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    echo -e "${{BLUE}}测试 #${{TOTAL_TESTS}}: ${{test_name}}${{NC}}"
    
    local cpp_file="${{py_file}}.cpp"
    local exe_file="${{py_file}}_exe"
    
    # 步骤1: 验证Python代码可以运行
    echo "  步骤1: 验证Python代码"
    if ! python3 "${{py_file}}" > /dev/null 2>&1; then
        echo -e "  ${{RED}}✗ Python代码运行失败${{NC}}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
    echo -e "  ${{GREEN}}✓ Python代码运行成功${{NC}}"
    
    # 步骤2: 编译Python到C++
    echo "  步骤2: 编译到C++"
    if ! cabal run fluxus -- --python "${{py_file}}" > "${{cpp_file}}" 2>&1; then
        echo -e "  ${{RED}}✗ 编译失败${{NC}}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
    echo -e "  ${{GREEN}}✓ 编译成功${{NC}}"
    
    # 步骤3: 检查C++语法
    echo "  步骤3: 检查C++语法"
    if ! g++ -std=c++20 -fsyntax-only "${{cpp_file}}" 2>&1; then
        echo -e "  ${{RED}}✗ C++语法错误${{NC}}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
    echo -e "  ${{GREEN}}✓ C++语法正确${{NC}}"
    
    # 步骤4: 编译C++
    echo "  步骤4: 编译C++"
    if ! g++ -std=c++20 -O2 "${{cpp_file}}" -o "${{exe_file}}" 2>&1; then
        echo -e "  ${{RED}}✗ C++编译失败${{NC}}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
    echo -e "  ${{GREEN}}✓ C++编译成功${{NC}}"
    
    PASSED_TESTS=$((PASSED_TESTS + 1))
    return 0
}}

# 主测试循环
echo "开始Python到C++验证测试..."
echo "=========================================="

# 查找所有Python测试文件
find "${{TEST_DIR}}" -name "test_*.py" | while read -r py_file; do
    test_name=$(basename "${{py_file}}" .py)
    test_python_to_cpp "${{test_name}}" "${{py_file}}"
    echo ""
done

echo "=========================================="
echo "测试总结"
echo "总测试数: ${{TOTAL_TESTS}}"
echo -e "${{GREEN}}通过: ${{PASSED_TESTS}}${{NC}}"
echo -e "${{RED}}失败: ${{FAILED_TESTS}}${{NC}}"

if [ ${{FAILED_TESTS}} -eq 0 ]; then
    echo -e "${{GREEN}}所有测试通过！${{NC}}"
    exit 0
else
    echo -e "${{RED}}有 ${{FAILED_TESTS}} 个测试失败${{NC}}"
    exit 1
fi
"""
    
    script_path = os.path.join(test_dir, "verify_cpp.sh")
    with open(script_path, 'w') as f:
        f.write(cpp_script)
    
    os.chmod(script_path, 0o755)
    print(f"创建C++验证脚本: {script_path}")
    return script_path

if __name__ == "__main__":
    print("创建扩展的Python测试用例...")
    print("=" * 50)
    
    # 创建测试文件
    test_dir = create_test_files()
    
    # 创建测试运行脚本
    create_test_runner(test_dir)
    
    # 创建C++验证脚本
    create_cpp_verification_script(test_dir)
    
    print("\n" + "=" * 50)
    print("测试用例创建完成！")
    print(f"测试目录: {test_dir}")
    print("\n使用方法:")
    print(f"1. 运行Python测试: python3 {test_dir}/run_tests.py")
    print(f"2. 验证C++编译: bash {test_dir}/verify_cpp.sh")
    print("=" * 50)
