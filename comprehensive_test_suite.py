#!/usr/bin/env python3
"""
全面的Python到C++编译测试套件
确保生成的C++代码没有语法错误且功能正确
"""

import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent
_HASKELL_BIN_DIRS = [Path.home() / ".ghcup" / "bin", Path.home() / ".cabal" / "bin"]


def _prepend_to_path(directory: Path) -> None:
    directory_str = str(directory)
    current_path = os.environ.get("PATH", "")
    segments = current_path.split(os.pathsep) if current_path else []
    if directory_str not in segments:
        os.environ["PATH"] = os.pathsep.join([directory_str] + segments) if segments else directory_str


for candidate in _HASKELL_BIN_DIRS:
    _prepend_to_path(candidate)


def ensure_haskell_toolchain() -> None:
    has_cabal = shutil.which("cabal") is not None
    has_ghc = shutil.which("ghc") is not None
    if has_cabal and has_ghc:
        return

    ensure_script = PROJECT_ROOT / "ensure_haskell_toolchain.sh"
    if ensure_script.exists():
        print("未检测到完整的 Haskell 工具链，正在执行 ensure_haskell_toolchain.sh ...")
        subprocess.run(["bash", str(ensure_script)], check=True)
    else:
        missing = []
        if not has_cabal:
            missing.append("cabal")
        if not has_ghc:
            missing.append("ghc")
        missing_text = "、".join(missing) if missing else "Haskell 工具链"
        raise RuntimeError(f"缺少 {missing_text}，且未找到 {ensure_script}")

    if shutil.which("cabal") is None or shutil.which("ghc") is None:
        raise RuntimeError("ensure_haskell_toolchain.sh 执行后仍无法找到 cabal/ghc。")


ensure_haskell_toolchain()

class Colors:
    GREEN = '\033[0;32m'
    RED = '\033[0;31m'
    BLUE = '\033[0;34m'
    YELLOW = '\033[1;33m'
    NC = '\033[0m'

def print_header(text):
    print(f"\n{Colors.BLUE}{'='*60}{Colors.NC}")
    print(f"{Colors.BLUE}{text}{Colors.NC}")
    print(f"{Colors.BLUE}{'='*60}{Colors.NC}\n")

def print_success(text):
    print(f"{Colors.GREEN}✓ {text}{Colors.NC}")

def print_error(text):
    print(f"{Colors.RED}✗ {text}{Colors.NC}")

def print_info(text):
    print(f"{Colors.YELLOW}ℹ {text}{Colors.NC}")

# 测试用例定义
TEST_CASES = [
    {
        "name": "简单打印",
        "code": "print(42)",
        "expected": "42\n"
    },
    {
        "name": "变量赋值",
        "code": "x = 100\nprint(x)",
        "expected": "100\n"
    },
    {
        "name": "算术运算",
        "code": "a = 10\nb = 5\nc = a + b\nprint(c)",
        "expected": "15\n"
    },
    {
        "name": "比较运算符 - 大于",
        "code": "x = 10\nif x > 5:\n    print(1)\nelse:\n    print(0)",
        "expected": "1\n"
    },
    {
        "name": "比较运算符 - 小于等于",
        "code": "x = 5\nif x <= 5:\n    print(1)\nelse:\n    print(0)",
        "expected": "1\n"
    },
    {
        "name": "比较运算符 - 等于",
        "code": "x = 10\nif x == 10:\n    print(1)\nelse:\n    print(0)",
        "expected": "1\n"
    },
    {
        "name": "简单函数",
        "code": """def add(a, b):
    return a + b

result = add(10, 20)
print(result)""",
        "expected": "30\n"
    },
    {
        "name": "递归函数 - 阶乘",
        "code": """def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))""",
        "expected": "120\n"
    },
    {
        "name": "递归函数 - 斐波那契",
        "code": """def fib(n):
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)

print(fib(7))""",
        "expected": "13\n"
    },
    {
        "name": "for循环",
        "code": """for i in range(3):
    print(i)""",
        "expected": "0\n1\n2\n"
    },
    {
        "name": "while循环",
        "code": """i = 0
while i < 3:
    print(i)
    i = i + 1""",
        "expected": "0\n1\n2\n"
    },
    {
        "name": "多个if-else",
        "code": """x = 15
if x > 20:
    print(1)
elif x > 10:
    print(2)
else:
    print(3)""",
        "expected": "2\n"
    }
]

def run_command(cmd, input_text=None, capture_output=True):
    """运行命令并返回结果"""
    try:
        result = subprocess.run(
            cmd,
            input=input_text,
            capture_output=capture_output,
            text=True,
            timeout=10
        )
        return result.returncode, result.stdout, result.stderr
    except subprocess.TimeoutExpired:
        return -1, "", "Timeout"
    except Exception as e:
        return -1, "", str(e)

def test_python_to_cpp(test_case, test_num):
    """测试单个Python到C++的编译"""
    print(f"\n{Colors.BLUE}测试 {test_num}: {test_case['name']}{Colors.NC}")
    print("-" * 60)
    
    with tempfile.TemporaryDirectory() as tmpdir:
        # 创建Python文件
        py_file = os.path.join(tmpdir, f"test_{test_num}.py")
        cpp_file = os.path.join(tmpdir, f"test_{test_num}.cpp")
        exe_file = os.path.join(tmpdir, f"test_{test_num}")
        
        with open(py_file, 'w') as f:
            f.write(test_case['code'])
        
        # 步骤1: 运行Python获取预期输出
        print_info("步骤1: 运行Python代码...")
        ret, py_output, py_err = run_command(['python3', py_file])
        if ret != 0:
            print_error(f"Python执行失败: {py_err}")
            return False
        print_success(f"Python输出: {repr(py_output)}")
        
        # 步骤2: 编译Python到C++
        print_info("步骤2: 编译Python到C++...")
        ret, cpp_code, compile_err = run_command(
            ['cabal', 'run', 'fluxus', '--', '--python', py_file]
        )
        
        if ret != 0:
            print_error(f"编译失败: {compile_err}")
            return False
        
        if not cpp_code or len(cpp_code) < 50:
            print_error("生成的C++代码为空或太短")
            return False
        
        # 保存C++代码
        with open(cpp_file, 'w') as f:
            f.write(cpp_code)
        
        print_success("C++代码已生成")
        
        # 检查是否有TODO注释（表示未实现的功能）
        if 'TODO' in cpp_code and 'PyComparison' in cpp_code:
            print_error("发现未实现的比较运算符（TODO注释）")
            print(f"C++代码片段:\n{cpp_code[:500]}")
            return False
        
        # 步骤3: 检查C++语法
        print_info("步骤3: 检查C++语法...")
        ret, _, syntax_err = run_command(
            ['g++', '-std=c++20', '-fsyntax-only', cpp_file]
        )
        
        if ret != 0:
            print_error(f"C++语法错误: {syntax_err}")
            print(f"C++代码:\n{cpp_code}")
            return False
        
        print_success("C++语法正确")
        
        # 步骤4: 编译C++
        print_info("步骤4: 编译C++代码...")
        ret, _, compile_err = run_command(
            ['g++', '-std=c++20', '-O2', cpp_file, '-o', exe_file]
        )
        
        if ret != 0:
            print_error(f"C++编译失败: {compile_err}")
            return False
        
        print_success("C++编译成功")
        
        # 步骤5: 运行C++程序
        print_info("步骤5: 运行C++程序...")
        ret, cpp_output, run_err = run_command([exe_file])
        
        if ret != 0:
            print_error(f"C++程序运行失败: {run_err}")
            return False
        
        print_success(f"C++输出: {repr(cpp_output)}")
        
        # 步骤6: 比较输出
        print_info("步骤6: 比较输出...")
        if py_output == cpp_output:
            print_success("输出完全一致！")
            return True
        else:
            print_error("输出不一致！")
            print(f"  Python: {repr(py_output)}")
            print(f"  C++:    {repr(cpp_output)}")
            return False

def main():
    print_header("Fluxus Python到C++编译全面测试")
    
    # 检查依赖
    print_info("检查依赖...")
    for cmd in ['python3', 'cabal', 'g++']:
        ret, _, _ = run_command(['which', cmd])
        if ret != 0:
            print_error(f"未找到命令: {cmd}")
            sys.exit(1)
    print_success("所有依赖已安装")
    
    # 构建项目
    print_info("构建Fluxus编译器...")
    ret, _, build_err = run_command(['cabal', 'build'])
    if ret != 0:
        print_error(f"构建失败: {build_err}")
        sys.exit(1)
    print_success("编译器构建成功")
    
    # 运行测试
    passed = 0
    failed = 0
    
    for i, test_case in enumerate(TEST_CASES, 1):
        try:
            if test_python_to_cpp(test_case, i):
                passed += 1
                print(f"{Colors.GREEN}✓✓✓ 测试 {i} 通过！{Colors.NC}")
            else:
                failed += 1
                print(f"{Colors.RED}✗✗✗ 测试 {i} 失败！{Colors.NC}")
        except Exception as e:
            failed += 1
            print_error(f"测试 {i} 异常: {e}")
    
    # 总结
    print_header("测试总结")
    total = passed + failed
    print(f"总测试数: {total}")
    print(f"{Colors.GREEN}通过: {passed}{Colors.NC}")
    print(f"{Colors.RED}失败: {failed}{Colors.NC}")
    print(f"成功率: {passed*100//total if total > 0 else 0}%")
    
    if failed == 0:
        print(f"\n{Colors.GREEN}{'='*60}")
        print("🎉 所有测试通过！Python到C++编译功能完全正常！")
        print(f"{'='*60}{Colors.NC}\n")
        return 0
    else:
        print(f"\n{Colors.RED}{'='*60}")
        print(f"⚠️  有 {failed} 个测试失败，需要修复")
        print(f"{'='*60}{Colors.NC}\n")
        return 1

if __name__ == '__main__':
    sys.exit(main())
