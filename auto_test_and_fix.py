#!/usr/bin/env python3
"""
自动测试和修复Python到C++编译
"""

import os
import shutil
import subprocess
import sys
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent
_PATH_CANDIDATES = [Path.home() / ".ghcup" / "bin", Path.home() / ".cabal" / "bin"]


def _prepend_to_path(directory: Path) -> None:
    directory_str = str(directory)
    current_path = os.environ.get("PATH", "")
    segments = current_path.split(os.pathsep) if current_path else []
    if directory_str not in segments:
        os.environ["PATH"] = os.pathsep.join([directory_str] + segments) if segments else directory_str


for candidate in _PATH_CANDIDATES:
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
