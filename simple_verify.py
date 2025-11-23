#!/usr/bin/env python3
"""简单验证脚本"""

import os
import shutil
import subprocess
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
