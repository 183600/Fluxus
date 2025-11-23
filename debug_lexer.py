#!/usr/bin/env python3
"""Debug script to test Python and Go lexers on example files."""

import os
import shutil
import subprocess
import sys
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

def test_python_lexer():
    """Test Python lexer on fibonacci.py"""
    try:
        with open('examples/python/fibonacci.py', 'r') as f:
            content = f.read()
        print("=== Python Fibonacci Content ===")
        print(content)
        print("\n=== Testing Python lexer ===")
        
        # Try to run the lexer
        result = subprocess.run(['cabal', 'run', 'fluxus', '--', 'examples/python/fibonacci.py'], 
                              capture_output=True, text=True, timeout=10)
        print("Return code:", result.returncode)
        print("STDOUT:", result.stdout)
        print("STDERR:", result.stderr)
        
    except Exception as e:
        print(f"Error testing Python lexer: {e}")

def test_go_lexer():
    """Test Go lexer on fibonacci.go"""
    try:
        with open('examples/go/fibonacci.go', 'r') as f:
            content = f.read()
        print("\n=== Go Fibonacci Content ===")
        print(content)
        print("\n=== Testing Go lexer ===")
        
        # Try to run the lexer
        result = subprocess.run(['cabal', 'run', 'fluxus', '--', 'examples/go/fibonacci.go'], 
                              capture_output=True, text=True, timeout=10)
        print("Return code:", result.returncode)
        print("STDOUT:", result.stdout)
        print("STDERR:", result.stderr)
        
    except Exception as e:
        print(f"Error testing Go lexer: {e}")

if __name__ == "__main__":
    test_python_lexer()
    test_go_lexer()