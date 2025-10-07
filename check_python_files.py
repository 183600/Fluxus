#!/usr/bin/env python3
import os
import sys
import subprocess

def check_python_file(filepath):
    """检查Python文件的语法，如果有错误返回错误信息，否则返回None"""
    try:
        result = subprocess.run(
            [sys.executable, '-m', 'py_compile', filepath],
            capture_output=True,
            text=True,
            timeout=10
        )
        if result.returncode != 0:
            return f"语法错误: {filepath}\n{result.stderr}"
        return None
    except subprocess.TimeoutExpired:
        return f"超时: {filepath}"
    except Exception as e:
        return f"检查失败: {filepath} - {str(e)}"

def main():
    """遍历项目中的所有Python文件并检查语法"""
    project_root = os.path.dirname(os.path.abspath(__file__))

    for root, dirs, files in os.walk(project_root):
        for file in files:
            if file.endswith('.py'):
                filepath = os.path.join(root, file)
                error = check_python_file(filepath)
                if error:
                    print(error)

if __name__ == "__main__":
    main()