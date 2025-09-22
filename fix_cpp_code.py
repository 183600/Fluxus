#!/usr/bin/env python3
"""
修复Fluxus生成的C++代码中的问题
"""

import re
import sys
import os

def fix_cpp_code(cpp_file_path):
    """修复C++代码中的fmt.Println问题"""

    if not os.path.exists(cpp_file_path):
        print(f"错误：文件 {cpp_file_path} 不存在")
        return False

    # 读取原始文件
    with open(cpp_file_path, 'r') as f:
        content = f.read()

    original_content = content

    print("=== 开始修复C++代码 ===")
    print(f"原始文件: {cpp_file_path}")
    print(f"原始文件大小: {len(content)} 字符")

    # 修复1: 将 /* expr */("...") 替换为正确的输出语句
    def replace_expr_comments(match):
        expr_content = match.group(1)
        print(f"发现表达式注释: /* expr */{expr_content}")

        # 如果是字符串字面量，转换为输出语句
        if expr_content.startswith('("') and expr_content.endswith('")'):
            string_content = expr_content[1:-1]  # 去掉引号
            print(f"  -> 转换为输出语句: std::cout << \"{string_content}\" << std::endl")
            return f'std::cout << "{string_content}" << std::endl;'

        # 如果是变量或复杂表达式，使用通用处理
        print(f"  -> 转换为通用输出: std::cout << {expr_content} << std::endl")
        return f'std::cout << {expr_content} << std::endl;'

    # 应用修复
    pattern = r'/\* expr \*/\((.*?)\)'
    fixed_content = re.sub(pattern, replace_expr_comments, content)

    # 修复2: 添加缺失的include语句
    if '#include <iostream>' not in fixed_content:
        fixed_content = fixed_content.replace('#include <string>', '#include <string>\n#include <iostream>')
        print("添加缺失的 #include <iostream>")

    # 修复3: 移除main函数外的无效void __temp;声明
    def remove_invalid_temp_declarations(content):
        lines = content.split('\n')
        cleaned_lines = []
        in_main_function = False
        main_brace_count = 0

        for line in lines:
            stripped = line.strip()

            # 检测是否进入main函数
            if 'int main()' in stripped:
                in_main_function = True
                cleaned_lines.append(line)
                continue

            # 在main函数内，计数大括号
            if in_main_function:
                if '{' in stripped:
                    main_brace_count += 1
                if '}' in stripped:
                    main_brace_count -= 1
                    if main_brace_count == 0:
                        in_main_function = False

                # 只保留非void __temp;的行
                if stripped != 'void __temp;':
                    cleaned_lines.append(line)
            else:
                # 在main函数外，跳过void __temp;声明
                if stripped != 'void __temp;':
                    cleaned_lines.append(line)

        return '\n'.join(cleaned_lines)

    fixed_content = remove_invalid_temp_declarations(fixed_content)
    print("移除了无效的 void __temp; 声明")

    # 修复4: 确保main函数有正确的return语句
    if 'int main()' in fixed_content and 'return 0;' not in fixed_content:
        fixed_content = fixed_content.replace('}', '\n    return 0;\n}')
        print("添加缺失的 return 0;")

    # 统计修复内容
    changes_made = fixed_content != original_content
    print(f"是否进行了修改: {'是' if changes_made else '否'}")

    if changes_made:
        # 创建备份
        backup_path = cpp_file_path + '.backup'
        with open(backup_path, 'w') as f:
            f.write(original_content)
        print(f"原始文件已备份到: {backup_path}")

        # 写入修复后的内容
        with open(cpp_file_path, 'w') as f:
            f.write(fixed_content)

        print("=== C++代码修复完成 ===")
        print(f"修复后的文件: {cpp_file_path}")
        print(f"修复后文件大小: {len(fixed_content)} 字符")
        print("\n=== 修复后的代码 ===")
        print(fixed_content)
        print("======================")

        return True
    else:
        print("未发现需要修复的问题")
        return False

def compile_and_test(cpp_file_path):
    """编译并测试C++代码"""

    # 编译C++代码
    exe_path = cpp_file_path.replace('.cpp', '_compiled')
    compile_cmd = f'g++ -o {exe_path} {cpp_file_path}'

    print(f"\n=== 编译C++代码 ===")
    print(f"编译命令: {compile_cmd}")

    result = os.system(compile_cmd)
    if result != 0:
        print(f"编译失败，退出码: {result}")
        return False

    print("编译成功！")

    # 运行测试
    print(f"\n=== 运行C++程序 ===")
    print(f"运行命令: {exe_path}")

    result = os.system(exe_path)
    if result != 0:
        print(f"运行失败，退出码: {result}")
        return False

    print("运行成功！")
    return True

def main():
    """主函数"""

    if len(sys.argv) != 2:
        print("用法: python3 fix_cpp_code.py <cpp文件路径>")
        sys.exit(1)

    cpp_file = sys.argv[1]

    # 修复代码
    if fix_cpp_code(cpp_file):
        # 编译和测试
        compile_and_test(cpp_file)
    else:
        print("无需修复，直接编译和测试")
        compile_and_test(cpp_file)

if __name__ == "__main__":
    main()