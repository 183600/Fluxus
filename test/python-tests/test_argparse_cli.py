#!/usr/bin/env python3
"""
测试Python的命令行参数处理：argparse模块
"""

import argparse
import sys
import os
from datetime import datetime

def test_basic_argparse():
    """测试基本的argparse功能"""
    print("=== Testing Basic argparse Functionality ===")
    
    # 创建基本的参数解析器
    parser = argparse.ArgumentParser(
        description='Sample CLI application for testing argparse',
        epilog='For more information, visit https://example.com'
    )
    
    # 添加位置参数
    parser.add_argument('filename', help='Input file to process')
    parser.add_argument('output', nargs='?', help='Output file (optional)')
    
    # 添加可选参数
    parser.add_argument('-v', '--verbose', action='store_true', help='Enable verbose output')
    parser.add_argument('-n', '--number', type=int, default=10, help='Number of iterations (default: 10)')
    parser.add_argument('--mode', choices=['fast', 'slow', 'normal'], default='normal', help='Processing mode')
    
    # 模拟命令行参数
    test_args = ['input.txt', 'output.txt', '--verbose', '--number', '20', '--mode', 'fast']
    
    try:
        args = parser.parse_args(test_args)
        
        print(f"Parsed arguments:")
        print(f"  filename: {args.filename}")
        print(f"  output: {args.output}")
        print(f"  verbose: {args.verbose}")
        print(f"  number: {args.number}")
        print(f"  mode: {args.mode}")
        
    except SystemExit:
        print("Argument parsing failed")

def test_advanced_argument_types():
    """测试高级参数类型"""
    print("\n=== Testing Advanced Argument Types ===")
    
    parser = argparse.ArgumentParser(description='Advanced argument types demo')
    
    # 文件类型参数
    parser.add_argument('-f', '--file', type=argparse.FileType('r'), help='Input file')
    parser.add_argument('-o', '--output', type=argparse.FileType('w'), help='Output file')
    
    # 自定义类型验证
    def positive_int(value):
        """自定义类型：正整数"""
        ivalue = int(value)
        if ivalue <= 0:
            raise argparse.ArgumentTypeError(f"{value} must be a positive integer")
        return ivalue
    
    def valid_date(value):
        """自定义类型：日期"""
        try:
            return datetime.strptime(value, '%Y-%m-%d')
        except ValueError:
            raise argparse.ArgumentTypeError(f"Invalid date format: {value}. Use YYYY-MM-DD")
    
    parser.add_argument('--threads', type=positive_int, default=4, help='Number of threads (positive integer)')
    parser.add_argument('--date', type=valid_date, help='Target date (YYYY-MM-DD)')
    parser.add_argument('--ratio', type=float, choices=[x/10 for x in range(11)], help='Ratio (0.0-1.0)')
    
    # 列表参数
    parser.add_argument('--tags', nargs='+', help='List of tags')
    parser.add_argument('--numbers', nargs='*', type=int, default=[1, 2, 3], help='List of numbers')
    parser.add_argument('--values', nargs=3, type=float, help='Exactly 3 float values')
    
    # 模拟命令行参数
    test_args = [
        '--threads', '8',
        '--date', '2024-01-15',
        '--ratio', '0.7',
        '--tags', 'python', 'argparse', 'cli',
        '--numbers', '10', '20', '30', '40',
        '--values', '1.1', '2.2', '3.3'
    ]
    
    try:
        args = parser.parse_args(test_args)
        
        print(f"Parsed advanced arguments:")
        print(f"  threads: {args.threads}")
        print(f"  date: {args.date}")
        print(f"  date type: {type(args.date)}")
        print(f"  ratio: {args.ratio}")
        print(f"  tags: {args.tags}")
        print(f"  numbers: {args.numbers}")
        print(f"  values: {args.values}")
        
    except SystemExit as e:
        print(f"Argument parsing failed: {e}")

def test_subcommands():
    """测试子命令"""
    print("\n=== Testing Subcommands ===")
    
    # 创建主解析器
    parser = argparse.ArgumentParser(description='Git-like CLI with subcommands')
    subparsers = parser.add_subparsers(dest='command', help='Available commands')
    
    # 添加子命令：init
    init_parser = subparsers.add_parser('init', help='Initialize a new project')
    init_parser.add_argument('project_name', help='Name of the project')
    init_parser.add_argument('--template', choices=['basic', 'advanced'], default='basic', help='Project template')
    init_parser.add_argument('--force', action='store_true', help='Force initialization')
    
    # 添加子命令：add
    add_parser = subparsers.add_parser('add', help='Add files to project')
    add_parser.add_argument('files', nargs='+', help='Files to add')
    add_parser.add_argument('--recursive', '-r', action='store_true', help='Add files recursively')
    add_parser.add_argument('--exclude', action='append', default=[], help='Patterns to exclude')
    
    # 添加子命令：status
    status_parser = subparsers.add_parser('status', help='Show project status')
    status_parser.add_argument('--verbose', '-v', action='store_true', help='Verbose output')
    status_parser.add_argument('--short', '-s', action='store_true', help='Short format')
    
    # 测试不同的子命令
    test_commands = [
        ['init', 'my_project', '--template', 'advanced'],
        ['add', 'file1.py', 'file2.py', '--recursive', '--exclude', '*.pyc'],
        ['status', '--verbose']
    ]
    
    for cmd_args in test_commands:
        print(f"\nTesting command: {' '.join(cmd_args)}")
        try:
            args = parser.parse_args(cmd_args)
            print(f"  Command: {args.command}")
            
            if args.command == 'init':
                print(f"  Project name: {args.project_name}")
                print(f"  Template: {args.template}")
                print(f"  Force: {args.force}")
            elif args.command == 'add':
                print(f"  Files: {args.files}")
                print(f"  Recursive: {args.recursive}")
                print(f"  Exclude: {args.exclude}")
            elif args.command == 'status':
                print(f"  Verbose: {args.verbose}")
                print(f"  Short: {args.short}")
                
        except SystemExit:
            print("  Command parsing failed")

def test_mutually_exclusive_groups():
    """测试互斥参数组"""
    print("\n=== Testing Mutually Exclusive Groups ===")
    
    parser = argparse.ArgumentParser(description='Deployment tool')
    
    # 创建互斥组
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('--dev', action='store_true', help='Deploy to development')
    group.add_argument('--staging', action='store_true', help='Deploy to staging')
    group.add_argument('--prod', action='store_true', help='Deploy to production')
    
    # 另一个互斥组
    format_group = parser.add_mutually_exclusive_group()
    format_group.add_argument('--json', action='store_true', help='JSON output format')
    format_group.add_argument('--yaml', action='store_true', help='YAML output format')
    format_group.add_argument('--xml', action='store_true', help='XML output format')
    
    # 测试有效的组合
    valid_combinations = [
        ['--dev'],
        ['--staging', '--json'],
        ['--prod', '--yaml', '--verbose']
    ]
    
    for args in valid_combinations:
        print(f"\nTesting valid args: {args}")
        try:
            # 添加verbose参数用于某些测试
            if '--verbose' not in args:
                args.append('--verbose')
            
            parsed = parser.parse_args(args)
            print(f"  Environment: dev={parsed.dev}, staging={parsed.staging}, prod={parsed.prod}")
            print(f"  Format: json={parsed.json}, yaml={parsed.yaml}, xml={parsed.xml}")
            
        except SystemExit:
            print("  Unexpected parsing failure")

def test_custom_actions():
    """测试自定义动作"""
    print("\n=== Testing Custom Actions ===")
    
    class StoreNameValue(argparse.Action):
        """自定义动作：存储名称-值对"""
        
        def __call__(self, parser, namespace, values, option_string=None):
            if not hasattr(namespace, self.dest):
                setattr(namespace, self.dest, {})
            
            name, value = values.split('=')
            getattr(namespace, self.dest)[name] = value
    
    parser = argparse.ArgumentParser(description='Custom actions demo')
    
    # 自定义动作
    parser.add_argument('--define', '-D', action=StoreNameValue, dest='definitions',
                       help='Define a name-value pair (format: name=value)')
    
    # 内置的特殊动作
    parser.add_argument('--version', action='version', version='%(prog)s 1.0.0')
    parser.add_argument('--append', action='append', dest='items', default=[],
                       help='Append to list (can be used multiple times)')
    parser.add_argument('--append-const', action='append_const', dest='flags', const='FLAG1',
                       help='Append constant to list')
    parser.add_argument('--count', '-c', action='count', default=0,
                       help='Count occurrences (can be used multiple times)')
    parser.add_argument('--store-const', action='store_const', dest='constant_value', const=42,
                       help='Store a constant value')
    
    # 测试自定义动作
    test_args = [
        '--define', 'key1=value1',
        '--define', 'key2=value2',
        '--append', 'item1',
        '--append', 'item2',
        '--count', '--count', '--count',
        '--store-const'
    ]
    
    try:
        args = parser.parse_args(test_args)
        
        print(f"Custom definitions: {args.definitions}")
        print(f"Appended items: {args.items}")
        print(f"Count value: {args.count}")
        print(f"Constant value: {args.constant_value}")
        
    except SystemExit:
        print("Custom action parsing failed")

def test_argument_validation():
    """测试参数验证和错误处理"""
    print("\n=== Testing Argument Validation ===")
    
    parser = argparse.ArgumentParser(description='Validation demo')
    
    # 必需参数
    parser.add_argument('--required', required=True, help='This argument is required')
    parser.add_argument('--optional', help='This argument is optional')
    
    # 参数依赖
    parser.add_argument('--config', help='Configuration file')
    parser.add_argument('--validate-config', action='store_true', help='Validate configuration')
    
    # 测试必需参数缺失
    print("Testing missing required argument:")
    try:
        args = parser.parse_args(['--optional', 'value'])
        print("ERROR: Should have failed due to missing required argument")
    except SystemExit:
        print("✓ Correctly failed due to missing required argument")
    
    # 测试有效参数
    print("\nTesting valid arguments:")
    try:
        args = parser.parse_args(['--required', 'value', '--config', 'config.json'])
        print(f"✓ Successfully parsed: required={args.required}, config={args.config}")
    except SystemExit:
        print("ERROR: Should have succeeded with valid arguments")

def test_help_generation():
    """测试帮助信息生成"""
    print("\n=== Testing Help Generation ===")
    
    parser = argparse.ArgumentParser(
        prog='my-tool',
        description='A comprehensive CLI tool for testing argparse',
        epilog='For more details, see the documentation at https://example.com/docs'
    )
    
    parser.add_argument('input', help='Input file to process')
    parser.add_argument('-v', '--verbose', action='count', default=0, 
                       help='Increase verbosity (use multiple times for more verbose)')
    parser.add_argument('--config', metavar='FILE', help='Configuration file path')
    parser.add_argument('--timeout', type=float, default=30.0, 
                       help='Timeout in seconds (default: 30.0)')
    
    # 生成并显示帮助信息
    print("Generated help message:")
    print("-" * 60)
    parser.print_help()
    print("-" * 60)

def demonstrate_real_world_cli():
    """演示真实世界的CLI应用"""
    print("\n=== Real-World CLI Example ===")
    
    class FileProcessor:
        """文件处理器 - 演示实际的CLI应用"""
        
        def __init__(self):
            self.setup_parser()
        
        def setup_parser(self):
            """设置参数解析器"""
            self.parser = argparse.ArgumentParser(
                prog='file-processor',
                description='Process and analyze files',
                formatter_class=argparse.RawDescriptionHelpFormatter,
                epilog="""
Examples:
  %(prog)s input.txt --output result.json --verbose
  %(prog)s input.txt --stats --format csv
  %(prog)s input.txt --convert --target-format xml
                """
            )
            
            # 位置参数
            self.parser.add_argument('input_file', help='Input file to process')
            
            # 处理模式（互斥）
            mode_group = self.parser.add_mutually_exclusive_group(required=True)
            mode_group.add_argument('--stats', action='store_true', help='Generate file statistics')
            mode_group.add_argument('--convert', action='store_true', help='Convert file format')
            mode_group.add_argument('--validate', action='store_true', help='Validate file content')
            
            # 输出选项
            output_group = self.parser.add_argument_group('output options')
            output_group.add_argument('-o', '--output', help='Output file (default: stdout)')
            output_group.add_argument('-f', '--format', choices=['json', 'csv', 'xml'], 
                                     default='json', help='Output format')
            output_group.add_argument('--pretty', action='store_true', help='Pretty print output')
            
            # 处理选项
            process_group = self.parser.add_argument_group('processing options')
            process_group.add_argument('-v', '--verbose', action='count', default=0,
                                      help='Increase verbosity')
            process_group.add_argument('--encoding', default='utf-8', help='File encoding')
            process_group.add_argument('--max-size', type=int, default=10*1024*1024,
                                      help='Maximum file size in bytes (default: 10MB)')
            
            # 转换特定选项
            convert_group = self.parser.add_argument_group('conversion options')
            convert_group.add_argument('--target-format', choices=['json', 'xml', 'yaml'],
                                     help='Target format for conversion')
            convert_group.add_argument('--preserve-structure', action='store_true',
                                     help='Preserve original structure during conversion')
        
        def process(self, args_list):
            """处理命令行参数"""
            try:
                args = self.parser.parse_args(args_list)
                
                print(f"Processing file: {args.input_file}")
                print(f"Mode: {'stats' if args.stats else 'convert' if args.convert else 'validate'}")
                print(f"Output format: {args.format}")
                print(f"Verbose level: {args.verbose}")
                
                if args.output:
                    print(f"Output file: {args.output}")
                
                if args.convert and args.target_format:
                    print(f"Target format: {args.target_format}")
                
                return True
                
            except SystemExit:
                return False
    
    # 测试文件处理器
    processor = FileProcessor()
    
    test_scenarios = [
        ['data.txt', '--stats', '--format', 'csv', '--verbose'],
        ['input.json', '--convert', '--target-format', 'xml', '--pretty'],
        ['config.yaml', '--validate', '--encoding', 'utf-8', '-vv']
    ]
    
    for scenario in test_scenarios:
        print(f"\nTesting scenario: {' '.join(scenario)}")
        processor.process(scenario)

def main():
    """主测试函数"""
    print("Python argparse Module Demonstration")
    print("=" * 50)
    
    # 运行所有测试
    test_basic_argparse()
    test_advanced_argument_types()
    test_subcommands()
    test_mutually_exclusive_groups()
    test_custom_actions()
    test_argument_validation()
    test_help_generation()
    demonstrate_real_world_cli()
    
    print("\n=== Summary ===")
    print("argparse features demonstrated:")
    print("- Basic argument parsing (positional and optional)")
    print("- Advanced argument types and validation")
    print("- Subcommands for complex CLI applications")
    print("- Mutually exclusive argument groups")
    print("- Custom actions for specialized processing")
    print("- Argument validation and error handling")
    print("- Help message generation and formatting")
    print("- Real-world CLI application patterns")
    print("\nKey best practices:")
    print("- Use meaningful argument names and help text")
    print("- Provide sensible defaults")
    print("- Validate input data types and ranges")
    print("- Use subcommands for complex applications")
    print("- Handle errors gracefully")

if __name__ == "__main__":
    main()