#!/usr/bin/env python3
import re
import os
from pathlib import Path

# 定义常用模块的导入映射
IMPORT_MAPPINGS = {
    'Fluxus.AST.Common': [
        'BinaryOp(..)', 'ComparisonOp(..)', 'CommonExpr(..)', 'Identifier(..)', 
        'Located(..)', 'Literal(..)', 'ModuleName(..)', 'QualifiedName(..)', 
        'SourcePos(..)', 'SourceSpan(..)', 'Type(..)', 'TypeVar(..)', 'UnaryOp(..)',
        'EscapeInfo(..)', 'MemoryLocation(..)', 'OwnershipInfo(..)', 'ExprAnnotations(..)'
    ],
    'Fluxus.AST.Go': [
        'GoExpr(..)', 'GoLiteral(..)', 'GoType(..)', 'GoDecl(..)', 'GoStmt(..)',
        'GoField(..)', 'GoFunction(..)', 'GoImport(..)'
    ],
    'Fluxus.AST.Python': [
        'PythonExpr(..)', 'PythonLiteral(..)', 'PythonArgument(..)', 'PythonStmt(..)',
        'PythonTypeExpr(..)', 'PythonParameter(..)', 'PythonImport(..)'
    ],
    'Fluxus.CodeGen.CPP.AST': [
        'CppDecl(..)', 'CppExpr(..)', 'CppStmt(..)', 'CppType(..)', 'CppLiteral(..)',
        'CppParam(..)', 'CppUnit(..)'
    ],
    'Control.Monad.State': [
        'State', 'StateT', 'get', 'gets', 'modify', 'put', 'runState', 'runStateT'
    ],
    'Control.Monad.Reader': [
        'Reader', 'ReaderT', 'ask', 'asks', 'runReader', 'runReaderT'
    ],
    'Control.Monad.Except': [
        'Except', 'ExceptT', 'throwError', 'catchError', 'runExcept', 'runExceptT'
    ],
    'Data.Hashable': [
        'Hashable(..)'
    ]
}

def fix_imports_in_file(file_path):
    """修复单个文件中的导入警告"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
    except:
        return False
    
    modified = False
    
    # 查找所有需要修复的导入
    for module, symbols in IMPORT_MAPPINGS.items():
        # 查找没有显式导入列表的导入语句
        pattern = f'^import\\s+{re.escape(module)}\\s*$'
        matches = re.findall(pattern, content, re.MULTILINE)
        
        if matches:
            # 替换为显式导入列表
            import_list = ', '.join(symbols)
            new_import = f'import {module} ({import_list})'
            content = re.sub(pattern, new_import, content, flags=re.MULTILINE)
            modified = True
            print(f"Fixed {module} import in {file_path}")
    
    # 特殊处理 Data.Hashable (Hashable(..)) 的情况
    pattern = r'import Data\.Hashable \(Hashable\(\.\.\.\)\)'
    if re.search(pattern, content):
        new_import = 'import Data.Hashable (Hashable(..))'
        content = re.sub(pattern, new_import, content)
        modified = True
        print(f"Fixed Data.Hashable import in {file_path}")
    
    if modified:
        with open(file_path, 'w') as f:
            f.write(content)
        return True
    
    return False

def main():
    """主函数"""
    src_dir = Path("src")
    
    if not src_dir.exists():
        print("src directory not found")
        return
    
    fixed_count = 0
    
    # 遍历所有 Haskell 源文件
    for hs_file in src_dir.rglob("*.hs"):
        if fix_imports_in_file(hs_file):
            fixed_count += 1
    
    print(f"Fixed imports in {fixed_count} files")

if __name__ == "__main__":
    main()