#!/usr/bin/env python3
import re
from pathlib import Path

def fix_all_import_issues():
    """修复所有导入问题"""
    
    # 需要修复的文件和对应的完整导入
    fixes = {
        'src/Fluxus/Analysis/CommonExprLowering.hs': [
            'import Fluxus.AST.Go (GoAST(..), GoExpr(..), GoLiteral(..))',
            'import Fluxus.AST.Python (PythonAST(..), PythonModule, PythonStmt(..), PythonArgument(..), PythonExpr(..), PythonLiteral(..))'
        ],
        'src/Fluxus/CodeGen/CPP/Go/TypeMapping.hs': [
            'import Fluxus.AST.Go (GoType(..), GoField(..), GoExpr(..), GoLiteral(..))',
            'import Fluxus.CodeGen.CPP.AST (CppType(..))'
        ],
        'src/Fluxus/CodeGen/CPP/Python.hs': [
            'import Fluxus.AST.Python (PythonExpr(..), PythonLiteral(..), PythonArgument(..), PythonStmt(..), PythonTypeExpr(..), PythonParameter(..), PythonImport(..))',
            'import Fluxus.CodeGen.CPP.AST (CppDecl(..), CppExpr(..), CppStmt(..), CppType(..), CppLiteral(..), CppParam(..), CppUnit(..))',
            'import Fluxus.CodeGen.CPP.Shared (ExprAnnotations(..), OwnershipInfo(..), MemoryLocation(..), Type(..), CppExpr(..), CppLiteral(..), CppType(..), CppGenState(..))',
            'import Fluxus.CodeGen.CPP.Monad (CppGenState(..))'
        ],
        'src/Fluxus/CodeGen/CPP/Go.hs': [
            'import Fluxus.AST.Go (GoDecl(..), GoExpr(..), GoLiteral(..), GoStmt(..), GoType(..))',
            'import Fluxus.CodeGen.CPP.AST (CppDecl(..), CppExpr(..), CppLiteral(..), CppParam(..), CppStmt(..), CppType(..), CppUnit(..))',
            'import Fluxus.CodeGen.CPP.Shared (CppGenState(..))'
        ],
        'src/Fluxus/CodeGen/Go.hs': [
            'import Fluxus.AST.Python (PythonExpr(..), PythonLiteral(..), PythonArgument(..), PythonStmt(..), PythonTypeExpr(..), PythonParameter(..), PythonImport(..))'
        ],
        'src/Fluxus/Parser/Python/Parser.hs': [
            'import Fluxus.AST.Python (PythonExpr(..), PythonLiteral(..), PythonArgument(..), PythonStmt(..), PythonTypeExpr(..), PythonParameter(..), PythonImport(..))'
        ],
        'src/Fluxus/Parser/Go/Parser.hs': [
            'import Fluxus.AST.Go (GoDecl(..), GoExpr(..), GoLiteral(..), GoStmt(..), GoType(..))'
        ],
        'src/Fluxus/Parser/Go/Parser/Statements.hs': [
            'import Fluxus.AST.Go (GoDecl(..), GoExpr(..), GoLiteral(..), GoStmt(..), GoType(..))'
        ],
        'src/Fluxus/Parser/Go/Parser/Declarations.hs': [
            'import Fluxus.AST.Go (GoDecl(..), GoExpr(..), GoLiteral(..), GoStmt(..), GoType(..))'
        ],
        'src/Fluxus/Parser/Go/Parser/Expressions.hs': [
            'import Fluxus.AST.Go (GoDecl(..), GoExpr(..), GoLiteral(..), GoStmt(..), GoType(..))'
        ]
    }
    
    fixed_count = 0
    
    # 修复导入
    for file_path, imports in fixes.items():
        try:
            with open(file_path, 'r') as f:
                content = f.read()
            
            modified = False
            for new_import in imports:
                # 提取模块名
                module_name = new_import.split(' ')[1]
                pattern = f'^import {re.escape(module_name)}.*$'
                
                if re.search(pattern, content, re.MULTILINE):
                    content = re.sub(pattern, new_import, content, flags=re.MULTILINE)
                    modified = True
                    print(f"Fixed {module_name} import in {file_path}")
            
            if modified:
                with open(file_path, 'w') as f:
                    f.write(content)
                fixed_count += 1
        except Exception as e:
            print(f"Error fixing {file_path}: {e}")
    
    print(f"Fixed imports in {fixed_count} files")

if __name__ == "__main__":
    fix_all_import_issues()