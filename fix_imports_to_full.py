#!/usr/bin/env python3
import re
from pathlib import Path

def fix_imports_to_full():
    """将特定模块的导入改回导入整个模块"""
    
    # 需要修复的文件和对应的模块
    fixes = {
        'src/Fluxus/Analysis/EscapeAnalysis.hs': [
            'import Fluxus.AST.Common',
            'import Control.Monad.State',
            'import Control.Monad.Reader'
        ],
        'src/Fluxus/Analysis/OwnershipInference.hs': [
            'import Fluxus.AST.Common',
            'import Control.Monad.State',
            'import Control.Monad.Reader',
            'import Control.Monad.Except'
        ],
        'src/Fluxus/Analysis/ShapeAnalysis.hs': [
            'import Fluxus.AST.Common',
            'import Control.Monad.State',
            'import Control.Monad.Reader',
            'import Control.Monad.Except'
        ],
        'src/Fluxus/Analysis/TypeInference.hs': [
            'import Fluxus.AST.Common',
            'import Control.Monad.State',
            'import Control.Monad.Except'
        ],
        'src/Fluxus/Analysis/SmartFallback.hs': [
            'import Fluxus.AST.Common',
            'import Control.Monad.State',
            'import Control.Monad.Reader'
        ],
        'src/Fluxus/Optimization/Devirtualization.hs': [
            'import Fluxus.AST.Common',
            'import Control.Monad.State',
            'import Control.Monad.Reader'
        ],
        'src/Fluxus/Optimization/Monomorphization.hs': [
            'import Fluxus.AST.Common',
            'import Control.Monad.State',
            'import Control.Monad.Reader'
        ],
        'src/Fluxus/Internal/Types.hs': [
            'import Fluxus.AST.Common',
            'import Control.Monad.State',
            'import Control.Monad.Reader',
            'import Control.Monad.Except'
        ],
        'src/Fluxus/Internal/Monad.hs': [
            'import Fluxus.Internal.Types',
            'import Control.Monad.State',
            'import Control.Monad.Reader',
            'import Control.Monad.Except'
        ],
        'src/Fluxus/Compiler/Driver.hs': [
            'import Fluxus.AST.Common',
            'import Fluxus.AST.Go',
            'import Fluxus.AST.Python',
            'import Control.Monad.State',
            'import Control.Monad.Reader',
            'import Control.Monad.Except'
        ]
    }
    
    fixed_count = 0
    
    # 修复导入
    for file_path, modules in fixes.items():
        try:
            with open(file_path, 'r') as f:
                content = f.read()
            
            modified = False
            for module in modules:
                # 查找并替换导入
                pattern = f'^import {re.escape(module)}.*$'
                if re.search(pattern, content, re.MULTILINE):
                    content = re.sub(pattern, f'import {module}', content, flags=re.MULTILINE)
                    modified = True
                    print(f"Fixed {module} import in {file_path}")
            
            if modified:
                with open(file_path, 'w') as f:
                    f.write(content)
                fixed_count += 1
        except Exception as e:
            print(f"Error fixing {file_path}: {e}")
    
    print(f"Fixed imports in {fixed_count} files")

if __name__ == "__main__":
    fix_imports_to_full()