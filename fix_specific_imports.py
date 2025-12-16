#!/usr/bin/env python3
import re
import os
from pathlib import Path

def fix_specific_imports():
    """修复特定的导入问题"""
    
    # 需要修复的文件和对应的导入
    fixes = {
        'src/Fluxus/AST/Go.hs': 'import Fluxus.AST.Common (BinaryOp, ComparisonOp, Identifier, Located, QualifiedName, UnaryOp)',
        'src/Fluxus/AST/Python.hs': 'import Fluxus.AST.Common (BinaryOp, ComparisonOp, Identifier, Located, ModuleName, QualifiedName, UnaryOp)',
        'src/Fluxus/Analysis/CommonExprLowering.hs': 'import Fluxus.AST.Common (BinaryOp, ComparisonOp, CommonExpr, Identifier, Located, Literal, QualifiedName, SourceSpan, UnaryOp)',
        'src/Fluxus/Analysis/EscapeAnalysis.hs': 'import Fluxus.AST.Common (EscapeInfo, Identifier, Located)',
        'src/Fluxus/Analysis/ShapeAnalysis.hs': 'import Fluxus.AST.Common (EscapeInfo, Identifier, Located, MemoryLocation, OwnershipInfo, Type)',
        'src/Fluxus/Analysis/OwnershipInference.hs': 'import Fluxus.AST.Common (EscapeInfo, Identifier, Located, MemoryLocation, OwnershipInfo, Type)',
        'src/Fluxus/Analysis/TypeInference.hs': 'import Fluxus.AST.Common (Identifier, Located, Type)',
        'src/Fluxus/Analysis/SmartFallback.hs': 'import Fluxus.AST.Common (Identifier, Located, Type)',
        'src/Fluxus/Optimization/Devirtualization.hs': 'import Fluxus.AST.Common (Identifier, Located)',
        'src/Fluxus/Optimization/Monomorphization.hs': 'import Fluxus.AST.Common (Identifier, Located, Type)',
        'src/Fluxus/Internal/Types.hs': 'import Fluxus.AST.Common (Identifier, Located, Type)',
        'src/Fluxus/CodeGen/CPP/Shared.hs': 'import Fluxus.AST.Common (ExprAnnotations, Identifier, Located, MemoryLocation, OwnershipInfo, Type)',
        'src/Fluxus/CodeGen/CPP/Monad.hs': 'import Fluxus.AST.Common (Located, SourceSpan)',
        'src/Fluxus/CodeGen/CPP/Python.hs': 'import Fluxus.AST.Common (Identifier, Located, Type, TypeVar)',
        'src/Fluxus/CodeGen/CPP/Go.hs': 'import Fluxus.AST.Common (BinaryOp, ComparisonOp, Identifier, Located, SourcePos, SourceSpan, UnaryOp)',
        'src/Fluxus/CodeGen/CPP/IdentifierSanitizer.hs': 'import Fluxus.CodeGen.CPP.AST (Identifier, Located)',
        'src/Fluxus/CodeGen/CPP/Go/TypeMapping.hs': 'import Fluxus.AST.Common (Identifier, Located, ModuleName, QualifiedName)',
        'src/Fluxus/CodeGen/Go.hs': 'import Fluxus.AST.Common (Identifier, Located, Type, TypeVar)',
        'src/Fluxus/Compiler/Driver.hs': 'import Fluxus.AST.Common (Identifier, Located, Type)',
    }
    
    # 处理 Control.Monad 模块
    monad_fixes = {
        'src/Fluxus/Analysis/EscapeAnalysis.hs': [
            'import Control.Monad.State (State, StateT, get, gets, modify, put, runState, runStateT)',
            'import Control.Monad.Reader (Reader, ReaderT, ask, asks, runReader, runReaderT)'
        ],
        'src/Fluxus/Analysis/ShapeAnalysis.hs': [
            'import Control.Monad.State (State, StateT, get, gets, modify, put, runState, runStateT)',
            'import Control.Monad.Reader (Reader, ReaderT, ask, asks, runReader, runReaderT)',
            'import Control.Monad.Except (Except, ExceptT, throwError, catchError, runExcept, runExceptT)'
        ],
        'src/Fluxus/Analysis/OwnershipInference.hs': [
            'import Control.Monad.State (State, StateT, get, gets, modify, put, runState, runStateT)',
            'import Control.Monad.Reader (Reader, ReaderT, ask, asks, runReader, runReaderT)',
            'import Control.Monad.Except (Except, ExceptT, throwError, catchError, runExcept, runExceptT)'
        ],
        'src/Fluxus/Analysis/TypeInference.hs': [
            'import Control.Monad.State (State, StateT, get, gets, modify, put, runState, runStateT)',
            'import Control.Monad.Except (Except, ExceptT, throwError, catchError, runExcept, runExceptT)'
        ],
        'src/Fluxus/Analysis/SmartFallback.hs': [
            'import Control.Monad.State (State, StateT, get, gets, modify, put, runState, runStateT)',
            'import Control.Monad.Reader (Reader, ReaderT, ask, asks, runReader, runReaderT)'
        ],
        'src/Fluxus/Optimization/Devirtualization.hs': [
            'import Control.Monad.State (State, StateT, get, gets, modify, put, runState, runStateT)',
            'import Control.Monad.Reader (Reader, ReaderT, ask, asks, runReader, runReaderT)'
        ],
        'src/Fluxus/Optimization/Monomorphization.hs': [
            'import Control.Monad.State (State, StateT, get, gets, modify, put, runState, runStateT)',
            'import Control.Monad.Reader (Reader, ReaderT, ask, asks, runReader, runReaderT)'
        ],
        'src/Fluxus/Internal/Types.hs': [
            'import Control.Monad.State (State, StateT, get, gets, modify, put, runState, runStateT)',
            'import Control.Monad.Reader (Reader, ReaderT, ask, asks, runReader, runReaderT)',
            'import Control.Monad.Except (Except, ExceptT, throwError, catchError, runExcept, runExceptT)'
        ],
        'src/Fluxus/Internal/Monad.hs': [
            'import Control.Monad.State (State, StateT, get, gets, modify, put, runState, runStateT)',
            'import Control.Monad.Reader (Reader, ReaderT, ask, asks, runReader, runReaderT)',
            'import Control.Monad.Except (Except, ExceptT, throwError, catchError, runExcept, runExceptT)'
        ],
        'src/Fluxus/Compiler/Driver.hs': [
            'import Control.Monad.State (State, StateT, get, gets, modify, put, runState, runStateT)',
            'import Control.Monad.Reader (Reader, ReaderT, ask, asks, runReader, runReaderT)',
            'import Control.Monad.Except (Except, ExceptT, throwError, catchError, runExcept, runExceptT)'
        ]
    }
    
    fixed_count = 0
    
    # 修复 Fluxus.AST.Common 导入
    for file_path, new_import in fixes.items():
        try:
            with open(file_path, 'r') as f:
                content = f.read()
            
            # 查找并替换 import Fluxus.AST.Common
            pattern = r'^import Fluxus\.AST\.Common.*$'
            if re.search(pattern, content, re.MULTILINE):
                content = re.sub(pattern, new_import, content, flags=re.MULTILINE)
                
                with open(file_path, 'w') as f:
                    f.write(content)
                
                print(f"Fixed Fluxus.AST.Common import in {file_path}")
                fixed_count += 1
        except Exception as e:
            print(f"Error fixing {file_path}: {e}")
    
    # 修复 Control.Monad 导入
    for file_path, imports in monad_fixes.items():
        try:
            with open(file_path, 'r') as f:
                content = f.read()
            
            modified = False
            for new_import in imports:
                module_name = new_import.split(' ')[1]
                pattern = f'^import {re.escape(module_name)}.*$'
                if re.search(pattern, content, re.MULTILINE):
                    content = re.sub(pattern, new_import, content, flags=re.MULTILINE)
                    modified = True
            
            if modified:
                with open(file_path, 'w') as f:
                    f.write(content)
                
                print(f"Fixed Control.Monad imports in {file_path}")
                fixed_count += 1
        except Exception as e:
            print(f"Error fixing {file_path}: {e}")
    
    print(f"Fixed imports in {fixed_count} files")

if __name__ == "__main__":
    fix_specific_imports()