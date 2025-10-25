#!/usr/bin/env python3
"""
Automatically fix remaining Haskell warnings based on warnings_full.txt
"""

import re
from pathlib import Path

def fix_file(filepath, fixes):
    """Apply fixes to a file"""
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        
        original = content
        for pattern, replacement in fixes:
            content = re.sub(pattern, replacement, content)
        
        if content != original:
            with open(filepath, 'w') as f:
                f.write(content)
            print(f"Fixed {filepath}")
            return True
        return False
    except Exception as e:
        print(f"Error fixing {filepath}: {e}")
        return False

# Define fixes for each file
fixes_by_file = {
    'src/Fluxus/CodeGen/Go.hs': [
        (r'generateDeclarations module_ config =', r'generateDeclarations pyModule config ='),
        (r'generateStmt stmt =', r'generateStmt stmtNode ='),
        (r'(\s+)stmt <- ', r'\1stmtNode <- '),
        (r'generateExpr expr =', r'generateExpr exprNode ='),
        (r'(\s+)expr <- ', r'\1exprNode <- '),
        (r'generateExpr \(CEList elems\)', r'generateExpr (CEList elemsNodes)'),
        (r'(\s+)elem <- ', r'\1elemNode <- '),
        (r'generateIfElse cond thenBody elseBody =', r'generateIfElse cond thenBody _elseBody ='),
        (r'generateForLoop target iter body =', r'generateForLoop target iter _body ='),
        (r'generateFuncDef name params body config =', r'generateFuncDef name params body _config ='),
    ],
    'src/Fluxus/Internal/Monad.hs': [
        (r'^import Control\.Monad\.IO\.Class$', r'-- import Control.Monad.IO.Class  -- unused'),
    ],
    'src/Fluxus/Optimization/Devirtualization.hs': [
        (r'analyzeCallSite expr =', r'analyzeCallSite _expr ='),
        (r'^recordOptimization ::', r'_recordOptimization ::'),
        (r'^recordOptimization ', r'_recordOptimization '),
        (r'^addTypeConstraint ::', r'_addTypeConstraint ::'),
        (r'^addTypeConstraint ', r'_addTypeConstraint '),
        (r'^getPossibleTypes ::', r'_getPossibleTypes ::'),
        (r'^getPossibleTypes ', r'_getPossibleTypes '),
    ],
    'src/Fluxus/Optimization/Monomorphization.hs': [
        (r'analyzePolymorphicCall expr =', r'analyzePolymorphicCall _expr ='),
    ],
    'src/Fluxus/Parser/Go/Lexer.hs': [
        (r'^import Data\.Char', r'-- import Data.Char  -- unused'),
        (r'^import qualified Text\.Megaparsec\.Char\.Lexer', r'-- import qualified Text.Megaparsec.Char.Lexer  -- unused'),
        (r'^import Text\.Megaparsec\.Char$', r'-- import Text.Megaparsec.Char  -- unused'),
        (r'^import Control\.Applicative$', r'-- import Control.Applicative  -- unused'),
        (r'(\s+)many \(satisfy', r'\1_ <- many (satisfy'),
        (r'(\s+)let span =', r'\1let spanLoc ='),
        (r'(\s+)Located span', r'\1Located spanLoc'),
        (r'(\s+)exp <-', r'\1expPart <-'),
        (r'(\s+)return \(num <> exp\)', r'\1return (num <> expPart)'),
    ],
    'src/Fluxus/Parser/Go/Parser.hs': [
        (r'import Control\.Monad \(void, when\)', r'import Control.Monad (void)'),
        (r'^import Control\.Applicative', r'-- import Control.Applicative  -- unused'),
        (r'^import qualified Control\.Applicative', r'-- import qualified Control.Applicative  -- unused'),
        (r'^import Text\.Megaparsec\.Char$', r'-- import Text.Megaparsec.Char  -- unused'),
        (r'^import Data\.List\.NonEmpty$', r'-- import Data.List.NonEmpty  -- unused'),
        (r'^import qualified Data\.List\.NonEmpty', r'-- import qualified Data.List.NonEmpty  -- unused'),
        (r'runGoParser filename tokens =', r'runGoParser filename tokensList ='),
        (r'(\s+)parse \(parseFile\) filename tokens', r'\1parse (parseFile) filename tokensList'),
        (r'(\s+)importDecls <-', r'\1_importDecls <-'),
        (r'(\s+)let init =', r'\1let initExpr ='),
        (r'(\s+)return \(init,', r'\1return (initExpr,'),
        (r'parseLabel label =', r'parseLabel labelText ='),
        (r'(\s+)keyword \(T\.pack label\)', r'\1keyword (T.pack labelText)'),
        (r'parseLiteral token =', r'parseLiteral tokenVal ='),
        (r'(\s+)case token of', r'\1case tokenVal of'),
        (r'parseRuneLiteral char =', r'parseRuneLiteral charVal ='),
        (r'(\s+)return \(CELiteral \(LChar char\)\)', r'\1return (CELiteral (LChar charVal))'),
        (r'^skipComments ::', r'_skipComments ::'),
        (r'^skipComments =', r'_skipComments ='),
        (r'(\s+)let span =', r'\1let spanLoc ='),
        (r'(\s+)Located span', r'\1Located spanLoc'),
        (r'^convertPos ::', r'_convertPos ::'),
        (r'^convertPos ', r'_convertPos '),
    ],
    'src/Fluxus/Parser/Python/Parser.hs': [
        (r'import Control\.Monad \(void, when\)', r'import Control.Monad (void)'),
        (r'^import Control\.Applicative', r'-- import Control.Applicative  -- unused'),
        (r'^import qualified Control\.Applicative', r'-- import qualified Control.Applicative  -- unused'),
        (r'^import Text\.Megaparsec\.Char$', r'-- import Text.Megaparsec.Char  -- unused'),
        (r'^import Data\.List\.NonEmpty$', r'-- import Data.List.NonEmpty  -- unused'),
        (r'^import qualified Data\.List\.NonEmpty', r'-- import qualified Data.List.NonEmpty  -- unused'),
        (r'runPythonParser filename tokens =', r'runPythonParser filename tokensList ='),
        (r'(\s+)parse \(parseModule\) filename tokens', r'\1parse (parseModule) filename tokensList'),
        (r'parseLiteral token =', r'parseLiteral tokenVal ='),
        (r'(\s+)case token of', r'\1case tokenVal of'),
        (r'parseListDisplay exprs =', r'parseListDisplay _exprs ='),
        (r'^parseComprehension ::', r'_parseComprehension ::'),
        (r'^parseComprehension ', r'_parseComprehension '),
        (r'^skipNewlines ::', r'_skipNewlines ::'),
        (r'^skipNewlines =', r'_skipNewlines ='),
        (r'^skipComments ::', r'_skipComments ::'),
        (r'^skipComments =', r'_skipComments ='),
        (r'(\s+)let span =', r'\1let spanLoc ='),
        (r'(\s+)Located span', r'\1Located spanLoc'),
        (r'^convertPos ::', r'_convertPos ::'),
        (r'^convertPos ', r'_convertPos '),
    ],
    'src/Fluxus/Runtime/Go.hs': [
        (r'^import qualified Data\.Vector', r'-- import qualified Data.Vector  -- unused'),
        (r'^import Foreign\.C\.Types$', r'-- import Foreign.C.Types  -- unused'),
        (r'^import Foreign\.C\.String$', r'-- import Foreign.C.String  -- unused'),
    ],
    'src/Fluxus/Runtime/Python.hs': [
        (r'^import Control\.Monad\.IO\.Class$', r'-- import Control.Monad.IO.Class  -- unused'),
        (r'(\s+)let refCount =', r'\1let refCountVal ='),
        (r'(\s+)return refCount$', r'\1return refCountVal'),
        (r'callPythonFunction runtime func args =', r'callPythonFunction _runtime func _args ='),
        (r'getPythonAttribute runtime obj attr =', r'getPythonAttribute _runtime obj attr ='),
    ],
    'src/Fluxus/Utils/Graph.hs': [
        (r'(\s+)let nodeData =', r'\1let nodeDataVal ='),
        (r'(\s+)return nodeData$', r'\1return nodeDataVal'),
        (r'(\s+)nodeId <-', r'\1nodeIdVal <-'),
        (r'(\s+)return nodeId$', r'\1return nodeIdVal'),
        (r'findCycles paths =', r'findCycles _paths ='),
    ],
    'src/Fluxus/Utils/Pretty.hs': [
        (r'^import qualified Data\.Text$', r'-- import qualified Data.Text  -- unused'),
        (r'^import System\.Console\.ANSI', r'-- import System.Console.ANSI  -- unused'),
        (r'^space ::', r'_space ::'),
        (r'^space =', r'_space ='),
        (r'^line ::', r'_line ::'),
        (r'^line =', r'_line ='),
        (r'^softline ::', r'_softline ::'),
        (r'^softline =', r'_softline ='),
        (r'^(<\+>) ::', r'_(<+>) ::'),
        (r'^a <\+> b =', r'_a <+> _b ='),
        (r'^hsep ::', r'_hsep ::'),
        (r'^hsep =', r'_hsep ='),
        (r'^vsep ::', r'_vsep ::'),
        (r'^vsep =', r'_vsep ='),
        (r'^sep ::', r'_sep ::'),
        (r'^sep =', r'_sep ='),
        (r'^cat ::', r'_cat ::'),
        (r'^cat =', r'_cat ='),
        (r'^hcat ::', r'_hcat ::'),
        (r'^hcat =', r'_hcat ='),
        (r'^vcat ::', r'_vcat ::'),
        (r'^vcat =', r'_vcat ='),
        (r'^parens ::', r'_parens ::'),
        (r'^parens =', r'_parens ='),
        (r'^brackets ::', r'_brackets ::'),
        (r'^brackets =', r'_brackets ='),
        (r'^braces ::', r'_braces ::'),
        (r'^braces =', r'_braces ='),
        (r'^angles ::', r'_angles ::'),
        (r'^angles =', r'_angles ='),
        (r'^indent ::', r'_indent ::'),
        (r'^indent =', r'_indent ='),
        (r'^hang ::', r'_hang ::'),
        (r'^hang =', r'_hang ='),
        (r'^align ::', r'_align ::'),
        (r'^align =', r'_align ='),
        (r'^group ::', r'_group ::'),
        (r'^group =', r'_group ='),
        (r'^nest ::', r'_nest ::'),
        (r'^nest =', r'_nest ='),
        (r'^list ::', r'_list ::'),
        (r'^list =', r'_list ='),
        (r'^tupled ::', r'_tupled ::'),
        (r'^tupled =', r'_tupled ='),
        (r'^punctuate ::', r'_punctuate ::'),
        (r'^punctuate =', r'_punctuate ='),
        (r'^bold ::', r'_bold ::'),
        (r'^bold =', r'_bold ='),
    ],
}

def main():
    fixed_count = 0
    for filepath, fixes in fixes_by_file.items():
        if fix_file(filepath, fixes):
            fixed_count += 1
    
    print(f"\nFixed {fixed_count} files")

if __name__ == '__main__':
    main()
