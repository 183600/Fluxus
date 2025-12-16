#!/usr/bin/env bash

# Fix common import warnings by adding explicit import lists

# Fix Go.hs
sed -i 's/import Fluxus\.AST\.Common/import Fluxus.AST.Common (BinaryOp(..), ComparisonOp(..), Identifier(..), Located(..), QualifiedName(..), UnaryOp(..))/' src/Fluxus/AST/Go.hs

# Fix Python.hs  
sed -i 's/import Fluxus\.AST\.Common/import Fluxus.AST.Common (BinaryOp(..), ComparisonOp(..), Identifier(..), Located(..), ModuleName(..), QualifiedName(..), UnaryOp(..))/' src/Fluxus/AST/Python.hs

# Fix CommonExprLowering.hs
sed -i 's/import Fluxus\.AST\.Common/import Fluxus.AST.Common (BinaryOp(..), ComparisonOp(..), CommonExpr(..), Identifier(..), Located(..), Literal(..), QualifiedName(..), UnaryOp(..))/' src/Fluxus/Analysis/CommonExprLowering.hs
sed -i 's/import Fluxus\.AST\.Go/import Fluxus.AST.Go (GoExpr(..), GoLiteral(..))/' src/Fluxus/Analysis/CommonExprLowering.hs
sed -i 's/import Fluxus\.AST\.Python/import Fluxus.AST.Python (PythonArgument(..), PythonExpr(..), PythonLiteral(..))/' src/Fluxus/Analysis/CommonExprLowering.hs

echo "Fixed imports in key files"