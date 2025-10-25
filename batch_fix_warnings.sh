#!/bin/bash

# Batch fix common warning patterns in Haskell files

echo "Fixing warnings in Haskell source files..."

# Fix unused 'op' parameter in TypeInference.hs
sed -i 's/inferExpr (CEBinaryOp op left right) = do/inferExpr (CEBinaryOp _op left right) = do/g' src/Fluxus/Analysis/TypeInference.hs

# Fix name shadowing in TypeInference.hs - resultType
sed -i 's/let resultType = freshTypeVar/let resultTypeVar = freshTypeVar/g' src/Fluxus/Analysis/TypeInference.hs
sed -i 's/return resultType$/return resultTypeVar/g' src/Fluxus/Analysis/TypeInference.hs

# Fix unused attr parameter
sed -i 's/inferExpr (CEAttribute obj attr) = do/inferExpr (CEAttribute obj _attr) = do/g' src/Fluxus/Analysis/TypeInference.hs

# Fix unused objType
sed -i 's/^  objType <- inferExpr/  _objType <- inferExpr/g' src/Fluxus/Analysis/TypeInference.hs

# Fix unused constraints in various places
sed -i 's/(\[Constraint\], constraints)/(\[Constraint\], constraintsList)/g' src/Fluxus/Analysis/TypeInference.hs

# Fix name shadowing 't' variables
sed -i 's/TList t -> TList (apply sub t)/TList tElem -> TList (apply sub tElem)/g' src/Fluxus/Analysis/TypeInference.hs

echo "Basic fixes applied. Running stack build to check..."
