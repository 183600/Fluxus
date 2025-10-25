#!/bin/bash

echo "Quick Compilation Test"
echo "======================"
echo ""

# Test compilation of key modules
echo "Testing compilation of modified modules..."

modules=(
    "Fluxus.Analysis.EscapeAnalysis"
    "Fluxus.Analysis.OwnershipInference"
    "Fluxus.Analysis.ShapeAnalysis"
    "Fluxus.Analysis.TypeInference"
    "Fluxus.CodeGen.Go"
    "Fluxus.Parser.Go.Parser"
    "Fluxus.Parser.Python.Parser"
    "Fluxus.Utils.Pretty"
)

success=0
failed=0

for module in "${modules[@]}"; do
    echo -n "Compiling $module... "
    if stack ghc -- -c "src/${module//.//}.hs" -fno-code -fforce-recomp 2>&1 | grep -q "warning:"; then
        echo "WARNINGS FOUND"
        ((failed++))
    else
        echo "OK"
        ((success++))
    fi
done

echo ""
echo "Results:"
echo "  Success: $success"
echo "  With warnings: $failed"
