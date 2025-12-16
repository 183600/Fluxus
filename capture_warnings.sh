#!/bin/bash
echo "Building and capturing warnings..."
cabal build lib:fluxus --flags="-fast production" --ghc-options="-Wall" 2>&1 | grep -E "warning:|Warning:" | tee warnings.log
echo "Warnings captured in warnings.log"