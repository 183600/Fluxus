# Optimization Library

This library provides optimization passes for the Fluxus compiler. It transforms Abstract Syntax Trees (ASTs) to improve performance, reduce code size, and enhance runtime efficiency.

## Features

- Constant folding optimization
- Dead code elimination
- Constant propagation
- Function inlining
- Loop vectorization
- Devirtualization
- Monomorphization
- Size reduction optimizations

## Usage

The Optimization library is used to improve the performance of ASTs:

1. Takes ASTs from the Parser or Type Inference libraries
2. Applies various optimization passes based on configuration
3. Produces optimized ASTs for code generation

## Modules

- `ConstantFolding.hs`: Evaluates constant expressions at compile time
- `DeadCodeElimination.hs`: Removes unreachable or unused code
- `ConstantPropagation.hs`: Propagates constant values through the program
- `Inlining.hs`: Inlines function calls for performance
- `Vectorization.hs`: Optimizes loops for SIMD execution
- `Devirtualization.hs`: Converts virtual calls to direct calls
- `Monomorphization.hs`: Creates specialized versions of generic functions
- `SizeReduction.hs`: Reduces code size through various techniques
- `BasicPasses.hs`: Basic optimization passes

## Dependencies

This library depends on the AST library for input data structures.