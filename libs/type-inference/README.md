# Type Inference Library

This library provides static type analysis capabilities for the Fluxus compiler. It analyzes Abstract Syntax Trees (ASTs) to infer types, perform escape analysis, and determine ownership patterns to enable optimized code generation.

## Features

- Global program type inference
- Escape analysis for memory management optimization
- Ownership inference for resource management
- Shape analysis for data structure optimization
- Smart fallback mechanisms for dynamic code
- Constraint solving for type checking

## Usage

The Type Inference library is used to analyze ASTs for type information:

1. Takes ASTs from the Parser library
2. Performs type inference on the entire program
3. Analyzes escape patterns for memory optimization
4. Determines ownership for resource management
5. Provides shape information for data structure optimization

## Modules

- `TypeInference.hs`: Main type inference engine
- `EscapeAnalysis.hs`: Escape analysis for memory optimization
- `OwnershipInference.hs`: Ownership inference for resource management
- `ShapeAnalysis.hs`: Shape analysis for data structures
- `SmartFallback.hs`: Fallback mechanisms for dynamic code

## Dependencies

This library depends on the AST library for input data structures.