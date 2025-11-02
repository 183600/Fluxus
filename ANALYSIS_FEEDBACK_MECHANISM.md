# Analysis Feedback Mechanism

## Overview

This document describes the feedback mechanism that enables analysis passes to annotate the AST and have those annotations consumed by the code generation stage.

## Problem Statement

Previously, the analysis passes (`typeInferenceStage` and `optimizationStage`) would traverse the AST and collect information but would:
1. Only record statistics and warnings
2. Return the AST unchanged
3. Not make their findings available to code generation

This meant that expensive analysis work was not being utilized to optimize the generated C++ code.

## Solution Architecture

### 1. Annotation Data Structures

**`ExprAnnotations`** (`Fluxus.AST.Common`):
- Aggregates analysis results for a single expression
- Fields:
  - `eaInferredType`: Type inferred by type inference pass
  - `eaOwnership`: Ownership and memory management information
  - `eaEscapeInfo`: Escape analysis results
  - `eaOptimizationNotes`: Textual hints from various passes

**`AnalysisAnnotations`** (`Fluxus.AST.Common`):
- Global map from expression fingerprints to their annotations
- Keyed by rendered CommonExpr representation
- Supports merging via `insertAnnotations`

### 2. Compiler State Integration

**`CompilerState`** (`Fluxus.Compiler.Driver`):
- Added `csAnalysisAnnotations :: AnalysisAnnotations`
- Initialized to `emptyAnnotations`
- Populated during analysis stages

### 3. Analysis Stage Updates

**Type Inference Stage**:
```haskell
-- When type inference succeeds, store the result
let annotation = ExprAnnotations
      { eaInferredType = Just inferredType
      , eaOwnership = Nothing
      , eaEscapeInfo = Nothing
      , eaOptimizationNotes = []
      }
modify $ \s -> s { csAnalysisAnnotations = insertAnnotations exprKey annotation (csAnalysisAnnotations s) }
```

**Optimization Stage**:
- Escape analysis results populate `eaEscapeInfo` and `eaOwnership`
- Shape analysis results add to `eaOptimizationNotes`
- Monomorphization and devirtualization results add optimization hints
- All results are merged into the global annotations map

### 4. Code Generation Integration

**`CppGenState`** (`Fluxus.CodeGen.CPP`):
- Added `cgsAnalysisAnnotations :: AnalysisAnnotations`
- Passed from compiler state via `generateCppWithAnnotations`

**Annotation Consumption**:
```haskell
lookupAndApplyAnnotations :: CommonExpr -> CppType -> CppCodeGen CppType
```
- Looks up annotations for an expression
- Refines C++ types based on inferred types
- Applies ownership information to select appropriate pointer types:
  - Stack allocation → raw type
  - Heap + unique ownership + movable → `std::unique_ptr`
  - Heap + shared ownership → `std::shared_ptr`
  - Heap + non-owning → raw pointer

### 5. Pipeline Flow

```
Source Code
    ↓
Parsing Stage → AST
    ↓
Type Inference Stage → AST + Type Annotations
    ↓
Optimization Stage → AST + Type/Ownership/Escape Annotations
    ↓
Code Generation Stage → Consumes Annotations → Optimized C++ Code
```

## Usage

The feedback mechanism is automatically enabled when `ccEnableAnalysis` is true in the compiler configuration. No user configuration is needed.

### Example

Given Python code:
```python
def process_data(x):
    result = x * 2  # Type inference: int, Escape: stack-allocated
    return result   # Ownership: movable, Escape: return
```

The compiler will:
1. Infer `result` has type `int`
2. Determine `result` is stack-allocated (doesn't escape to heap)
3. Recognize it can be moved on return
4. Generate optimized C++ using direct values instead of heap allocation

## Benefits

1. **Type-guided code generation**: Use inferred types to select optimal C++ types
2. **Memory optimization**: Apply escape analysis to choose stack vs heap allocation
3. **Smart pointer selection**: Use ownership information to pick `unique_ptr` vs `shared_ptr`
4. **Reduced overhead**: Eliminate unnecessary reference counting when ownership is unique
5. **Verification**: Analysis results are now testable through generated code quality

## Feature Flags

- `ccEnableAnalysis`: Enable/disable analysis passes (default: true)
- Analysis annotations are always collected but only consumed if optimization level ≥ O2
- `cgcUseSmartPointers`: Controls whether smart pointer suggestions are applied

## Future Enhancements

1. **Direct AST annotation**: Consider adding optional annotation fields directly to AST nodes
2. **More granular consumption**: Use annotations in more code generation decisions (variable declarations, function parameters, etc.)
3. **Cross-module annotations**: Persist annotations for cross-module optimization
4. **Visualization**: Generate annotation reports for debugging and verification
