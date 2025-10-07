# AST Library

This library provides the Abstract Syntax Tree (AST) definitions for the Fluxus compiler. It contains data structures that represent the syntactic structure of Python and Go programs after parsing.

## Features

- Language-agnostic AST nodes in `Common.hs`
- Python-specific AST definitions in `Python.hs`
- Go-specific AST definitions in `Go.hs`
- Type-safe representations of language constructs
- Location tracking for error reporting
- Support for both Python and Go language features

## Usage

This library is meant to be used as a dependency for other components of the Fluxus compiler:

1. Parser library produces ASTs from source code
2. Type inference library analyzes ASTs for type information
3. Code generation library transforms ASTs into C++ code
4. Optimization library modifies ASTs to improve performance

## Modules

- `Common.hs`: Shared AST definitions used by both languages
- `Python.hs`: Python-specific AST nodes and structures
- `Go.hs`: Go-specific AST nodes and structures

## Data Structures

### Common AST Types

- `Identifier`: Represents variable and function names
- `SourceSpan`: Location information for error reporting
- `BinaryOp`, `UnaryOp`: Operator representations
- `Type`: Type system representations

### Python AST Types

- `PythonAST`: Top-level Python AST wrapper
- `PythonModule`: Represents a Python module
- `PythonStmt`: Python statements (if, while, for, function definitions, etc.)
- `PythonExpr`: Python expressions (literals, variables, function calls, etc.)
- `PythonPattern`: Pattern matching constructs

### Go AST Types

- `GoAST`: Top-level Go AST wrapper
- `GoPackage`: Represents a Go package
- `GoFile`: Represents a single Go source file
- `GoDecl`: Go declarations (function, type, variable, constant)
- `GoStmt`: Go statements (if, switch, for, goroutines, etc.)
- `GoExpr`: Go expressions (literals, variables, function calls, etc.)
- `GoType`: Go type system representations

## Dependencies

This library has no dependencies on other Fluxus libraries, making it a foundational component.