# Code Generation Library

This library provides C++ code generation capabilities for the Fluxus compiler. It transforms Abstract Syntax Trees (ASTs) from Python and Go source code into optimized C++ code that can be compiled with standard C++ compilers.

## Features

- C++ code generation from Python ASTs
- C++ code generation from Go ASTs
- Modern C++ output (C++20/23 compliant)
- Smart pointer management
- Type mapping from source languages to C++
- Optimization-aware code generation
- Template-based code generation
- Pretty printing of generated code

## Usage

The CodeGen library is used to convert ASTs into C++ code:

1. Takes ASTs from the Parser library
2. Maps source language types to C++ types
3. Generates optimized C++ code
4. Outputs formatted C++ source files

## Modules

- `CPP.hs`: Main C++ code generation module
- `Go.hs`: Go-specific code generation utilities

## Key Data Structures

### C++ AST Types

- `CppUnit`: Represents a complete C++ compilation unit
- `CppDecl`: C++ declarations (classes, functions, variables)
- `CppStmt`: C++ statements (if, while, for, etc.)
- `CppExpr`: C++ expressions (literals, variables, function calls)
- `CppType`: C++ type system representations
- `CppLiteral`: C++ literal values

### Configuration

- `CppGenConfig`: Configuration options for code generation
- `CppGenState`: Internal state during code generation

## Dependencies

This library depends on the AST library for input data structures.