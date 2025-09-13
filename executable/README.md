# Fluxus Compiler Executable

This is the main executable for the Fluxus compiler, a high-performance hybrid C++ AOT compiler that translates Python and Go source code into optimized C++.

## Features

- Compiles Python and Go source code to optimized C++
- Supports multiple optimization levels (O0-O3, Os)
- Provides interoperability with runtime environments
- Generates modern C++20/23 compliant code
- Parallel compilation support
- Detailed error reporting and diagnostics

## Usage

```bash
# Compile a Python file
fluxus --python -O2 main.py -o output

# Compile a Go file
fluxus --go -O3 main.go -o output

# Compile multiple files
fluxus --python -O2 *.py -o output
```

## Dependencies

This executable depends on all Fluxus libraries:
- Parser library for source code parsing
- AST library for data structures
- Type Inference library for type analysis
- Optimization library for code optimization
- CodeGen library for C++ code generation
- CLI library for coordination

It also requires:
- A C++ compiler (clang++ or g++)
- Standard C++ libraries