# CLI Library

This library provides the command-line interface and coordination logic for the Fluxus compiler. It orchestrates all other libraries to provide a complete compilation pipeline from source code to optimized binaries.

## Features

- Command-line argument parsing
- Configuration management
- Compilation pipeline coordination
- Error handling and reporting
- Progress tracking and statistics
- Integration with C++ toolchain

## Usage

The CLI library is used to coordinate the entire compilation process:

1. Parse command-line arguments
2. Configure compilation options
3. Orchestrate the compilation pipeline:
   - Parse source files
   - Perform type inference
   - Apply optimizations
   - Generate C++ code
   - Compile with C++ compiler
   - Link into final executable

## Modules

- `Compiler/Driver.hs`: Main compilation driver
- `Compiler/Config.hs`: Configuration management
- `Utils/`: Utility functions
- `Internal/`: Internal helper functions

## Dependencies

This library depends on all other Fluxus libraries:
- Parser library for source code parsing
- AST library for data structures
- Type Inference library for type analysis
- Optimization library for code optimization
- CodeGen library for C++ code generation