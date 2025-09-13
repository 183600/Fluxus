# Parser Library

This library provides lexical analysis and parsing capabilities for Python and Go source code. It converts source text into Abstract Syntax Trees (ASTs) that can be processed by other components of the Fluxus compiler.

## Features

- Python lexer and parser implementations
- Go lexer and parser implementations
- Megaparsec-based parsing framework
- Tokenization of source code
- Detailed error reporting with source location tracking
- Support for both Python and Go language syntax

## Usage

The Parser library is used to convert source code files into AST representations:

1. Lexical analysis converts text into tokens
2. Parsing converts tokens into structured ASTs
3. Error handling with precise location information

## Modules

### Python Parsing

- `Python/Lexer.hs`: Tokenizes Python source code
- `Python/Parser.hs`: Parses Python tokens into ASTs

### Go Parsing

- `Go/Lexer.hs`: Tokenizes Go source code
- `Go/Parser.hs`: Parses Go tokens into ASTs

## Dependencies

This library depends on the AST library for data structures.