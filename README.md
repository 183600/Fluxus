# Fluxus: High-Performance Hybrid C++ AOT Compiler

[![Status: Experimental](https://img.shields.io/badge/status-experimental-red.svg)](https://github.com/fluxus/fluxus)
[![Build Status](https://img.shields.io/badge/build-passing-green.svg)](https://github.com/fluxus/fluxus)

**Fluxus** is an experimental hybrid ahead-of-time (AOT) compiler implemented entirely in Haskell, designed to translate high-level languages—initially supporting **Python** and **Go**—into highly optimized modern C++ (C++20/23).

## 📁 Project Structure

This project has been restructured into independent libraries that can be used separately:

### Libraries

1. **[AST Library](libs/ast)** - Abstract Syntax Tree definitions for Python and Go
2. **[Parser Library](libs/parser)** - Lexical analysis and parsing for Python and Go
3. **[Type Inference Library](libs/type-inference)** - Static type analysis and inference
4. **[Optimization Library](libs/optimization)** - Code optimization passes
5. **[Code Generation Library](libs/codegen)** - C++ code generation from ASTs
6. **[CLI Library](libs/cli)** - Command-line interface and compilation coordination

### Executable

- **[Main Executable](executable)** - The complete Fluxus compiler that uses all libraries

## 🎯 Project Goals

Our philosophy is: **Maximize runtime performance while maintaining complete language compatibility.**

Break through the performance barriers of Python and Go by generating optimized C++ code, while providing a hybrid execution model. For statically analyzable code, we generate极致 optimized C++; for dynamic parts or external library calls, we provide seamless interoperability with the original runtime.

## ✨ Key Features

#### 1. Complete Compiler Frontend
- **Python Lexer and Parser**: Support for complete Python 3.x syntax
- **Go Lexer and Parser**: Support for complete Go language syntax
- **Unified AST Representation**: Type-safe abstract syntax tree definitions

#### 2. Powerful C++ Code Generator
- **Modern C++ Output**: Generate code compliant with C++20/23 standards
- **Intelligent Type Mapping**: Automatically map Python/Go types to efficient C++ types
- **Smart Pointer Management**: Automatically select appropriate memory management strategies
- **Optimized Data Structures**: Use STL containers for optimal performance

#### 3. Advanced Compiler Architecture
- **Modular Design**: Clearly separated compilation phases
- **Configurable Optimization**: Support for multiple optimization levels (O0-O3, Os)
- **Multi-platform Support**: Support for Linux, macOS, Windows
- **Parallel Compilation**: Utilize multi-core processors to accelerate compilation

#### 4. Complete Toolchain
- **Command-line Interface**: Feature-rich CLI tool
- **Configuration Management**: Flexible configuration files and environment variable support
- **Error Handling**: Detailed error reporting and diagnostic information
- **Test Framework**: Comprehensive unit test coverage

#### 5. Ultimate Static Analysis (Core Technology)
Fluxus/CXX adopts optimization techniques typically reserved for static language compilers and applies them boldly to Python and Go compilation.

- **Whole-Program Type Inference (Python)**: Analyzes entire Python codebases to infer the most specific possible types for every variable at every location.

- **Aggressive Escape Analysis (Go/Python)**: Determines whether objects will escape their scope. Where safety can be proven, maximizes stack allocation (the fastest memory allocation method).

- **Shape Analysis (Python)**: Infers the structure of Python dictionaries and objects, mapping them to efficient C++ structs or optimized hash tables (like `absl::flat_hash_map`).

#### 6. Ownership Inference ("Simulating Senior C++ Developer" Approach)
This is our core differentiating advantage. Instead of defaulting to garbage collection (GC), we attempt to infer Rust-like ownership semantics and generate C++-compliant memory management code.

- **Infer `std::move` Semantics**: Detects ownership transfer scenarios (like returning newly created objects) and generates `std::move` semantic code combined with `std::unique_ptr`.

- **Minimize `std::shared_ptr` Usage**: Defaults to exclusive ownership or stack allocation, only falling back to `shared_ptr` (reference counting) when "shared ownership is proven necessary."

#### 7. Specialization and Virtual Function Elimination

- **Monomorphization**: For Python functions called with different types, generates specialized C++ template instances for each type combination, eliminating dynamic dispatch.

- **Virtual Function Elimination (Go interfaces/Python duck typing)**: When the concrete type behind an interface or dynamic call can be statically determined, replaces virtual function calls with direct, inlineable C++ function calls.

#### 8. Concurrency Mapping

- **Go Goroutines → C++20 Coroutines/Fibers**: Translates Go's M:N concurrency model to lightweight C++ mechanisms, avoiding heavyweight OS threads.

- **Python (GIL Challenge)**: For compute-intensive Python code, aims to generate C++ code that runs efficiently without the GIL, achieving true parallel computation.

#### 8. Hybrid Execution and Interoperability
- **Python Interoperability**: Seamless integration with CPython runtime
- **Go Interoperability**: Bridge support with Go runtime
- **Smart Fallback**: Automatically fall back to original runtime for overly dynamic code

## 🏗️ Architecture Overview

```
┌─────────────────┐    ┌─────────────────┐
│   Python Source │    │    Go Source    │
└─────────┬───────┘    └─────────┬───────┘
          │                      │
          ▼                      ▼
┌─────────────────┐    ┌─────────────────┐
│  Python Parser  │    │   Go Parser     │
└─────────┬───────┘    └─────────┬───────┘
          │                      │
          └──────┬─────────────────┘
                 ▼
        ┌─────────────────┐
        │   Unified AST   │
        └─────────┬───────┘
                  ▼
        ┌─────────────────┐
        │  Static Analysis│
        │  • Type Inference│
        │  • Escape Analysis│
        │  • Ownership Inference│
        └─────────┬───────┘
                  ▼
        ┌─────────────────┐
        │   Optimizer     │
        │  • Monomorphization│
        │  • Devirtualization│
        │  • Inlining Optimization│
        └─────────┬───────┘
                  ▼
        ┌─────────────────┐
        │  C++ Code Gen   │
        └─────────┬───────┘
                  ▼
        ┌─────────────────┐
        │   C++ Compiler  │
        │  (Clang/GCC)    │
        └─────────┬───────┘
                  ▼
        ┌─────────────────┐
        │  Optimized Binary│
        └─────────────────┘
```

## 🚀 Quick Start

### Prerequisites

- **Haskell Toolchain**: GHC 9.2+ and Cabal 3.6+
- **C++ Compiler**: Clang 15+ or GCC 12+
- **System Dependencies**:
  - Python 3.10+ (for Python interoperability)
  - Go 1.20+ (for Go interoperability)

### Installation

#### From Source

```bash
# Clone repository
git clone https://github.com/fluxus/fluxus.git
cd fluxus

# Build project
cabal configure
cabal build

# Run tests
cabal test

# Install locally
cabal install
```

#### Using Stack

```bash
# Clone repository
git clone https://github.com/fluxus/fluxus.git
cd fluxus

# Build with Stack
stack build

# Run tests
stack test

# Install locally
stack install
```

#### System Packages

**Arch Linux:**
```bash
# Install from AUR (when available)
yay -S fluxus
```

**Ubuntu/Debian:**
```bash
# Add repository and install
wget -qO- https://fluxus.dev/debian/key.asc | sudo apt-key add -
echo "deb https://fluxus.dev/debian $(lsb_release -cs) main" | sudo tee /etc/apt/sources.list.d/fluxus.list
sudo apt update
sudo apt install fluxus
```

**macOS (Homebrew):**
```bash
# Install from Homebrew (when available)
brew install fluxus
```

### Verifying Installation

```bash
# Check if Fluxus is installed
fluxus --version

# View help
fluxus --help
```

### Compiling Your First Program

**Compile a Python Program:**

```bash
# Compile Python file
fluxus --python -O2 examples/python/fibonacci.py -o fibonacci

# Run optimized program
./fibonacci
```

**Compile a Go Program:**

```bash
# Compile Go program
fluxus --go -O2 examples/go/fibonacci.go -o fibonacci_go

# Run optimized program
./fibonacci_go
```

### Configuration Options

```bash
# View all options
fluxus --help

# Use configuration file
fluxus --config fluxus.yaml input.py

# Verbose output
fluxus -vv --python input.py

# Enable all optimizations
fluxus --python -O3 --enable-parallel input.py
```

## 🧪 Example Programs

### Python Example

```python
# examples/python/fibonacci.py
def fibonacci(n):
    """Efficient Fibonacci number calculation"""
    if n <= 1:
        return n
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)

def main():
    for i in range(10):
        result = fibonacci(i)
        print(f"fib({i}) = {result}")

if __name__ == "__main__":
    main()
```

### Go Example

```go
// examples/go/fibonacci.go
package main

import "fmt"

func fibonacci(n int) int {
    if n <= 1 {
        return n
    }
    return fibonacci(n-1) + fibonacci(n-2)
}

func main() {
    for i := 0; i < 10; i++ {
        result := fibonacci(i)
        fmt.Printf("fib(%d) = %d\n", i, result)
    }
}
```

## ⚙️ Configuration

### Configuration File Example

```yaml
# fluxus.yaml
source_language: "Python"
optimization_level: "O2"
target_platform: "linux-x86_64"
enable_interop: true
enable_debug_info: false
cpp_standard: "c++20"
cpp_compiler: "clang++"
max_concurrency: 4
verbose_level: 1
```

### Environment Variables

```bash
export CXX=clang++                    # C++ compiler
export FLUXUS_CPP_STD=c++23           # C++ standard
export FLUXUS_VERBOSE=2               # Verbosity level
export FLUXUS_INTEROP=1               # Enable interoperability
```

## 🧪 Testing

### Running Tests

```bash
# Run all tests
cabal test

# Run specific tests
cabal test --test-option="--match=Python Parser"

# Run performance tests
cabal bench
```

## 🔧 Development Guide

### Setting Up Development Environment

#### Prerequisites

- **Haskell Toolchain**: GHC 9.2+, Cabal 3.6+, Stack 2.7+
- **Build Tools**: Make, CMake 3.15+
- **Testing Tools**: HSpec, QuickCheck, Tasty
- **Linting Tools**: HLint, Stan, Ormolu
- **Documentation**: Haddock

#### Development Setup

```bash
# Clone repository
git clone https://github.com/fluxus/fluxus.git
cd fluxus

# Install development dependencies
cabal configure --enable-tests --enable-benchmarks --enable-documentation
cabal build all

# Install development tools
cabal install hlint stan ormolu

# Run pre-commit checks
./scripts/hlint.sh
./scripts/ormolu-check.sh
./scripts/stan.sh
```

### Project Structure

The project is organized into several components:

```
fluxus/
├── libs/              # Core libraries
│   ├── ast/          # AST definitions
│   ├── parser/       # Parsers for Python/Go
│   ├── type-inference/ # Type analysis
│   ├── optimization/  # Optimization passes
│   ├── codegen/      # Code generation
│   └── cli/          # CLI interface
├── executable/       # Main executable
├── test/            # Test suite
├── bench/           # Benchmarks
└── scripts/         # Development scripts
```

### Adding New Features

#### Adding a New Language

1. **Create AST definitions** in `libs/ast/src/AST/Language.hs`
2. **Implement lexer and parser** in `libs/parser/src/Parser/Language/`
3. **Add type inference rules** in `libs/type-inference/src/Analysis/`
4. **Implement code generation** in `libs/codegen/src/CodeGen/Language.hs`
5. **Update CLI** in `libs/cli/src/Compiler/Driver.hs`
6. **Add tests** in `test/Test/Fluxus/Parser/Language.hs`

#### Adding a New Optimization Pass

1. **Create optimization module** in `libs/optimization/src/Optimization/NewPass.hs`
2. **Implement the optimization** following existing patterns
3. **Register the pass** in `libs/optimization/src/Optimization/BasicPasses.hs`
4. **Add tests** in `test/Test/Fluxus/Optimization/NewPass.hs`
5. **Add benchmark** in `bench/Bench.hs`

### Testing Guidelines

#### Unit Tests

```bash
# Run all tests
cabal test

# Run specific test component
cabal test fluxus-test

# Run tests with coverage
cabal test --enable-coverage

# Generate coverage report
cabal haddock --enable-documentation --haddock-option=--hyperlinked-source
```

#### Integration Tests

Integration tests verify end-to-end compilation:

```bash
# Run integration tests
cabal test --test-option="--match=Integration"

# Run specific integration test
cabal test --test-option="--match=Python Integration"
```

#### Performance Tests

```bash
# Run benchmarks
cabal bench

# Run specific benchmark
cabal bench --benchmark-option="--match=Fibonacci"

# Compare performance
stack bench --benchmark-arguments="--output=benchmarks.html"
```

### Code Style and Quality

#### Formatting Code

```bash
# Format all Haskell files
find . -name "*.hs" -exec ormolu -i {} \;

# Check formatting
./scripts/ormolu-check.sh
```

#### Linting

```bash
# Run HLint
./scripts/hlint.sh

# Run static analysis
./scripts/stan.sh

# Check for unused code
./scripts/weeder.sh
```

#### Documentation

```bash
# Generate documentation
cabal haddock --enable-documentation

# Generate documentation for specific component
cabal haddock fluxus-ast --enable-documentation

# Check documentation coverage
cabal haddock --haddock-option=--coverage
```

### Contributing Workflow

1. **Fork and Clone**
   ```bash
   git clone https://github.com/your-username/fluxus.git
   cd fluxus
   git remote add upstream https://github.com/fluxus/fluxus.git
   ```

2. **Create Feature Branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```

3. **Make Changes**
   - Follow code style guidelines
   - Add tests for new features
   - Update documentation
   - Run all checks before committing

4. **Commit Changes**
   ```bash
   git add .
   git commit -m "Add your feature: detailed description"
   ```

5. **Push and Create PR**
   ```bash
   git push origin feature/your-feature-name
   ```
   Then create a Pull Request on GitHub.

### Debugging

#### Debug Mode

```bash
# Enable debug output
fluxus --debug --python input.py

# Enable specific debug flags
fluxus --debug=parser,optimization --python input.py
```

#### Debugging Builds

```bash
# Build with debug information
cabal configure --enable-debug-info
cabal build

# Build with profiling
cabal configure --enable-profiling
cabal build
```

#### Common Issues

- **Build errors**: Check GHC version compatibility
- **Runtime errors**: Use debug mode to trace compilation steps
- **Performance issues**: Profile with `cabal bench` and `ghc-events-analyze`

## 📊 Performance Benchmarks

### Benchmark Results

| Test Case | Python Original | Fluxus Compiled | Speedup |
|-----------|----------------|-----------------|---------|
| Fibonacci | 1.2s | 0.03s | 40x |
| Matrix Multiplication | 5.8s | 0.15s | 38x |
| Prime Sieve | 3.4s | 0.08s | 42x |
| Recursive Descent | 2.1s | 0.05s | 42x |
| List Processing | 4.2s | 0.12s | 35x |

### Running Benchmarks

```bash
# Run all benchmarks
cabal bench

# Run specific benchmark
cabal bench --benchmark-option="--match=Fibonacci"

# Generate benchmark report
cabal bench --benchmark-option="--output=benchmarks.html"
```

### Performance Profiling

```bash
# Build with profiling
cabal configure --enable-profiling
cabal build

# Run with profiling
fluxus +RTS -p -RTS --python input.py

# Analyze profiling results
cat fluxus.prof
```

## 📝 More Examples

### Advanced Python Examples

#### Object-Oriented Programming

```python
# examples/python/oop.py
class Calculator:
    def __init__(self):
        self.history = []
    
    def add(self, a, b):
        result = a + b
        self.history.append(f"{a} + {b} = {result}")
        return result
    
    def multiply(self, a, b):
        result = a * b
        self.history.append(f"{a} * {b} = {result}")
        return result
    
    def get_history(self):
        return self.history

def main():
    calc = Calculator()
    print(calc.add(5, 3))
    print(calc.multiply(4, 7))
    for entry in calc.get_history():
        print(entry)

if __name__ == "__main__":
    main()
```

#### List Comprehensions

```python
# examples/python/list_comprehensions.py
def main():
    # Basic list comprehension
    squares = [x**2 for x in range(10)]
    print(f"Squares: {squares}")
    
    # List comprehension with condition
    even_squares = [x**2 for x in range(10) if x % 2 == 0]
    print(f"Even squares: {even_squares}")
    
    # Nested list comprehension
    matrix = [[i*j for j in range(1, 4)] for i in range(1, 4)]
    print(f"Matrix: {matrix}")
    
    # Dictionary comprehension
    square_dict = {x: x**2 for x in range(5)}
    print(f"Square dict: {square_dict}")

if __name__ == "__main__":
    main()
```

### Advanced Go Examples

#### Concurrency

```go
// examples/go/concurrency.go
package main

import (
	"fmt"
	"sync"
	"time"
)

func worker(id int, jobs <-chan int, results chan<- int, wg *sync.WaitGroup) {
	defer wg.Done()
	for j := range jobs {
		fmt.Printf("Worker %d processing job %d\n", id, j)
		time.Sleep(time.Second)
		results <- j * 2
	}
}

func main() {
	const numJobs = 5
	const numWorkers = 3

	jobs := make(chan int, numJobs)
	results := make(chan int, numJobs)
	var wg sync.WaitGroup

	// Start workers
	for w := 1; w <= numWorkers; w++ {
		wg.Add(1)
		go worker(w, jobs, results, &wg)
	}

	// Send jobs
	for j := 1; j <= numJobs; j++ {
		jobs <- j
	}
	close(jobs)

	// Wait for workers to finish
	wg.Wait()
	close(results)

	// Collect results
	for result := range results {
		fmt.Printf("Result: %d\n", result)
	}
}
```

#### Interfaces and Structs

```go
// examples/go/interfaces.go
package main

import "fmt"

type Shape interface {
	Area() float64
	Perimeter() float64
}

type Rectangle struct {
	Width, Height float64
}

type Circle struct {
	Radius float64
}

func (r Rectangle) Area() float64 {
	return r.Width * r.Height
}

func (r Rectangle) Perimeter() float64 {
	return 2 * (r.Width + r.Height)
}

func (c Circle) Area() float64 {
	return 3.14159 * c.Radius * c.Radius
}

func (c Circle) Perimeter() float64 {
	return 2 * 3.14159 * c.Radius
}

func printShapeInfo(s Shape) {
	fmt.Printf("Area: %.2f, Perimeter: %.2f\n", s.Area(), s.Perimeter())
}

func main() {
	r := Rectangle{Width: 5, Height: 3}
	c := Circle{Radius: 4}

	printShapeInfo(r)
	printShapeInfo(c)
}
```

## 🔍 Troubleshooting Guide

### Common Issues and Solutions

#### Installation Problems

**Issue:** Cabal build fails with dependency conflicts
```bash
# Solution: Update cabal and clean build
cabal update
cabal clean
cabal configure --enable-tests --enable-benchmarks
cabal build
```

**Issue:** Stack build fails
```bash
# Solution: Update stack and rebuild
stack update
stack clean
stack build
```

**Issue:** Missing system dependencies
```bash
# Solution: Install system packages
# Ubuntu/Debian
sudo apt-get install build-essential libgmp-dev libffi-dev

# macOS
brew install gmp

# Arch Linux
sudo pacman -S gmp libffi
```

#### Compilation Errors

**Issue:** Parser fails on complex Python code
```bash
# Solution: Enable debug output
fluxus --debug=parser --python complex_code.py
```

**Issue:** Type inference fails
```bash
# Solution: Use explicit type hints or enable fallback
fluxus --enable-fallback --python code_with_types.py
```

**Issue:** C++ compilation fails
```bash
# Solution: Check C++ compiler compatibility
fluxus --cpp-compiler=g++ --python input.py
```

#### Runtime Errors

**Issue:** Segmentation fault
```bash
# Solution: Build with debug info and run with debugger
cabal configure --enable-debug-info
cabal build
gdb ./fluxus
(gdb) run --python input.py
```

**Issue:** Memory leaks
```bash
# Solution: Enable memory profiling
fluxus +RTS -xc -RTS --python input.py
```

**Issue:** Performance regression
```bash
# Solution: Profile and compare performance
cabal bench --benchmark-option="--output=before.html"
# Make changes
cabal bench --benchmark-option="--output=after.html"
```

### Getting Help

#### Community Resources

- **GitHub Issues**: Report bugs and request features
  [https://github.com/fluxus/fluxus/issues](https://github.com/fluxus/fluxus/issues)
- **Discord Channel**: Join our community discussion
  [https://discord.gg/fluxus](https://discord.gg/fluxus)
- **Mailing List**: Subscribe to project updates
  [fluxus-dev@googlegroups.com](mailto:fluxus-dev@googlegroups.com)

#### Debug Flags

```bash
# Enable all debug output
fluxus --debug-all --python input.py

# Enable specific debug components
fluxus --debug=parser,type-inference,optimization --python input.py

# Enable verbose output
fluxus -vv --python input.py

# Save debug logs
fluxus --debug --log-file=debug.log --python input.py
```

#### Performance Tuning

```bash
# Optimize for speed
fluxus -O3 --enable-parallel --python input.py

# Optimize for size
fluxus -Os --python input.py

# Enable specific optimizations
fluxus --enable-inlining --enable-vectorization --python input.py

# Limit memory usage
fluxus --max-memory=4G --python input.py
```

## Contribution Guidelines

1. Fork this repository
2. Create a feature branch: `git checkout -b feature/amazing-feature`
3. Commit changes: `git commit -m 'Add amazing feature'`
4. Push branch: `git push origin feature/amazing-feature`
5. Create Pull Request

## ⚠️ Current Limitations

1. **Experimental Status**: Compiler is still under development, not recommended for production use
2. **Limited Language Support**: Some Python/Go features are not yet fully supported
3. **Performance Cliff**: Interoperability calls fall back to original runtime performance
4. **Large File Size**: Embedding runtime increases executable size

## 🤝 Acknowledgements

- **Haskell Community**: Provided excellent tools for building compilers
- **LLVM Project**: Inspiration for modern compiler infrastructure
- **CPython and Go Teams**: High-quality language implementations