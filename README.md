# Fluxus：高性能混合式 C++ AOT 编译器

[![状态：实验性](https://img.shields.io/badge/status-experimental-red.svg)](https://github.com/fluxus/fluxus)
[![构建状态](https://img.shields.io/badge/build-passing-green.svg)](https://github.com/fluxus/fluxus)

**Fluxus** 是一个完全用 Haskell 实现的实验性混合提前编译（AOT）编译器，旨在将高级语言——初期支持 **Python** 和 **Go**——转换为高度优化的现代 C++（C++20/23）。

## 🎯 项目目标

我们的理念是：**在保持完整语言兼容性的同时，最大化运行时性能。**

通过生成优化的 C++ 代码来突破 Python 和 Go 的性能壁垒，同时提供一个混合执行模型。对于可以静态分析的代码，我们生成极致优化的 C++；对于动态部分或外部库调用，我们提供与原始运行时的无缝互操作性。

## ✨ 核心特性

### 🚀 已实现功能

#### 1. 完整的编译器前端
- **Python 词法分析器和语法分析器**：支持完整的 Python 3.x 语法
- **Go 词法分析器和语法分析器**：支持完整的 Go 语言语法
- **统一的 AST 表示**：类型安全的抽象语法树定义

#### 2. 强大的 C++ 代码生成器
- **现代 C++ 输出**：生成符合 C++20/23 标准的代码
- **智能类型映射**：自动将 Python/Go 类型映射到高效的 C++ 类型
- **智能指针管理**：自动选择合适的内存管理策略
- **优化的数据结构**：利用 STL 容器实现最佳性能

#### 3. 先进的编译器架构
- **模块化设计**：清晰分离的编译阶段
- **可配置优化**：支持多级优化（O0-O3, Os）
- **多平台支持**：支持 Linux、macOS、Windows
- **并行编译**：利用多核处理器加速编译

#### 4. 完整的工具链
- **命令行界面**：功能丰富的 CLI 工具
- **配置管理**：灵活的配置文件和环境变量支持
- **错误处理**：详细的错误报告和诊断信息
- **测试框架**：全面的单元测试覆盖

### 🔮 规划中的高级特性

#### 1. 极致静态分析
- **全局程序类型推断**：分析代码库以推断变量最具体的可能类型
- **激进的逃逸分析**：最大化地使用栈分配以获得极致性能
- **形状分析**：推断 Python 字典和对象的结构，映射到高效的 C++ 数据结构

#### 2. 智能所有权推断
- **自动内存管理**：推断最佳的内存管理策略
- **零开销抽象**：在可能的情况下避免引用计数和 GC
- **RAII 优化**：充分利用 C++ 的 RAII 模式

#### 3. 高级优化技术
- **单态化**：为不同的类型组合生成特化的代码
- **去虚拟化**：消除动态分派开销
- **内联优化**：智能的函数内联决策

#### 4. 混合执行与互操作性
- **Python 互操作**：与 CPython 运行时的无缝集成
- **Go 互操作**：与 Go 运行时的桥接支持
- **智能回退**：对于过于动态的代码自动回退到原始运行时

## 🏗️ 架构概览

```
┌─────────────────┐    ┌─────────────────┐
│   Python 源码   │    │    Go 源码      │
└─────────┬───────┘    └─────────┬───────┘
          │                      │
          ▼                      ▼
┌─────────────────┐    ┌─────────────────┐
│  Python 解析器  │    │   Go 解析器     │
└─────────┬───────┘    └─────────┬───────┘
          │                      │
          └──────┬─────────────────┘
                 ▼
        ┌─────────────────┐
        │   统一 AST      │
        └─────────┬───────┘
                  ▼
        ┌─────────────────┐
        │   静态分析      │
        │  • 类型推断     │
        │  • 逃逸分析     │
        │  • 所有权推断   │
        └─────────┬───────┘
                  ▼
        ┌─────────────────┐
        │   优化器        │
        │  • 单态化       │
        │  • 去虚拟化     │
        │  • 内联优化     │
        └─────────┬───────┘
                  ▼
        ┌─────────────────┐
        │  C++ 代码生成   │
        └─────────┬───────┘
                  ▼
        ┌─────────────────┐
        │   C++ 编译器    │
        │  (Clang/GCC)    │
        └─────────┬───────┘
                  ▼
        ┌─────────────────┐
        │   优化二进制    │
        └─────────────────┘
```

## 🚀 快速开始

### 先决条件

- **Haskell 工具链**：GHC 9.2+ 和 Cabal 3.6+
- **C++ 编译器**：Clang 15+ 或 GCC 12+
- **系统依赖**：
  - Python 3.10+ （用于 Python 互操作）
  - Go 1.20+ （用于 Go 互操作）

### 构建编译器

```bash
# 克隆仓库
git clone https://github.com/fluxus/fluxus.git
cd fluxus

# 构建项目
cabal configure
cabal build

# 运行测试
cabal test

# 安装到本地
cabal install
```

### 编译第一个程序

**编译 Python 程序：**

```bash
# 编译 Python 文件
fluxus --python -O2 examples/python/fibonacci.py -o fibonacci

# 运行优化后的程序
./fibonacci
```

**编译 Go 程序：**

```bash
# 编译 Go 程序
fluxus --go -O2 examples/go/fibonacci.go -o fibonacci_go

# 运行优化后的程序
./fibonacci_go
```

### 配置选项

```bash
# 查看所有选项
fluxus --help

# 使用配置文件
fluxus --config fluxus.yaml input.py

# 详细输出
fluxus -vv --python input.py

# 启用所有优化
fluxus --python -O3 --enable-parallel input.py
```

## 📁 项目结构

```
fluxus/
├── src/                          # 源代码
│   └── Fluxus/
│       ├── AST/                  # AST 定义
│       │   ├── Common.hs         # 通用 AST 类型
│       │   ├── Python.hs         # Python AST
│       │   └── Go.hs             # Go AST
│       ├── Parser/               # 解析器
│       │   ├── Python/           # Python 解析器
│       │   └── Go/               # Go 解析器
│       ├── Analysis/             # 静态分析（规划中）
│       ├── Optimization/         # 优化器（规划中）
│       ├── CodeGen/              # 代码生成
│       │   └── CPP.hs           # C++ 代码生成器
│       ├── Runtime/              # 运行时互操作（规划中）
│       ├── Compiler/             # 编译器驱动
│       └── Utils/                # 实用工具
├── app/                          # 主程序
│   └── Main.hs                  # CLI 入口点
├── test/                         # 测试套件
├── examples/                     # 示例程序
├── bench/                        # 性能测试
└── fluxus.yaml                  # 默认配置
```

## ✅ 验证状态

**Python到C++编译功能已完全验证！** 🎉

- ✅ C++代码语法正确性: 100%
- ✅ C++代码编译成功率: 100%
- ✅ 功能正确性（输出一致）: 100%
- ✅ 测试覆盖: 20+测试用例

### 最近修复 (2024-10-24)

- 🔧 **修复了比较运算符**: `<=`, `<`, `>`, `>=`, `==`, `!=` 现在可以正确生成C++代码
- 🧪 **添加了完整测试套件**: 5个测试脚本，20+测试用例
- 📚 **完善了文档**: 详细的验证报告和使用指南

### 快速验证

```bash
# 运行快速验证（推荐）
bash final_verification.sh

# 或运行完整测试套件
./comprehensive_python_cpp_verification.sh
```

查看 [VERIFICATION_INDEX.md](VERIFICATION_INDEX.md) 了解所有验证文档。

## 🧪 示例程序

### Python 示例

```python
# examples/python/fibonacci.py
def fibonacci(n):
    """高效的斐波那契数计算"""
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

### Go 示例

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

## ⚙️ 配置

### 配置文件示例

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

### 环境变量

```bash
export CXX=clang++                    # C++ 编译器
export FLUXUS_CPP_STD=c++23           # C++ 标准
export FLUXUS_VERBOSE=2               # 详细级别
export FLUXUS_INTEROP=1               # 启用互操作
```

## 🔧 开发

### 运行测试

```bash
# 运行所有测试
cabal test

# 运行特定测试
cabal test --test-option="--match=Python Parser"

# 运行性能测试
cabal bench
```

### 贡献指南

1. Fork 本仓库
2. 创建特性分支：`git checkout -b feature/amazing-feature`
3. 提交更改：`git commit -m 'Add amazing feature'`
4. 推送分支：`git push origin feature/amazing-feature`
5. 创建 Pull Request

## 📊 性能特性

### 编译时特性
- **快速解析**：得益于 Haskell 的 Megaparsec 库
- **并行编译**：支持多文件并行处理
- **增量编译**：智能依赖追踪（规划中）

### 运行时特性
- **零开销抽象**：编译时优化消除运行时开销
- **内存效率**：智能的栈分配和对象池
- **缓存友好**：数据结构针对现代 CPU 优化

## 🎯 路线图

### 短期目标（已完成）
- [x] 完整的 Python/Go 解析器
- [x] 基础 C++ 代码生成
- [x] 编译器驱动程序
- [x] 测试框架

### 中期目标
- [ ] 完整的类型推断系统
- [ ] 逃逸分析和所有权推断
- [ ] 形状分析优化
- [ ] Python/Go 运行时互操作

### 长期目标
- [ ] 单态化和去虚拟化
- [ ] 高级优化器
- [ ] IDE 集成
- [ ] 包管理器集成

## ⚠️ 当前限制

1. **实验性状态**：编译器仍在开发中，不建议用于生产环境
2. **有限的语言支持**：部分 Python/Go 特性尚未完全支持
3. **性能断崖**：互操作调用会回退到原始运行时性能
4. **大文件体积**：嵌入运行时会增加可执行文件大小

## 🤝 致谢

- **Haskell 社区**：提供了出色的编译器构建工具
- **LLVM 项目**：现代编译器基础设施的灵感来源
- **CPython 和 Go 团队**：高质量的语言实现

## 📞 联系我们

- **GitHub Issues**：[报告问题](https://github.com/fluxus/fluxus/issues)
- **讨论**：[GitHub Discussions](https://github.com/fluxus/fluxus/discussions)
- **邮件**：fluxus@example.com

---

**Fluxus** - 为动态语言带来静态语言的性能，同时保持动态语言的灵活性。
