# Go 包声明测试用例文档

## 概述

在 `test/Test/Fluxus/Parser/Go.hs` 中添加了全面的 Go 包声明测试用例。

## 测试内容

### 1. 包声明基础测试

#### 1.1 package main 声明
```go
package main
```
- 测试最常见的可执行程序包声明
- 验证解析器能正确识别 `main` 包

#### 1.2 自定义包名
```go
package mypackage
```
- 测试自定义包名的解析
- 验证包名可以是任意有效标识符

#### 1.3 包名中的下划线
```go
package my_package
```
- 测试包名中包含下划线的情况
- 验证符合 Go 标识符规范

#### 1.4 包名中的数字
```go
package package123
```
- 测试包名中包含数字的情况
- 验证数字可以出现在标识符中（但不能开头）

### 2. 包声明与其他元素组合

#### 2.1 包声明 + 导入语句
```go
package main

import "fmt"
```
- 测试包声明后跟随 import 语句
- 验证文件结构的正确解析

#### 2.2 包声明 + 函数定义
```go
package main

func main() {
}
```
- 测试包声明后跟随函数定义
- 验证完整的 Go 文件结构

#### 2.3 包声明 + 变量声明
```go
package main

var x int
```
- 测试包声明后跟随变量声明
- 验证顶层声明的解析

### 3. 常见标准库包名测试

测试以下常见的 Go 标准库包名：
- `fmt` - 格式化 I/O
- `os` - 操作系统功能
- `io` - I/O 原语
- `net` - 网络功能
- `http` - HTTP 客户端和服务器
- `strings` - 字符串操作
- `bytes` - 字节切片操作
- `time` - 时间功能

### 4. 错误情况测试

#### 4.1 缺少包声明
```go
func main() {
}
```
- 测试没有包声明的文件
- 验证解析器能正确拒绝无效文件

## 测试特点

### 完整性
- 覆盖了包声明的所有基本场景
- 包含正常情况和错误情况
- 测试了包声明与其他语言元素的组合

### 实用性
- 测试了实际开发中常见的包名
- 包括标准库常用包名
- 验证了 Go 语言规范中的包声明要求

### 可维护性
- 使用 HSpec 测试框架
- 清晰的测试描述
- 易于扩展和修改

## 运行测试

### 运行所有测试
```bash
stack test
```

### 只运行包声明测试
```bash
stack test --test-arguments "--match 'Package Declaration'"
```

### 运行 Go 解析器所有测试
```bash
stack test --test-arguments "--match 'Go Parser'"
```

## 测试结构

```
test/Test/Fluxus/Parser/Go.hs
├── lexerSpec              # 词法分析器测试
├── lexerTokensSpec        # Token 覆盖测试
├── basicTypesSpec         # 基本类型测试
└── parserSpec             # 语法分析器测试
    └── packageDeclSpec    # 包声明测试（新增）
        ├── 基础包声明测试
        ├── 组合测试
        ├── 标准库包名测试
        └── 错误情况测试
```

## 关键要点

1. **包声明是必需的**：每个 Go 文件都必须以 `package` 声明开头
2. **package main 特殊性**：`package main` 表示可执行程序
3. **包名规则**：包名必须是有效的 Go 标识符
4. **文件结构**：包声明必须在文件开头，在任何导入或声明之前

## 相关文件

- `test/Test/Fluxus/Parser/Go.hs` - 测试实现
- `src/Fluxus/Parser/Go/Parser.hs` - Go 解析器实现
- `src/Fluxus/AST/Go.hs` - Go AST 定义
