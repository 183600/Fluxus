# Go 包声明示例

本目录包含各种 Go 包声明的示例，展示了不同的使用场景和最佳实践。

## 示例列表

### 1. `01_package_main.go` - 可执行程序包
- 展示 `package main` 的使用
- 包含 `main()` 函数作为程序入口
- 这是创建可执行程序的标准方式

### 2. `02_package_library.go` - 库包
- 展示自定义包名的使用
- 用于创建可重用的库代码
- 导出的函数可被其他包使用

### 3. `03_package_with_underscore.go` - 带下划线的包名
- 展示包名中使用下划线
- 虽然允许，但不是推荐的命名风格
- Go 社区更倾向于简短的小写名称

### 4. `04_package_with_imports.go` - 包声明与导入
- 展示包声明后的导入语句
- 使用分组导入的标准格式
- 导入多个标准库包

### 5. `05_package_with_types.go` - 包声明与类型定义
- 展示包声明后的类型、常量和变量定义
- 演示结构体、类型别名和常量组
- 展示包级别的变量声明

### 6. `06_standard_library_style.go` - 标准库风格
- 模仿 Go 标准库的包结构
- 展示构造函数模式
- 演示方法定义

## Go 包声明规则

### 基本规则

1. **必须在文件开头**
   ```go
   package packagename
   ```
   包声明必须是文件的第一条语句（注释除外）

2. **每个文件一个包**
   一个 Go 文件只能属于一个包

3. **同目录同包**
   同一目录下的所有 `.go` 文件必须属于同一个包

### package main 特殊性

- `package main` 定义了一个可执行程序
- 必须包含 `func main()` 作为程序入口
- 编译后生成可执行文件

### 包命名约定

1. **小写字母**：包名应该使用小写字母
2. **简短**：包名应该简短且有意义
3. **无下划线**：通常避免使用下划线（虽然语法允许）
4. **单数形式**：使用单数而非复数（如 `string` 而非 `strings`，但标准库有例外）

### 包的可见性

- **大写字母开头**：导出的（public），可被其他包访问
  ```go
  func PublicFunction() {}
  type PublicType struct {}
  ```

- **小写字母开头**：未导出的（private），只能在包内访问
  ```go
  func privateFunction() {}
  type privateType struct {}
  ```

## 文件结构顺序

标准的 Go 文件结构：

```go
// 1. 包声明
package mypackage

// 2. 导入语句
import (
    "fmt"
    "os"
)

// 3. 常量定义
const (
    MaxSize = 100
)

// 4. 变量定义
var (
    defaultValue = 42
)

// 5. 类型定义
type MyType struct {
    Field string
}

// 6. 函数定义
func MyFunction() {
    // ...
}
```

## 编译和运行

### 编译可执行程序
```bash
go build 01_package_main.go
./01_package_main
```

### 或直接运行
```bash
go run 01_package_main.go
```

### 编译库包
```bash
go build 02_package_library.go
# 生成 .a 文件（归档文件）
```

## 最佳实践

1. **包名与目录名一致**：包名应该与其所在目录名相同
2. **避免通用名称**：不要使用 `util`、`common` 等过于通用的包名
3. **单一职责**：每个包应该有明确的职责
4. **文档注释**：为导出的类型和函数添加文档注释
5. **测试文件**：使用 `_test.go` 后缀的文件进行测试

## 相关资源

- [Go 语言规范 - 包](https://go.dev/ref/spec#Packages)
- [Effective Go - 包](https://go.dev/doc/effective_go#names)
- [Go 代码审查评论 - 包名](https://github.com/golang/go/wiki/CodeReviewComments#package-names)
