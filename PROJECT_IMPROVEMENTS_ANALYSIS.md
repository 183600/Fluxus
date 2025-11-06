# Fluxus 项目改进分析

## 摘要
- **CLI 选项处理存在 UX 问题**：当前 `--help`、`--version` 等信息性参数会被当成配置错误处理，命令行会以非零状态退出。
- **配置合并逻辑会复写/重复默认路径**：当同时加载默认配置和 `fluxus.yaml` 时，`include_paths`、`library_paths` 等字段会被追加两遍，并且难以覆盖默认值。
- **C++ 渲染的 `try/finally` 语法不合法**：`Fluxus.CodeGen.CPP.AST` 在输出 `CppTry` 时直接打印 `finally` 关键字，导致 Python `with`、`try/finally` 等结构生成的 C++ 代码无法编译。

## 重点改进方向

### 1. CLI 信息性参数需要非错误退出路径
- **观察**：`Fluxus.Compiler.Config.parseCommandLineArgs`（约第 86-178 行）遇到 `--help` 或 `--version` 时返回 `Left`；`Main` 模块在 `loadConfig` 失败时统一输出 `Configuration error` 并 `exitFailure`。
- **影响**：用户执行 `fluxus --help` 会看到“Configuration error: Usage ...”并得到非零退出码，不符合常见 CLI 约定，也不便于脚本化使用。
- **建议**：
  1. 为信息性参数返回一个显式的标记（例如 `Right (HelpRequested ...)`），在 `Main` 中识别后打印信息并 `exitSuccess`。
  2. 顺带为未知参数提供更加友好的提示（现在直接报错并退出 1）。
  3. 添加针对 `--help`、`--version` 的 CLI 行为测试，防止回归。

### 2. 配置合并会重复默认 include/library 路径
- **观察**：`CompilerConfig` 的 `FromJSON` 实例在字段缺省时填充默认值（如 `include_paths` 默认为 `["/usr/include", "/usr/local/include"]`）。`mergeConfigs`（约第 190-213 行）又执行 `override ++ base`，导致在既有默认值的情况下默认路径被重复追加。
- **影响**：最终传给 C++ 编译器的 `-I`/`-L` 参数会重复出现，且用户无法通过 YAML 清空默认搜索路径。这也会让配置层面的 override 语义与预期不符。
- **建议**：
  1. 把 `FromJSON` 中的默认值改成 `Nothing`，仅在缺省时保留 `Maybe`，在 `mergeConfigs` 时根据 `Maybe` 决定是否覆盖。
  2. 调整合并策略为“优先 override，否则沿用 base”，而非盲目拼接，并确保列表字段去重。
  3. 增加一个配置单元测试，验证不同来源（默认 / YAML / 环境变量 / CLI）的优先级和覆盖行为。

### 3. 生成的 C++ `finally` 语句非法
- **观察**：`Fluxus.CodeGen.CPP.AST.renderCppStmt`（约第 245-249 行）将 `CppTry` 渲染为：`try { ... }` + 若 finally 分支非空则直接拼接 `finally { ... }`。然而标准 C++ 并不存在 `finally` 关键字。`Fluxus.CodeGen.CPP.Python` 中的 `generatePythonWith`、`generatePythonTry` 会构建带 finally 块的 `CppTry`。
- **影响**：一旦 Python 源码出现 `with`、`try/finally`、`try/else` 等结构，生成的 C++ 源码无法通过编译，破坏了“完整语法支持”的承诺。
- **建议**：
  1. 改写 `CppTry` 的渲染逻辑：将 finally 块降级为 RAII 守卫或在 `try` 后显式调用清理函数，并保持捕获块合法。
  2. 对 `generatePythonWith` 采用 RAII helper（例如自动生成一个栈对象，其析构函数调用 `__exit__`）。
  3. 增加涵盖 `with`/`try` 的集成测试，确保生成的 C++ 至少能通过编译。

## 其他观察
- `parseStage` 直接调用 `TIO.readFile`，但没有捕获 `IOException`；当输入文件不存在或无法读取时进程会以运行时异常崩溃。建议在该阶段捕获异常并转化为 `FileSystemError`，保持错误处理的一致性。
- `generateCppFromGo` 无论是否使用 channel 都会内联生成一整套 `Channel` 模板类，可考虑按需生成或拆分到独立头文件以减少输出体积。
