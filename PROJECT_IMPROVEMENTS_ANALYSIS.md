# Fluxus 项目改进分析

## 摘要
- 📊 **单文件编译统计失真**：`compileFile` 路径下从未设置 `csTotalFiles`，导致 CLI 输出始终显示 `Total files: 0`，与实际处理数量不符。
- 🧵 **并行编译开关形同虚设**：`ccEnableParallel` 只被传入 C++ 代码生成配置，`compileProject` 仍使用顺序的 `mapM`，多文件项目无法并行化。
- 🗂️ **`--stop-at-codegen` 输出不可控**：停在代码生成阶段时，`-o/--output` 被完全忽略，生成的 `.cpp` 文件仍散落在工作目录内部结构中。
- 📣 **CLI 帮助描述与默认行为冲突**：`printUsage` 声称 `--strict` 为默认值，但 `defaultConfig` 明确将严格模式关闭，会误导使用者。

## 重点改进方向

### 1. 修正单文件编译统计总数
#### 现状
- 代码位置：`Fluxus.Compiler.Driver.compileFile`（约第 466 行起）仅在流水线结束时调用 `incrementProcessedFiles`，从未调整 `csTotalFiles`。
- 与之对比，`compileProject` 在入口处将 `csTotalFiles` 设置为输入文件数量（约第 487-494 行）。

#### 影响
- 运行单文件编译时，CLI 输出的统计信息通常为：
  ```
  Files processed: 1
  Total files: 0
  ```
  这会让自动化脚本或指标采集系统误判编译成果不完整。

#### 改进建议
- 在 `compileFile` 开始处显式设置 `csTotalFiles = 1`（仅当当前值仍为 0 时），或在 `runCompilerMain` 解析到单一输入后统一赋值。
- 为避免重复计数，可在多文件路径中保留现有逻辑。

### 2. 兑现并行编译承诺
#### 现状
- `ccEnableParallel` 贯穿配置文件、CLI、环境变量，但在驱动层仅用于构造 `CppGenConfig`（`codeGenStage` 附近）。
- `compileProject` 仍以顺序 `mapM compileFileArtifacts` 处理全部输入文件，缺乏任何并行手段。

#### 影响
- 多文件工程在 CPU 充足的情况下仍线性运行，性能与“并行编译”承诺不符。
- README/帮助文案持续宣传并行能力，实际体验会削弱对工具的信任。

#### 改进建议
- 在 `compileProject` 中根据 `ccEnableParallel` 选择 `mapM` 或 `mapConcurrently`（需要引入 `Control.Concurrent.Async`）。
- 注意保留日志顺序、错误聚合与 `csIntermediateFiles` 记录；必要时可在 `CompilerState` 中累加线程安全的结构，再在主线程汇总。

### 3. 让停在代码生成阶段的输出可配置
#### 现状
- `compileFileArtifacts` 固定调用 `makeIntermediatePath`，即便用户通过 `-o`/`--output` 指定路径也不会生效。
- 在 `compileProject` + `ccStopAtCodegen` 场景下，函数只是返回 `ccOutputPath`（或工作目录）字符串，并未移动、重命名或链接任何 `.cpp` 文件。

#### 影响
- 用户无法控制生成 C++ 文件的落盘位置；同一项目多次执行会堆积哈希目录结构，难以纳入现有构建系统。
- CLI 帮助与 README 示例中推荐的 `-o`/`--output` 在该模式下形同虚设。

#### 改进建议
- 明确 `--stop-at-codegen` 的输出语义：
  - 若 `-o` 指向目录，则将 `.cpp` 写入该目录并保留原文件名。
  - 若 `-o` 指向具体文件且仅编译单个输入，则直接使用该文件路径。
- 对多文件场景，可默认把 `.cpp` 聚合到 `ccOutputPath` 指定的目录，否则回退到当前的工作目录策略。

### 4. 修复 CLI 帮助与默认行为的矛盾
#### 现状
- `app/Main.hs` 中 `printUsage` 的“Build Options”段落将 `--strict` 标注为“(default)”（约第 205-208 行）。
- `Fluxus.Compiler.Driver.defaultConfig` 明确设置 `ccStrictMode = False`。
- README 与内嵌文档也说明“严格模式默认关闭”。

#### 影响
- 使用者会误以为 CLI 已默认开启严格模式，从而忽略 `--no-strict`，最终看到的实际行为与预期不符。
- 为团队编写操作手册或自动化脚本时，帮助手册的错误指引会造成额外调试成本。

#### 改进建议
- 将 `printUsage` 中的文案调换为 “`--no-strict` … (default)” 或移除默认标记，保持与真实默认值一致。
- 若后续允许通过配置文件切换默认值，可在 Usage 中动态读取 `defaultConfig`，避免再次出现硬编码漂移。

## 其他观察
- `cleanupIntermediateFiles` 只删除跟踪到的文件，不会清空 `resolveWorkPath` 生成的层级目录；在后续实现可配置输出路径时，可一并考虑目录清理策略。
