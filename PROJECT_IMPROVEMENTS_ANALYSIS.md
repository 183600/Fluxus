# Fluxus 项目改进分析

## 摘要
- 🧩 **工作目录内的产物存在文件名碰撞风险**：`resolveWorkPath`（`Compiler/Driver.hs` 第 368-376 行）会直接取源文件的 `takeFileName`，在 `--work-dir` 下生成的 `.cpp/.o` 完全依赖裸文件名，跨目录的同名文件会互相覆盖。
- 📦 **多文件 `--stop-at-codegen` 命令返回的输出路径缺乏可用信息**：`compileProject` 的停止于代码生成分支（同文件第 469-474 行）只返回 `"."` 或工作目录本身，CLI 最终打印的 `Output:` 无法指向真正生成的 C++ 文件。
- 🔗 **命令行累积的库/路径顺序被反转**：`parseCommandLineArgs` 中的 `prependUnique`（`Compiler/Config.hs` 第 327-338 行）会将最新的 `--link/--include/--library-path` 插到列表头部，造成链接顺序与用户输入相反，容易触发静态链接错误。

## 重点改进方向

### 1. 解决工作目录产物的名称碰撞
#### 现状
- `makeIntermediatePath` 通过 `resolveWorkPath` 将源文件的扩展名替换为 `.cpp/.o`，但只保留了文件名。
- 一旦同时编译 `src/core/main.py` 与 `examples/main.py`，两者都会落到 `work_dir/main.cpp`，后一次编译会直接覆盖前者。

#### 影响
- `--stop-at-codegen` 或保留中间文件时，会得到被覆盖的输出，调试失效。
- 多模块工程很难并行构建或在 CI 中缓存，因为产物缺乏可预测的唯一性。

#### 改进建议
- 生成中间路径时保留源文件的相对目录结构（例如 `work_dir/src/core/main.cpp`）。
- 如果需要保持扁平结构，可附加哈希前缀/目录（如 `work_dir/src_core_main.cpp` 或 `work_dir/ab12cd_main.cpp`）。
- 同步更新 `cleanupIntermediateFiles` 和回归测试，覆盖两个同名文件的编译场景。

### 2. 丰富 `--stop-at-codegen` 的返回信息
#### 现状
- `compileProject` 在 `ccStopAtCodegen` 为 `True` 时只返回 `fromMaybe defaultLocation (ccOutputPath config)`，默认值要么是 `"."`，要么是工作目录。
- CLI 层的 `Output:` 日志因此无法指向真实生成的 `.cpp` 文件列表。

#### 影响
- 使用者无法快速定位生成的 C++ 文件，需要手动搜索工作目录。
- 后续自动化脚本（例如复制或打包生成的 C++ 代码）无法依赖 CLI 输出。

#### 改进建议
- 返回具体的生成文件集合（例如 `NonEmpty FilePath`）并在 CLI 中打印全部路径。
- 或者至少返回工作目录下的绝对路径，并在日志中枚举 `csIntermediateFiles`。
- 为多文件 `--stop-at-codegen` 添加集成测试，确保输出包含可定位的信息。

### 3. 保持命令行参数的顺序一致
#### 现状
- `parseCommandLineArgs` 使用 `prependUnique` 将新的路径/库放在列表头部。
- 对于 `--link foo --link bar`，内部顺序变成 `["bar", "foo"]`，导致链接器以 `-lbar -lfoo` 的顺序运行。

#### 影响
- 静态链接时库顺序至关重要，顺序反转会出现 “undefined reference” 等错误。
- 包含路径与库路径的顺序也被倒置，可能破坏覆盖预期。

#### 改进建议
- 改用尾部追加（`existing ++ [value]`）或专门维护顺序的去重逻辑。
- 为 `--link/--include/--library-path` 编写顺序相关的单元测试，验证 CLI 输入顺序与内部顺序一致。
- 文档中说明顺序语义，并确保配置文件合并时也遵循相同策略。

## 其他观察
- 文档与示例中仍存在大量“HyperStatic”品牌字样，建议在品牌统一时一并更新，避免用户混淆当前仓库的定位。
