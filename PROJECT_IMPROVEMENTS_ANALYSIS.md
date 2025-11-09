# Fluxus 项目改进分析

## 摘要
- 🗂️ **ccWorkDirectory 未参与中间产物管理**：`setupCompilerEnvironment` 虽然会创建工作目录，但 `compileFileArtifacts`/`compileCpp` 仍直接在源文件旁写出 `.cpp/.o`，导致 stop-at-codegen 或多文件编译时依旧污染源码目录。
- ⚙️ **CLI 无法选择配置文件**：当前 `loadConfig` 固定读取 `fluxus.yaml`，`parseCommandLineArgs` 也没有 `--config` 选项，README 中的示例命令会触发 “Unknown option” 错误，使用者无法切换不同配置。
- 🚨 **配置解析失败被静默忽略**：`loadConfig` 对 `loadConfigFromFile` 返回的错误统一回退到默认配置，既不冒泡也无日志，用户难以及时发现 YAML 拼写问题。
- 🔁 **HyperStatic 品牌仍散落在代码生成配置**：`codeGenStage` 和 `defaultCppGenConfig` 仍写死 `hyperstatic` 命名空间/头卫士，`compileProject` 的默认输出也叫 `hyperstatic_output`，与项目更名后的文档不一致。

## 重点改进方向

### 1. 让 ccWorkDirectory 主导中间产物路径
#### 现状
- 代码位置：`setupCompilerEnvironment`（Driver 约第 326-360 行）仅负责创建 `ccWorkDirectory`。
- 生成阶段 `compileFileArtifacts`（同文件第 397 行）直接使用 `replaceExtension inputFile ".cpp"`，`compileCpp` 也对 `.cpp` 文件调用 `replaceExtension` 生成 `.o`。
- `cleanupIntermediateFiles` 删除的仍是源目录中的文件，未迁移到工作目录。

#### 影响
- stop-at-codegen 模式下 `.cpp` 文件依旧散落在源目录。
- 多文件项目无法使用统一的构建输出目录，和 IDE/CI 的 “build/” 约定冲突。
- 使用者误以为 `--work-dir` 能隔离产物，实际却只创建了空目录。

#### 改进建议
- 在写入 `.cpp`/`.o`/最终二进制前，通过 `ccWorkDirectory` 重写路径（例如提供 `withWorkDir` 辅助函数）。
- 将 `cleanupIntermediateFiles` 与 `linkObjects` 也适配工作目录，以便目录清理逻辑一致。
- 回归测试覆盖：设置 `ccWorkDirectory` 后编译，验证源目录无新产物、工作目录存在期望文件。

### 2. 为 CLI 提供可配置的配置文件入口
#### 现状
- `loadConfig`（Config 第 180-207 行）始终从当前目录加载 `fluxus.yaml`。
- `parseCommandLineArgs` 缺少 `--config`/`-c` 等分支，README 中 `fluxus --config custom.yaml app.py` 会被 `_ | "--" \`isPrefixOf\` arg` 捕获为未知选项。
- 无法在同一仓库内切换 “开发配置”“生产配置”。

#### 影响
- 用户不得不覆盖单一的 `fluxus.yaml`，不利于多环境及团队协作。
- 文档示例与实际实现不符，降低信任度。

#### 改进建议
- 在 CLI 解析阶段支持 `--config PATH`（可多次出现，约定最后一次生效或按顺序合并）。
- `loadConfig` 接受可选路径参数，并在未找到文件时沿用已有回退策略。
- 更新 README/usage 文案及相关测试，确保命令行示例可直接运行。

### 3. 显式暴露配置文件解析错误
#### 现状
- `loadConfigFromFile` 返回 `Left "Failed to parse config file: ..."`。
- `loadConfig` 中 `case configFromFile of Left _ -> baseConfig` 直接吞掉错误。
- CLI 视角只得到默认配置，且没有任何警告。

#### 影响
- YAML 拼写或缩进错误会被静默忽略，编译过程使用默认参数，问题定位困难。
- 破坏 “配置 > 默认” 的优先级预期。

#### 改进建议
- 当 `loadConfigFromFile` 返回 `Left err` 时，将错误转成 `Left err` 直接返回给 CLI。
- 可在错误信息中提示 `--config-validate` 或类似命令，以便用户单独校验。
- 添加针对格式错误 YAML 的测试，确保未来不会回退到沉默模式。

### 4. 清理残留的 HyperStatic 品牌标识
#### 现状
- `codeGenStage`（Driver 第 717-726 行）仍写死 `cgcNamespace = "hyperstatic"`、`cgcHeaderGuard = "HYPERSTATIC_GENERATED"`。
- `Fluxus.CodeGen.CPP.Monad.defaultCppGenConfig`（第 128-137 行）沿用同样的默认值。
- `compileProject` 在未指定输出时生成 `hyperstatic_output`。
- `app/Main.hs` 顶层注释仍是 “HyperStatic/CXX compiler”。

#### 影响
- 生成的代码和产物名称与 Fluxus 品牌不一致，给用户造成 “fork 或旧版本” 的错觉。
- 后续若允许用户自定义命名空间，需要首先拆除这些硬编码。

#### 改进建议
- 将默认命名空间/头卫士更新为 `fluxus`/`FLUXUS_GENERATED`，同时允许通过配置覆盖。
- 调整 `compileProject` 默认输出名，并更新示例/测试期望。
- 审核代码库中的 `HyperStatic` 字样，逐步替换为 Fluxus。

## 其他观察
- 测试描述及示例产物中仍有大量 “HyperStatic” 字样，建议在品牌统一的同一迭代处理，避免回归测试期望反复修改。
