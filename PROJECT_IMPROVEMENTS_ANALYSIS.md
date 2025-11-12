# Fluxus 项目改进分析

## 摘要
- 🔁 **多文件 stop-at-codegen 输出路径不明确**：`compileProject` 在 `ccStopAtCodegen` 为 `True` 时仅返回工作目录（或用户手动指定的输出路径），而不是实际生成的 `.cpp` 列表，导致 CLI 输出 `Output: .` 之类的路径提示，用户需要自行去工作目录内查找哈希散列后的子目录。
- ⚠️ **CLI `--strict` 默认值文案与实际行为不符**：`app/Main.hs` 的 `printUsage` 仍提示 “Treat warnings as errors (default)”，但当前 `defaultConfig` 里 `ccStrictMode = False`，文档与行为出现分歧。
- 🧹 **工作目录清理仅删除文件，不会回收哈希子目录**：`cleanupIntermediateFiles` 使用 `removeFile` 删除 `.cpp/.o`，但不会清理 `resolveWorkPath` 创建的哈希文件夹，频繁构建会让工作目录残留大量空目录。
- 🪟 **Windows 目标缺省产物缺少 `.exe` 扩展名**：`compileFile`/`compileProject` 默认输出 `fluxus_output` 等裸文件名，`linkObjects` 也不会根据 `TargetPlatform` 自动附加 `.exe`，Windows 用户得到的可执行文件无法直接双击运行。
- 🪪 **示例与验证资产仍大量残留 HyperStatic 品牌**：`examples/**`, `debug_*.cpp`, 多份验证报告和脚本依旧以 “HyperStatic/CXX Compiler” 抬头，与代码里已切换到 `fluxus` 命名空间不一致。

## 重点改进方向

### 1. stop-at-codegen 应提供明确的产出定位
**现状**：`Fluxus.Compiler.Driver.compileProject`（约 500 行附近）在 `ccStopAtCodegen` 为真时直接返回工作目录或 `ccOutputPath`。CLI 最终向用户打印 `Output: /tmp/build` 之类的目录，但所有 `.cpp` 文件被写入诸如 `/tmp/build/1528743341/module.cpp` 的哈希子目录中。

**影响**：
- 用户需要自行遍历工作目录才能找到每个输入对应的 `.cpp`，增加使用门槛。
- IDE/CI 想要消费这些文件时缺乏稳定的约定，难以脚本化处理。

**建议**：
- `compileProject` 返回明确的文件列表或汇总文件（例如写出 `codegen-manifest.json`）。
- CLI 输出中列出实际生成的相对路径，或提示 Manifest 的位置。

### 2. 更新 CLI 帮助以匹配新的严格模式默认值
**现状**：`app/Main.hs` 中 `printUsage` 的 `--strict` 行仍声称“(default)”。

**影响**：
- 新用户以为默认会在缺失特性时立即报错，实际仍会降级为运行时回退，和 README “默认关闭严格模式” 的说明冲突。

**建议**：
- 将帮助文案改成 “Treat warnings as errors (default: off)” 或类似措辞，并在 README/示例命令保持一致。

### 3. 扩展中间产物清理逻辑以移除空目录
**现状**：`cleanupIntermediateFiles` 仅调用 `removeFile`，不会删除 `ccWorkDirectory` 下的哈希目录。

**影响**：
- 每次编译都会留下若干空文件夹（如 `/tmp/build/412398765/`），长期使用会拖慢 `find`、`ls` 等操作，也影响缓存同步。

**建议**：
- 在删除文件后尝试调用 `removePathForcibly` 或自定义逻辑递归清理空目录（受 `ccKeepIntermediates` 控制）。
- 为避免竞态，可在 `resolveWorkPath` 中记录目录清单，清理时逆序删空。

### 4. 针对 Windows 目标自动补齐可执行扩展名
**现状**：`defaultOutputLocation`/`linkObjects` 不会根据 `ccTargetPlatform` 调整输出文件名。

**影响**：
- Windows 平台默认得到名为 `fluxus_output` 的可执行文件，双击会被当作“未知文件”处理。
- README 中的 `./fibonacci` 示例在 Windows 下同样失效。

**建议**：
- 在未显式指定 `ccOutputPath`、且目标平台为 `Windows_x86_64` 时自动附加 `.exe`。
- 若用户已提供自定义输出但无扩展名，可考虑给出提示或提供 `--force-windows-suffix` 选项。

### 5. 统一示例与文档中的品牌标识
**现状**：绝大多数示例 `.cpp`、调试脚本和验证报告仍以 “HyperStatic/CXX Compiler” 为头注释或命名空间（`examples/python/fibonacci.cpp` 即一例）。

**影响**：
- 新用户难以分辨哪些文件属于 Fluxus 的当前产出，哪些是旧的遗留工件。
- 自动化校验（如对比生成结果）容易因命名空间不匹配而产生噪音。

**建议**：
- 批量更新示例、测试资产和报告中的标识，确保与 `codeGenStage` 默认的 `fluxus` 命名空间一致。
- 为历史工件保留专门的迁移说明，避免误判为当前输出。

---

上述改进集中在易用性与一致性层面，投入较小即可显著提升用户体验，并避免文档与实现之间产生新的认知偏差。