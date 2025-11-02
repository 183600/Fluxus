# Fluxus 项目改进分析

## 摘要
- C++ 代码生成器出现了两套并行实现（`Fluxus.CodeGen.CPP` 与 `Fluxus.CodeGen.CPP.*`），当前构建与测试只覆盖前者，导致类型定义与状态管理重复且逐渐分叉。
- Python/Go 代码生成在遇到未实现语法时会静默丢弃语句或生成占位常量，默认配置下难以及时暴露功能缺口，存在误编译风险。
- 编译管线的分析/优化阶段（类型推断、逃逸、所有权、形状分析等）仍停留在记录警告/统计，未把结果回灌到 AST 或代码生成，错失潜在优化收益。

## 重点改进方向

### 1. 清理并统一 C++ 代码生成器实现
- **观测**：`Fluxus.CodeGen.CPP` 在 ~L65-L213 内重新定义了 `CppGenConfig`、`CppGenState`、`CppUnit` 等类型，并直接运行自己的 `StateT + Writer` 单子；与此同时，子目录 `Fluxus/CodeGen/CPP/Types.hs`、`Monad.hs`、`Emit.hs`、`Go.hs`（例如 L1-L153、L46-L140 等）提供了另一套带有 `ExceptT`、`cgsPendingStmts` 等能力的实现，但在 `fluxus.cabal` 中既没有暴露，也没有被主模块引用。
- **风险**：两套实现持续分叉（如 `CPP.hs` 中新增的 `cgcStrictMode`、`cgsFatalErrors` 字段，与 `CPP/Monad.hs` 中的 `cgsPendingStmts`）时，CLI 与测试仍只走旧实现，而新文件逐渐演化成「僵尸代码」，维护成本和出错概率显著上升。
- **建议**：明确保留一套实现：要么让 `Fluxus.CodeGen.CPP` 复用 `CPP.Types`/`CPP.Monad` 并在 Cabal 中暴露所需模块，要么删除失效的子目录；同时更新测试确保引用统一入口，避免重复维护。

### 2. 未支持语句被静默吞掉，导致潜在误编译
- **观测**：`generatePythonStmt`（`Fluxus.CodeGen.CPP` ~L454-L507）在遇到 `PyWith`、复杂解构赋值等语句时调用 `reportNotImplemented` 后直接返回 `cppNoop`，默认 `strictMode = False` 仅发出警告，最终生成的 C++ 将缺失对应逻辑。类似地，`generateGoDecl`/`generateGoExpr`（同文件 ~L1102、~L1426）用 `CppLiteral 0` 或 `CppCommentDecl` 作为占位。
- **风险**：编译器表面成功输出 C++，但运行语义被悄悄丢弃，对用户而言属于 silent failure。
- **建议**：将这些分支切换为显式错误（抛出 `CppNotImplemented` / `CppUnsupported`），或生成调用运行时 fallback（例如注入 `PyRuntime::raiseNotImplemented`），并考虑默认启用严格模式；同时补充针对常见语法的测试，确保一旦遇到未覆盖语法立即失败。

### 3. 分析/优化阶段缺乏回灌机制
- **观测**：`typeInferenceStage` 与 `optimizationStage`（`Fluxus.Compiler.Driver` ~L508-L610）遍历 `CommonExpr` 并记录 `csTypeEnvironment`、`recordOptimizationStat`、`addWarning`，但最终都 `return ast`，没有更新 AST 或让下游 `codeGenStage` 获得类型/所有权信息；`csTypeEnvironment` 也仅在同文件内读取一次。
- **风险**：分析 pass 的运行开销换来的只是日志，未被代码生成或运行时利用，真正的类型优化和内存策略无法落地，外界也难以验证其有效性。
- **建议**：为分析结果设计承载结构（例如对 AST 附加注解、扩展 `CppGenState`），并让 `Fluxus.CodeGen.CPP` 在生成变量/函数调用时消费这些信息；若分析尚未 ready，可通过特性旗明确关闭或收窄对外曝光的开关，避免用户误以为优化已生效。

## 其他观察
- `Fluxus.CodeGen.Go`（Python → Go）在 `generateStatement` / `generateParameter` 中将绝大多数类型硬编码为 `int`（~L104-L165），仅能应对极简示例；若该功能面向用户，需要补齐类型/语法映射或在 CLI 中标注为实验性质。
