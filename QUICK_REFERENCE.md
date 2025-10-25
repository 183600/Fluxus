# Python到C++编译快速参考

## 一键验证

```bash
# 最快速的验证方法
bash final_verification.sh
```

## 常用命令

### 编译单个Python文件

```bash
cabal run fluxus -- --python input.py > output.cpp
```

### 完整流程

```bash
# 1. 编译Python到C++
cabal run fluxus -- --python test.py > test.cpp

# 2. 编译C++
g++ -std=c++20 -O2 test.cpp -o test_exe

# 3. 运行
./test_exe
```

### 验证输出一致性

```bash
# 比较Python和C++输出
python3 test.py > py_out.txt
./test_exe > cpp_out.txt
diff py_out.txt cpp_out.txt
```

## 支持的Python特性

| 特性 | 示例 | 状态 |
|------|------|------|
| 打印 | `print(42)` | ✅ |
| 变量 | `x = 10` | ✅ |
| 算术 | `a + b` | ✅ |
| 比较 | `x > 5` | ✅ |
| 条件 | `if x > 5: ...` | ✅ |
| 循环 | `for i in range(10): ...` | ✅ |
| 函数 | `def add(a, b): return a + b` | ✅ |
| 递归 | `factorial(n)` | ✅ |

## 测试脚本

| 脚本 | 用途 | 运行时间 |
|------|------|----------|
| `final_verification.sh` | 快速验证 | ~30秒 |
| `end_to_end_test.sh` | 端到端测试 | ~1分钟 |
| `comprehensive_python_cpp_verification.sh` | 完整测试 | ~2分钟 |
| `auto_test_and_fix.py` | Python测试 | ~1分钟 |

## 故障排除

### 问题: 编译失败

```bash
cabal clean
cabal build
```

### 问题: C++语法错误

```bash
# 查看生成的C++代码
cat output.cpp

# 检查语法
g++ -std=c++20 -fsyntax-only output.cpp
```

### 问题: 输出不匹配

```bash
# 详细比较
python3 test.py
./test_exe
```

## 关键修复

**比较运算符** (已修复)
- 之前: 生成 `// TODO: Implement PyComparison`
- 现在: 正确生成 `if (n <= 1)` 等

## 文档

- `VERIFICATION_README.md` - 完整使用指南
- `PYTHON_CPP_VERIFICATION_REPORT.md` - 详细报告
- `WORK_SUMMARY.md` - 工作总结

## 示例

### 输入 (Python)

```python
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))
```

### 输出 (C++)

```cpp
#include <string>
#include <iostream>

auto factorial(auto n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}

int main() {
    std::cout << factorial(5) << std::endl;
    return 0;
}
```

### 结果

```
Python: 120
C++:    120
✅ 一致！
```
