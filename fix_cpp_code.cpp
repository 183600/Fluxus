#include <iostream>
#include <string>
#include <vector>
#include <optional>
#include <unordered_map>
#include <tuple>
#include <utility>
#include <type_traits>
#include <functional>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <algorithm>
#include <typeinfo>
#include <cstring>

using namespace std;

// helpers
template<typename T> static inline std::ostream& operator<<(std::ostream& os, const std::vector<T>& v);
template<typename T> static inline std::string to_str(const T& v){ std::ostringstream os; os<<v; return os.str(); }
static inline std::string to_str(const std::string& s){ return s; }
static inline std::string to_str(const char* s){ return std::string(s); }
static inline std::string to_str(double v){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1)<<v; return os.str(); }
static inline std::string to_str(float v){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1)<<v; return os.str(); }
template<typename T> static inline int to_int(const T& v){ if constexpr (std::is_convertible_v<T,std::string>) return std::stoi(v); else return static_cast<int>(v); }
template<typename T> static inline float to_float(const T& v){ if constexpr (std::is_convertible_v<T,std::string>) return std::stof(v); else return static_cast<float>(v); }
template<typename C> static inline int sum(const C& c){ int s=0; for(const auto& e: c) s += e; return s; }
template<typename T> static inline int sum(std::initializer_list<T> c){ int s=0; for(const auto& e: c) s += e; return s; }
template<typename T> static inline size_t len(const T& c){ return c.size(); }
static inline size_t len(const char* s){ return std::char_traits<char>::length(s); }
template<typename K, typename V> static inline std::string to_str(const std::unordered_map<K,V>&){ return std::string("{...}"); }
// minimal ABC base to support 'class X : public ABC'
struct ABC {};
// minimal math namespace replacement for 'math.pi'
struct __fluxus_math { static constexpr double pi = 3.141592653589793; static inline double sqrt(double x){ return std::sqrt(x); } } math;
// minimal sys module stub
namespace sys { static std::vector<std::string> argv; }
namespace asyncio { template<typename F> static inline void run(F f){ f(); } }

// vector to string conversion for printing
template<typename T>
static inline std::string vec_to_str(const std::vector<T>& v) {
    std::ostringstream os;
    os << "[";
    for (size_t i = 0; i < v.size(); ++i) {
        if (i > 0) os << ", ";
        if constexpr (std::is_same_v<T, std::string>) {
            os << "'" << v[i] << "'";
        } else {
            os << v[i];
        }
    }
    os << "]";
    return os.str();
}

// nested vector to string
template<typename T>
static inline std::string vec_to_str(const std::vector<std::vector<T>>& v) {
    std::ostringstream os;
    os << "[";
    for (size_t i = 0; i < v.size(); ++i) {
        if (i > 0) os << ", ";
        os << vec_to_str(v[i]);
    }
    os << "]";
    return os.str();
}

// stream operator for vectors
template<typename T>
static inline std::ostream& operator<<(std::ostream& os, const std::vector<T>& v) {
    os << vec_to_str(v);
    return os;
}
template<typename K, typename V> static inline std::ostream& operator<<(std::ostream& os, const std::unordered_map<K,V>&){ os << "{...}"; return os; }
template<typename K, typename V> static inline auto py_items(const std::unordered_map<K,V>& m){ std::vector<std::pair<K,V>> v; v.reserve(m.size()); for(const auto& kv: m){ v.emplace_back(kv.first, kv.second);} return v; }


std::string fix_cpp_code(std::string cpp_file_path) {
    /* docstring */
    if (not os.path.exists(cpp_file_path)) {
        std::cout << to_str("错误：文件 ") << to_str(cpp_file_path) << to_str(" 不存在") << std::endl;
        return false;
    }
    {
        auto content = f.read();
    }
    auto original_content = content;
    std::cout << "=== 开始修复C++代码 ===" << std::endl;
    std::cout << to_str("原始文件: ") << to_str(cpp_file_path) << std::endl;
    std::cout << to_str("原始文件大小: ") << to_str(len(content)) << to_str(" 字符") << std::endl;
    std::function<int()> replace_expr_comments = [&]() {
        auto expr_content = match.group(1);
        std::cout << to_str("发现表达式注释: /* expr */") << to_str(expr_content) << std::endl;
        if (expr_content.startswith('("') and expr_content.endswith('")')) {
            auto string_content = expr_content[1:-1];
            std::cout << to_str("  -> 转换为输出语句: std::cout << \\\"") << to_str(string_content) << to_str("\\\" << std::endl") << std::endl;
            return to_str("std::cout << \"") + to_str(string_content) + to_str("\" << std::endl;");
        }
        std::cout << to_str("  -> 转换为通用输出: std::cout << ") << to_str(expr_content) << to_str(" << std::endl") << std::endl;
        return to_str("std::cout << ") + to_str(expr_content) + to_str(" << std::endl;");
    };
    auto pattern = r'/\* expr \*/\((.*?)\)';
    auto fixed_content = re.sub(pattern, replace_expr_comments, content);
    if ('#include <iostream>' not in fixed_content) {
        auto fixed_content = fixed_content.replace('#include <string>', '#include <string>\n#include <iostream>');
        std::cout << "添加缺失的 #include <iostream>" << std::endl;
    }
    std::function<int()> remove_invalid_temp_declarations = [&]() {
        auto lines = content.split('\n');
        auto cleaned_lines = std::vector<int>{};
        auto in_main_function = false;
        auto main_brace_count = 0;
        for (auto line : lines) {
            auto stripped = line.strip();
            if ('int main()' in stripped) {
                auto in_main_function = true;
                cleaned_lines.push_back(line);
                continue;
            }
            if (in_main_function) {
                if ('{' in stripped) {
                    main_brace_count += 1;
                }
                if ('}' in stripped) {
                    main_brace_count -= 1;
                    if (main_brace_count == 0) {
                        auto in_main_function = false;
                    }
                }
                if (stripped != 'void __temp;') {
                    cleaned_lines.push_back(line);
                }
            }
            else {
                if (stripped != 'void __temp;') {
                    cleaned_lines.push_back(line);
                }
            }
        }
        return '\n'.join(cleaned_lines);
    };
    auto fixed_content = remove_invalid_temp_declarations(fixed_content);
    std::cout << "移除了无效的 void __temp; 声明" << std::endl;
    if ('int main()' in fixed_content and 'return 0;' not in fixed_content) {
        auto fixed_content = fixed_content.replace('}', '\n    return 0;\n}');
        std::cout << "添加缺失的 return 0;" << std::endl;
    }
    auto changes_made = fixed_content != original_content;
    std::cout << to_str("是否进行了修改: ") << to_str((changes_made ? '是' : '否')) << std::endl;
    if (changes_made) {
        auto backup_path = cpp_file_path + '.backup';
        {
            f.write(original_content);
        }
        std::cout << to_str("原始文件已备份到: ") << to_str(backup_path) << std::endl;
        {
            f.write(fixed_content);
        }
        std::cout << "=== C++代码修复完成 ===" << std::endl;
        std::cout << to_str("修复后的文件: ") << to_str(cpp_file_path) << std::endl;
        std::cout << to_str("修复后文件大小: ") << to_str(len(fixed_content)) << to_str(" 字符") << std::endl;
        std::cout << "\n=== 修复后的代码 ===" << std::endl;
        std::cout << fixed_content << std::endl;
        std::cout << "======================" << std::endl;
        return true;
    }
    else {
        std::cout << "未发现需要修复的问题" << std::endl;
        return false;
    }
}


bool compile_and_test(std::string cpp_file_path) {
    /* docstring */
    auto exe_path = cpp_file_path.replace('.cpp', '_compiled');
    auto compile_cmd = to_str("g++ -o ") + to_str(exe_path) + to_str(" ") + to_str(cpp_file_path);
    std::cout << to_str("\\n=== 编译C++代码 ===") << std::endl;
    std::cout << to_str("编译命令: ") << to_str(compile_cmd) << std::endl;
    auto result = os.system(compile_cmd);
    if (result != 0) {
        std::cout << to_str("编译失败，退出码: ") << to_str(result) << std::endl;
        return false;
    }
    std::cout << "编译成功！" << std::endl;
    std::cout << to_str("\\n=== 运行C++程序 ===") << std::endl;
    std::cout << to_str("运行命令: ") << to_str(exe_path) << std::endl;
    auto result = os.system(exe_path);
    if (result != 0) {
        std::cout << to_str("运行失败，退出码: ") << to_str(result) << std::endl;
        return false;
    }
    std::cout << "运行成功！" << std::endl;
    return true;
}


int main() {
    /* docstring */
    if (len(sys::argv) != 2) {
        std::cout << "用法: python3 fix_cpp_code.py <cpp文件路径>" << std::endl;
        /* return (1; */
    }
    auto cpp_file = sys::argv[1];
    if (fix_cpp_code(cpp_file)) {
        compile_and_test(cpp_file);
    }
    else {
        std::cout << "无需修复，直接编译和测试" << std::endl;
        compile_and_test(cpp_file);
    }
    return 0;
}


