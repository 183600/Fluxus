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


int basic_types() {
    auto int_num = 42;
    auto float_num = 3.14;
    auto string_var = "Hello";
    auto bool_var = true;
    return {;
        "int": int_num,;
        "float": float_num,;
        "string": string_var,;
        "bool": bool_var;
    }
    };
}


int control_flow() {
    auto x = 10;
    if (x > 5) {
        auto result = "大于5";
    }
    else {
        auto result = "小于等于5";
    }
    auto sum_result = 0;
    for (int i = 1; i < 6; ++i) {
        sum_result += i;
    }
    return {;
        "if_result": result,;
        "sum": sum_result;
    }
    };
}


int functions_demo() {
    std::function<int()> add = [&]() {
        return a + b;
    };
    auto result = add(3, 4);
    return {;
        "add_result": result;
    }
    };
}


int main() {
    bool __fluxus_exc=false;
    std::cout << "开始测试..." << std::endl;
    {
        auto result1 = basic_types();
        std::cout << "✓ 基本数据类型测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 基本数据类型测试失败: ") << to_str(e) << std::endl;
        auto result1 = 0;
    }
    {
        auto result2 = control_flow();
        std::cout << "✓ 控制流测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 控制流测试失败: ") << to_str(e) << std::endl;
        auto result2 = 0;
    }
    {
        auto result3 = functions_demo();
        std::cout << "✓ 函数测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 函数测试失败: ") << to_str(e) << std::endl;
        auto result3 = 0;
    }
    std::cout << "所有测试完成!" << std::endl;
    /* return {; */
        "basic_types": result1,;
        "control_flow": result2,;
        "functions": result3;
    }
    };
    return 0;
}


