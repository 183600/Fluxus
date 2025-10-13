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
    auto negative_int = -17;
    auto big_int = 10**100;
    auto float_num = 3.14159;
    auto scientific = 1.23e-4;
    auto single_quote = 'Hello';
    auto double_quote = "World";
    auto raw_string = r"原始字符串\n";
    auto f_string = to_str("插值字符串: ") + to_str(int_num) + to_str(", ") + to_str(float_num);
    auto bool_true = true;
    auto bool_false = false;
    auto complex_num = 3 + 4j;
    auto none_value = 0;
    auto bytes_data = b'hello';
    auto bytearray_data = bytearray(b'world');
    return {;
        "int": int_num,;
        "float": float_num,;
        "string": single_quote + " " + double_quote,;
        "bool": bool_true,;
        "complex": complex_num,;
        "none": none_value,;
        "bytes": bytes_data,;
        "bytearray": bytearray_data;
    }
    };
}


int container_types() {
    auto empty_list = std::vector<int>{};
    auto simple_list = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    auto mixed_list = std::vector<std::string>{to_str(1), "two", to_str(3.0), to_str(true), to_str(0)};
    auto nested_list = std::vector{std::vector<decltype(1)>{1, 2}, std::vector<decltype(3)>{3, 4}, std::vector<decltype(5)>{5, 6}};
    auto list_comprehension = 0;
    auto empty_tuple = ();
    auto simple_tuple = (1, 2, 3);
    auto single_element_tuple = (1,);
    auto empty_dict = {};
    auto simple_dict = std::unordered_map<std::string, std::string>{{"name", "Alice"}, {"age", to_str(30)}, {"city", "New York"}};
    auto dict_comprehension = std::unordered_map<std::string, int>{{"x", 0}};
    auto empty_set = set();
    auto simple_set = {1, 2, 3, 4, 5};
    auto set_comprehension = 0;
    auto frozen_set = frozenset(std::vector<decltype(1)>{1, 2, 3, 4, 5});
    return {;
        "list": simple_list,;
        "tuple": simple_tuple,;
        "dict": simple_dict,;
        "set": simple_set,;
        "frozenset": frozen_set;
    }
    };
}


int control_flow() {
    auto x = 10;
    if (x > 10) {
        auto result = "大于10";
    }
    else if (x == 10) {
        auto result = "等于10";
    }
    else {
        auto result = "小于10";
    }
    auto sum_result = 0;
    for (int i = 1; i < 11; ++i) {
        sum_result += i;
    }
    auto count = 0;
    while (count < 5) {
        count += 1;
    }
    for (int i = 0; i < 10; ++i) {
        if (i == 3) {
            continue;
        }
        if (i == 7) {
            break;
        }
    }
    auto matrix = std::vector{std::vector<decltype(1)>{1, 2, 3}, std::vector<decltype(4)>{4, 5, 6}, std::vector<decltype(7)>{7, 8, 9}};
    auto flattened = std::vector<int>{};
    for (auto row : matrix) {
        for (auto item : row) {
            flattened.push_back(item);
        }
    }
    auto max_value = (5 > 3 ? 10 : 3);
    return {;
        "if_result": result,;
        "sum": sum_result,;
        "count": count,;
        "flattened": flattened,;
        "max_value": max_value;
    }
    };
}


std::string functions_demo() {
    std::function<int()> add = [&]() {
        return a + b;
    };
    std::function<int()> greet = [&]() {
        return to_str("Hello, ") + to_str(name) + to_str("!");
    };
    std::function<int()> sum_all = [&]() {
        return sum(args);
    };
    auto square = [](auto x) { return  x**2; };
    std::function<int()> factorial = [&]() {
        if (n <= 1) {
            return 1;
        }
        return n * factorial(n - 1);
    };
    auto result1 = add(5, 3);
    auto result2 = greet("Alice");
    auto result3 = sum_all(1, 2, 3, 4, 5);
    auto result4 = square(5);
    auto result5 = factorial(5);
    return {;
        "add": result1,;
        "greet": result2,;
        "sum_all": result3,;
        "square": result4,;
        "factorial": result5;
    }
    };
}


int classes_demo() {
    class Animal { public: Animal() = default; };
    class Dog { public: Dog() = default; };
    auto animal = Animal("Generic Animal");
    auto dog = Dog("Rex", "German Shepherd");
    return {;
        "animal": animal.speak(),;
        "dog": dog.speak();
    }
    };
}


int exceptions_demo() {
    bool __fluxus_exc=false;
    {
        auto result = 0;
if ((0) == 0) { __fluxus_exc = true; result = 0; }
else { result = ((1.0*(10))/(0)); }

    }
    if (__fluxus_exc) {
        auto result = "除零错误";
    }
    return {;
        "division_result": result;
    }
    };
}


int comprehensions() {
    auto squares = 0;
    auto even_squares = 0;
    auto square_dict = std::unordered_map<std::string, int>{{"x", 0}};
    auto square_set = 0;
    auto matrix = std::vector{std::vector<decltype(1)>{1, 2, 3}, std::vector<decltype(4)>{4, 5, 6}, std::vector<decltype(7)>{7, 8, 9}};
    auto flattened = 0;
    auto sum_of_squares = sum(std::vector<int>{});
    return {;
        "squares": squares,;
        "even_squares": even_squares,;
        "square_dict": square_dict,;
        "square_set": square_set,;
        "flattened": flattened,;
        "sum_of_squares": sum_of_squares;
    }
    };
}


int main() {
    bool __fluxus_exc=false;
    std::cout << "开始测试Python语法特性..." << std::endl;
    auto results = {};
    {
        auto results["basic_types"] = basic_types();
        std::cout << "✓ 基本数据类型测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 基本数据类型测试失败: ") << to_str(e) << std::endl;
        auto results["basic_types"] = 0;
    }
    {
        auto results["container_types"] = container_types();
        std::cout << "✓ 容器类型测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 容器类型测试失败: ") << to_str(e) << std::endl;
        auto results["container_types"] = 0;
    }
    {
        auto results["control_flow"] = control_flow();
        std::cout << "✓ 控制流测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 控制流测试失败: ") << to_str(e) << std::endl;
        auto results["control_flow"] = 0;
    }
    {
        auto results["functions"] = functions_demo();
        std::cout << "✓ 函数测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 函数测试失败: ") << to_str(e) << std::endl;
        auto results["functions"] = 0;
    }
    {
        auto results["classes"] = classes_demo();
        std::cout << "✓ 类和对象测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 类和对象测试失败: ") << to_str(e) << std::endl;
        auto results["classes"] = 0;
    }
    {
        auto results["exceptions"] = exceptions_demo();
        std::cout << "✓ 异常处理测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 异常处理测试失败: ") << to_str(e) << std::endl;
        auto results["exceptions"] = 0;
    }
    {
        auto results["comprehensions"] = comprehensions();
        std::cout << "✓ 列表推导式测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 列表推导式测试失败: ") << to_str(e) << std::endl;
        auto results["comprehensions"] = 0;
    }
    std::cout << "\n所有测试完成!" << std::endl;
    /* return results; */
    return 0;
}


