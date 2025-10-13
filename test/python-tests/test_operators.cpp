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


int main() {
    std::cout << "=== Bitwise Operators ===" << std::endl;
    auto a = 60;
    auto b = 13;
    std::cout << to_str("a & b = ") << to_str(a & b) << std::endl;
    std::cout << to_str("a | b = ") << to_str(a | b) << std::endl;
    std::cout << to_str("a ^ b = ") << to_str(a ^ b) << std::endl;
    std::cout << to_str("~a = ") << to_str(~a) << std::endl;
    std::cout << to_str("a << 2 = ") << to_str(a << 2) << std::endl;
    std::cout << to_str("a >> 2 = ") << to_str(a >> 2) << std::endl;
    std::cout << "\n=== Identity Operators ===" << std::endl;
    auto x = std::vector<decltype(1)>{1, 2, 3};
    auto y = x;
    auto z = std::vector<decltype(1)>{1, 2, 3};
    std::cout << to_str("x is y: ") << to_str(x is y) << std::endl;
    std::cout << to_str("x is z: ") << to_str(x is z) << std::endl;
    std::cout << to_str("x is not z: ") << to_str(x is not z) << std::endl;
    std::cout << to_str("x == z: ") << to_str(x == z) << std::endl;
    auto val1 = 0;
    auto val2 = 0;
    std::cout << to_str("val1 is None: ") << to_str(val1 is 0) << std::endl;
    std::cout << to_str("val1 is not None: ") << to_str(val1 is not 0) << std::endl;
    std::cout << "\n=== Ternary Operator ===" << std::endl;
    auto age = 18;
    auto status = (age >= 18 ? "adult" : "minor");
    std::cout << to_str("Status: ") << to_str(status) << std::endl;
    auto num = 5;
    auto result = (num > 0 ? "positive" : "negative" if num < 0 else "zero");
    std::cout << to_str("Number is: ") << to_str(result) << std::endl;
    auto x = 10;
    auto y = 20;
    auto max_val = (x > y ? x : y);
    std::cout << to_str("Max value: ") << to_str(max_val) << std::endl;
    std::cout << "\n=== Walrus Operator ===" << std::endl;
    if ((n := 10) > 5) {
    }
    auto numbers = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    auto doubled = 0;
    std::cout << to_str("Doubled values > 4: ") << to_str(doubled) << std::endl;
    std::cout << "\n=== Comparison Chaining ===" << std::endl;
    auto x = 5;
    std::cout << to_str("1 < x < 10: ") << to_str(1 < x < 10) << std::endl;
    std::cout << to_str("1 < x < 4: ") << to_str(1 < x < 4) << std::endl;
    std::cout << to_str("x > 0 and x < 10: ") << to_str(x > 0 and x < 10) << std::endl;
    auto a, b, c = 1, 2, 3;
    std::cout << to_str("a < b < c: ") << to_str(a < b < c) << std::endl;
    std::cout << to_str("a < b and b < c: ") << to_str(a < b and b < c) << std::endl;
    std::cout << "\n=== All tests completed ===" << std::endl;
    return 0;
}

