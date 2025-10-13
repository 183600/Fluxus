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


int add_three(int a, int b, int c) {
    return a + b + c;
}


std::string greet(std::string name, std::string age) {
    return to_str("Hello ") + to_str(name) + to_str(", you are ") + to_str(age) + to_str(" years old");
}


int main() {
    std::cout << "=== Basic Unpacking ===" << std::endl;
    auto a, b = 1, 2;
    std::cout << to_str("a=") << to_str(a) << to_str(", b=") << to_str(b) << std::endl;
    auto x, y, z = std::vector<decltype(1)>{1, 2, 3};
    std::cout << to_str("x=") << to_str(x) << to_str(", y=") << to_str(y) << to_str(", z=") << to_str(z) << std::endl;
    auto a, b, c = "abc";
    std::cout << to_str("a=") << to_str(a) << to_str(", b=") << to_str(b) << to_str(", c=") << to_str(c) << std::endl;
    std::cout << "\n=== Nested Unpacking ===" << std::endl;
    auto (a, b), (c, d) = std::vector<std::string>{to_str((1, 2)), to_str((3, 4))};
    std::cout << to_str("a=") << to_str(a) << to_str(", b=") << to_str(b) << to_str(", c=") << to_str(c) << to_str(", d=") << to_str(d) << std::endl;
    auto nested_list = std::vector{std::vector<decltype(1)>{1, 2}, std::vector<decltype(3)>{3, {4, 5}}};
    auto [x, y], [z, [w, v]] = nested_list;
    std::cout << to_str("x=") << to_str(x) << to_str(", y=") << to_str(y) << to_str(", z=") << to_str(z) << to_str(", w=") << to_str(w) << to_str(", v=") << to_str(v) << std::endl;
    std::cout << "\n=== Extended Unpacking ===" << std::endl;
    auto first, *rest = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    std::cout << to_str("first=") << to_str(first) << to_str(", rest=") << to_str(rest) << std::endl;
    auto first, *middle, last = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    std::cout << to_str("first=") << to_str(first) << to_str(", middle=") << to_str(middle) << to_str(", last=") << to_str(last) << std::endl;
    auto a, b, *rest = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    std::cout << to_str("a=") << to_str(a) << to_str(", b=") << to_str(b) << to_str(", rest=") << to_str(rest) << std::endl;
    auto *head, tail = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    std::cout << to_str("head=") << to_str(head) << to_str(", tail=") << to_str(tail) << std::endl;
    auto first, *middle, second_last, last = std::vector<decltype(1)>{1, 2, 3, 4, 5, 6};
    std::cout << to_str("first=") << to_str(first) << to_str(", middle=") << to_str(middle) << to_str(", second_last=") << to_str(second_last) << to_str(", last=") << to_str(last) << std::endl;
    std::cout << "\n=== Unpacking Strings ===" << std::endl;
    auto first, *middle, last = "hello";
    std::cout << to_str("first=") << to_str(first) << to_str(", middle=") << to_str(middle) << to_str(", last=") << to_str(last) << std::endl;
    std::cout << "\n=== Unpacking in Function Calls ===" << std::endl;
    auto numbers = std::vector<decltype(1)>{1, 2, 3};
    auto result = add_three(*numbers);
    std::cout << to_str("add_three(*[1,2,3]) = ") << to_str(result) << std::endl;
    auto person = std::unordered_map<std::string, std::string>{{"name", "Alice"}, {"age", to_str(25)}};
    auto message = greet(**person);
    std::cout << to_str("greet(**person) = ") << to_str(message) << std::endl;
    std::cout << "\n=== Multiple Unpacking ===" << std::endl;
    auto list1 = std::vector<decltype(1)>{1, 2};
    auto list2 = std::vector<decltype(3)>{3, 4};
    auto combined = std::vector<decltype(*list1)>{*list1, *list2, 5, 6};
    std::cout << to_str("Combined list: ") << to_str(combined) << std::endl;
    auto dict1 = std::unordered_map<std::string, int>{{"a", 1}, {"b", 2}};
    auto dict2 = std::unordered_map<std::string, int>{{"c", 3}, {"d", 4}};
    auto combined_dict = {**dict1, **dict2};
    std::cout << to_str("Combined dict: ") << to_str(combined_dict) << std::endl;
    std::cout << "\n=== Unpacking in Comprehensions ===" << std::endl;
    auto pairs = std::vector<std::string>{to_str((1, 2)), to_str((3, 4)), to_str((5, 6))};
    auto sums = 0;
    std::cout << to_str("Sums: ") << to_str(sums) << std::endl;
    std::cout << "\n=== Unpacking with enumerate ===" << std::endl;
    auto words = std::vector<std::string>{"hello", "world", "python"};
    for (size_t i = 0; i < words.size(); ++i) {
        auto word = words[i];
    }
    std::cout << "\n=== Unpacking with zip ===" << std::endl;
    auto names = std::vector<std::string>{"Alice", "Bob", "Charlie"};
    auto ages = std::vector<decltype(25)>{25, 30, 35};
    for (size_t __zip_i = 0; __zip_i < std::min({names.size(), ages.size()}); ++__zip_i) {
        auto name = names[__zip_i];
        auto age = ages[__zip_i];
    }
    std::cout << "\n=== Unpacking in For Loops ===" << std::endl;
    auto data = std::vector<std::string>{(1, "a"), (2, "b"), (3, "c")};
    for (auto [num, char] : data) {
    }
    std::cout << "\n=== Ignoring Values ===" << std::endl;
    auto a, _, c = std::vector<decltype(1)>{1, 2, 3};
    std::cout << to_str("a=") << to_str(a) << to_str(", c=") << to_str(c) << to_str(" (ignored middle value)") << std::endl;
    auto first, *_, last = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    std::cout << to_str("first=") << to_str(first) << to_str(", last=") << to_str(last) << to_str(" (ignored middle values)") << std::endl;
    std::cout << "\n=== All unpacking tests completed ===" << std::endl;
    return 0;
}

