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


class MyClass {
    public:
        MyClass() = default;
        void placeholder();
    public:
        // generated placeholder fields
};


void my_func() {
    /* pass */
}


int main() {
    std::cout << "=== Type Conversion ===" << std::endl;
    std::cout << to_str("int('123'): ") << to_str(to_int('123')) << std::endl;
    std::cout << to_str("int(45.67): ") << to_str(to_int(45.67)) << std::endl;
    std::cout << to_str("int('1010', 2): ") << to_str(to_int('1010', 2)) << std::endl;
    std::cout << to_str("int('FF', 16): ") << to_str(to_int('FF', 16)) << std::endl;
    std::cout << to_str("float('3.14'): ") << to_str(to_float('3.14')) << std::endl;
    std::cout << to_str("float(5): ") << to_str(to_float(5)) << std::endl;
    std::cout << to_str("str(123): ") << to_str(to_str(123)) << std::endl;
    std::cout << to_str("str(3.14): ") << to_str(to_str(3.14)) << std::endl;
    std::cout << to_str("bool(0): ") << to_str(bool(0)) << std::endl;
    std::cout << to_str("bool(1): ") << to_str(bool(1)) << std::endl;
    std::cout << to_str("bool(''): ") << to_str(bool('')) << std::endl;
    std::cout << to_str("bool('hello'): ") << to_str(bool('hello')) << std::endl;
    std::cout << to_str("bool([]): ") << to_str(bool([])) << std::endl;
    std::cout << to_str("bool([1]): ") << to_str(bool(std::vector<decltype(1)>{1})) << std::endl;
    std::cout << "\n=== Math Functions ===" << std::endl;
    std::cout << to_str("abs(-5): ") << to_str(abs(-5)) << std::endl;
    std::cout << to_str("abs(-3.14): ") << to_str(abs(-3.14)) << std::endl;
    std::cout << to_str("round(3.14159): ") << to_str(round(3.14159)) << std::endl;
    std::cout << to_str("round(3.14159, 2): ") << to_str(round(3.14159, 2)) << std::endl;
    std::cout << to_str("round(3.5): ") << to_str(round(3.5)) << std::endl;
    std::cout << to_str("pow(2, 3): ") << to_str(pow(2, 3)) << std::endl;
    std::cout << to_str("pow(2, 3, 5): ") << to_str(pow(2, 3, 5)) << std::endl;
    std::cout << "\n=== Sequence Functions ===" << std::endl;
    auto numbers = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    std::cout << to_str("len(numbers): ") << to_str(len(numbers)) << std::endl;
    std::cout << to_str("min(numbers): ") << to_str(min(numbers)) << std::endl;
    std::cout << to_str("max(numbers): ") << to_str(max(numbers)) << std::endl;
    std::cout << to_str("sum(numbers): ") << to_str(sum(numbers)) << std::endl;
    std::cout << to_str("min('hello'): ") << to_str(min('hello')) << std::endl;
    std::cout << to_str("max('hello'): ") << to_str(max('hello')) << std::endl;
    std::cout << "\n=== sorted and reversed ===" << std::endl;
    auto unsorted = std::vector<decltype(3)>{3, 1, 4, 1, 5, 9, 2, 6};
    std::cout << to_str("sorted(unsorted): ") << to_str(sorted(unsorted)) << std::endl;
    std::cout << to_str("sorted(unsorted, reverse=True): ") << to_str(sorted(unsorted, reverse=true)) << std::endl;
    auto text = "hello";
    std::cout << to_str("list(reversed(text)): ") << to_str(list(reversed(text))) << std::endl;
    std::cout << to_str("list(reversed([1,2,3])): ") << to_str(list(reversed(std::vector<decltype(1)>{1, 2, 3}))) << std::endl;
    std::cout << "\n=== range ===" << std::endl;
    std::cout << to_str("list(range(5)): ") << to_str(list(range(5))) << std::endl;
    std::cout << to_str("list(range(2, 8)): ") << to_str(list(range(2, 8))) << std::endl;
    std::cout << to_str("list(range(0, 10, 2)): ") << to_str(list(range(0, 10, 2))) << std::endl;
    std::cout << to_str("list(range(10, 0, -1)): ") << to_str(list(range(10, 0, -1))) << std::endl;
    std::cout << "\n=== enumerate ===" << std::endl;
    auto fruits = std::vector<std::string>{'apple', 'banana', 'cherry'};
    for (size_t i = 0; i < fruits.size(); ++i) {
        auto fruit = fruits[i];
    }
    for (size_t i = 0; i < fruits, start=1.size(); ++i) {
        auto fruit = fruits, start=1[i];
    }
    std::cout << "\n=== zip ===" << std::endl;
    auto names = std::vector<std::string>{'Alice', 'Bob', 'Charlie'};
    auto ages = std::vector<decltype(25)>{25, 30, 35};
    auto cities = std::vector<std::string>{'NYC', 'LA', 'SF'};
    for (size_t __zip_i = 0; __zip_i < std::min({names.size(), ages.size(), cities.size()}); ++__zip_i) {
        auto name = names[__zip_i];
        auto age = ages[__zip_i];
        auto city = cities[__zip_i];
    }
    auto zipped = list(zip(names, ages));
    std::cout << to_str("zipped: ") << to_str(zipped) << std::endl;
    std::cout << "\n=== map ===" << std::endl;
    auto numbers = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    auto squared = list(map(lambda x: x**2, numbers));
    std::cout << to_str("squared: ") << to_str(squared) << std::endl;
    auto words = std::vector<std::string>{'hello', 'world', 'python'};
    auto upper = list(map(str.upper, words));
    std::cout << to_str("upper: ") << to_str(upper) << std::endl;
    std::cout << "\n=== filter ===" << std::endl;
    auto numbers = std::vector<decltype(1)>{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    auto even = list(filter(lambda x: x % 2 == 0, numbers));
    std::cout << to_str("even: ") << to_str(even) << std::endl;
    auto words = std::vector<std::string>{'apple', 'banana', 'cherry', 'avocado'};
    auto a_words = list(filter(lambda w: w.startswith('a'), words));
    std::cout << to_str("words starting with 'a': ") << to_str(a_words) << std::endl;
    std::cout << "\n=== all and any ===" << std::endl;
    std::cout << to_str("all([True, True, True]): ") << to_str(all(std::vector<decltype(true)>{true, true, true})) << std::endl;
    std::cout << to_str("all([True, False, True]): ") << to_str(all(std::vector<decltype(true)>{true, false, true})) << std::endl;
    std::cout << to_str("all([]): ") << to_str(all([])) << std::endl;
    std::cout << to_str("any([False, False, False]): ") << to_str(any(std::vector<decltype(false)>{false, false, false})) << std::endl;
    std::cout << to_str("any([False, True, False]): ") << to_str(any(std::vector<decltype(false)>{false, true, false})) << std::endl;
    std::cout << to_str("any([]): ") << to_str(any([])) << std::endl;
    auto numbers = std::vector<decltype(2)>{2, 4, 6, 8};
    std::cout << to_str("all even: ") << to_str(0) << std::endl;
    std::cout << "\n=== type and isinstance ===" << std::endl;
    auto x = 5;
    std::cout << to_str("type(5): ") << to_str(type(x)) << std::endl;
    std::cout << to_str("isinstance(5, int): ") << to_str(true) << std::endl;
    std::cout << to_str("isinstance(5, (int, float)): ") << to_str(true) << std::endl;
    auto s = "hello";
    std::cout << to_str("type('hello'): ") << to_str(type(s)) << std::endl;
    std::cout << to_str("isinstance('hello', str): ") << to_str(true) << std::endl;
    std::cout << "\n=== callable ===" << std::endl;
    std::cout << to_str("callable(my_func): ") << to_str(callable(my_func)) << std::endl;
    std::cout << to_str("callable(5): ") << to_str(callable(5)) << std::endl;
    std::cout << to_str("callable(lambda x: x): ") << to_str(callable(lambda x) << std::endl;
    std::cout << "\n=== id ===" << std::endl;
    auto a = std::vector<decltype(1)>{1, 2, 3};
    auto b = a;
    auto c = std::vector<decltype(1)>{1, 2, 3};
    std::cout << to_str("id(a) == id(b): ") << to_str(id(a) == id(b)) << std::endl;
    std::cout << to_str("id(a) == id(c): ") << to_str(id(a) == id(c)) << std::endl;
    std::cout << "\n=== divmod ===" << std::endl;
    auto quotient, remainder = divmod(17, 5);
    std::cout << to_str("divmod(17, 5): ") << to_str(quotient) << to_str(", ") << to_str(remainder) << std::endl;
    std::cout << "\n=== chr and ord ===" << std::endl;
    std::cout << to_str("ord('A'): ") << to_str(ord('A')) << std::endl;
    std::cout << to_str("chr(65): ") << to_str(chr(65)) << std::endl;
    std::cout << to_str("ord('a'): ") << to_str(ord('a')) << std::endl;
    std::cout << to_str("chr(97): ") << to_str(chr(97)) << std::endl;
    std::cout << "\n=== hex, oct, bin ===" << std::endl;
    std::cout << to_str("hex(255): ") << to_str(hex(255)) << std::endl;
    std::cout << to_str("oct(8): ") << to_str(oct(8)) << std::endl;
    std::cout << to_str("bin(10): ") << to_str(bin(10)) << std::endl;
    std::cout << "\n=== Collection Constructors ===" << std::endl;
    std::cout << to_str("list('hello'): ") << to_str(list('hello')) << std::endl;
    std::cout << to_str("tuple([1,2,3]): ") << to_str(tuple(std::vector<decltype(1)>{1, 2, 3})) << std::endl;
    std::cout << to_str("set([1,2,2,3,3]): ") << to_str(set(std::vector<decltype(1)>{1, 2, 2, 3, 3})) << std::endl;
    std::cout << to_str("dict([('a',1), ('b',2)]): ") << to_str(dict(std::vector<std::string>{('a', 1), ('b', 2)})) << std::endl;
    std::cout << "\n=== Unzipping ===" << std::endl;
    auto pairs = std::vector<std::string>{(1, 'a'), (2, 'b'), (3, 'c')};
    auto numbers, letters = zip(*pairs);
    std::cout << to_str("numbers: ") << to_str(numbers) << std::endl;
    std::cout << to_str("letters: ") << to_str(letters) << std::endl;
    std::cout << "\n=== Attribute Functions ===" << std::endl;
    auto obj = MyClass();
    std::cout << to_str("hasattr(obj, 'x'): ") << to_str(hasattr(obj, 'x')) << std::endl;
    std::cout << to_str("getattr(obj, 'x'): ") << to_str(getattr(obj, 'x')) << std::endl;
    setattr(obj, 'y', 20);
    std::cout << to_str("getattr(obj, 'y'): ") << to_str(getattr(obj, 'y')) << std::endl;
    std::cout << to_str("getattr(obj, 'z', 'default'): ") << to_str(getattr(obj, 'z', 'default')) << std::endl;
    std::cout << "\n=== All builtin function tests completed ===" << std::endl;
    return 0;
}

