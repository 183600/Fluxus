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
    std::cout << "=== Tuple Creation ===" << std::endl;
    auto empty_tuple = ();
    auto single_tuple = (1,);
    auto pair_tuple = (1, 2);
    auto multi_tuple = (1, 2, 3, 4, 5);
    auto mixed_tuple = (1, "hello", 3.14, true);
    std::cout << to_str("Empty tuple: ") << to_str(empty_tuple) << std::endl;
    std::cout << to_str("Single tuple: ") << to_str(single_tuple) << std::endl;
    std::cout << to_str("Pair tuple: ") << to_str(pair_tuple) << std::endl;
    std::cout << to_str("Multi tuple: ") << to_str(multi_tuple) << std::endl;
    std::cout << to_str("Mixed tuple: ") << to_str(mixed_tuple) << std::endl;
    auto tuple_no_parens = 1, 2, 3, 4;
    std::cout << to_str("Tuple without parens: ") << to_str(tuple_no_parens) << std::endl;
    std::cout << "\n=== Tuple Indexing and Slicing ===" << std::endl;
    auto numbers = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
    std::cout << to_str("First element: ") << to_str(numbers[0]) << std::endl;
    std::cout << to_str("Last element: ") << to_str(numbers[-1]) << std::endl;
    std::cout << to_str("Slice [2:5]: ") << to_str(numbers[2) << std::endl;
    std::cout << to_str("Slice [:3]: ") << to_str(numbers[) << std::endl;
    std::cout << to_str("Slice [5:]: ") << to_str(numbers[5) << std::endl;
    std::cout << to_str("Slice [::2]: ") << to_str(numbers[) << std::endl;
    std::cout << to_str("Slice [::-1]: ") << to_str(numbers[) << std::endl;
    std::cout << "\n=== Tuple Unpacking ===" << std::endl;
    auto point = (10, 20);
    auto x, y = point;
    std::cout << to_str("x = ") << to_str(x) << to_str(", y = ") << to_str(y) << std::endl;
    auto a, b, c = 1, 2, 3;
    std::cout << to_str("a = ") << to_str(a) << to_str(", b = ") << to_str(b) << to_str(", c = ") << to_str(c) << std::endl;
    auto a, b = b, a;
    std::cout << to_str("After swap: a = ") << to_str(a) << to_str(", b = ") << to_str(b) << std::endl;
    auto nested = ((1, 2), (3, 4));
    auto (a, b), (c, d) = nested;
    std::cout << to_str("Nested unpacking: a=") << to_str(a) << to_str(", b=") << to_str(b) << to_str(", c=") << to_str(c) << to_str(", d=") << to_str(d) << std::endl;
    std::cout << "\n=== Tuple Operations ===" << std::endl;
    auto tuple1 = (1, 2, 3);
    auto tuple2 = (4, 5, 6);
    auto combined = tuple1 + tuple2;
    std::cout << to_str("Concatenation: ") << to_str(combined) << std::endl;
    auto repeated = tuple1 * 3;
    std::cout << to_str("Repetition: ") << to_str(repeated) << std::endl;
    std::cout << to_str("Length of tuple1: ") << to_str(len(tuple1)) << std::endl;
    std::cout << to_str("2 in tuple1: ") << to_str(2 in tuple1) << std::endl;
    std::cout << to_str("10 in tuple1: ") << to_str(10 in tuple1) << std::endl;
    std::cout << "\n=== Tuple Methods ===" << std::endl;
    auto numbers = (1, 2, 3, 2, 4, 2, 5);
    std::cout << to_str("Count of 2: ") << to_str(numbers.count(2)) << std::endl;
    std::cout << to_str("Index of 3: ") << to_str(numbers.index(3)) << std::endl;
    std::cout << to_str("Index of first 2: ") << to_str(numbers.index(2)) << std::endl;
    std::cout << "\n=== Tuples as Dictionary Keys ===" << std::endl;
    auto locations = {;
    };
    std::cout << to_str("Location at (0, 0): ") << to_str(locations[(0, 0)]) << std::endl;
    std::cout << to_str("Location at (1, 0): ") << to_str(locations[(1, 0)]) << std::endl;
    std::cout << "\n=== Nested Tuples ===" << std::endl;
    auto matrix = (;
    );
    std::cout << to_str("Matrix: ") << to_str(matrix) << std::endl;
    std::cout << to_str("Element [1][2]: ") << to_str(matrix[1][2]) << std::endl;
    std::cout << "\n=== Tuple Comparison ===" << std::endl;
    auto t1 = (1, 2, 3);
    auto t2 = (1, 2, 3);
    auto t3 = (1, 2, 4);
    std::cout << to_str("t1 == t2: ") << to_str(t1 == t2) << std::endl;
    std::cout << to_str("t1 == t3: ") << to_str(t1 == t3) << std::endl;
    std::cout << to_str("t1 < t3: ") << to_str(t1 < t3) << std::endl;
    std::cout << "\n=== Type Conversion ===" << std::endl;
    auto my_list = std::vector<decltype(1)>{1, 2, 3, 4};
    auto my_tuple = tuple(my_list);
    std::cout << to_str("List to tuple: ") << to_str(my_tuple) << std::endl;
    auto back_to_list = list(my_tuple);
    std::cout << to_str("Tuple to list: ") << to_str(back_to_list) << std::endl;
    std::cout << "\n=== All tuple tests completed ===" << std::endl;
    return 0;
}

